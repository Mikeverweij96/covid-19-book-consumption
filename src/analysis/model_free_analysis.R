library(haven)
library(dplyr)
library(ggplot2)
library(car)
library(readxl)
library(base)
library(data.table)
library(tidyr)
library(tibble)
library("lubridate")
#########################
## MODEL FREE STATS #####
#########################
# load the data
covid_stringency<-fread("../../gen/temp/covid_stringency_prepared.csv") #save the data in a dataframe
user_info<- fread("../../gen/temp/users_cleaned.csv")
all_books <-fread("../../gen/temp/books_cleaned.csv") 

all_books<- all_books %>% select (-V1)
################################################################################
################################################################################
################################################################################
# create an overview of number of books added per week globally 
# define the week in which each book is added (we will aggregate on this varible later on)
all_books$first_day_of_week_added<- floor_date(as.Date(all_books$date_added, "%Y-%m/-%d"), unit="week", week_start = 1)
all_books$first_day_of_week_added <- as.Date(all_books$first_day_of_week_added)


# make sure covid stringency first day of week is stored as date.
covid_stringency$first_day_of_week<-as.Date(covid_stringency$first_day_of_week)

all_books_weekly_added<- all_books %>% group_by(first_day_of_week_added) %>% summarise(total=sum(dummy))

#add the covid stringencies
all_books_weekly_added <- all_books_weekly_added %>% left_join(covid_stringency, by=c("first_day_of_week_added" = "first_day_of_week"))

# fill the NAs with 0
all_books_weekly_added<- all_books_weekly_added %>% replace(is.na(.), 0)

################################################################################
################################################################################
################################################################################
#Summary Statistics

#create a more accurate recent publish variable:
all_books<- all_books %>% mutate(most_recent = ifelse(!is.na(date_pub), as.numeric(difftime(date_added, date_pub, units ="days")), as.numeric(NA)))
all_books <- all_books %>% mutate(most_recent = ifelse(most_recent < 366, 1,0))

#then go over all variables
for (var in 1:ncol(all_books)){
  coll<- all_books[,..var]
  print("##########################")
  print(colnames(coll))
  coll<-as.numeric(unlist(coll))
  print(summary(coll)[4])
  print(summary(coll)[3])
  print(sd(coll, na.rm=TRUE))
  print(min(coll, na.rm=TRUE))
  print(max(coll, na.rm=TRUE))
  print(sum(is.na(coll)))
}

summary(all_books)

for (var in 1:ncol(user_info)){
  coll<- user_info[,..var]
  print("##########################")
  print(colnames(coll))
  coll<-as.numeric(unlist(coll))
  print(summary(coll)[4])
  print(summary(coll)[3])
  print(sd(coll, na.rm=TRUE))
  print(min(coll, na.rm=TRUE))
  print(max(coll, na.rm=TRUE))
  print(sum(is.na(coll)))
}

summary(user_info)


#################################################################################
#################################################################################
################################################################################
#Create an overview of number of active users each month
user_info <- user_info %>% select(-V1)
users_joined_monthly <- user_info %>% group_by(Joined) %>% summarize(join=sum(dummy))
users_joined_monthly$Joined <- as.Date(users_joined_monthly$Joined)
users_last_monthly <- user_info %>% group_by(Last) %>% summarize(leave=sum(dummy))
users_last_monthly$Last <- gsub('28', '01', users_last_monthly$Last)
users_last_monthly$Last<-as.Date(users_last_monthly$Last)

#join the joines and leaves and fill the NA's with zeros
users_active_monthly<- users_joined_monthly %>% full_join(users_last_monthly, by = c("Joined" = "Last"))
users_active_monthly$leave <- ifelse(is.na(users_active_monthly$leave), 0, users_active_monthly$leave)
users_active_monthly$join <- ifelse(is.na(users_active_monthly$join), 0, users_active_monthly$join)

#remove the variables that we will no longer use
remove(users_joined_monthly)
remove(users_last_monthly)


#create the varible to store the number of active users in
users_active_monthly$active<-0

#loop through each month and compute how many users there were active
for (month in 1:nrow(users_active_monthly)){
  if (month ==1){
    users_active_monthly$active[month]<- users_active_monthly$join[month]-users_active_monthly$leave[month]
  }
  if (month>1){
    users_active_monthly$active[month]<-users_active_monthly$active[month-1] +users_active_monthly$join[month]-users_active_monthly$leave[month]
  }
}
# plot the number of active users over time
plot(users_active_monthly$Joined, users_active_monthly$active, type="l")


################################################################################
################################################################################
################################################################################
#now add the number of active users each month month to the weekly overview
all_books_weekly_added$Nr_active_users<-0
for (week in 1:nrow(all_books_weekly_added)){
  month_nr<-month(as.Date(pull(all_books_weekly_added[week,'first_day_of_week_added'])))
  year_nr<-year(as.Date(pull(all_books_weekly_added[week,'first_day_of_week_added'])))
  first_day<-as.Date(paste0(year_nr, "-",month_nr, "-01"))
  all_books_weekly_added[week, 'Nr_active_users'] <-pull(users_active_monthly[which(users_active_monthly$Joined==first_day), 'active'])
}
all_books_weekly_added$added_per_user <- all_books_weekly_added$total/all_books_weekly_added$Nr_active_users

################################################################################
################################################################################
################################################################################
#Operationalization statistics

op_qty_added<- all_books %>% group_by(first_day_of_week_added) %>% summarise(tot= sum(dummy))
summary(op_qty_added)
op_pages_added<- all_books %>% group_by(first_day_of_week_added) %>% summarise(tot= sum(num_pages, na.rm=TRUE))
summary(op_pages_added)
op_days_per_book<- all_books %>% group_by(first_day_of_week_added) %>% summarise(tot= mean(read_time_days, na.rm=TRUE))
summary(op_days_per_book)
op_pages_per_day<- all_books %>% group_by(first_day_of_week_added) %>% summarise(tot= mean(pages_per_day, na.rm=TRUE))
summary(op_pages_per_day)
op_rating_per_book<- all_books %>% group_by(first_day_of_week_added) %>% summarise(tot= mean(`user rating`, na.rm=TRUE))
summary(op_rating_per_book)
op_age_of_book<- all_books %>% group_by(first_day_of_week_added) %>% summarise(tot= mean(age_of_book,na.rm=TRUE))
summary(op_age_of_book)
op_perc_old_book<- all_books %>% group_by(first_day_of_week_added) %>% summarise(tot= mean(nostalgic, na.rm=TRUE))
summary(op_perc_old_book)
op_perc_recent_book<- all_books %>% group_by(first_day_of_week_added) %>% summarise(tot= mean(recent, na.rm=TRUE))
summary(op_perc_recent_book)
op_perc_re_read<- all_books %>% group_by(first_day_of_week_added) %>% summarise(tot= mean((read_count>1), na.rm=TRUE))
summary(op_perc_re_read)
op_pages_per_book<- all_books %>% group_by(first_day_of_week_added) %>% summarise(tot= mean(num_pages, na.rm=TRUE))
summary(op_pages_per_book)
op_ratings_per_book<- all_books %>% group_by(first_day_of_week_added) %>% summarise(tot= mean(num_ratings, na.rm=TRUE))
summary(op_ratings_per_book)

summary(covid_stringency)



summary(all_books_weekly_added$added_per_user)
sd(all_books_weekly_added$added_per_user, na.rm=TRUE)

##############################
sd(op_qty_added$tot, na.rm=TRUE)
sd(op_pages_added$tot, na.rm=TRUE)
sd(op_days_per_book$tot, na.rm=TRUE)
sd(op_pages_per_day$tot, na.rm=TRUE)
sd(op_rating_per_book$tot, na.rm=TRUE)
sd(op_age_of_book$tot, na.rm=TRUE)
sd(op_perc_old_book$tot, na.rm=TRUE)
sd(op_perc_recent_book$tot, na.rm=TRUE)
sd(op_perc_re_read$tot, na.rm=TRUE)
sd(op_pages_per_book$tot, na.rm=TRUE)
sd(op_ratings_per_book$tot, na.rm=TRUE)

sd(covid_stringency$Average, na.rm=TRUE)


################################################################################
################################################################################
################################################################################
# Investigate the differences between different kinds of readers
weekly_added_per_user <- all_books %>% group_by(`reader id`,first_day_of_week_added) %>% summarise(num_books = sum(dummy))


# how we define Experienced Readers:
# active > 1 year
# at least 1 book a month (> 0.03288 books a day)
user_info<- user_info %>% mutate(experienced = ifelse(days_active>364 & books_per_day > 1/12, 1,0))
summary(user_info$experienced)

#what information do we want to add:
info_to_add <- user_info %>% select(user_id, Age, Male, Country, experienced)
weekly_added_per_user <- weekly_added_per_user %>% left_join(info_to_add, by=c("reader id" = "user_id"))
weekly_added_per_user$dummy<-1
remove(info_to_add)


exp_books_weekly_added<- weekly_added_per_user %>% group_by(first_day_of_week_added, experienced) %>% summarise(active_users = sum(dummy), books_added = sum(num_books))

















#find the number of active experienced readers and non-experienced readers each month:
users_joined_monthly <- user_info %>% filter(experienced ==1) %>% group_by(Joined) %>% summarize(join=sum(dummy))
users_joined_monthly$Joined <- as.Date(users_joined_monthly$Joined)
users_last_monthly <- user_info %>% filter(experienced == 1) %>% group_by(Last) %>% summarize(leave=sum(dummy))
users_last_monthly$Last <- gsub('28', '01', users_last_monthly$Last)
users_last_monthly$Last<-as.Date(users_last_monthly$Last)

users_active_monthly_exp<- users_joined_monthly %>% full_join(users_last_monthly, by = c("Joined" = "Last"))
users_active_monthly_exp$leave <- ifelse(is.na(users_active_monthly_exp$leave), 0, users_active_monthly_exp$leave)
users_active_monthly_exp$join <- ifelse(is.na(users_active_monthly_exp$join), 0, users_active_monthly_exp$join)
#compute the number of active users in a month:
users_active_monthly_exp$active<-0

for (month in 1:nrow(users_active_monthly_exp)){
  if (month ==1){
    users_active_monthly_exp$active[month]<- users_active_monthly_exp$join[month]-users_active_monthly_exp$leave[month]
  }
  if (month>1){
    users_active_monthly_exp$active[month]<-users_active_monthly_exp$active[month-1] +users_active_monthly_exp$join[month]-users_active_monthly_exp$leave[month]
    
  }
  
}

users_active_monthly <- users_active_monthly %>% left_join(users_active_monthly_exp, by = 'Joined', suffix = c("", "exp"))

users_active_monthly$active_non_exp<- users_active_monthly$active - users_active_monthly$activeexp

#add to the weekly experienced data
non_exp_actives<-users_active_monthly %>% select(Joined, active_non_exp)
exp_actives<-users_active_monthly %>% select(Joined, activeexp)


#now compute for each week in our dataset what percentage of average monthly books added is added in that week
exp_books_weekly_added$Nr_active_users<-0
exp_weekly<- exp_books_weekly_added %>% filter(experienced == 1)

for (week in 1:nrow(exp_weekly)){
  month_nr<-month(as.Date(pull(exp_weekly[week,'first_day_of_week_added'])))
  year_nr<-year(as.Date(pull(exp_weekly[week,'first_day_of_week_added'])))
  first_day<-as.Date(paste0(year_nr, "-",month_nr, "-01"))
  exp_weekly[week, 'Nr_active_users'] <-pull(exp_actives[which(exp_actives$Joined==first_day), 'activeexp'])
}







all_books_weekly_added$added_per_user <- all_books_weekly_added$num_books/all_books_weekly_added$Nr_active_users


weekly_average_per_user<- all_books_weekly_added %>% filter(first_day_of_week_added > "2014-01-01") %>% group_by(week) %>% summarise(average=mean(added_per_user))

#now add the weekly average per user
all_books_weekly_added$weekly_average_per_user<-0
for (week in 1:nrow(all_books_weekly_added)){
  week_nr<- week(as.Date(pull(all_books_weekly_added[week,'first_day_of_week_added'])))
  
  all_books_weekly_added[week, 'weekly_average_per_user'] <- pull(weekly_average_per_user[week_nr, 'average'])
  
}
all_books_weekly_added$Percentage_of_weekly_average_per_user <- (all_books_weekly_added$added_per_user/all_books_weekly_added$weekly_average_per_user)





















