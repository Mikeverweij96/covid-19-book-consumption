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

experienced_users_active_monthly<- users_joined_monthly %>% full_join(users_last_monthly, by = c("Joined" = "Last"))
experienced_users_active_monthly$leave <- ifelse(is.na(experienced_users_active_monthly$leave), 0, experienced_users_active_monthly$leave)
experienced_users_active_monthly$join <- ifelse(is.na(experienced_users_active_monthly$join), 0, experienced_users_active_monthly$join)
#compute the number of active users in a month:
experienced_users_active_monthly$active<-0

for (month in 1:nrow(experienced_users_active_monthly)){
  if (month ==1){
    experienced_users_active_monthly$active[month]<- experienced_users_active_monthly$join[month]-experienced_users_active_monthly$leave[month]
  }
  if (month>1){
    experienced_users_active_monthly$active[month]<-experienced_users_active_monthly$active[month-1] +experienced_users_active_monthly$join[month]-experienced_users_active_monthly$leave[month]
    
  }
  
}

users_active_monthly <- users_active_monthly %>% left_join(experienced_users_active_monthly, by = 'Joined', suffix = c("", "exp"))

users_active_monthly$active_non_exp<- users_active_monthly$active - users_active_monthly$activeexp


#add to the weekly experienced data
non_exp_actives<-users_active_monthly %>% select(Joined, active_non_exp)
exp_actives<-users_active_monthly %>% select(Joined, activeexp)

#For the experienced users:
exp_books_weekly_added$Nr_active_users<-0
exp_weekly<- exp_books_weekly_added %>% filter(experienced == 1)

for (week in 1:nrow(exp_weekly)){
  month_nr<-month(as.Date(pull(exp_weekly[week,'first_day_of_week_added'])))
  year_nr<-year(as.Date(pull(exp_weekly[week,'first_day_of_week_added'])))
  first_day<-as.Date(paste0(year_nr, "-",month_nr, "-01"))
  exp_weekly[week, 'Nr_active_users'] <-pull(exp_actives[which(exp_actives$Joined==first_day), 'activeexp'])
}

#For the non-experienced users:
non_exp_weekly<- exp_books_weekly_added %>% filter(experienced == 0)

for (week in 1:nrow(non_exp_weekly)){
  month_nr<-month(as.Date(pull(non_exp_weekly[week,'first_day_of_week_added'])))
  year_nr<-year(as.Date(pull(non_exp_weekly[week,'first_day_of_week_added'])))
  first_day<-as.Date(paste0(year_nr, "-",month_nr, "-01"))
  non_exp_weekly[week, 'Nr_active_users'] <-pull(non_exp_actives[which(non_exp_actives$Joined==first_day), 'active_non_exp'])
}

remove(exp_books_weekly_added)
remove(exp_actives)
remove(experienced_users_active_monthly)
remove(non_exp_actives)


#join the data 
exp_weekly<- exp_weekly %>% full_join(non_exp_weekly, by= 'first_day_of_week_added', suffix=c("_exp", "_non_exp"))
exp_weekly$books_per_exp <- exp_weekly$books_added_exp/exp_weekly$Nr_active_users_exp
exp_weekly$books_per_non_exp<- exp_weekly$books_added_non_exp / exp_weekly$Nr_active_users_non_exp

#filter for time since 2014:
exp_weekly <- exp_weekly %>% filter(first_day_of_week_added < "2022-01-01" & first_day_of_week_added > "2014-01-01")

#add the covid data
covid_data_to_add <- covid_stringency %>% select(first_day_of_week, Average)

#merge:
exp_weekly<- exp_weekly %>% left_join(covid_data_to_add, by= c("first_day_of_week_added" = "first_day_of_week"))

# fill the NAs with 0
exp_weekly<- exp_weekly %>% replace(is.na(.), 0)
exp_weekly$percentage_experienced <- exp_weekly$Nr_active_users_exp / (exp_weekly$Nr_active_users_exp+exp_weekly$Nr_active_users_non_exp)

#means during and not during pandemic
not_during_covid <- exp_weekly %>% filter(Average<65)
summary(not_during_covid)
during_covid<- exp_weekly %>% filter(Average > 65)
summary(during_covid)

#Check if number of experienced users changed 
plot(exp_weekly$first_day_of_week_added, exp_weekly$percentage_experienced, type = "l", ylim=c(0,1), xlim=c(as.Date("2014-01-01"), as.Date("2021-08-06")))
plot(users_active_monthly$Joined, users_active_monthly$active, type="l", xlim=c(as.Date("2014-01-01"), as.Date("2021-08-06")))


###############################################################################
#Further investigate different groups:
#user_info <- user_info %>% mutate(Age_group = ifelse(Age<35, 'young', ifelse(Age>55, 'old', ifelse(is.na(Age), 'NA', 'middle'))))

info_to_add <- user_info %>% select(Country, user_id, Male, Age)

all_books<- all_books %>% left_join(info_to_add, by = c("reader id" = "user_id"))

#find the age when added:
all_books <- all_books %>% mutate(Age_when_added = round(Age+time_length(interval(as.Date("2022-02-02"), date_added), "years")))
all_books <- all_books %>% mutate(Age_group_when_added = ifelse(Age_when_added<35, 'young', ifelse(Age_when_added>55, 'old', ifelse(is.na(Age_when_added), 'NA', 'middle'))))


################################################################################
#gender
weekly_added_per_group <- all_books %>% filter(!is.na(Male))%>%  ##############                    <- change category here
  filter(first_day_of_week_added < "2022-01-01" & first_day_of_week_added > "2014-01-01") %>%
  group_by(`Male`,first_day_of_week_added) %>%  ##########                                        <- change category here 
  
  summarise(
  nr_books_per_week = sum(dummy),
  nr_pages_per_week = sum(num_pages, na.rm = TRUE),
  average_daily_rating = mean(`user rating`, na.rm=TRUE),
  average_age = mean(age_of_book, na.rm=TRUE),
  perc_daily_nostalgic = mean(nostalgic, na.rm = TRUE),
  perc_daily_recent = mean(most_recent, na.rm = TRUE),
  average_pages_per_book = mean(num_pages, na.rm = TRUE),
  average_num_ratings_per_book = mean(num_ratings, na.rm=TRUE)
)


#add the covid stringencies:
weekly_added_per_group<- weekly_added_per_group %>% left_join(covid_data_to_add, by=c("first_day_of_week_added"= "first_day_of_week"))
#fill the NA's:
weekly_added_per_group<- weekly_added_per_group %>% replace(is.na(.), 0)



#add the number of active members
users_joined_monthly <- user_info %>% filter(Male ==1) %>%    ##################3                      <- aanpassen
  group_by(Joined) %>% summarize(join=sum(dummy))
users_joined_monthly$Joined <- as.Date(users_joined_monthly$Joined)

users_last_monthly <- user_info %>% filter(Male == 1) %>%       ##################3                   <- aanpassen
  group_by(Last) %>% summarize(leave=sum(dummy))
users_last_monthly$Last <- gsub('28', '01', users_last_monthly$Last)
users_last_monthly$Last<-as.Date(users_last_monthly$Last)

groups_users_active_monthly<- users_joined_monthly %>% full_join(users_last_monthly, by = c("Joined" = "Last"))
groups_users_active_monthly$leave <- ifelse(is.na(groups_users_active_monthly$leave), 0, groups_users_active_monthly$leave)
groups_users_active_monthly$join <- ifelse(is.na(groups_users_active_monthly$join), 0, groups_users_active_monthly$join)
#compute the number of active users in a month:
groups_users_active_monthly$active<-0

for (month in 1:nrow(groups_users_active_monthly)){
  if (month ==1){
    groups_users_active_monthly$active[month]<- groups_users_active_monthly$join[month]-groups_users_active_monthly$leave[month]
  }
  if (month>1){
    groups_users_active_monthly$active[month]<-groups_users_active_monthly$active[month-1] +groups_users_active_monthly$join[month]-groups_users_active_monthly$leave[month]
  }
}

#store the info
groups_users_active_monthly_overview<-groups_users_active_monthly


#add the number of active members 2
users_joined_monthly <- user_info %>% filter(Male ==0) %>%    ##################3                      <- aanpassen
  group_by(Joined) %>% summarize(join=sum(dummy))
users_joined_monthly$Joined <- as.Date(users_joined_monthly$Joined)

users_last_monthly <- user_info %>% filter(Male == 0) %>%       ##################3                   <- aanpassen
  group_by(Last) %>% summarize(leave=sum(dummy))
users_last_monthly$Last <- gsub('28', '01', users_last_monthly$Last)
users_last_monthly$Last<-as.Date(users_last_monthly$Last)

groups_users_active_monthly<- users_joined_monthly %>% full_join(users_last_monthly, by = c("Joined" = "Last"))
groups_users_active_monthly$leave <- ifelse(is.na(groups_users_active_monthly$leave), 0, groups_users_active_monthly$leave)
groups_users_active_monthly$join <- ifelse(is.na(groups_users_active_monthly$join), 0, groups_users_active_monthly$join)
#compute the number of active users in a month:
groups_users_active_monthly$active<-0

for (month in 1:nrow(groups_users_active_monthly)){
  if (month ==1){
    groups_users_active_monthly$active[month]<- groups_users_active_monthly$join[month]-groups_users_active_monthly$leave[month]
  }
  if (month>1){
    groups_users_active_monthly$active[month]<-groups_users_active_monthly$active[month-1] +groups_users_active_monthly$join[month]-groups_users_active_monthly$leave[month]
  }
}


#join with overview
groups_users_active_monthly_overview<-groups_users_active_monthly_overview %>% 
  full_join(groups_users_active_monthly, by="Joined", 
  suffix = c("Male", "Female"))                                        ########                 <- aanpassen

#fill NA's 
groups_users_active_monthly_overview <- groups_users_active_monthly_overview %>% replace(is.na(.), 0)

#add first day of month to the weekly_added_per_group variable
weekly_added_per_group$first_day_of_month_added<- floor_date(as.Date(weekly_added_per_group$first_day_of_week_added, "%Y-%m/-%d"), unit="month", week_start = 1)

#join the data:
data_to_add <- groups_users_active_monthly_overview %>% select(Joined, activeMale, activeFemale)
weekly_added_per_group <- weekly_added_per_group %>% left_join(data_to_add, by= c("first_day_of_month_added"= "Joined"))

#compute the number of books per active reader
weekly_added_per_group$Books_per_user_Male <- weekly_added_per_group$nr_books_per_week / weekly_added_per_group$activeMale        ################ <- aanpassen
weekly_added_per_group$Books_per_user_Female <- weekly_added_per_group$nr_books_per_week / weekly_added_per_group$activeFemale   ################# <- aanpassen
weekly_added_per_group$pages_per_user_Male <- weekly_added_per_group$nr_pages_per_week / weekly_added_per_group$activeMale        ################ <- aanpassen
weekly_added_per_group$pages_per_user_Female <- weekly_added_per_group$nr_pages_per_week / weekly_added_per_group$activeFemale   ################# <- aanpassen




weekly_added_per_male_during <- weekly_added_per_group %>% filter(Male == 1 & Average >65) ################# <- aanpassen
weekly_added_per_male_not_during <- weekly_added_per_group %>% filter(Male == 1 & Average <65) ################# <- aanpassen
weekly_added_per_female_during <- weekly_added_per_group %>% filter(Male == 0 & Average >65) ################# <- aanpassen
weekly_added_per_female_not_during <- weekly_added_per_group %>% filter(Male == 0 & Average <65)################# <- aanpassen

summary(weekly_added_per_male_during)[4,] ################# <- aanpassen
summary(weekly_added_per_male_not_during)[4,] ################# <- aanpassen
summary(weekly_added_per_female_during)[4,] ################# <- aanpassen
summary(weekly_added_per_female_not_during)[4,] ################# <- aanpassen

#################################################################################
################################################################################
# Age group
weekly_added_per_group <- all_books %>% filter(!is.na(Age_group_when_added))%>%  ##############                    <- change category here
  filter(first_day_of_week_added < "2022-01-01" & first_day_of_week_added > "2014-01-01") %>%
  group_by(`Age_group_when_added`,first_day_of_week_added) %>%  ##########                                        <- change category here 
  
  summarise(
    nr_books_per_week = sum(dummy),
    nr_pages_per_week = sum(num_pages, na.rm = TRUE),
    average_daily_rating = mean(`user rating`, na.rm=TRUE),
    average_age = mean(age_of_book, na.rm=TRUE),
    perc_daily_nostalgic = mean(nostalgic, na.rm = TRUE),
    perc_daily_recent = mean(most_recent, na.rm = TRUE),
    average_pages_per_book = mean(num_pages, na.rm = TRUE),
    average_num_ratings_per_book = mean(num_ratings, na.rm=TRUE)
  )


#add the covid stringencies:
weekly_added_per_group<- weekly_added_per_group %>% left_join(covid_data_to_add, by=c("first_day_of_week_added"= "first_day_of_week"))
#fill the NA's:
weekly_added_per_group<- weekly_added_per_group %>% replace(is.na(.), 0)



#add the number of active members
#we will do this as follows:
# first we filter for the users that have their age defined:
users_with_age <- user_info %>% filter(!is.na(Age_group))

# add the year in which the user is born:
users_with_age$birth_year <- (as.Date("2021-12-31") -365.25*users_with_age$Age)
users_with_age <- users_with_age %>% select (user_id, Age, Joined, Last, birth_year)
  
  
print(as.Date("2022-02-02")-365.25*Age)
  
  
all_books <- all_books %>% mutate(Age_when_added = round(Age+time_length(interval(as.Date("2022-02-02"), date_added), "years")))

users_per_month<- setDT(users_with_age)[, list(user_id = user_id, birth_year= birth_year, month=seq(Joined, Last, by = "month")), by=1:nrow(users_with_age)]
users_per_month <- users_per_month %>% mutate(age = round(time_length(interval(as.Date(birth_year), as.Date(month)), "years")))
users_per_month <- users_per_month %>% mutate(Age_group = ifelse(age<35, 'young', ifelse(age>55, 'old', ifelse(is.na(age), 'NA', 'middle'))))

users_per_month$dummy<-1
users_per_month <- users_per_month %>% group_by(month) %>% summarise(youngs = sum(Age_group== "young"), middles = sum(Age_group == "middle"), olds = sum(Age_group == "old"))
users_per_month$month <- as.Date(users_per_month$month)

#add first day of month to the weekly_added_per_group variable
weekly_added_per_group$first_day_of_month_added<- floor_date(as.Date(weekly_added_per_group$first_day_of_week_added, "%Y-%m/-%d"), unit="month", week_start = 1)

#join the data:
weekly_added_per_group <- weekly_added_per_group %>% left_join(users_per_month, by= c("first_day_of_month_added"= "month"))

#compute the number of books per active reader
weekly_added_per_group$Books_per_user_young <- weekly_added_per_group$nr_books_per_week / weekly_added_per_group$youngs        ################ <- aanpassen
weekly_added_per_group$Books_per_user_middle <- weekly_added_per_group$nr_books_per_week / weekly_added_per_group$middles   ################# <- aanpassen
weekly_added_per_group$pages_per_user_young <- weekly_added_per_group$nr_pages_per_week / weekly_added_per_group$youngs       ################ <- aanpassen
weekly_added_per_group$pages_per_user_middle <- weekly_added_per_group$nr_pages_per_week / weekly_added_per_group$middles   ################# <- aanpassen
weekly_added_per_group$Books_per_user_old <- weekly_added_per_group$nr_books_per_week / weekly_added_per_group$olds  ################# <- aanpassen
weekly_added_per_group$pages_per_user_old <- weekly_added_per_group$nr_pages_per_week / weekly_added_per_group$olds       ################ <- aanpassen



weekly_added_per_young_during <- weekly_added_per_group %>% filter(Age_group_when_added == "young" & Average >65) ################# <- aanpassen
weekly_added_per_young_not_during <- weekly_added_per_group %>% filter(Age_group_when_added == "young" & Average <65) ################# <- aanpassen
weekly_added_per_middle_during <- weekly_added_per_group %>% filter(Age_group_when_added == "middle" & Average >65) ################# <- aanpassen
weekly_added_per_middle_not_during <- weekly_added_per_group %>% filter(Age_group_when_added == "middle" & Average <65) ################# <- aanpassen
weekly_added_per_old_during <- weekly_added_per_group %>% filter(Age_group_when_added == "old" & Average >65) ################# <- aanpassen
weekly_added_per_old_not_during <- weekly_added_per_group %>% filter(Age_group_when_added == "old" & Average <65) ################# <- aanpassen



summary(weekly_added_per_young_during)[4,] ################# <- aanpassen
summary(weekly_added_per_young_not_during)[4,] ################# <- aanpassen
summary(weekly_added_per_middle_during)[4,] ################# <- aanpassen
summary(weekly_added_per_middle_not_during)[4,] ################# <- aanpassen
summary(weekly_added_per_old_during)[4,] ################# <- aanpassen
summary(weekly_added_per_old_not_during)[4,] ################# <- aanpassen



#################################################################################
# experience
user_experience<- user_info %>% select(user_id, experienced)
all_books <- all_books %>% left_join(user_experience, by = c("reader id" = "user_id"))

weekly_added_per_group <- all_books %>% filter(!is.na(experienced))%>%  ##############                    <- change category here
  filter(first_day_of_week_added < "2022-01-01" & first_day_of_week_added > "2014-01-01") %>%
  group_by(`experienced`,first_day_of_week_added) %>%  ##########                                        <- change category here 
  
  summarise(
    nr_books_per_week = sum(dummy),
    nr_pages_per_week = sum(num_pages, na.rm = TRUE),
    average_daily_rating = mean(`user rating`, na.rm=TRUE),
    average_age = mean(age_of_book, na.rm=TRUE),
    perc_daily_nostalgic = mean(nostalgic, na.rm = TRUE),
    perc_daily_recent = mean(most_recent, na.rm = TRUE),
    average_pages_per_book = mean(num_pages, na.rm = TRUE),
    average_num_ratings_per_book = mean(num_ratings, na.rm=TRUE)
  )


#add the covid stringencies:
weekly_added_per_group<- weekly_added_per_group %>% left_join(covid_data_to_add, by=c("first_day_of_week_added"= "first_day_of_week"))
#fill the NA's:
weekly_added_per_group<- weekly_added_per_group %>% replace(is.na(.), 0)



#add the number of active members
users_joined_monthly <- user_info %>% filter(experienced ==1) %>%    ##################3                      <- aanpassen
  group_by(Joined) %>% summarize(join=sum(dummy))
users_joined_monthly$Joined <- as.Date(users_joined_monthly$Joined)

users_last_monthly <- user_info %>% filter(experienced == 1) %>%       ##################3                   <- aanpassen
  group_by(Last) %>% summarize(leave=sum(dummy))
users_last_monthly$Last <- gsub('28', '01', users_last_monthly$Last)
users_last_monthly$Last<-as.Date(users_last_monthly$Last)

groups_users_active_monthly<- users_joined_monthly %>% full_join(users_last_monthly, by = c("Joined" = "Last"))
groups_users_active_monthly$leave <- ifelse(is.na(groups_users_active_monthly$leave), 0, groups_users_active_monthly$leave)
groups_users_active_monthly$join <- ifelse(is.na(groups_users_active_monthly$join), 0, groups_users_active_monthly$join)
#compute the number of active users in a month:
groups_users_active_monthly$active<-0

for (month in 1:nrow(groups_users_active_monthly)){
  if (month ==1){
    groups_users_active_monthly$active[month]<- groups_users_active_monthly$join[month]-groups_users_active_monthly$leave[month]
  }
  if (month>1){
    groups_users_active_monthly$active[month]<-groups_users_active_monthly$active[month-1] +groups_users_active_monthly$join[month]-groups_users_active_monthly$leave[month]
  }
}

#store the info
groups_users_active_monthly_overview<-groups_users_active_monthly


#add the number of active members 2
users_joined_monthly <- user_info %>% filter(experienced ==0) %>%    ##################3                      <- aanpassen
  group_by(Joined) %>% summarize(join=sum(dummy))
users_joined_monthly$Joined <- as.Date(users_joined_monthly$Joined)

users_last_monthly <- user_info %>% filter(experienced == 0) %>%       ##################3                   <- aanpassen
  group_by(Last) %>% summarize(leave=sum(dummy))
users_last_monthly$Last <- gsub('28', '01', users_last_monthly$Last)
users_last_monthly$Last<-as.Date(users_last_monthly$Last)

groups_users_active_monthly<- users_joined_monthly %>% full_join(users_last_monthly, by = c("Joined" = "Last"))
groups_users_active_monthly$leave <- ifelse(is.na(groups_users_active_monthly$leave), 0, groups_users_active_monthly$leave)
groups_users_active_monthly$join <- ifelse(is.na(groups_users_active_monthly$join), 0, groups_users_active_monthly$join)
#compute the number of active users in a month:
groups_users_active_monthly$active<-0

for (month in 1:nrow(groups_users_active_monthly)){
  if (month ==1){
    groups_users_active_monthly$active[month]<- groups_users_active_monthly$join[month]-groups_users_active_monthly$leave[month]
  }
  if (month>1){
    groups_users_active_monthly$active[month]<-groups_users_active_monthly$active[month-1] +groups_users_active_monthly$join[month]-groups_users_active_monthly$leave[month]
  }
}


#join with overview
groups_users_active_monthly_overview<-groups_users_active_monthly_overview %>% 
  full_join(groups_users_active_monthly, by="Joined", 
            suffix = c("experienced", "not_experienced"))                                        ########                 <- aanpassen

#fill NA's 
groups_users_active_monthly_overview <- groups_users_active_monthly_overview %>% replace(is.na(.), 0)

#add first day of month to the weekly_added_per_group variable
weekly_added_per_group$first_day_of_month_added<- floor_date(as.Date(weekly_added_per_group$first_day_of_week_added, "%Y-%m/-%d"), unit="month", week_start = 1)

#join the data:
data_to_add <- groups_users_active_monthly_overview %>% select(Joined, activeexperienced, activenot_experienced)
weekly_added_per_group <- weekly_added_per_group %>% left_join(data_to_add, by= c("first_day_of_month_added"= "Joined"))

#compute the number of books per active reader
weekly_added_per_group$Books_per_user_experienced <- weekly_added_per_group$nr_books_per_week / weekly_added_per_group$activeexperienced        ################ <- aanpassen
weekly_added_per_group$Books_per_user_not_experienced <- weekly_added_per_group$nr_books_per_week / weekly_added_per_group$activenot_experienced  ################# <- aanpassen
weekly_added_per_group$pages_per_user_experienced <- weekly_added_per_group$nr_pages_per_week / weekly_added_per_group$activeexperienced       ################ <- aanpassen
weekly_added_per_group$pages_per_user_not_experienced <- weekly_added_per_group$nr_pages_per_week / weekly_added_per_group$activenot_experienced  ################# <- aanpassen




weekly_added_per_experienced_during <- weekly_added_per_group %>% filter(experienced == 1 & Average >65) ################# <- aanpassen
weekly_added_per_experienced_not_during <- weekly_added_per_group %>% filter(experienced == 1 & Average <65) ################# <- aanpassen
weekly_added_per_not_experienced_during <- weekly_added_per_group %>% filter(experienced == 0 & Average >65) ################# <- aanpassen
weekly_added_per_not_experienced_not_during <- weekly_added_per_group %>% filter(experienced == 0 & Average <65)################# <- aanpassen

summary(weekly_added_per_experienced_during)[4,] ################# <- aanpassen
summary(weekly_added_per_experienced_not_during)[4,] ################# <- aanpassen
summary(weekly_added_per_not_experienced_during)[4,] ################# <- aanpassen
summary(weekly_added_per_not_experienced_not_during)[4,] ################# <- aanpassen



#################################################################################
#################################################################################
#all users

weekly_added_per_group <- all_books %>% filter(!is.na(dummy))%>%  ##############                    <- change category here
  filter(first_day_of_week_added < "2022-01-01" & first_day_of_week_added > "2014-01-01") %>%
  group_by(`dummy`,first_day_of_week_added) %>%  ##########                                        <- change category here 
  
  summarise(
    nr_books_per_week = sum(dummy),
    nr_pages_per_week = sum(num_pages, na.rm = TRUE),
    average_daily_rating = mean(`user rating`, na.rm=TRUE),
    average_age = mean(age_of_book, na.rm=TRUE),
    perc_daily_nostalgic = mean(nostalgic, na.rm = TRUE),
    perc_daily_recent = mean(most_recent, na.rm = TRUE),
    average_pages_per_book = mean(num_pages, na.rm = TRUE),
    average_num_ratings_per_book = mean(num_ratings, na.rm=TRUE)
  )


#add the covid stringencies:
weekly_added_per_group<- weekly_added_per_group %>% left_join(covid_data_to_add, by=c("first_day_of_week_added"= "first_day_of_week"))
#fill the NA's:
weekly_added_per_group<- weekly_added_per_group %>% replace(is.na(.), 0)



#add the number of active members
users_joined_monthly <- user_info %>% filter(dummy ==1) %>%    ##################3                      <- aanpassen
  group_by(Joined) %>% summarize(join=sum(dummy))
users_joined_monthly$Joined <- as.Date(users_joined_monthly$Joined)

users_last_monthly <- user_info %>% filter(dummy == 1) %>%       ##################3                   <- aanpassen
  group_by(Last) %>% summarize(leave=sum(dummy))
users_last_monthly$Last <- gsub('28', '01', users_last_monthly$Last)
users_last_monthly$Last<-as.Date(users_last_monthly$Last)

groups_users_active_monthly<- users_joined_monthly %>% full_join(users_last_monthly, by = c("Joined" = "Last"))
groups_users_active_monthly$leave <- ifelse(is.na(groups_users_active_monthly$leave), 0, groups_users_active_monthly$leave)
groups_users_active_monthly$join <- ifelse(is.na(groups_users_active_monthly$join), 0, groups_users_active_monthly$join)
#compute the number of active users in a month:
groups_users_active_monthly$active<-0

for (month in 1:nrow(groups_users_active_monthly)){
  if (month ==1){
    groups_users_active_monthly$active[month]<- groups_users_active_monthly$join[month]-groups_users_active_monthly$leave[month]
  }
  if (month>1){
    groups_users_active_monthly$active[month]<-groups_users_active_monthly$active[month-1] +groups_users_active_monthly$join[month]-groups_users_active_monthly$leave[month]
  }
}

#store the info
groups_users_active_monthly_overview<-groups_users_active_monthly

#fill NA's 
groups_users_active_monthly_overview <- groups_users_active_monthly_overview %>% replace(is.na(.), 0)

#add first day of month to the weekly_added_per_group variable
weekly_added_per_group$first_day_of_month_added<- floor_date(as.Date(weekly_added_per_group$first_day_of_week_added, "%Y-%m/-%d"), unit="month", week_start = 1)

#join the data:
data_to_add <- groups_users_active_monthly_overview %>% select(Joined, active)
weekly_added_per_group <- weekly_added_per_group %>% left_join(data_to_add, by= c("first_day_of_month_added"= "Joined"))

#compute the number of books per active reader
weekly_added_per_group$Books_per_user <- weekly_added_per_group$nr_books_per_week / weekly_added_per_group$active        ################ <- aanpassen
weekly_added_per_group$pages_per_user <- weekly_added_per_group$nr_pages_per_week / weekly_added_per_group$active     ################ <- aanpassen


weekly_added_per_user_during <- weekly_added_per_group %>% filter(Average >65) ################# <- aanpassen
weekly_added_per_user_not_during <- weekly_added_per_group %>% filter(Average <65) ################# <- aanpassen

summary(weekly_added_per_user_during)[4,] ################# <- aanpassen
summary(weekly_added_per_user_not_during)[4,] ################# <- aanpassen

#################################################################################
#prepare a dataframe to store the data in
cn<-c("Group", "Average stringency", "Books per week", "Pages per week", "Average rating", "Average age", "Percentage nostalgic", "Percentage recent", "Pages per book", "Number of ratings per book", "Books per week", "Pages per week during COVID", "Average rating during COVID", "Average age during COVID", "Percentage nostalgic during COVID", "Percentage recent during COVID", "Pages per book during COVID", "Number of ratings per book during COVID")
df<- data.frame(0,0.5,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
names(df)<-cn
#################################################################################
#Country
Country_of_interest = "Turkey"


weekly_added_per_group <- all_books %>% filter(Country == Country_of_interest)%>%  ##############                    <- change category here
  filter(first_day_of_week_added < "2022-01-01" & first_day_of_week_added > "2014-01-01") %>%
  group_by(`dummy`,first_day_of_week_added) %>%  ##########                                        <- change category here 
  
  summarise(
    nr_books_per_week = sum(dummy),
    nr_pages_per_week = sum(num_pages, na.rm = TRUE),
    average_daily_rating = mean(`user rating`, na.rm=TRUE),
    average_age = mean(age_of_book, na.rm=TRUE),
    perc_daily_nostalgic = mean(nostalgic, na.rm = TRUE),
    perc_daily_recent = mean(most_recent, na.rm = TRUE),
    average_pages_per_book = mean(num_pages, na.rm = TRUE),
    average_num_ratings_per_book = mean(num_ratings, na.rm=TRUE)
  )


#add the covid stringencies:
covid_data_to_add <- covid_stringency %>% select(first_day_of_week, Country_of_interest)
colnames(covid_data_to_add) <- c('first_day_of_week', "Average")
mean_stringency<- mean(covid_data_to_add$Average)
weekly_added_per_group<- weekly_added_per_group %>% left_join(covid_data_to_add, by=c("first_day_of_week_added"= "first_day_of_week"))
#fill the NA's:
weekly_added_per_group<- weekly_added_per_group %>% replace(is.na(.), 0)


#add the number of active members
users_joined_monthly <- user_info %>% filter(Country == Country_of_interest) %>%    ##################3                      <- aanpassen
  group_by(Joined) %>% summarize(join=sum(dummy))
users_joined_monthly$Joined <- as.Date(users_joined_monthly$Joined)

users_last_monthly <- user_info %>% filter(Country == Country_of_interest) %>%       ##################3                   <- aanpassen
  group_by(Last) %>% summarize(leave=sum(dummy))
users_last_monthly$Last <- gsub('28', '01', users_last_monthly$Last)
users_last_monthly$Last<-as.Date(users_last_monthly$Last)

groups_users_active_monthly<- users_joined_monthly %>% full_join(users_last_monthly, by = c("Joined" = "Last"))
groups_users_active_monthly$leave <- ifelse(is.na(groups_users_active_monthly$leave), 0, groups_users_active_monthly$leave)
groups_users_active_monthly$join <- ifelse(is.na(groups_users_active_monthly$join), 0, groups_users_active_monthly$join)
#compute the number of active users in a month:
groups_users_active_monthly$active<-0

for (month in 1:nrow(groups_users_active_monthly)){
  if (month ==1){
    groups_users_active_monthly$active[month]<- groups_users_active_monthly$join[month]-groups_users_active_monthly$leave[month]
  }
  if (month>1){
    groups_users_active_monthly$active[month]<-groups_users_active_monthly$active[month-1] +groups_users_active_monthly$join[month]-groups_users_active_monthly$leave[month]
  }
}

#store the info
groups_users_active_monthly_overview<-groups_users_active_monthly

#fill NA's 
groups_users_active_monthly_overview <- groups_users_active_monthly_overview %>% replace(is.na(.), 0)

#add first day of month to the weekly_added_per_group variable
weekly_added_per_group$first_day_of_month_added<- floor_date(as.Date(weekly_added_per_group$first_day_of_week_added, "%Y-%m/-%d"), unit="month", week_start = 1)

#join the data:
data_to_add <- groups_users_active_monthly_overview %>% select(Joined, active)
weekly_added_per_group <- weekly_added_per_group %>% left_join(data_to_add, by= c("first_day_of_month_added"= "Joined"))

#compute the number of books per active reader
weekly_added_per_group$Books_per_user <- weekly_added_per_group$nr_books_per_week / weekly_added_per_group$active        ################ <- aanpassen
weekly_added_per_group$pages_per_user <- weekly_added_per_group$nr_pages_per_week / weekly_added_per_group$active     ################ <- aanpassen


weekly_added_per_user_during <- weekly_added_per_group %>% filter(Average >65) ################# <- aanpassen
weekly_added_per_user_not_during <- weekly_added_per_group %>% filter(Average <65) ################# <- aanpassen


#store the data in a dataframe:
no_bpw<-mean(weekly_added_per_user_not_during$Books_per_user)
no_ppw<-mean(weekly_added_per_user_not_during$pages_per_user)
no_ar<-mean(weekly_added_per_user_not_during$average_daily_rating)
no_aa<-mean(weekly_added_per_user_not_during$average_age)
no_pn<-mean(weekly_added_per_user_not_during$perc_daily_nostalgic)
no_pr<-mean(weekly_added_per_user_not_during$perc_daily_recent)
no_ppb<-mean(weekly_added_per_user_not_during$average_pages_per_book)
no_rpb<-mean(weekly_added_per_user_not_during$average_num_ratings_per_book)

du_bpw<-mean(weekly_added_per_user_during$Books_per_user)
du_ppw<-mean(weekly_added_per_user_during$pages_per_user)
du_ar<-mean(weekly_added_per_user_during$average_daily_rating)
du_aa<-mean(weekly_added_per_user_during$average_age)
du_pn<-mean(weekly_added_per_user_during$perc_daily_nostalgic)
du_pr<-mean(weekly_added_per_user_during$perc_daily_recent)
du_ppb<-mean(weekly_added_per_user_during$average_pages_per_book)
du_rpb<-mean(weekly_added_per_user_during$average_num_ratings_per_book)


df[nrow(df) + 1,] = c(Country_of_interest, mean_stringency, no_bpw, no_ppw, no_ar, no_aa, no_pn, no_pr, no_ppb, no_rpb,du_bpw, du_ppw, du_ar, du_aa, du_pn, du_pr, du_ppb, du_rpb)









