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
## REGRESSION STARTED ####
#########################
# load the data
covid_stringency<-fread("../../gen/temp/covid_stringency_prepared.csv") #save the data in a dataframe
user_info<- fread("../../gen/temp/users_cleaned.csv")
all_books <-fread("../../gen/temp/books_cleaned.csv") 

all_books<- all_books %>% select (-V1) #remove variable we won't be using
user_info<-user_info %>% select(-V1)


#only keep the records with a date started
all_books<- all_books %>% filter(!is.na(date_started))

################################################################################
# create a variable that represents the first day of the week of the week in which the book was started, such that we can later aggregate on a weekly level:
all_books$first_day_of_week_started<- floor_date(as.Date(all_books$date_started, "%Y-%m/-%d"), unit="week", week_start = 1)
all_books$first_day_of_week_started <- as.Date(all_books$first_day_of_week_started)




################################################################################
################################################################################
################################################################################
# start with examining the number of books started per user per week, only looking at the people who have started their gender.
weekly_per_user<- all_books %>% group_by(`reader id`, first_day_of_week_started) %>% summarise(total=sum(dummy))


# create a list of all weeks between first and last time something was started
first_day_per_user<-weekly_per_user %>% group_by(`reader id`) %>% summarise(first=min(first_day_of_week_started))
last_day_per_user<-weekly_per_user %>% group_by(`reader id`) %>% summarise(last=max(first_day_of_week_started))

#then, create an empty dataframe in which we will later store all data
df_col_names<-c("date", "user")
weekly_per_user_complete<- data.frame(as.Date("2022-01-01"),NA)
names(weekly_per_user_complete)<-df_col_names


#loop from the first till the last day:
count=1

for (user in first_day_per_user$`reader id`){
  count<-count+1
  first_day<-first_day_per_user[which(first_day_per_user$`reader id`==user), 2]
  last_day<-last_day_per_user[which(last_day_per_user$`reader id`==user),2]
  days_of_interest<-as.data.frame(seq(as.Date(pull(first_day[1,1])), as.Date(pull(last_day[1,1])), by="weeks"))
  days_of_interest$user<-user
  names(days_of_interest)<-df_col_names
  weekly_per_user_complete<-rbind(weekly_per_user_complete,days_of_interest)
  
}

# filter for the time period of interest
all_books14<- all_books %>% filter(first_day_of_week_started > "2013-12-24" & first_day_of_week_started < "2022-01-07")
weekkly_per_user_complete <- weekly_per_user_complete %>% filter(date > "2013-12-24" & date < "2022-01-07")

#add the number of books read each week
weekly_per_user_complete<-weekly_per_user_complete%>% left_join(weekly_per_user, by=c("user"="reader id", "date"="first_day_of_week_started"))

#fill the NAs with zeros:
weekly_per_user_complete<- weekly_per_user_complete %>% replace(is.na(.), 0)


# add the country of origin to the data
data_to_add<-user_info%>% select(user_id,Country)
weekly_per_user_complete<-weekly_per_user_complete%>% left_join(data_to_add, by=c("user"="user_id"))


# next, we add the weekly stringency per country to the data.
covid_stingency_long<-pivot_longer(covid_stringency, 3:34)


# add the covid stringencies:
covid_stingency_long$first_day_of_week<-as.Date(covid_stingency_long$first_day_of_week)
covid_stingency_long<-covid_stingency_long%>% select(-V1) #remove what we will not use
weekly_per_user_complete <- weekly_per_user_complete %>% left_join(covid_stingency_long, by=c("date"="first_day_of_week", "Country"="name"))

# fill the NA's:
weekly_per_user_complete<- weekly_per_user_complete %>% replace(is.na(.), 0)

################################################################################
# regression:
# on the user level
weekly_per_user_complete$week<-week(weekly_per_user_complete$date)
weekly_per_user_complete$week<-as.character(weekly_per_user_complete$week)

# to check if we obtain similar results:
regression_nr_books <- lm((total)~ value+week+date+Country, weekly_per_user_complete)
summary(regression_nr_books)



write.csv2(as.data.frame(summary(regression_nr_books)$coefficients), file = "../../gen/output/model1_started_qty.csv", fileEncoding = "UTF-8")




################################################################################
################################################################################
################################################################################
# days per book
all_books_read_time <- all_books14 %>% filter(!is.na(read_time_days))
days_per_user<- all_books_read_time %>%group_by(`reader id`,first_day_of_week_started) %>% summarise(days=mean(read_time_days))

#add the user info

# add the country of origin to the data and the gender
data_to_add<-user_info%>% select(user_id,Country)
days_per_user<-days_per_user%>% left_join(data_to_add, by=c("reader id"="user_id"))


# next, we add the weekly stringency per country to the data.
covid_stingency_long<-pivot_longer(covid_stringency, 3:34)


# add the covid stringencies:
covid_stingency_long$first_day_of_week<-as.Date(covid_stingency_long$first_day_of_week)
covid_stingency_long<-covid_stingency_long%>% select(-V1) #remove what we will not use
days_per_user <- days_per_user %>% left_join(covid_stingency_long, by=c("first_day_of_week_started"="first_day_of_week", "Country"="name"))

# fill the NA's:
days_per_user<- days_per_user %>% replace(is.na(.), 0)


#add the week number
days_per_user$week<-as.character(week(days_per_user$first_day_of_week_started))

################################################################################
#compute the regression to check if our model stays similar to model 1
regression_days <- lm((days)~ value+week+first_day_of_week_started+Country, days_per_user)
summary(regression_days)
write.csv2(as.data.frame(summary(regression_days)$coefficients), file = "../../gen/output/model1_started_days.csv", fileEncoding = "UTF-8")


################################################################################
################################################################################
################################################################################
# rating
all_books_rating <- all_books14 %>% filter(!is.na(`user rating`))
rating_per_user<- all_books_rating %>%group_by(`reader id`,first_day_of_week_started) %>% summarise(rating=mean(`user rating`))

#add the user info

# add the country of origin to the data
data_to_add<-user_info%>% select(user_id,Country)
rating_per_user<-rating_per_user%>% left_join(data_to_add, by=c("reader id"="user_id"))


# next, we add the weekly stringency per country to the data.
covid_stingency_long<-pivot_longer(covid_stringency, 3:34)


# add the covid stringencies:
covid_stingency_long$first_day_of_week<-as.Date(covid_stingency_long$first_day_of_week)
covid_stingency_long<-covid_stingency_long%>% select(-V1) #remove what we will not use
rating_per_user <- rating_per_user %>% left_join(covid_stingency_long, by=c("first_day_of_week_started"="first_day_of_week", "Country"="name"))

# fill the NA's:
rating_per_user<- rating_per_user %>% replace(is.na(.), 0)


#add the week number
rating_per_user$week<-as.character(week(rating_per_user$first_day_of_week_started))

################################################################################
#compute the regression
regression_rating <- lm((rating)~ value+week+first_day_of_week_started+Country, rating_per_user)
summary(regression_rating)
write.csv2(as.data.frame(summary(regression_rating)$coefficients), file = "../../gen/output/model1_started_rating.csv", fileEncoding = "UTF-8")



################################################################################
################################################################################
################################################################################
# length
all_books_length <- all_books14 %>% filter(!is.na(num_pages))
length_per_user<- all_books_length %>%group_by(`reader id`,first_day_of_week_started) %>% summarise(length=mean(num_pages))

#add the user info

# add the country of origin to the data and the gender
data_to_add<-user_info%>% select(user_id,Country)
length_per_user<-length_per_user%>% left_join(data_to_add, by=c("reader id"="user_id"))


# next, we add the weekly stringency per country to the data.
covid_stingency_long<-pivot_longer(covid_stringency, 3:34)


# add the covid stringencies:
covid_stingency_long$first_day_of_week<-as.Date(covid_stingency_long$first_day_of_week)
covid_stingency_long<-covid_stingency_long%>% select(-V1) #remove what we will not use
length_per_user <- length_per_user %>% left_join(covid_stingency_long, by=c("first_day_of_week_started"="first_day_of_week", "Country"="name"))

# fill the NA's:
length_per_user<- length_per_user %>% replace(is.na(.), 0)


#add the week number
length_per_user$week<-as.character(week(length_per_user$first_day_of_week_started))

################################################################################
#compute the regression
regression_length <- lm((length)~ value+week+first_day_of_week_started+Country, length_per_user)
summary(regression_length)
write.csv2(as.data.frame(summary(regression_length)$coefficients), file = "../../gen/output/model1_started_length.csv", fileEncoding = "UTF-8")


################################################################################
################################################################################
################################################################################
# nostalgic
all_books_nostalgic <- all_books14 %>% filter(!is.na(nostalgic))
nostalgic_per_user<- all_books_nostalgic %>%group_by(`reader id`,first_day_of_week_started) %>% summarise(nostalgic=mean(nostalgic))

#add the user info

# add the country of origin to the data and the gender
data_to_add<-user_info%>% select(user_id,Country)
nostalgic_per_user<-nostalgic_per_user%>% left_join(data_to_add, by=c("reader id"="user_id"))


# next, we add the weekly stringency per country to the data.
covid_stingency_long<-pivot_longer(covid_stringency, 3:34)


# add the covid stringencies:
covid_stingency_long$first_day_of_week<-as.Date(covid_stingency_long$first_day_of_week)
covid_stingency_long<-covid_stingency_long%>% select(-V1) #remove what we will not use
nostalgic_per_user <- nostalgic_per_user %>% left_join(covid_stingency_long, by=c("first_day_of_week_started"="first_day_of_week", "Country"="name"))

# fill the NA's:
nostalgic_per_user<- nostalgic_per_user %>% replace(is.na(.), 0)


#add the week number
nostalgic_per_user$week<-as.character(week(nostalgic_per_user$first_day_of_week_started))

################################################################################
#compute the regression
regression_nostalgic <- lm((nostalgic)~ value+week+first_day_of_week_started+Country, nostalgic_per_user)
summary(regression_nostalgic)
write.csv2(as.data.frame(summary(regression_nostalgic)$coefficients), file = "../../gen/output/model1_started_nostalgic.csv", fileEncoding = "UTF-8")

################################################################################
################################################################################
################################################################################
# recent
all_books_recent <- all_books14 %>% filter(!is.na(recent))
recent_per_user<- all_books_recent %>%group_by(`reader id`,first_day_of_week_started) %>% summarise(recent=mean(recent))

#add the user info

# add the country of origin to the data and gender
data_to_add<-user_info%>% select(user_id,Country)
recent_per_user<-recent_per_user%>% left_join(data_to_add, by=c("reader id"="user_id"))


# next, we add the weekly stringency per country to the data.
covid_stingency_long<-pivot_longer(covid_stringency, 3:34)


# add the covid stringencies:
covid_stingency_long$first_day_of_week<-as.Date(covid_stingency_long$first_day_of_week)
covid_stingency_long<-covid_stingency_long%>% select(-V1) #remove what we will not use
recent_per_user <- recent_per_user %>% left_join(covid_stingency_long, by=c("first_day_of_week_started"="first_day_of_week", "Country"="name"))

# fill the NA's:
recent_per_user<- recent_per_user %>% replace(is.na(.), 0)


#add the week number
recent_per_user$week<-as.character(week(recent_per_user$first_day_of_week_started))

################################################################################
#compute the regression
regression_recent <- lm((recent)~ value+week+first_day_of_week_started+Country, recent_per_user)
summary(regression_recent)
write.csv2(as.data.frame(summary(regression_recent)$coefficients), file = "../../gen/output/model1_started_recent.csv", fileEncoding = "UTF-8")
