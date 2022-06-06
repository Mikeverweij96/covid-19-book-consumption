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
## REGRESSION ANALYSIS ##
#########################
# load the data
covid_stringency<-fread("../../gen/temp/covid_stringency_prepared.csv") #save the data in a dataframe
user_info<- fread("../../gen/temp/users_cleaned.csv")
all_books <-fread("../../gen/temp/books_cleaned.csv") 

all_books<- all_books %>% select (-V1) #remove variable we won't be using
################################################################################
# create a variable that represents the first day of the week of the week in which the book was added, such that we can later aggregate on a weekly level:
all_books$first_day_of_week_added<- floor_date(as.Date(all_books$date_added, "%Y-%m/-%d"), unit="week", week_start = 1)
all_books$first_day_of_week_added <- as.Date(all_books$first_day_of_week_added)


################################################################################
################################################################################
################################################################################
# start with examining the number of books added per user per week
weekly_per_user<- all_books %>% group_by(`reader id`, first_day_of_week_added) %>% summarise(total=sum(dummy))

# create a list of all weeks between first and last time something was added
first_day_per_user<-weekly_per_user %>% group_by(`reader id`) %>% summarise(first=min(first_day_of_week_added))
last_day_per_user<-weekly_per_user %>% group_by(`reader id`) %>% summarise(last=max(first_day_of_week_added))

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


#add the number of books read each week
weekly_per_user_complete<-weekly_per_user_complete%>% left_join(weekly_per_user, by=c("user"="reader id", "date"="first_day_of_week_added"))

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


# filter for the time period of interest
all_books14<- all_books %>% filter(first_day_of_week_added > "2013-12-24" & first_day_of_week_added < "2022-01-07")
weekly_per_user_complete<- weekly_per_user_complete %>% filter(date > "2013-12-24" & date < "2022-01-07")
weekly_per_user_complete<-weekly_per_user_complete %>% filter(!(Country == "0")) #remove the dummy country

################################################################################
# regression:
# on the user level
weekly_per_user_complete$week<-week(weekly_per_user_complete$date)
weekly_per_user_complete$week<-as.character(weekly_per_user_complete$week)

regression_nr_books <- lm((total)~ value+week+date+Country, weekly_per_user_complete)
summary(regression_nr_books)
write.csv2(as.data.frame(summary(regression_nr_books)$coefficients), file = "../../gen/output/model1_qty.csv", fileEncoding = "UTF-8")



# on the country level
weekly_per_user_complete$dummy<-1

weekly_per_country<- weekly_per_user_complete %>% group_by(date,Country) %>% summarise(total = sum(total), users=sum(dummy),covid=mean(value) )
weekly_per_country$week<-as.character(week(weekly_per_country$date))
weekly_per_country$per_user<-weekly_per_country$total/weekly_per_country$users

weekly_per_country <- weekly_per_country %>% filter(per_user>0)
regression_nr_books <- lm(log(per_user)~ covid+week+date+Country, weekly_per_country)
summary(regression_nr_books)
write.csv2(as.data.frame(summary(regression_nr_books)$coefficients), file = "../../gen/output/model1b_qty.csv", fileEncoding = "UTF-8")

################################################################################
################################################################################
################################################################################
# days per book
all_books_read_time <- all_books14 %>% filter(!is.na(read_time_days))
days_per_user<- all_books_read_time %>%group_by(`reader id`,first_day_of_week_added) %>% summarise(days=mean(read_time_days))

#add the user info

# add the country of origin to the data
data_to_add<-user_info%>% select(user_id,Country)
days_per_user<-days_per_user%>% left_join(data_to_add, by=c("reader id"="user_id"))


# next, we add the weekly stringency per country to the data.
covid_stingency_long<-pivot_longer(covid_stringency, 3:34)


# add the covid stringencies:
covid_stingency_long$first_day_of_week<-as.Date(covid_stingency_long$first_day_of_week)
covid_stingency_long<-covid_stingency_long%>% select(-V1) #remove what we will not use
days_per_user <- days_per_user %>% left_join(covid_stingency_long, by=c("first_day_of_week_added"="first_day_of_week", "Country"="name"))

# fill the NA's:
days_per_user<- days_per_user %>% replace(is.na(.), 0)


#add the week number
days_per_user$week<-as.character(week(days_per_user$first_day_of_week_added))

################################################################################
#compute the regression
regression_days <- lm((days)~ value+week+first_day_of_week_added+Country, days_per_user)
summary(regression_days)
write.csv2(as.data.frame(summary(regression_days)$coefficients), file = "../../gen/output/model1_days.csv", fileEncoding = "UTF-8")


# on the country level
weekly_per_country<- days_per_user %>% group_by(first_day_of_week_added,Country) %>% summarise(days = mean(days),covid=mean(value) )
weekly_per_country$week<-as.character(week(weekly_per_country$first_day_of_week_added))

regression_days <- lm(log(days)~ covid+week+first_day_of_week_added+Country, weekly_per_country)
summary(regression_days)
write.csv2(as.data.frame(summary(regression_days)$coefficients), file = "../../gen/output/model1b_days.csv", fileEncoding = "UTF-8")



################################################################################
################################################################################
################################################################################
# rating
all_books_rating <- all_books14 %>% filter(!is.na(`user rating`))
rating_per_user<- all_books_rating %>%group_by(`reader id`,first_day_of_week_added) %>% summarise(rating=mean(`user rating`))

#add the user info

# add the country of origin to the data
data_to_add<-user_info%>% select(user_id,Country)
rating_per_user<-rating_per_user%>% left_join(data_to_add, by=c("reader id"="user_id"))


# next, we add the weekly stringency per country to the data.
covid_stingency_long<-pivot_longer(covid_stringency, 3:34)


# add the covid stringencies:
covid_stingency_long$first_day_of_week<-as.Date(covid_stingency_long$first_day_of_week)
covid_stingency_long<-covid_stingency_long%>% select(-V1) #remove what we will not use
rating_per_user <- rating_per_user %>% left_join(covid_stingency_long, by=c("first_day_of_week_added"="first_day_of_week", "Country"="name"))

# fill the NA's:
rating_per_user<- rating_per_user %>% replace(is.na(.), 0)


#add the week number
rating_per_user$week<-as.character(week(rating_per_user$first_day_of_week_added))

################################################################################
#compute the regression
regression_rating <- lm((rating)~ value+week+first_day_of_week_added+Country, rating_per_user)
summary(regression_rating)
write.csv2(as.data.frame(summary(regression_rating)$coefficients), file = "../../gen/output/model1_rating.csv", fileEncoding = "UTF-8")


# on the country level
weekly_per_country<- rating_per_user %>% group_by(first_day_of_week_added,Country) %>% summarise(rating = mean(rating),covid=mean(value) )
weekly_per_country$week<-as.character(week(weekly_per_country$first_day_of_week_added))

regression_rating <- lm(log(rating)~ covid+week+first_day_of_week_added+Country, weekly_per_country)
summary(regression_rating)
write.csv2(as.data.frame(summary(regression_rating)$coefficients), file = "../../gen/output/model1b_rating.csv", fileEncoding = "UTF-8")


################################################################################
################################################################################
################################################################################
# length
all_books_length <- all_books14 %>% filter(!is.na(num_pages))
length_per_user<- all_books_length %>%group_by(`reader id`,first_day_of_week_added) %>% summarise(length=mean(num_pages))

#add the user info

# add the country of origin to the data
data_to_add<-user_info%>% select(user_id,Country)
length_per_user<-length_per_user%>% left_join(data_to_add, by=c("reader id"="user_id"))


# next, we add the weekly stringency per country to the data.
covid_stingency_long<-pivot_longer(covid_stringency, 3:34)


# add the covid stringencies:
covid_stingency_long$first_day_of_week<-as.Date(covid_stingency_long$first_day_of_week)
covid_stingency_long<-covid_stingency_long%>% select(-V1) #remove what we will not use
length_per_user <- length_per_user %>% left_join(covid_stingency_long, by=c("first_day_of_week_added"="first_day_of_week", "Country"="name"))

# fill the NA's:
length_per_user<- length_per_user %>% replace(is.na(.), 0)


#add the week number
length_per_user$week<-as.character(week(length_per_user$first_day_of_week_added))

################################################################################
#compute the regression
regression_length <- lm((length)~ value+week+first_day_of_week_added+Country, length_per_user)
summary(regression_length)
write.csv2(as.data.frame(summary(regression_length)$coefficients), file = "../../gen/output/model1_length.csv", fileEncoding = "UTF-8")

# on the country level
weekly_per_country<- length_per_user %>% group_by(first_day_of_week_added,Country) %>% summarise(length = mean(length),covid=mean(value) )
weekly_per_country$week<-as.character(week(weekly_per_country$first_day_of_week_added))

regression_length <- lm(log(length)~ covid+week+first_day_of_week_added+Country, weekly_per_country)
summary(regression_length)
write.csv2(as.data.frame(summary(regression_length)$coefficients), file = "../../gen/output/model1b_length.csv", fileEncoding = "UTF-8")

################################################################################
################################################################################
################################################################################
# nostalgic
all_books_nostalgic <- all_books14 %>% filter(!is.na(nostalgic))
nostalgic_per_user<- all_books_nostalgic %>%group_by(`reader id`,first_day_of_week_added) %>% summarise(nostalgic=mean(nostalgic))

#add the user info

# add the country of origin to the data
data_to_add<-user_info%>% select(user_id,Country)
nostalgic_per_user<-nostalgic_per_user%>% left_join(data_to_add, by=c("reader id"="user_id"))


# next, we add the weekly stringency per country to the data.
covid_stingency_long<-pivot_longer(covid_stringency, 3:34)


# add the covid stringencies:
covid_stingency_long$first_day_of_week<-as.Date(covid_stingency_long$first_day_of_week)
covid_stingency_long<-covid_stingency_long%>% select(-V1) #remove what we will not use
nostalgic_per_user <- nostalgic_per_user %>% left_join(covid_stingency_long, by=c("first_day_of_week_added"="first_day_of_week", "Country"="name"))

# fill the NA's:
nostalgic_per_user<- nostalgic_per_user %>% replace(is.na(.), 0)


#add the week number
nostalgic_per_user$week<-as.character(week(nostalgic_per_user$first_day_of_week_added))

################################################################################
#compute the regression
regression_nostalgic <- lm((nostalgic)~ value+week+first_day_of_week_added+Country, nostalgic_per_user)
summary(regression_nostalgic)
write.csv2(as.data.frame(summary(regression_nostalgic)$coefficients), file = "../../gen/output/model1_nostalgic.csv", fileEncoding = "UTF-8")

# on the country level
user_info_to_add<-user_info%>% select(user_id,Country)
all_books_nostalgic<-all_books_nostalgic%>% left_join(user_info_to_add, by=c("reader id"= "user_id"))
weekly_per_country<- all_books_nostalgic %>% group_by(first_day_of_week_added,Country) %>% summarise(nostalgic = mean(nostalgic) )
weekly_per_country <- weekly_per_country %>% left_join(covid_stingency_long, by=c("first_day_of_week_added"="first_day_of_week", "Country"="name"))

# fill the NA's:
weekly_per_country<- weekly_per_country %>% replace(is.na(.), 0)


weekly_per_country$week<-as.character(week(weekly_per_country$first_day_of_week_added))

regression_nostalgic <- lm(log(nostalgic)~ value+week+first_day_of_week_added+Country, weekly_per_country)
summary(regression_nostalgic)
write.csv2(as.data.frame(summary(regression_nostalgic)$coefficients), file = "../../gen/output/model1b_nostalgic.csv", fileEncoding = "UTF-8")
################################################################################
################################################################################
################################################################################
# recent
all_books_recent <- all_books14 %>% filter(!is.na(recent))
recent_per_user<- all_books_recent %>%group_by(`reader id`,first_day_of_week_added) %>% summarise(recent=mean(recent))

#add the user info

# add the country of origin to the data
data_to_add<-user_info%>% select(user_id,Country)
recent_per_user<-recent_per_user%>% left_join(data_to_add, by=c("reader id"="user_id"))


# next, we add the weekly stringency per country to the data.
covid_stingency_long<-pivot_longer(covid_stringency, 3:34)


# add the covid stringencies:
covid_stingency_long$first_day_of_week<-as.Date(covid_stingency_long$first_day_of_week)
covid_stingency_long<-covid_stingency_long%>% select(-V1) #remove what we will not use
recent_per_user <- recent_per_user %>% left_join(covid_stingency_long, by=c("first_day_of_week_added"="first_day_of_week", "Country"="name"))

# fill the NA's:
recent_per_user<- recent_per_user %>% replace(is.na(.), 0)


#add the week number
recent_per_user$week<-as.character(week(recent_per_user$first_day_of_week_added))

################################################################################
#compute the regression
regression_recent <- lm((recent)~ value+week+first_day_of_week_added+Country, recent_per_user)
summary(regression_recent)
write.csv2(as.data.frame(summary(regression_recent)$coefficients), file = "../../gen/output/model1_recent.csv", fileEncoding = "UTF-8")

# on the country level
user_info_to_add<-user_info%>% select(user_id,Country)
all_books_recent<-all_books_recent%>% left_join(user_info_to_add, by=c("reader id"= "user_id"))
weekly_per_country<- all_books_recent %>% group_by(first_day_of_week_added,Country) %>% summarise(recent = mean(recent) )
weekly_per_country <- weekly_per_country %>% left_join(covid_stingency_long, by=c("first_day_of_week_added"="first_day_of_week", "Country"="name"))

# fill the NA's:
weekly_per_country<- weekly_per_country %>% replace(is.na(.), 0)


weekly_per_country$week<-as.character(week(weekly_per_country$first_day_of_week_added))

regression_recent <- lm(log(recent)~ value+week+first_day_of_week_added+Country, weekly_per_country)
summary(regression_recent)
write.csv2(as.data.frame(summary(regression_recent)$coefficients), file = "../../gen/output/model1b_recent.csv", fileEncoding = "UTF-8")

