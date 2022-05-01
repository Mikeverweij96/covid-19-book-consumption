library(haven)
library(dplyr)
library(ggplot2)
library(car)
library(readxl)
library(base)
library(data.table)

#########################
### DATA PREPARATION ####
#########################
#Load the data
user_info<-fread("../../gen/temp/users_after_rescrape.csv") #save the data in a dataframe


################################################################################
#remove columns that we will not use for this thesis
user_info<- user_info %>% select(-V1)
user_info<- user_info %>% select(-`Unnamed: 0`)
user_info<- user_info %>% select(-User.Name)
user_info<- user_info %>% select(-dupl)
user_info<- user_info %>% select(-Date.joined) #this is the date that the user joined the country book club, not GR. We will not use this variable. 
user_info<- user_info %>% select(-Nr_Books_scraped) #this variable is also captured in the Nr_books_in_data variable
user_info<- user_info %>% select(-Books.read) #this variable is also captured in the Nr.Books.Read variable
user_info<- user_info %>% select(-Enough_scraped) #this variable was added during scraping as an internal check, but converting the data to csv has resulted in values that are useless


################################################################################
#Add the correct user id to the data
user_info <- user_info %>% mutate(user_id = gsub('https://www.goodreads.com/user/show','',User.Url))


#################################################################################
#compare the nmber of books scraped (somewhere in February or March) to the number of books that were on the user profile in the beginning of January:
user_info$perc_scraped<-user_info$Nr_Books_in_data/user_info$Nr.Books.Read

#inspect how the number of books changed between scraping the user page and book shelf:
summary(user_info$perc_scraped)

#filter for users that had 2 times less or two times more books on their shelf at scraping compared to the number that were on their shelf in January:
few_scraped <- user_info %>% filter(perc_scraped <0.5 | perc_scraped >2)

#from these 52 observations, we now select the users for which the number of books changed by more than 50, and we will check their book shelf to be sure we scraped the correct number of books.
few_scraped<- few_scraped %>% filter(abs(Nr_Books_in_data-Nr.Books.Read)>50)

#After checking the bookselfs of these 12 observations, we find that their book shelf really changed by this amount in these one to three months time.
#Hence, we conclude that our data is correctly scraped (after adding the re-scrape in the previous step)
#################################################################################
#extract the user details from the Details text

user_info$Det <- user_info$Details #first we copy the Details in a second column, such that we can modify it, while we can still check the original details.


# remove all text if person hasn't added any details:
No_details <- user_info %>% filter(grepl("hasn't added any details yet.", Det))
print(paste0('We find ', nrow(No_details) ,' users without any details.'))
user_info$Det <- ifelse(grepl("hasn't added any details yet.", user_info$Det), "", user_info$Det)


#extract the age from the details:
user_info$Age <- ifelse(grepl("Age ", user_info$Det), gsub('^.*Age\\s*|\\s*\\,.*$', '', user_info$Det), "")
user_info$Det <- ifelse(grepl("Age ", user_info$Det), sub(".*?\\,","", user_info$Det), user_info$Det)
user_info$Det <- ifelse(grepl("Age ", user_info$Det), "", user_info$Det)

Age<- user_info %>% filter(Age >0)
Age$Age<- as.numeric(Age$Age)
print(paste0('We find ', nrow(Age) ,' users with their age defined.'))

# some summary statistics about the ages of the users in the data set:
print(summary(Age$Age))

# we remove all ages >120 years and <18 (These are in total 9 users)
user_info$Age <- as.numeric(user_info$Age)
user_info$Age[user_info$Age>120 | user_info$Age<18]<-NA
#then we have the following statistics about age:
print(summary(user_info$Age))


#extract the gender from the details:
user_info$Male <- ifelse(grepl("Female", user_info$Det), "0", ifelse(grepl("Male", user_info$Det), "1", ""))
user_info$Male <- as.numeric(user_info$Male)
Male <- user_info %>% filter(Male == 1)
Female <- user_info %>% filter(Male == 0)

print(paste0('In our dataset, we have ', nrow(Male),' people who have identified to be male'))
print(paste0(' and ', nrow(Female), ' people who have identified to be female.'))
print(paste0('In total ', nrow(user_info)-nrow(Male)-nrow(Female), ' people have not identified their gender, i.e. the vast majority'))

#Some summary statistics about gender (Male = 1, Female = 0):
print(summary(user_info$Male))


################################################################################
# transform the activity period to two real dates

#start with the date joined
user_info$Joined <- gsub('^.*Joined in \\s*|\\s*\\last.*$', '', user_info$Activity)

user_info$Joined <- gsub(',', '', user_info$Joined)

user_info$Joined<- gsub('January', '01 01', user_info$Joined)
user_info$Joined<- gsub('February', '02 01', user_info$Joined)
user_info$Joined<- gsub('March', '03 01', user_info$Joined)
user_info$Joined<- gsub('April', '04 01', user_info$Joined)
user_info$Joined<- gsub('May', '05 01', user_info$Joined)
user_info$Joined<- gsub('June', '06 01', user_info$Joined)
user_info$Joined<- gsub('July', '07 01', user_info$Joined)
user_info$Joined<- gsub('August', '08 01', user_info$Joined)
user_info$Joined<- gsub('September', '09 01', user_info$Joined)
user_info$Joined<- gsub('October', '10 01', user_info$Joined)
user_info$Joined<- gsub('November', '11 01', user_info$Joined)
user_info$Joined<- gsub('December', '12 01', user_info$Joined)

user_info$Joined<-as.Date(user_info$Joined, "%m %d %Y")


# nex,t conver the date last active
user_info$Last <- sub(".*?         ","", user_info$Activity)

user_info$Last <- gsub('last active in', '', user_info$Last)
user_info$Last <- gsub(' last active this month', '04 01 2022', user_info$Last)

#Convert Last dates
user_info$Last<- gsub('January', '01 28', user_info$Last)
user_info$Last<- gsub('February', '02 28', user_info$Last)
user_info$Last<- gsub('March', '03 28', user_info$Last)
user_info$Last<- gsub('April', '04 28', user_info$Last)
user_info$Last<- gsub('May', '05 28', user_info$Last)
user_info$Last<- gsub('June', '06 28', user_info$Last)
user_info$Last<- gsub('July', '07 28', user_info$Last)
user_info$Last<- gsub('August', '08 28', user_info$Last)
user_info$Last<- gsub('September', '09 28', user_info$Last)
user_info$Last<- gsub('October', '10 28', user_info$Last)
user_info$Last<- gsub('November', '11 28', user_info$Last)
user_info$Last<- gsub('December', '12 28', user_info$Last)

user_info$Last<-as.Date(user_info$Last, "%m %d %Y")

#Deal with users that have only used Goodreads for 1 month (for these users, only their join date is captured)
user_info$Last[is.na(user_info$Last)]<- user_info$Joined[is.na(user_info$Last)]+27
#################################################################################
#Create an overview of number of active users each month
user_info$dummy<-1
users_joined_monthly <- user_info %>% group_by(Joined) %>% summarize(join=sum(dummy))

users_last_monthly <- user_info %>% group_by(Last) %>% summarize(leave=sum(dummy))
users_last_monthly$Last <- gsub('28', '01', users_last_monthly$Last)
users_last_monthly$Last<-as.Date(users_last_monthly$Last)

user_active<- users_joined_monthly %>% left_join(users_last_monthly, by = c("Joined" = "Last"))
user_active$leave <- ifelse(is.na(user_active$leave), 0, user_active$leave)

#compute the number of active users in a month:
user_active$active<-0

for (month in 1:nrow(user_active)){
  if (month ==1){
    user_active$active[month]<- user_active$join[month]-user_active$leave[month]
  }
  if (month>1){
  user_active$active[month]<-user_active$active[month-1] +user_active$join[month]-user_active$leave[month]
  
  }
  
}
plot(user_active$Joined, user_active$active)


################################################################################
#active users per country
countries<- user_info %>% group_by(Country) %>% summarise(sum(dummy)) %>% select(Country)

for (country in 1:nrow(countries)){
  country_text<-countries$Country[country]
  

  users_joined_monthly_country <- user_info %>% filter(Country == country_text) %>% group_by(Joined) %>% summarize(join=sum(dummy))
  
  users_last_monthly_country <- user_info %>% filter(Country == country_text) %>% group_by(Last) %>% summarize(leave=sum(dummy))
  users_last_monthly_country$Last <- gsub('28', '01', users_last_monthly_country$Last)
  users_last_monthly_country$Last<-as.Date(users_last_monthly_country$Last)
  
  user_active_country<- users_joined_monthly_country %>% full_join(users_last_monthly_country, by = c("Joined" = "Last"))
  user_active_country$leave <- ifelse(is.na(user_active_country$leave), 0, user_active_country$leave)
  user_active_country$join <- ifelse(is.na(user_active_country$join), 0, user_active_country$join)
  
  
  #compute the number of active users in a month:
  user_active_country$active<-0
  user_active_country<- user_active_country %>% arrange(Joined)
  for (month in 1:nrow(user_active_country)){
    if (month ==1){
      user_active_country$active[month]<- user_active_country$join[month]-user_active_country$leave[month]
    }
    if (month>1){
      user_active_country$active[month]<-user_active_country$active[month-1] +user_active_country$join[month]-user_active_country$leave[month]
    }
    
    
  }


  user_active_country<-user_active_country %>% select(Joined, active)
  
  colnames(user_active_country)[which(colnames(user_active_country)=='Joined')] <- 'Joined_country'
  
  user_active<- user_active %>% left_join(user_active_country, by=c("Joined"= "Joined_country"), suffix= c("", country_text))
  
}


# fill the NA's 
for (coll in 2:ncol(user_active)){
  
  for (ro in 1:nrow(user_active)){
    if (ro ==1){
      user_active[ro,coll]<-ifelse(is.na(user_active[ro,coll]), 0, user_active[ro,coll])
    }
    if (ro>1){
      user_active[ro,coll]<-ifelse(is.na(user_active[ro,coll]), user_active[ro-1,coll], user_active[ro,coll])
    }
    
  }
  
  
  
}

#################################################################################
#Change the number of ratings, average rating and number of reviews to numeric

user_info$Nr.Ratings <- gsub(' ratings', "", user_info$Nr.Ratings)
user_info$Nr.Ratings <- gsub(' rating', "", user_info$Nr.Ratings)
user_info$Nr.Ratings <- as.numeric(user_info$Nr.Ratings)
user_info$Avg.Rating<- gsub(' avg)', "", user_info$Avg.Rating)
user_info$Avg.Rating<- gsub('\\(', "", user_info$Avg.Rating)
user_info$Avg.Rating<- as.numeric(user_info$Avg.Rating)


#Change number of reviews:
user_info$Nr.Reviews <- gsub(' reviews', "", user_info$Nr.Reviews)
user_info$Nr.Reviews <- gsub(' review', "", user_info$Nr.Reviews)
user_info$Nr.Reviews <- as.numeric(user_info$Nr.Reviews)

#Change the friends variable
user_info$Nr.Friends <- sub(".*?Friends ","", user_info$Nr.Friends)
user_info$Nr.Friends <- gsub('\\(|\\)', "", user_info$Nr.Friends)
user_info$Nr.Friends <- as.numeric(user_info$Nr.Friends)


#remove columns that we will not use anymore:
user_info <- user_info %>% select(-c(Details, Activity, User.Url, Det))

#################################################################################
#save the data
write.csv(user_info, file = "../../gen/temp/users_prepared.csv", fileEncoding = "UTF-8")
write.csv(user_active, file = "../../gen/temp/active_users.csv", fileEncoding = "UTF-8")


