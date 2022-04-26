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
#load the data
user_info<-fread("../../data/all_users.csv") #save the data in a dataframe

#add a dummy for easy computation
all_books$dummy<-1

################################################################################

#102 users changed status to Private, so we could not re-scrape.







#only keep the users for which we have scraped books
user_info<- user_info %>% filter(!is.na(Nr_Books_scraped))

#Add the correct user id to the data
user_info <- user_info %>% mutate(user_id = gsub('https://www.goodreads.com/user/show','',User.Url))


#Add a column with the actual nr books per user in the books data
all_books$dummy<-1
info_book_based<-all_books %>% group_by(`reader id`) %>% summarise(books = sum(dummy))

user_info$Books_final<-0
for (user in 1:nrow(info_book_based)){
  userid<-info_book_based[user,1]
  
  
  user_info[which(user_info$user_id==as.character(userid)), 'Books_final'] <- as.numeric(info_book_based[user,2])
  
}

#Find out percentage left with:
user_info$perc_left<- user_info$Books_final / user_info$Nr.Books.Read