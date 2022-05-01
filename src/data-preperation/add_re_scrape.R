library(haven)
library(dplyr)
library(ggplot2)
library(car)
library(readxl)
library(base)
library(data.table)
#########################
## ADD RE-SCRAPED DATA###
#########################

################################################################################
#Unfortunately, we ended up with two book data sets, as we had to re-run the scraper for 418 users.
#For these users, we initally did not scrape all the books from their shelf, due to connection errors during scraping.
#Hence, we now have to correct for that.
################################################################################

#load the data
books_rescrape<-fread("../../data/books_re_scrape.csv") #save the data in a dataframe
users_rescrape<-fread("../../data/users_re_scrape.csv")
user_info<-fread("../../data/all_users.csv")
all_books <-fread("../../data/all_books.csv")

################################################################################
#add the correct user id to the data
user_info <- user_info %>% mutate(user_id = gsub('https://www.goodreads.com/user/show','',User.Url))
users_rescrape <- users_rescrape %>% mutate(user_id = gsub('https://www.goodreads.com/user/show','',User.Url))
ids_rescrape<- users_rescrape$user_id

#remove the books from the re-scraped users from the book set and the user info set
all_books <- all_books %>% filter(!(`reader id` %in% ids_rescrape))
user_info <- user_info %>% filter(!(user_id %in% ids_rescrape))

#select only the books in the rescrape book set of users that are also in the user rescrape set
books_rescrape <- books_rescrape %>% filter(`reader id` %in% ids_rescrape)


#now, add the rescraped books and rescraped user ids to the datasets again.
all_books<- rbind(all_books, books_rescrape, fill=TRUE)
user_info <- rbind(user_info, users_rescrape, use.names=FALSE)
################################################################################
#inspect for how many users we have scraped 0 books (i.e. the users that have changed their setting to private)
private_users<-user_info %>% filter(Nr_Books_scraped == 0)
#we find that 89 users in our dataset have set their account to private after 1-1-2022.

#remove these users from the user dataset
user_info <- user_info %>% filter(Nr_Books_scraped>0)

#finally, it appeared that we scraped 10 users two times. Hence, we have to remove these duplicates
all_books <- all_books %>% distinct(`reader id`, `book url`, `date added`, .keep_all=TRUE)

################################################################################
#check if the number of scraped books per user in the book set match the info in the user info set
all_books$dummy<-1
info_book_based<-all_books %>% group_by(`reader id`) %>% summarise(books = sum(dummy))

user_info$Nr_Books_in_data<-0
for (user in 1:nrow(info_book_based)){
  userid<-info_book_based[user,1]
  
  user_info[which(user_info$user_id==as.character(userid)), 'Nr_Books_in_data'] <- as.numeric(info_book_based[user,2])
  
}


#do the check (below element should end up with 0 rows):
users_dismatch<- user_info %>% mutate(diff = Nr_Books_in_data - Nr_Books_scraped) %>% filter(!(diff ==0))
nrow(users_dismatch)

################################################################################
#Finally, save the data
dir.create("../../gen/")
dir.create("../../gen/temp")
write.csv(all_books, file = "../../gen/temp/books_after_rescrape.csv", fileEncoding = "UTF-8")
write.csv(user_info, file = "../../gen/temp/users_after_rescrape.csv", fileEncoding = "UTF-8")