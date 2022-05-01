library(haven)
library(dplyr)
library(ggplot2)
library(car)
library(readxl)
library(base)
library(data.table)

#########################
### DATA CLEANING #######
#########################
#Load the data
user_info<-fread("../../gen/temp/users_prepared.csv") #save the data in a dataframe
all_books<-fread("../../gen/temp/books_prepared.csv") #save the data in a dataframe

#################################################################################
#inspect the data for weird values
summary(user_info)
#we  see that we have negative number of comments, we see some very big numbers for number of ratings, reviews and books read. 
# it seems that the number of friends is bound to 999 and we have no-one with zero friends (these are the NA's). 
# we see someone who is 115, we need to inspect this. 
# we see that we still have people who joined after december 2021, these should be removed. 
#################################################################################
# We start with inspecting the negative number of comments
Negative_comments<- user_info %>% filter(Nrments <0)
#we see that there are 8 users with negative comments. #We will inspect the user pages of these people.
#we see that these people really have negative comments in the groups. We assume this is a bug and set the Nr comments to zero for these people.
user_info <- user_info %>% mutate(Nrments = ifelse(Nrments<0, 0, Nrments))
#################################################################################
# next, we inspect the users with lots of reviews and books on their shelf.
# we will base ourselves on the number of active days on goodreads for this. 
user_info$days_active<- difftime(user_info$Last, user_info$Joined, units = "days")
user_info$days_active <- as.numeric(user_info$days_active)
summary(user_info$days_active)
# these seem reasonable.
# we now compute the average number of books per day
user_info$books_per_day <- user_info$Nr_Books_in_data / user_info$days_active
summary(user_info$books_per_day)
#we find that on average, people read 0.1 books per day. However, we also have someone who has 41 books in a day. 
# we assume that 1 book a day is already quit much, so examine everyone who read more than a book a day:
Many_books_per_day <- user_info %>% filter(books_per_day>1)

# it seems that relatively many users in this subset were only active for a month. 
# we know that when people register, they are asked to add all books they have read before.
# hence, this might be the reason that these people have relatively many books per days active.
# inspect if our idea is true
only_one_month<- user_info %>% filter(days_active<31)
only_one_months_many_books <- Many_books_per_day %>% filter(days_active <31)
nrow(only_one_month)/nrow(user_info)
nrow(only_one_months_many_books)/nrow(Many_books_per_day)

# we find that in the normal data set 3,2% of users was only one month active, while in the many books dataset 43,7% was active for only 1 month.
# based on this and the risk of recall bias, we decide to remove all books that were added on the first active day.
books_per_day<- all_books %>% group_by(`reader id`, date_added) %>% summarise(total = sum(dummy))

first_day <- books_per_day %>% group_by(`reader id`) %>% arrange(date_added) %>% filter(row_number()==1)

#now, lets see if our assumption holds
mean(books_per_day$total)
mean(first_day$total)
#hence, our assumption holds. We see that on an average active day, users add 2.48 books to their shelf, while on their first active day they add 22.75 books to their shelf.
# therefore, as a next step, we remove all books that were added on the first day, to limit the risk for recall bias. 
first_day$delete<-paste0(as.character(first_day$`reader id`), as.character(first_day$date_added))
all_books$delete<-paste0(as.character(all_books$`reader id`), as.character(all_books$date_added))
all_books<- all_books %>% filter(!(delete %in% first_day$delete))
all_books<- all_books %>% select(-delete)
# we removed 2549565 books and now are left with 15699330 books. 

#next, we add the new amount of books for each user to the user info data set
books_per_user <- all_books %>% group_by(`reader id`) %>% summarise(total = sum(dummy))
colnames(books_per_user)[which(colnames(books_per_user)=='reader id')] <- 'reader_id'

# we find that we now only have 100,291 people in our book list, while this previously was 112,087. 
user_info <- user_info %>% left_join(books_per_user, by = c("user_id"= "reader_id"), suffix= c("", "books_left"))

#remove users from user info for which we no longer have any books (i.e. the users that only added books on one day)
user_info <- user_info %>% filter(total >0)

#now again, we are going to inspect the number of books read per active day.
user_info$books_per_day <- user_info$total / user_info$days_active
summary(user_info$books_per_day)
#we find that on average, people read 0.079 books per day. However, we also have someone who has 33,74 books in a day. 
# we assume that 1 book a day is already quit much, so examine everyone who read more than a book a day:
Many_books_per_day <- user_info %>% filter(books_per_day>1)

only_one_months_many_books <- Many_books_per_day %>% filter(days_active <31)
nrow(only_one_months_many_books)/nrow(Many_books_per_day)
#still, 12.3% of users in this set was active for less than a month. This is however much better than the 42% we found previously.
# Moreover, the amount of users with more than 1 book per active day ar elimited to 358 users, only 0.36 % of all users. 
# To not be too restrictive, we decide that 2 books per active day is the limit we accept as feasible. Hence, we remove all users with more books than that.
user_to_remove<- user_info %>% filter(books_per_day >2)
#these are 99 users, with the following statistics:
summary(user_to_remove)
summary(user_info)
sum(user_to_remove$total)
all_books<- all_books %>% filter(!(`reader id` %in% user_to_remove$user_id))
user_info <- user_info %>% filter(!(user_id %in% user_to_remove$user_id))


#################################################################################
#now again, we inspect the data:
summary(user_info)
# the number of books in the data for one user is now maximum 8215. We assume this is feasible.

#we will now inspect the very 'old' readers, i.e. everyone above 98:
old_readers<- user_info %>% filter(Age >100)
#these are only 15 readers. we will inspect their user profiles and try to find out if their age is reasonable
# based on profile pictures, writing style of reviews and general interests, all 15 users appear to be younger than 50 years.
# it seems that all users above 100 indicated to be around 99 / 100 years old when signing up. This is likely a typical age you give when you do not really want to register your age.
# hence, they are most likely not older than 100. Therefore, we set their ages to NA.
user_info <- user_info %>% mutate(Age = ifelse(Age >100 , as.numeric(NA), Age))

#################################################################################
#next, we will deal with removing data from 2022, since we strated scraping at 1-1-2022, we set 31-12-2021 as the last date of our interest. 
# everything that happened after this date will be a bit biased, due to the 3 months of data collection.
all_books<- all_books %>% filter(date_added < "2022-01-01")
summary(all_books)

# we see that we still have some books that were started in 2022. We have to take a look at these books:
books2022<- all_books %>% filter(date_started > "2021-12-31")
#after inspecting a few of these books, it seems that these people added the book prior to start reading it.
# this is possible, since we assume that these people were alreading planning to read a book, and hence added it.
# we keep these books in the data, since we will also be investigating the number of books added each period.
# removing these books would bias the data set.

#we also see that some dates added were before the launch of goodreads. This seems to be a mistake. we inspect these books:
books_added_before2007 <- all_books %>% filter(date_added < "2007-01-01")

#after inspecting these books, and checking them on GR, these dates are actually correct. We assume this is a goodreads bug (likely from the early ages)
# we remove these 322 books from the data.
all_books<- all_books %>% filter(date_added > "2006-12-31")
summary(all_books)

#we will also remove the books that were read or finished before the start of goodreads, since we will not be investigating this period. 
old_reading_periods <- all_books %>% filter(date_started_month < "2007-01-01" | date_read_month < "2007-01-01")

#we remove these 206,037 books from the data.
all_books<- all_books %>% filter(!(date_started_month < "2007-01-01") | is.na(date_started_month))
all_books<- all_books %>% filter(!(date_read_month < "2007-01-01") | is.na(date_read_month))

summary(all_books)
################################################################################
#operationalization of variables
#reading pace
#pages per day read:
all_books<- all_books %>% mutate(pages_per_day = ifelse(is.na(read_time_days), num_pages/(read_time_months+1), num_pages/(read_time_days+1)))

summary(all_books)
#we see that some read lots of pages per day, which is not realisticly.
# we find that a quick reader can read 60 pages in one hour. 
# assuming that an individual will at least sleep 6 hours a day, we set the maximum number of feasible pages per day to 1080.
many_pages_per_day<- all_books %>% filter(pages_per_day>1080)

# it appears that many of these were read so fast because only the month started and finished were present and wee initially set the days reading to 0.
# this is obviously not realistic. We therefore examine how many days on average a reader takes to read a book, when they read within a month:
fast_books<- all_books %>% filter(read_time_days <31)
summary(fast_books$read_time_days)
#We find that the mean is 5.294 days. Hence, we add 5 days to the read times months if this was only based on months.
all_books<- all_books %>% mutate(read_time_months = ifelse(is.na(read_time_days), ifelse(read_time_months == 0, 5, read_time_months), read_time_months))

#we compute the pages per day again
all_books<- all_books %>% mutate(pages_per_day = ifelse(is.na(read_time_days), num_pages/(read_time_months+1), num_pages/(read_time_days+1)))

all_books<- all_books %>% mutate(pages_per_day = ifelse(pages_per_day >1080, as.numeric(NA), pages_per_day))


# to operationalize the 'nostalgic' lable, we compute how many years after publication one started reading. If not avaiable, we use the year of adding.
all_books <- all_books %>% mutate(age_of_book = ifelse(!is.na(date_started_month), year(date_started_month)-year_pub, year(date_added)-year_pub))
summary(all_books$age_of_book)
#some books appear to be published after adding. We see this as infeasible, and remove the age of books for these books.
all_books <- all_books %>% mutate(age_of_book = ifelse( age_of_book<0, as.numeric(NA), age_of_book))


#lets say, between 10 and 80 years nostalgic? 
all_books <- all_books %>% mutate(nostalgic = ifelse(age_of_book > 9 & age_of_book <81, 1, 0))

#lets say, recent publications are 1 or 0 years old
all_books <- all_books %>% mutate(recent = ifelse(age_of_book <2, 1, 0))

#################################################################################
#we now update the number of books in user_info for the final time
books_per_user <- all_books %>% group_by(`reader id`) %>% summarise(total = sum(dummy))
colnames(books_per_user)[which(colnames(books_per_user)=='reader id')] <- 'reader_id'

# we find that we now only have 99,641 people in our book list, while this initially was 112,087. 
user_info <- user_info %>% left_join(books_per_user, by = c("user_id"= "reader_id"), suffix= c("", "books_left"))

#remove users from user info for which we no longer have any books (i.e. the users that only added books on one day)
user_info <- user_info %>% filter(totalbooks_left>0)

################################################################################
#finally, we reduce the data frames to only the columns that we will be using for the analysis
user_info <- user_info %>% select(-V1)
user_info <- user_info %>% select(-Nr_Books_in_data)
user_info <- user_info %>% select(- perc_scraped)
user_info <- user_info %>% select(-dummy)
user_info <- user_info %>% select(-total)

################################################################################
#save the datasets
write.csv(all_books, file = "../../gen/temp/books_cleaned.csv", fileEncoding = "UTF-8")
write.csv(user_info, file = "../../gen/temp/users_cleaned.csv", fileEncoding = "UTF-8")
