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
user_info<-read.csv("C:/Users/mikev/OneDrive - TU Eindhoven/UvT/3 & 4 - Thesis Marketing Analysis/Data/Complete books info/1 tm 82500 user info updated.csv", sep=";") #save the data in a dataframe
all_books2<-fread("../../data/all_books.csv") #save the data in a dataframe




#only keep the data for which we have scraped books:
user_info<- user_info %>% filter(!is.na(Nr_Books_scraped))
sum(user_info$Nr_Books_scraped)


#add a dummy for easy computation
all_books2$dummy<-1

#################################################################################
#After here, no more numbers 
#################################################################################
#add the correct user id to the user info
user_info <- user_info %>% mutate(user_id = gsub('https://www.goodreads.com/user/show','',User.Url))


#Find what duplicates were
duplicated_books<- all_books2 %>% filter(duplicated(all_books2))
#remove duplicates:
all_books2 <- all_books2 %>% filter(!duplicated(all_books2))


#drop worthless columns:
all_books2<- all_books2 %>% select(-V1)
all_books2<- all_books2 %>% select(-isbn13)


#num pages to numeric
all_books2$num_pages<- gsub('pp', '', all_books2$num_pages)
all_books2$num_pages<- gsub(',', '', all_books2$num_pages)
all_books2$num_pages<-as.numeric(all_books2$num_pages)

#Set odd number of pages to NA (we assume there are no books with no pages or books with more than 10.000 pages, which would be more than two times all harry potter books)
all_books2<-all_books2 %>% mutate(num_pages = ifelse(num_pages > 9998 | num_pages <1, NA, num_pages))


#Change user rating to numeric
all_books2$`user rating`<- gsub('did not like it', '1', all_books2$`user rating`)
all_books2$`user rating`<- gsub('it was ok', '2', all_books2$`user rating`)
all_books2$`user rating`<- gsub('really liked it', '4', all_books2$`user rating`)
all_books2$`user rating`<- gsub('liked it', '3', all_books2$`user rating`)
all_books2$`user rating`<- gsub('it was amazing', '5', all_books2$`user rating`)

all_books2$`user rating`<-as.numeric(all_books2$`user rating`)

#Change number of ratings to numeric
all_books2$num_ratings<- gsub(',', '', all_books2$num_ratings)
all_books2$num_ratings<-as.numeric(all_books2$num_ratings)

#################################################################################
#Modify user info 
#################################################################################
#Add a column with the actual nr books per user in the books data
all_books2$dummy<-1
info_book_based<-all_books2 %>% group_by(`reader id`) %>% summarise(books = sum(dummy))

user_info$Books_final<-0
for (user in 1:nrow(info_book_based)){
  userid<-info_book_based[user,1]
  
  
  user_info[which(user_info$user_id==as.character(userid)), 'Books_final'] <- as.numeric(info_book_based[user,2])
  
}

#Find out percentage left with:
user_info$perc_left<- user_info$Books_final / user_info$Nr.Books.Read

#################################################################################
#Rename dates
colnames(all_books2)[which(colnames(all_books2)=='date read')] <- 'date_read'
colnames(all_books2)[which(colnames(all_books2)=='date added')] <- 'date_added'


#Convert dates added
all_books2$`date_added`<- gsub('Jan', '01', all_books2$`date_added`)
all_books2$`date_added`<- gsub('Feb', '02', all_books2$`date_added`)
all_books2$`date_added`<- gsub('Mar', '03', all_books2$`date_added`)
all_books2$`date_added`<- gsub('Apr', '04', all_books2$`date_added`)
all_books2$`date_added`<- gsub('May', '05', all_books2$`date_added`)
all_books2$`date_added`<- gsub('Jun', '06', all_books2$`date_added`)
all_books2$`date_added`<- gsub('Jul', '07', all_books2$`date_added`)
all_books2$`date_added`<- gsub('Aug', '08', all_books2$`date_added`)
all_books2$`date_added`<- gsub('Sep', '09', all_books2$`date_added`)
all_books2$`date_added`<- gsub('Oct', '10', all_books2$`date_added`)
all_books2$`date_added`<- gsub('Nov', '11', all_books2$`date_added`)
all_books2$`date_added`<- gsub('Dec', '12', all_books2$`date_added`)


all_books2$'date_added'<-as.Date(all_books2$'date_added', "%m %d, %Y")


#################################################################################
#Convert dates started
all_books2$`date_started`<- gsub('Jan', '01', all_books2$`date_started`)
all_books2$`date_started`<- gsub('Feb', '02', all_books2$`date_started`)
all_books2$`date_started`<- gsub('Mar', '03', all_books2$`date_started`)
all_books2$`date_started`<- gsub('Apr', '04', all_books2$`date_started`)
all_books2$`date_started`<- gsub('May', '05', all_books2$`date_started`)
all_books2$`date_started`<- gsub('Jun', '06', all_books2$`date_started`)
all_books2$`date_started`<- gsub('Jul', '07', all_books2$`date_started`)
all_books2$`date_started`<- gsub('Aug', '08', all_books2$`date_started`)
all_books2$`date_started`<- gsub('Sep', '09', all_books2$`date_started`)
all_books2$`date_started`<- gsub('Oct', '10', all_books2$`date_started`)
all_books2$`date_started`<- gsub('Nov', '11', all_books2$`date_started`)
all_books2$`date_started`<- gsub('Dec', '12', all_books2$`date_started`)

all_books2$date_started_month<-gsub(".{0,3}\\,", "", all_books2$'date_started')
all_books2$date_started_month<-as.Date(paste("15 ", all_books2$date_started_month), " %d %m %Y")

all_books2$date_started<-as.Date(all_books2$'date_started', "%m %d, %Y")



#################################################################################
#Convert dates read
all_books2$`date_read`<- gsub('Jan', '01', all_books2$`date_read`)
all_books2$`date_read`<- gsub('Feb', '02', all_books2$`date_read`)
all_books2$`date_read`<- gsub('Mar', '03', all_books2$`date_read`)
all_books2$`date_read`<- gsub('Apr', '04', all_books2$`date_read`)
all_books2$`date_read`<- gsub('May', '05', all_books2$`date_read`)
all_books2$`date_read`<- gsub('Jun', '06', all_books2$`date_read`)
all_books2$`date_read`<- gsub('Jul', '07', all_books2$`date_read`)
all_books2$`date_read`<- gsub('Aug', '08', all_books2$`date_read`)
all_books2$`date_read`<- gsub('Sep', '09', all_books2$`date_read`)
all_books2$`date_read`<- gsub('Oct', '10', all_books2$`date_read`)
all_books2$`date_read`<- gsub('Nov', '11', all_books2$`date_read`)
all_books2$`date_read`<- gsub('Dec', '12', all_books2$`date_read`)

all_books2$date_read_month<-gsub(".{0,3}\\,", "", all_books2$'date_read')
all_books2$date_read_month<-as.Date(paste("15 ", all_books2$date_read_month), " %d %m %Y")

all_books2$date_read<-as.Date(all_books2$'date_read', "%m %d, %Y")

################################################################################
#Change the date published variable to only the year
all_books2$date_pub<-gsub(".*\\, |Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec| ", "", all_books2$date_pub)


################################################################################
#Check the dates for values that are not possible
all_books2<-all_books2 %>% mutate(date_started = fifelse(date_started<"1901-01-01" | date_started>"2022-5-5", as.Date(NA), date_started))
all_books2<-all_books2 %>% mutate(date_started_month = fifelse(date_started_month<"1901-01-01" | date_started_month>"2022-5-5", as.Date(NA), date_started_month))


all_books2<-all_books2 %>% mutate(date_read = fifelse(date_read<"1901-01-01" | date_read>"2022-5-5", as.Date(NA), date_read))
all_books2<-all_books2 %>% mutate(date_read_month = fifelse(date_read_month<"1901-01-01" | date_read_month>"2022-5-5", as.Date(NA), date_read_month))


################################################################################
#Calculate the read speed
all_books2$read_time_days<-difftime(all_books2$date_read, all_books2$date_started, units = "days")
all_books2$read_time_months<-ifelse((is.na(all_books2$date_started) | is.na(all_books2$date_read)), difftime(all_books2$date_read_month, all_books2$date_started_month, units = "days"), all_books2$read_time_days)

#################################################################################

test2<- all_books2 %>% group_by(`book url`) %>% summarise(su =sum(dummy)) %>% filter(su >10)
test3<- test2 %>% filter(su>50)
test4<- all_books2 %>% group_by(`book title`) %>% summarise(su =sum(dummy))

sum(test3$su)/sum(all_books2$dummy)

#save the data
dir.create("../../gen/temp")
write.csv(all_books, file = "../../gen/temp/books_cleaned.csv", fileEncoding = "UTF-8")
