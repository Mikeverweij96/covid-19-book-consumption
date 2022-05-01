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
all_books<-fread("../../gen/temp/books_after_rescrape.csv") #save the data in a dataframe

#add a dummy for easy computation
all_books$dummy<-1


#################################################################################
#find if we had duplicate books (this should not be the case!)
duplicated_books<- all_books %>% filter(duplicated(all_books))
#remove duplicate books:
all_books <- all_books %>% filter(!duplicated(all_books))


#drop columns that we will not use in this thesis:
all_books<- all_books %>% select(-V1)
all_books<- all_books %>% select(-isbn13)
all_books<- all_books %>% select(-`book title`)
all_books<- all_books %>% select(-author_name)

# changes the num_pages varaible to numeric
all_books$num_pages<- gsub('pp', '', all_books$num_pages)
all_books$num_pages<- gsub(',', '', all_books$num_pages)
all_books$num_pages<-as.numeric(all_books$num_pages)


#set odd number of pages to NA (we assume there are no books with no pages or books with more than 10.000 pages, as this would be more than two times all harry potter books)
all_books<-all_books %>% mutate(num_pages = ifelse(num_pages > 9999 | num_pages <1, NA, num_pages))


#convert user rating to numeric
all_books$`user rating`<- gsub('did not like it', '1', all_books$`user rating`)
all_books$`user rating`<- gsub('it was ok', '2', all_books$`user rating`)
all_books$`user rating`<- gsub('really liked it', '4', all_books$`user rating`)
all_books$`user rating`<- gsub('liked it', '3', all_books$`user rating`)
all_books$`user rating`<- gsub('it was amazing', '5', all_books$`user rating`)

all_books$`user rating`<-as.numeric(all_books$`user rating`)


#Change number of ratings to numeric
all_books$num_ratings<- gsub(',', '', all_books$num_ratings)
all_books$num_ratings<-as.numeric(all_books$num_ratings)

#################################################################################
#Convert the date variables to actual dates
#################################################################################
#Rename dates to get a more uniform format
colnames(all_books)[which(colnames(all_books)=='date read')] <- 'date_read'
colnames(all_books)[which(colnames(all_books)=='date added')] <- 'date_added'

#Convert dates added
all_books$`date_added`<- gsub('Jan', '01', all_books$`date_added`)
all_books$`date_added`<- gsub('Feb', '02', all_books$`date_added`)
all_books$`date_added`<- gsub('Mar', '03', all_books$`date_added`)
all_books$`date_added`<- gsub('Apr', '04', all_books$`date_added`)
all_books$`date_added`<- gsub('May', '05', all_books$`date_added`)
all_books$`date_added`<- gsub('Jun', '06', all_books$`date_added`)
all_books$`date_added`<- gsub('Jul', '07', all_books$`date_added`)
all_books$`date_added`<- gsub('Aug', '08', all_books$`date_added`)
all_books$`date_added`<- gsub('Sep', '09', all_books$`date_added`)
all_books$`date_added`<- gsub('Oct', '10', all_books$`date_added`)
all_books$`date_added`<- gsub('Nov', '11', all_books$`date_added`)
all_books$`date_added`<- gsub('Dec', '12', all_books$`date_added`)


all_books$'date_added'<-as.Date(all_books$'date_added', "%m %d, %Y")


#################################################################################
#Convert dates started
all_books$`date_started`<- gsub('Jan', '01', all_books$`date_started`)
all_books$`date_started`<- gsub('Feb', '02', all_books$`date_started`)
all_books$`date_started`<- gsub('Mar', '03', all_books$`date_started`)
all_books$`date_started`<- gsub('Apr', '04', all_books$`date_started`)
all_books$`date_started`<- gsub('May', '05', all_books$`date_started`)
all_books$`date_started`<- gsub('Jun', '06', all_books$`date_started`)
all_books$`date_started`<- gsub('Jul', '07', all_books$`date_started`)
all_books$`date_started`<- gsub('Aug', '08', all_books$`date_started`)
all_books$`date_started`<- gsub('Sep', '09', all_books$`date_started`)
all_books$`date_started`<- gsub('Oct', '10', all_books$`date_started`)
all_books$`date_started`<- gsub('Nov', '11', all_books$`date_started`)
all_books$`date_started`<- gsub('Dec', '12', all_books$`date_started`)

#23, 138, 493

#since many users only included the month they started the book, we add a variable that only represents the month started. We take the arbitrary day 15 as we have to specify a day.
all_books$date_started_month<-gsub(".{0,3}\\,", "", all_books$'date_started')
all_books$date_started_month<-as.Date(paste("15 ", all_books$date_started_month), " %d %m %Y")

#now we can set the original date started variable to date format, as we have secured that the dates with only a month are still stored in another variable
all_books$date_started<-as.Date(all_books$'date_started', "%m %d, %Y")


#################################################################################
#Convert dates read
all_books$`date_read`<- gsub('Jan', '01', all_books$`date_read`)
all_books$`date_read`<- gsub('Feb', '02', all_books$`date_read`)
all_books$`date_read`<- gsub('Mar', '03', all_books$`date_read`)
all_books$`date_read`<- gsub('Apr', '04', all_books$`date_read`)
all_books$`date_read`<- gsub('May', '05', all_books$`date_read`)
all_books$`date_read`<- gsub('Jun', '06', all_books$`date_read`)
all_books$`date_read`<- gsub('Jul', '07', all_books$`date_read`)
all_books$`date_read`<- gsub('Aug', '08', all_books$`date_read`)
all_books$`date_read`<- gsub('Sep', '09', all_books$`date_read`)
all_books$`date_read`<- gsub('Oct', '10', all_books$`date_read`)
all_books$`date_read`<- gsub('Nov', '11', all_books$`date_read`)
all_books$`date_read`<- gsub('Dec', '12', all_books$`date_read`)

#since many users only included the month they finished the book, we add a variable that only represents the month started. We take the arbitrary day 15 as we have to specify a day.
all_books$date_read_month<-gsub(".{0,3}\\,", "", all_books$'date_read')
all_books$date_read_month<-as.Date(paste("15 ", all_books$date_read_month), " %d %m %Y")

#now we can set the original date finsished variable to date format, as we have secured that the dates with only a month are still stored in another variable
all_books$date_read<-as.Date(all_books$'date_read', "%m %d, %Y")

################################################################################
#Inspect for how many of the books we have a complete publish date, only a publish year and month, only a year or no info at all
all_books$pub_date_format<-nchar(all_books$date_pub)
df_pub_date<- all_books %>% group_by(pub_date_format) %>% summarise(nr_occur=sum(dummy))

#After inspecting the data, we see that the variables with 1,2,3,4,5 represent years (note 1 character is for example the year 4, and 5 characters the year -1800. These books are often part of the Epic of Gilgamesh, hence, it is not an infeasible publish date.)
#We see that 7 corresponds with 'unknown'.
#We see that 8 corresponds to month and year only
# We see that 12 and 13 correspond with full date. 

#Next, we compute number of occurences of each format:
Year_only<-nrow(all_books %>% select(pub_date_format) %>% filter(pub_date_format %in% c(1,2,3,4,5)))/nrow(all_books)
Unknown<-nrow(all_books %>% select(pub_date_format) %>% filter(pub_date_format %in% c(7)))/nrow(all_books)
Year_month<-nrow(all_books %>% select(pub_date_format) %>% filter(pub_date_format %in% c(8)))/nrow(all_books)
Year_month_day<-nrow(all_books %>% select(pub_date_format) %>% filter(pub_date_format %in% c(12,13)))/nrow(all_books)


#Create a variable that only captures the year of publishing, since not for all books there is info on the exact date. 
all_books$year_pub<-gsub(".*\\, |Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec| ", "", all_books$date_pub)
all_books$year_pub<-as.numeric(all_books$year_pub)

#Convert publish date
all_books$`date_pub`<- gsub('Jan', '01', all_books$`date_pub`)
all_books$`date_pub`<- gsub('Feb', '02', all_books$`date_pub`)
all_books$`date_pub`<- gsub('Mar', '03', all_books$`date_pub`)
all_books$`date_pub`<- gsub('Apr', '04', all_books$`date_pub`)
all_books$`date_pub`<- gsub('May', '05', all_books$`date_pub`)
all_books$`date_pub`<- gsub('Jun', '06', all_books$`date_pub`)
all_books$`date_pub`<- gsub('Jul', '07', all_books$`date_pub`)
all_books$`date_pub`<- gsub('Aug', '08', all_books$`date_pub`)
all_books$`date_pub`<- gsub('Sep', '09', all_books$`date_pub`)
all_books$`date_pub`<- gsub('Oct', '10', all_books$`date_pub`)
all_books$`date_pub`<- gsub('Nov', '11', all_books$`date_pub`)
all_books$`date_pub`<- gsub('Dec', '12', all_books$`date_pub`)

all_books$date_pub<-as.Date(all_books$date_pub, "%m %d, %Y") #note that we only have this accurate data for 60% of the books

#reomve the pub_date_format variable
all_books<- all_books %>% select(-pub_date_format)
################################################################################

#Check the dates for values that are not possible
all_books<-all_books %>% mutate(date_started = fifelse(date_started<"1901-01-01" | date_started>"2022-5-5", as.Date(NA), date_started))
all_books<-all_books %>% mutate(date_started_month = fifelse(date_started_month<"1901-01-01" | date_started_month>"2022-5-5", as.Date(NA), date_started_month))


all_books<-all_books %>% mutate(date_read = fifelse(date_read<"1901-01-01" | date_read>"2022-5-5", as.Date(NA), date_read))
all_books<-all_books %>% mutate(date_read_month = fifelse(date_read_month<"1901-01-01" | date_read_month>"2022-5-5", as.Date(NA), date_read_month))

all_books<-all_books %>% mutate(date_pub = fifelse(date_pub>"2022-5-5", as.Date(NA), date_pub))
all_books<-all_books %>% mutate(year_pub = fifelse(year_pub>2022, as.numeric(NA), year_pub))


################################################################################
#Calculate the read speed
all_books$read_time_days<-difftime(all_books$date_read, all_books$date_started, units = "days")
all_books$read_time_months<-ifelse((is.na(all_books$date_started) | is.na(all_books$date_read)), difftime(all_books$date_read_month, all_books$date_started_month, units = "days"), all_books$read_time_days)

all_books$read_time_days<-as.numeric(all_books$read_time_days)

################################################################################
#We find out that 1500 records have a negative read time. 
#After inspecting a sample of these records, it seems that this is an issue that occurs if a reader adds a book for the second time bus does not (correctly) register both start and end dates.
#We decide to remove the dates of these records, as we assume that they are not reliable. 
all_books<- all_books %>% mutate(date_started = fifelse((read_time_days<0 & !is.na(read_time_days)) | (read_time_months<0 & !is.na(read_time_months)), as.Date(NA), date_started))
all_books<- all_books %>% mutate(date_read = fifelse((read_time_days<0 & !is.na(read_time_days)) | (read_time_months<0 & !is.na(read_time_months)), as.Date(NA), date_read))
all_books<- all_books %>% mutate(date_started_month = fifelse((read_time_days<0 & !is.na(read_time_days)) | (read_time_months<0 & !is.na(read_time_months)), as.Date(NA), date_started_month))
all_books<- all_books %>% mutate(date_read_month = fifelse((read_time_days<0 & !is.na(read_time_days)) | (read_time_months<0 & !is.na(read_time_months)), as.Date(NA), date_read_month))
all_books<- all_books %>% mutate(read_time_days = fifelse((read_time_days<0 & !is.na(read_time_days)) | (read_time_months<0 & !is.na(read_time_months)), as.numeric(NA), read_time_days))
all_books<- all_books %>% mutate(read_time_months = fifelse((read_time_days<0 & !is.na(read_time_days)) | (read_time_months<0 & !is.na(read_time_months)), as.numeric(NA), read_time_months))


################################################################################
#save the data
dir.create("../../gen/")
dir.create("../../gen/temp")
write.csv(all_books, file = "../../gen/temp/books_prepared.csv", fileEncoding = "UTF-8")

