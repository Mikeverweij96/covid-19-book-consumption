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
##RAW USER STATISTICS ###
#########################
# load the data
user_raw<- fread("C:/Users/mikev/OneDrive - TU Eindhoven/UvT/3 & 4 - Thesis Marketing Analysis/Data/4 - Complete User Info/Complete_User_info_RAW.csv")

# find duplicates

duplicates <- user_raw %>% filter(duplicated(`User Url`))

user_raw <- user_raw %>% mutate(duplicated = ifelse (`User Url` %in% duplicates$`User Url`,1,0))

user_raw <- user_raw %>% mutate(Country = ifelse(duplicated == 1, "UNCLEAR", Country))

user_raw2 <- user_raw %>% filter(!duplicated(`User Url`))
user_raw2$dummy<-1
user_raw2$Books <- gsub("readâ???Z              ", "", user_raw2$`Nr Books Read`)
user_raw2$Books <- gsub("\\(", "", user_raw2$Books)
user_raw2$Books <- gsub("\\)", "", user_raw2$Books)
user_raw2$Books <- as.numeric(user_raw2$Books)
user_raw2$zero <- ifelse(user_raw2$Books == 0, 1, 0)

info<- user_raw2 %>% group_by(Country) %>% summarise(total = sum(dummy), privates = sum(Details == 'PRIVATE', na.rm=TRUE), authors = sum(Details == "AUTHOR", na.rm=TRUE),
                                                     zeros = sum(zero, na.rm=TRUE),
                                                     books = sum(Books, na.rm=TRUE))



write.csv2(info, "first_info_users.csv")
