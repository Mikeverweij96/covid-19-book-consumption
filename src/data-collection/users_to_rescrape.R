library(haven)
library(dplyr)
library(ggplot2)
library(car)
library(readxl)
library(base)
library(data.table)

#########################
### Check for rescrape ##
#########################

#load and merge the book lists
All_users<-fread("C:/Users/mikev/OneDrive - TU Eindhoven/UvT/3 & 4 - Thesis Marketing Analysis/Data/Complete books info/1 tm END user info updated.csv") #save the data in a dataframe

All_users$Perc_scraped<-All_users$Nr_Books_scraped/All_users$Nr.Books.Read

Users_not_enough<- All_users %>% filter(Perc_scraped < 0.99)

Users_not_enough$diff<-Users_not_enough$Nr.Books.Read-Users_not_enough$Nr_Books_scraped

Users_not_enough <- Users_not_enough %>% filter(diff>10)

Users_not_enough <- Users_not_enough %>% filter(Perc_scraped>0)


write.csv2(Users_not_enough, file = "Users_re_scrape.csv", fileEncoding = "UTF-8")

