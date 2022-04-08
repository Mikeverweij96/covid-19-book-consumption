library(haven)
library(dplyr)
library(ggplot2)
library(car)
library(readxl)
library(base)
library(data.table)
library(googledrive)
library(tidyverse)

Users_frame<-read.csv("C:/Users/mikev/OneDrive - TU Eindhoven/UvT/3 & 4 - Thesis Marketing Analysis/Data/4 - Complete User Info/Complete_User_info_RAW.csv", sep=";")

users_frame_filtered<- Users_frame %>% filter(Details != "AUTHOR")
users_frame_filtered<-users_frame_filtered %>% filter(Details !="PRIVATE")

users_frame_filtered$Nr.Books.Read <- (gsub("readâ???Z | // ", "", users_frame_filtered$Nr.Books.Read))
users_frame_filtered$Nr.Books.Read <- (gsub("[()]", "", users_frame_filtered$Nr.Books.Read))

users_frame_filtered$Nr.Books.Read<-as.numeric(users_frame_filtered$Nr.Books.Read)



users_frame_filtered<-users_frame_filtered %>% filter(Nr.Books.Read !=0)

users_frame_filtered$dupl<-duplicated(users_frame_filtered$User.Url)


users_double<-users_frame_filtered%>%filter(dupl == TRUE) %>% select(User.Url)

users_frame_filtered<-users_frame_filtered %>% mutate(Country = ifelse(User.Url %in% users_double$User.Url, "UNKNOWN", Country ))



users_frame_filtered<-users_frame_filtered %>% mutate(Country = ifelse(dupl == FALSE, Country, "UNKNOWN"))

users_frame_filtered<-users_frame_filtered %>% filter(dupl == FALSE)

test_subset<-users_frame_filtered[1:15,]


write.csv2(test_subset, file = "C:/Users/mikev/Downloads/Test_Subset_GR.csv")
write.csv2(users_frame_filtered, file = "C:/Users/mikev/Downloads/Filtered_Users_info.csv")
