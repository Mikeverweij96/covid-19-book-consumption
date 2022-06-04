library(dplyr)
library(readxl)
library(base)
library(stringr)
library(data.table)
library(googledrive)
#########################
#### DATA DOWNLOAD #####
#########################
#create the data folder
dir.create("../../data/")


#download and save users_re_scrape.csv
data_id <-"1olIXw6-RvB0E3Ziygs9Hy7YnLVtD-gN8" #the id of the dataset
drive_download(as_id(data_id), path = "users_re_scrape.csv", overwrite = TRUE) #download the data from the drive
users_re_scrape<-fread("users_re_scrape.csv", sep=";") #save the data in a dataframe
write.csv2(users_re_scrape, "../../data/users_re_scrape.csv") #save the data to csv


#download and save stringency_index.csv
data_id <-"1GfYDmtlDK1WaGJ4cQaV0sT7tiq5UoeKS" #the id of the dataset
drive_download(as_id(data_id), path = "stringency_index.csv", overwrite = TRUE) #download the data from the drive
stringency_index<-fread("stringency_index.csv", sep=",") #save the data in a dataframe
write.csv2(stringency_index, "../../data/stringency_index.csv") #save the data to csv


#download and save books_re_scrape.csv
data_id <-"1W9mr826Dh_q2I4EqDj7__Ey0GhiXudMT" #the id of the dataset
drive_download(as_id(data_id), path = "books_re_scrape.csv", overwrite = TRUE) #download the data from the drive
books_re_scrape<-fread("books_re_scrape.csv", sep=";") #save the data in a dataframe
write.csv2(books_re_scrape, "../../data/books_re_scrape.csv") #save the data to csv


#download and save all_users.csv
data_id <-"1jTASzztEJVfrNtL3uScxZJhc06ec3XBV" #the id of the dataset
drive_download(as_id(data_id), path = "all_users.csv", overwrite = TRUE) #download the data from the drive
all_users<-fread("all_users.csv", sep=";") #save the data in a dataframe
write.csv2(all_users, "../../data/all_users.csv") #save the data to csv


#download and save all_books.csv
data_id <-"1YB3GBejDk0jxxWw-s8cukpy0yEl4cVxX" #the id of the dataset
drive_download(as_id(data_id), path = "all_books.csv", overwrite = TRUE) #download the data from the drive
all_books<-fread("all_books.csv") #save the data in a dataframe
write.csv2(all_books, "../../data/all_books.csv") #save the data to csv

