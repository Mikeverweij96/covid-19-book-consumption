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

#load and merge the book lists
books1<-fread("C:/Users/mikev/OneDrive - TU Eindhoven/UvT/3 & 4 - Thesis Marketing Analysis/Data/Complete books info/1 tm 7000 book list.csv") #save the data in a dataframe
books2<-fread("C:/Users/mikev/OneDrive - TU Eindhoven/UvT/3 & 4 - Thesis Marketing Analysis/Data/Complete books info/7000 tm 25000 book list.csv") #save the data in a dataframe
books3<-fread("C:/Users/mikev/OneDrive - TU Eindhoven/UvT/3 & 4 - Thesis Marketing Analysis/Data/Complete books info/25000 tm 61500 book list.csv") #save the data in a dataframe
books4<-fread("C:/Users/mikev/OneDrive - TU Eindhoven/UvT/3 & 4 - Thesis Marketing Analysis/Data/Complete books info/61500 tm 74500 book list.csv") #save the data in a dataframe
books5<-fread("C:/Users/mikev/OneDrive - TU Eindhoven/UvT/3 & 4 - Thesis Marketing Analysis/Data/Complete books info/74500 tm 82500 booklist.csv") #save the data in a dataframe
books6<-fread("C:/Users/mikev/OneDrive - TU Eindhoven/UvT/3 & 4 - Thesis Marketing Analysis/Data/Complete books info/82500 tm 10200 book list.csv")
books7<-fread("C:/Users/mikev/OneDrive - TU Eindhoven/UvT/3 & 4 - Thesis Marketing Analysis/Data/Complete books info/102000 tm end booklist.csv")

#load the individual info files for checking purpose
info1<-fread("C:/Users/mikev/OneDrive - TU Eindhoven/UvT/3 & 4 - Thesis Marketing Analysis/Data/Complete books info/1 tm 7000 user info updated.csv") #save the data in a dataframe
info2<-fread("C:/Users/mikev/OneDrive - TU Eindhoven/UvT/3 & 4 - Thesis Marketing Analysis/Data/Complete books info/7000 tm 25000 user info updated.csv") #save the data in a dataframe
info3<-fread("C:/Users/mikev/OneDrive - TU Eindhoven/UvT/3 & 4 - Thesis Marketing Analysis/Data/Complete books info/25000 tm 61500 user info updated.csv") #save the data in a dataframe
info4<-fread("C:/Users/mikev/OneDrive - TU Eindhoven/UvT/3 & 4 - Thesis Marketing Analysis/Data/Complete books info/61500 tm 74500 user info updated.csv") #save the data in a dataframe
info5<-fread("C:/Users/mikev/OneDrive - TU Eindhoven/UvT/3 & 4 - Thesis Marketing Analysis/Data/Complete books info/74500 tm 82500 user info.csv") #save the data in a dataframe
info6<-fread("C:/Users/mikev/OneDrive - TU Eindhoven/UvT/3 & 4 - Thesis Marketing Analysis/Data/Complete books info/82500 tm 102000 user info updated.csv")


#Check if the number of books scraped match what we have in our data
sum(info1$Nr_Books_scraped, na.rm=TRUE)==nrow(books1) #This should differ, because of the 500 extra in info
sum(info2$Nr_Books_scraped, na.rm=TRUE)==nrow(books2)
sum(info3$Nr_Books_scraped, na.rm=TRUE)==nrow(books3)
sum(info4$Nr_Books_scraped, na.rm=TRUE)==nrow(books4)
sum(info5$Nr_Books_scraped, na.rm=TRUE)==nrow(books5)
sum(info6$Nr_Books_scraped, na.rm=TRUE)==nrow(books6)

#remove the double scraped books:
books2<- books2 %>% filter(`reader id` != '/45253097-ivy-montiel-de-repente-no-ltimo-livro-blog-liter-rio')
books3<- books3 %>% filter(`reader id` != '/45253097-ivy-montiel-de-repente-no-ltimo-livro-blog-liter-rio')
books4<- books4 %>% filter(`reader id` != '/45253097-ivy-montiel-de-repente-no-ltimo-livro-blog-liter-rio')
books5<- books5 %>% filter(`reader id` != '/45253097-ivy-montiel-de-repente-no-ltimo-livro-blog-liter-rio')
books5<- books5 %>% filter(`reader id` != '/45253097-ivy-montiel-de-repente-no-ltimo-livro-blog-liter-rio')
books6<- books6 %>% filter(`reader id` != '/45253097-ivy-montiel-de-repente-no-ltimo-livro-blog-liter-rio')
books7<- books7 %>% filter(`reader id` != '/45253097-ivy-montiel-de-repente-no-ltimo-livro-blog-liter-rio')

#merge the books
all_books<-rbind(books1, books2, books3, books4, books5, books6, books7)

#save the data
dir.create("../../data/")
write.csv(all_books, file = "../../data/all_books.csv", fileEncoding = "UTF-8")