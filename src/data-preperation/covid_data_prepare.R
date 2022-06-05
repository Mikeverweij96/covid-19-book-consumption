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
### COVID Data Prepare ##
#########################
# load the data
covid_stringency<-fread("../../data/stringency_index.csv", dec=',' ) #save the data in a dataframe
user_info<- fread("../../gen/temp/users_cleaned.csv")

# remove columns that we will not ues:
covid_stringency<-covid_stringency %>% select(-V1)
covid_stringency<- covid_stringency %>% select(-country_code)


################################################################################
# find the countries of interest:
user_info$dummy<-1
countries<- user_info %>% group_by(Country) %>% summarise(total=sum(dummy))

# only keep the countries of interest
covid_stringency2<- covid_stringency%>% filter(country_name %in% countries$Country)

#we see that we miss 5 observations. We assume this is due to differences in spelling. Hence, we correct for this

missing_countries <- countries %>% filter(!(Country %in% covid_stringency2$country_name))
#Filipines:
user_info$Country <- ifelse(user_info$Country == "Filipines", "Philippines", user_info$Country)
#Latvija
user_info$Country <- ifelse(user_info$Country == "Latvija", "Latvia", user_info$Country)
#UAE
user_info$Country <- ifelse(user_info$Country == "UAE", "United Arab Emirates", user_info$Country)
#UK
user_info$Country <- ifelse(user_info$Country == "UK", "United Kingdom", user_info$Country)

#Re-do the filter for countries of interest:
countries<- user_info %>% group_by(Country) %>% summarise(total=sum(dummy))

covid_stringency<- covid_stringency%>% filter(country_name %in% countries$Country)
################################################################################
#change format
covid_stringency<- as.data.frame(t(covid_stringency))
colnames(covid_stringency) <- as.character(covid_stringency[1,])
covid_stringency <- covid_stringency[-1,]

covid_stringency<-add_column(covid_stringency, Date = rownames(covid_stringency), .after=0)

#################################################################################
#set dates to actual Dates
covid_stringency$Date<- gsub('Jan', '-01-', covid_stringency$Date)
covid_stringency$Date<- gsub('Feb', '-02-', covid_stringency$Date)
covid_stringency$Date<- gsub('Mar', '-03-', covid_stringency$Date)
covid_stringency$Date<- gsub('Apr', '-04-', covid_stringency$Date)
covid_stringency$Date<- gsub('May', '-05-', covid_stringency$Date)
covid_stringency$Date<- gsub('Jun', '-06-', covid_stringency$Date)
covid_stringency$Date<- gsub('Jul', '-07-', covid_stringency$Date)
covid_stringency$Date<- gsub('Aug', '-08-', covid_stringency$Date)
covid_stringency$Date<- gsub('Sep', '-09-', covid_stringency$Date)
covid_stringency$Date<- gsub('Oct', '-10-', covid_stringency$Date)
covid_stringency$Date<- gsub('Nov', '-11-', covid_stringency$Date)
covid_stringency$Date<- gsub('Dec', '-12-', covid_stringency$Date)

covid_stringency$Date<-as.Date(covid_stringency$Date, "%d-%m-%Y")
#################################################################################
# remove data from 2022
covid_stringency <- covid_stringency %>% filter(Date < "2022-01-01")

# set each date to the monday of that week. 
covid_stringency$first_day_of_week<- floor_date(as.Date(covid_stringency$Date, "%Y-%m/-%d"), unit="week", week_start = 1)

# group by weeks:
covid_stringency[,2:32] <- as.data.frame(lapply(covid_stringency[,2:32], as.numeric))

covid_stringency_weekly<- covid_stringency %>% select(-Date)%>% group_by(first_day_of_week) %>% summarise(across(everything(), mean))

################################################################################
# compute a weighted average:
total<- sum(countries$total) - 870 # minus unkown
countries$percentage <- countries$total/total

copy_covid_stringency_weekly<-covid_stringency_weekly
for (coll in 2:ncol(covid_stringency_weekly)){
  country_name <- colnames(covid_stringency_weekly[,coll])
  percentage <- countries$percentage[which(countries$Country == country_name)]
  
  
  copy_covid_stringency_weekly[,country_name] <- copy_covid_stringency_weekly[,country_name]*percentage
  

}
covid_stringency_weekly$Average<- rowSums(copy_covid_stringency_weekly[,2:32])
remove(copy_covid_stringency_weekly)

################################################################################
# save the data
write.csv(user_info, file = "../../gen/temp/users_cleaned.csv", fileEncoding = "UTF-8")
write.csv(covid_stringency_weekly, file = "../../gen/temp/covid_stringency_prepared.csv", fileEncoding = "UTF-8")



