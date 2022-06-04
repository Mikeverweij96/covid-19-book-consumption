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
## MODEL FREE ANALYSIS ##
#########################
# load the data
covid_stringency<-fread("../../gen/temp/covid_stringency_prepared.csv") #save the data in a dataframe
user_info<- fread("../../gen/temp/users_cleaned.csv")
all_books <-fread("../../gen/temp/books_cleaned.csv") 

all_books<- all_books %>% select (-V1)
################################################################################
# create an overview of number of books added per week globally 

all_books$first_day_of_week_added<- floor_date(as.Date(all_books$date_added, "%Y-%m/-%d"), unit="week", week_start = 1)
all_books$first_day_of_week_added <- as.Date(all_books$first_day_of_week_added)
covid_stringency$first_day_of_week<-as.Date(covid_stringency$first_day_of_week)
all_books_weekly_added<- all_books %>% group_by(first_day_of_week_added) %>% summarise(total=sum(dummy))

#add the covid stringencies
all_books_weekly_added <- all_books_weekly_added %>% left_join(covid_stringency, by=c("first_day_of_week_added" = "first_day_of_week"))

# fill the NAs with 0
all_books_weekly_added<- all_books_weekly_added %>% replace(is.na(.), 0)



#################################################################################
#first plot (total number of books added against all dates)
time <- all_books_weekly_added$first_day_of_week_added
number_of_books_added <- all_books_weekly_added$total
## second data set on a very different scale
COVID_19_stringency <- all_books_weekly_added$Average


par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis


plot(time, type="l", number_of_books_added, xlab= "", ylab = "", main = "Number of books added each week compared to COVID-19 stringency index",
     
     at=c(as.Date("2007-01-01"),as.Date("2008-01-01"),as.Date("2009-01-01"),as.Date("2010-01-01"),as.Date("2011-01-01"),as.Date("2012-01-01"),as.Date("2013-01-01"),as.Date("2014-01-01"),as.Date("2015-01-01"),as.Date("2016-01-01"),as.Date("2017-01-01"),
          as.Date("2018-01-01"), as.Date("2019-01-01"),as.Date("2020-01-01"),as.Date("2021-01-01"),as.Date("2022-01-01"))
    ) # first plot
axis(side=2, at = pretty(range(number_of_books_added)))

abline(h=pretty(range(number_of_books_added)), v=c(as.Date("2007-01-01"),as.Date("2008-01-01"),as.Date("2009-01-01"),as.Date("2010-01-01"),as.Date("2011-01-01"),as.Date("2012-01-01"),as.Date("2013-01-01"),as.Date("2014-01-01"),as.Date("2015-01-01"),as.Date("2016-01-01"),as.Date("2017-01-01"),
                                            as.Date("2018-01-01"), as.Date("2019-01-01"),as.Date("2020-01-01"),as.Date("2021-01-01"),as.Date("2022-01-01"))
       , col="grey85")


mtext("# books added", side=2, line=3)
par(new = TRUE)
plot(time, COVID_19_stringency, type = "l", lty=2,lwd=2, axes = FALSE, bty = "n", xlab = "", ylab = "", col="darkgrey")
axis(side=4, at = pretty(range(z)))
mtext("COVID-19 stringency", side=4, line=3)

par(new = TRUE)
plot(time, type="l", lwd=2, number_of_books_added, xlab= "", ylab = "")
mtext("Date (January 1st of year)", side=1, line=3)


legend("topleft", inset = 0.05, legend = c("Number of books added", "COVID-19 Stringency"), lty=1:2, col=c("black","darkgray"), lwd =2, bg="white")
################################################################################
#second plot (total number of books added against dates from 2013)

time <- all_books_weekly_added$first_day_of_week_added
number_of_books_added <- all_books_weekly_added$total
## second data set on a very different scale
COVID_19_stringency <- all_books_weekly_added$Average
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(time, type="l", number_of_books_added,   xlim=c(as.Date("2013-01-01"),as.Date("2021-12-31")),
     at=c(as.Date("2013-01-01"),as.Date("2014-01-01"),as.Date("2015-01-01"),as.Date("2016-01-01"),as.Date("2017-01-01"),
          as.Date("2018-01-01"), as.Date("2019-01-01"),as.Date("2020-01-01"),as.Date("2021-01-01"),as.Date("2021-12-31"))
) # first plot
axis(side=2, at = pretty(range(number_of_books_added)))
par(new = TRUE)
plot(time, COVID_19_stringency, type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", col="gray", xlim=c(as.Date("2013-01-01"),as.Date("2021-12-31")))
axis(side=4, at = pretty(range(z)))
mtext("COVID-19 stringency", side=4, line=3)

#################################################################################
# third plot (from 2013, corrected for seasonality)
# we want to correct for seasonality. we will do this by computing the average number of monthly books since 2013, and then compute for each week the deviance
all_books<- all_books %>% mutate(month_added= month(date_added))
monthly_average<- all_books %>% filter(date_added> "2013-01-01") %>% group_by(month_added) %>% summarise(average = sum(dummy)/9)

#now compute for each week in our dataset what percentage of average monthly books added is added in that week
all_books_weekly_added$Monthly_average<-0
for (week in 1:nrow(all_books_weekly_added)){
  month_of_week<- month(as.Date(pull(all_books_weekly_added[week,'first_day_of_week_added'])))
  all_books_weekly_added[week, 'Monthly_average'] <- pull(monthly_average[month_of_week, 'average'])
  
}
all_books_weekly_added$Peercentage_of_monthly_average <- all_books_weekly_added$total/all_books_weekly_added$Monthly_average




time <- all_books_weekly_added$first_day_of_week_added
number_of_books_added <- all_books_weekly_added$Peercentage_of_monthly_average
## second data set on a very different scale
COVID_19_stringency <- all_books_weekly_added$Average
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(time, type="l", number_of_books_added,   xlim=c(as.Date("2013-01-01"),as.Date("2021-12-31")),
     at=c(as.Date("2013-01-01"),as.Date("2014-01-01"),as.Date("2015-01-01"),as.Date("2016-01-01"),as.Date("2017-01-01"),
          as.Date("2018-01-01"), as.Date("2019-01-01"),as.Date("2020-01-01"),as.Date("2021-01-01"),as.Date("2021-12-31"))
) # first plot
axis(side=2, at = pretty(range(number_of_books_added)))

par(new = TRUE)
plot(time, COVID_19_stringency, type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", col="gray", xlim=c(as.Date("2013-01-01"),as.Date("2021-12-31")))
axis(side=4, at = pretty(range(z)))
mtext("COVID-19 stringency", side=4, line=3)
#################################################################################
#correct for average number of books added per week
all_books_weekly_added$week<- week(all_books_weekly_added$first_day_of_week_added)
weekly_average<- all_books_weekly_added %>% filter(first_day_of_week_added > "2014-01-01") %>% group_by(week) %>% summarise(average=mean(total))


#now compute for each week in our dataset what percentage of average monthly books added is added in that week
all_books_weekly_added$weekly_average<-0
for (week in 1:nrow(all_books_weekly_added)){
  week_nr<- week(as.Date(pull(all_books_weekly_added[week,'first_day_of_week_added'])))
  all_books_weekly_added[week, 'weekly_average'] <- pull(weekly_average[week_nr, 'average'])
  
}
all_books_weekly_added$Peercentage_of_weekly_average <- all_books_weekly_added$total/all_books_weekly_added$weekly_average



time <- all_books_weekly_added$first_day_of_week_added
number_of_books_added <- all_books_weekly_added$Peercentage_of_weekly_average
## second data set on a very different scale
COVID_19_stringency <- all_books_weekly_added$Average
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(time, type="l", number_of_books_added,   xlim=c(as.Date("2013-01-01"),as.Date("2021-12-31")),
     at=c(as.Date("2013-01-01"),as.Date("2014-01-01"),as.Date("2015-01-01"),as.Date("2016-01-01"),as.Date("2017-01-01"),
          as.Date("2018-01-01"), as.Date("2019-01-01"),as.Date("2020-01-01"),as.Date("2021-01-01"),as.Date("2021-12-31"))
) # first plot
axis(side=2, at = pretty(range(number_of_books_added)))

par(new = TRUE)
plot(time, COVID_19_stringency, type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", col="red", xlim=c(as.Date("2013-01-01"),as.Date("2021-12-31")))
axis(side=4, at = pretty(range(z)))
mtext("COVID-19 stringency", side=4, line=3)



################################################################################
#correct for number of active users

#Create an overview of number of active users each month
user_info$dummy<-1
user_info <- user_info %>% select(-V1)
users_joined_monthly <- user_info %>% group_by(Joined) %>% summarize(join=sum(dummy))
users_joined_monthly$Joined <- as.Date(users_joined_monthly$Joined)
users_last_monthly <- user_info %>% group_by(Last) %>% summarize(leave=sum(dummy))
users_last_monthly$Last <- gsub('28', '01', users_last_monthly$Last)
users_last_monthly$Last<-as.Date(users_last_monthly$Last)

user_active<- users_joined_monthly %>% left_join(users_last_monthly, by = c("Joined" = "Last"))
user_active$leave <- ifelse(is.na(user_active$leave), 0, user_active$leave)

#compute the number of active users in a month:
user_active$active<-0

for (month in 1:nrow(user_active)){
  if (month ==1){
    user_active$active[month]<- user_active$join[month]-user_active$leave[month]
  }
  if (month>1){
    user_active$active[month]<-user_active$active[month-1] +user_active$join[month]-user_active$leave[month]
    
  }
  
}
plot(user_active$Joined, user_active$active)



all_books_weekly_added$week<- week(all_books_weekly_added$first_day_of_week_added)

#now compute for each week in our dataset what percentage of average monthly books added is added in that week
all_books_weekly_added$Nr_active_users<-0
for (week in 1:nrow(all_books_weekly_added)){
  month_nr<-month(as.Date(pull(all_books_weekly_added[week,'first_day_of_week_added'])))
  year_nr<-year(as.Date(pull(all_books_weekly_added[week,'first_day_of_week_added'])))
  first_day<-as.Date(paste0(year_nr, "-",month_nr, "-01"))
  all_books_weekly_added[week, 'Nr_active_users'] <-pull(user_active[which(user_active$Joined==first_day), 'active'])
}
all_books_weekly_added$added_per_user <- all_books_weekly_added$total/all_books_weekly_added$Nr_active_users


weekly_average_per_user<- all_books_weekly_added %>% filter(first_day_of_week_added > "2014-01-01") %>% group_by(week) %>% summarise(average=mean(added_per_user))

#now add the weekly average per user
all_books_weekly_added$weekly_average_per_user<-0
for (week in 1:nrow(all_books_weekly_added)){
  week_nr<- week(as.Date(pull(all_books_weekly_added[week,'first_day_of_week_added'])))

  all_books_weekly_added[week, 'weekly_average_per_user'] <- pull(weekly_average_per_user[week_nr, 'average'])

}
all_books_weekly_added$Percentage_of_weekly_average_per_user <- (all_books_weekly_added$added_per_user/all_books_weekly_added$weekly_average_per_user)



#################################################################################
#plot books per user against all time
time <- all_books_weekly_added$first_day_of_week_added
number_of_books_added <- all_books_weekly_added$added_per_user
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(time, type="l", number_of_books_added, xlab= "", ylab = "", main = "Number of books added each week per active user compared to COVID-19 stringency index",
     
     at=c(as.Date("2007-01-01"),as.Date("2008-01-01"),as.Date("2009-01-01"),as.Date("2010-01-01"),as.Date("2011-01-01"),as.Date("2012-01-01"),as.Date("2013-01-01"),as.Date("2014-01-01"),as.Date("2015-01-01"),as.Date("2016-01-01"),as.Date("2017-01-01"),
          as.Date("2018-01-01"), as.Date("2019-01-01"),as.Date("2020-01-01"),as.Date("2021-01-01"),as.Date("2022-01-01"))
) # first plot
axis(side=2, at = pretty(range(number_of_books_added)))

abline(h=pretty(range(number_of_books_added)), v=c(as.Date("2007-01-01"),as.Date("2008-01-01"),as.Date("2009-01-01"),as.Date("2010-01-01"),as.Date("2011-01-01"),as.Date("2012-01-01"),as.Date("2013-01-01"),as.Date("2014-01-01"),as.Date("2015-01-01"),as.Date("2016-01-01"),as.Date("2017-01-01"),
                                                   as.Date("2018-01-01"), as.Date("2019-01-01"),as.Date("2020-01-01"),as.Date("2021-01-01"),as.Date("2022-01-01"))
       , col="grey85")


mtext("# books added per active user", side=2, line=3)
par(new = TRUE)
plot(time, COVID_19_stringency, type = "l", lty=2,lwd=2, axes = FALSE, bty = "n", xlab = "", ylab = "", col="darkgrey")
axis(side=4, at = pretty(range(z)))
mtext("COVID-19 stringency", side=4, line=3)

par(new = TRUE)
plot(time, type="l", lty=1, lwd=2, number_of_books_added, xlab= "", ylab = "")
mtext("Date (January 1st of year)", side=1, line=3)


legend("topleft", inset = 0.05, legend = c("Number of books added per active user", "COVID-19 Stringency"), lty=1:2, col=c("black","darkgray"), lwd =2, bg="white")



#################################################################################
#plot books per user from 2014 onward
time <- all_books_weekly_added$first_day_of_week_added
number_of_books_added <- all_books_weekly_added$added_per_user
left_limits= c(0, 1)
time_limits = c(as.Date("2014-01-01"), as.Date("2022-01-01"))
title = "Number of books added each week per active user compared to COVID-19 stringency index"
left_title = "# books added per active user"


par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(time, type="n", number_of_books_added, xlab= "", ylab = "", main = title, at=seq(time_limits[1],time_limits[2], by="years") , xlim=time_limits, ylim=left_limits ) # initiate plot layout
axis(side=2, at = pretty(range(left_limits))) # add right ticks
abline(h=pretty(range(left_limits)), v=seq(time_limits[1],time_limits[2], by="years"), col="grey85") # add raster
mtext(left_title, side=2, line=3) # add title to the left 

par(new = TRUE) #add new line
plot(time, COVID_19_stringency, type = "l", lty=2,lwd=2, axes = FALSE, bty = "n", xlab = "", ylab = "", col="darkgrey" , xlim=time_limits, ylim=c(0,100)) #add the covid line
axis(side=4, at = pretty(range(COVID_19_stringency)))
mtext("COVID-19 stringency", side=4, line=3)

par(new = TRUE)
plot(time, type="l", lty=1, lwd=2, number_of_books_added, xlab= "", ylab = "", xlim=time_limits, ylim=left_limits)
mtext("Date (January 1st of year)", side=1, line=3)

legend("topleft", inset = 0.03, legend = c("Number of books added per active user", "COVID-19 Stringency"), lty=1:2, col=c("black","darkgray"), lwd =2, bg="white")


#################################################################################
#Compare against base level 2018 & 2019
data_18_19<-all_books_weekly_added %>% filter(first_day_of_week_added > "2017-12-24" & first_day_of_week_added < "2020-01-01")
data_18_19 <- data_18_19 %>% mutate(week = week(first_day_of_week_added)) %>% group_by(week)%>% summarise(avg = mean(added_per_user))

data_20_21 <- all_books_weekly_added %>% filter(first_day_of_week_added > "2019-12-24" & first_day_of_week_added < "2022-01-01")
data_20_21$week<-week(data_20_21$first_day_of_week_added)

data_20_21<- data_20_21 %>% left_join(data_18_19, by='week')
data_20_21$perc_increase<- data_20_21$added_per_user/data_20_21$avg
data_20_21$perc_increase<- data_20_21$perc_increase*100-100


#plot
time <- data_20_21$first_day_of_week_added
number_of_books_added <- data_20_21$perc_increase
COVID_19_stringency20_21 <- data_20_21$Average
left_limits= c(0, 100)
time_limits = c(as.Date("2020-01-01"), as.Date("2022-01-01"))
title = "Number of books added each week per active user compared to COVID-19 stringency index"
left_title = "% more books per user compared to 2018-2019"


par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(time, type="n", number_of_books_added, xlab= "", ylab = "", main = title, at=seq(time_limits[1],time_limits[2], by="years") , xlim=time_limits, ylim=left_limits ) # initiate plot layout
axis(side=2, at = pretty(range(left_limits))) # add right ticks
abline(h=pretty(range(left_limits)), v=seq(time_limits[1],time_limits[2], by="years"), col="grey85") # add raster
mtext(left_title, side=2, line=3) # add title to the left 

par(new = TRUE) #add new line
plot(time, COVID_19_stringency20_21, type = "l", lty=2,lwd=2, axes = FALSE, bty = "n", xlab = "", ylab = "", col="darkgrey" , xlim=time_limits, ylim=c(0,100)) #add the covid line
axis(side=4, at = pretty(range(COVID_19_stringency20_21)))
mtext("COVID-19 stringency", side=4, line=3)

par(new = TRUE)
plot(time, type="l", lty=1, lwd=2, number_of_books_added, xlab= "", ylab = "", xlim=time_limits, ylim=left_limits)
mtext("Date (January 1st of year)", side=1, line=3)

legend("topleft", inset = 0.02, legend = c(left_title, "COVID-19 Stringency"), lty=1:2, col=c("black","darkgray"), lwd =2, bg="white")
















#################################################################################
#plot books per user from 2014 onward and correct for seasonality
time <- all_books_weekly_added$first_day_of_week_added
number_of_books_added <- all_books_weekly_added$Percentage_of_weekly_average_per_user
left_limits= c(0.5, 1.5)
time_limits = c(as.Date("2014-01-01"), as.Date("2022-01-01"))
title = "Books added per active user corrected by weekly average in comparison to COVID-19 Stringency"
left_title = "books added per active user / weekly average"


par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(time, type="n", number_of_books_added, xlab= "", ylab = "", main = title, at=seq(time_limits[1],time_limits[2], by="years") , xlim=time_limits, ylim=left_limits ) # initiate plot layout
axis(side=2, at = pretty(range(left_limits))) # add right ticks
abline(h=pretty(range(left_limits)), v=seq(time_limits[1],time_limits[2], by="years"), col="grey85") # add raster
mtext(left_title, side=2, line=3) # add title to the left 

par(new = TRUE) #add new line
plot(time, COVID_19_stringency, type = "l", lty=2,lwd=2, axes = FALSE, bty = "n", xlab = "", ylab = "", col="darkgrey" , xlim=time_limits) #add the covid line
axis(side=4, at = pretty(range(COVID_19_stringency)))
mtext("COVID-19 stringency", side=4, line=3)

par(new = TRUE)
plot(time, type="l", lty=1, lwd=2, number_of_books_added, xlab= "", ylab = "", xlim=time_limits, ylim=left_limits)
mtext("Date (January 1st of year)", side=1, line=3)

legend("topleft", inset = 0.05, legend = c("Number of books added per active user", "COVID-19 Stringency"), lty=1:2, col=c("black","darkgray"), lwd =2, bg="white")


################################################################################
################################################################################
################################################################################
# date started
# create an overview of number of books started per week globally 

all_books$first_day_of_week_started<- floor_date(as.Date(all_books$date_started, "%Y-%m/-%d"), unit="week", week_start = 1)
all_books$first_day_of_week_started <- as.Date(all_books$first_day_of_week_started)

all_books_weekly_started<- all_books %>% filter(date_started <"2022-01-01") %>% group_by(first_day_of_week_started) %>% summarise(total=sum(dummy))

#add the covid stringencies
all_books_weekly_started <- all_books_weekly_started %>% left_join(covid_stringency, by=c("first_day_of_week_started" = "first_day_of_week"))

# fill the NAs with 0
all_books_weekly_started<- all_books_weekly_started %>% replace(is.na(.), 0)

#################################################################################
#first plot (all dates)
time <- all_books_weekly_started$first_day_of_week_started
number_of_books_started <- all_books_weekly_started$total
COVID_19_stringency <- all_books_weekly_added$Average
COVID_19_stringency<-c(0,0,COVID_19_stringency)
left_limits= c(0,35000)
time_limits = c(as.Date("2007-01-01"), as.Date("2022-01-01"))
title = "Books started and finished each week compared to COVID-19 Stringency"
left_title = "# books per active user"


par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(time, type="n", number_of_books_started, xlab= "", ylab = "", main = title, at=seq(time_limits[1],time_limits[2], by="years") , xlim=time_limits, ylim=left_limits ) # initiate plot layout
axis(side=2, at = pretty(range(left_limits))) # add right ticks
abline(h=pretty(range(left_limits)), v=seq(time_limits[1],time_limits[2], by="years"), col="grey85") # add raster
mtext(left_title, side=2, line=3) # add title to the left 

par(new = TRUE) #add new line

plot(time, COVID_19_stringency, type = "l", lty=2,lwd=2, axes = FALSE, bty = "n", xlab = "", ylab = "", col="darkgrey" , xlim=time_limits) #add the covid line
axis(side=4, at = pretty(range(COVID_19_stringency)))
mtext("COVID-19 stringency", side=4, line=3)

par(new = TRUE)
plot(time, type="l", lty=1, lwd=1, number_of_books_started, xlab= "", ylab = "", xlim=time_limits, ylim=left_limits, col="red")
mtext("Date (January 1st of year)", side=1, line=3)

par(new = TRUE)
plot(time, type="l", lty=1, lwd=1, number_of_books_read, xlab= "", ylab = "", xlim=time_limits, ylim=left_limits)
mtext("Date (January 1st of year)", side=1, line=3)


legend("topleft", inset = 0.05, legend = c("Number of books started per active user", "Number of books finished per active user", "COVID-19 Stringency"), lty=c(1,1,2), col=c("black","red", "darkgray"), lwd =2, bg="white")


#################################################################################
#compute percentage of books for which date started and finished is added.
book_shelf<- all_books %>% group_by(first_day_of_week_added) %>% summarise(total_added = sum(dummy), total_started= sum(!(is.na(date_started))), total_read= sum(!(is.na(date_read))))

book_shelf$perc_started<- book_shelf$total_started/book_shelf$total_added
book_shelf$perc_read<- book_shelf$total_read/book_shelf$total_added



#Plot the fractions to see if there is indeed an incerase in percentage for which reading period is defined. 

#################################################################################
#first plot (all dates)
time <- book_shelf$first_day_of_week_added
number_of_books_started <- book_shelf$perc_started
number_of_books_read<- book_shelf$perc_read
left_limits= c(0,1)
time_limits = c(as.Date("2007-01-01"), as.Date("2022-01-01"))
title = "Fraction of books added for which date started and finished is specified"
left_title = "Fraction of books"


par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(time, type="l", number_of_books_started, xlab= "", ylab = "", main = title, at=seq(time_limits[1],time_limits[2], by="years") , xlim=time_limits, ylim=left_limits ) # initiate plot layout
axis(side=2, at = pretty(range(left_limits))) # add right ticks
abline(h=pretty(range(left_limits)), v=seq(time_limits[1],time_limits[2], by="years"), col="grey85") # add raster
mtext(left_title, side=2, line=3) # add title to the left 

par(new = TRUE) #add new line

plot(time, number_of_books_read, type = "l",lwd=1, axes = FALSE, bty = "n", xlab = "", ylab = "", col="darkgrey" , xlim=time_limits, ylim=left_limits) #add the covid line

mtext("Date (January 1st of year)", side=1, line=3)


legend("topleft", inset = 0.05, legend = c("books started", "books finished"), lty=c(1,1), col=c("black","darkgrey"), lwd =2, bg="white")

#################################################################################
#################################################################################
##################################################################################
#Correlation
book_shelf_of_interest<- book_shelf_read[366:783,]

cor.test(book_shelf_of_interest$total_read, book_shelf_of_interest$total_added)
cor.test(book_shelf_of_interest$total_started, book_shelf_of_interest$total_added)
cor.test(book_shelf_of_interest$total_read, book_shelf_of_interest$total_started)

################################################################################
#################################################################################
#################################################################################
#correct for average number of books started per week
all_books_weekly_started$week<- week(all_books_weekly_started$first_day_of_week_started)
weekly_average<- all_books_weekly_started %>% filter(first_day_of_week_started > "2013-01-01") %>% group_by(week) %>% summarise(average=mean(total))


#now compute for each week in our dataset what percentage of average monthly books started is started in that week
all_books_weekly_started$weekly_average<-0
for (week in 1:nrow(all_books_weekly_started)){
  week_nr<- week(as.Date(pull(all_books_weekly_started[week,'first_day_of_week_started'])))
  all_books_weekly_started[week, 'weekly_average'] <- pull(weekly_average[week_nr, 'average'])
  
}
all_books_weekly_started$Peercentage_of_weekly_average <- all_books_weekly_started$total/all_books_weekly_started$weekly_average



time <- all_books_weekly_started$first_day_of_week_started
number_of_books_started <- all_books_weekly_started$Peercentage_of_weekly_average
## second data set on a very different scale
COVID_19_stringency <- all_books_weekly_started$Average
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(time, type="l", number_of_books_started,   xlim=c(as.Date("2013-01-01"),as.Date("2021-12-31")),
     at=c(as.Date("2013-01-01"),as.Date("2014-01-01"),as.Date("2015-01-01"),as.Date("2016-01-01"),as.Date("2017-01-01"),
          as.Date("2018-01-01"), as.Date("2019-01-01"),as.Date("2020-01-01"),as.Date("2021-01-01"),as.Date("2021-12-31"))
) # first plot
axis(side=2, at = pretty(range(number_of_books_started)))

par(new = TRUE)
plot(time, COVID_19_stringency, type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", col="red", xlim=c(as.Date("2013-01-01"),as.Date("2021-12-31")))
axis(side=4, at = pretty(range(z)))
mtext("COVID-19 stringency", side=4, line=3)

################################################################################
################################################################################
################################################################################
#create a more accurate recent publish variable:
all_books<- all_books %>% mutate(most_recent = ifelse(!is.na(date_pub), as.numeric(difftime(date_added, date_pub, units ="days")), as.numeric(NA)))
all_books <- all_books %>% mutate(most_recent = ifelse(most_recent < 366, 1,0))
all_books <- all_books %>% mutate(read_time_days = ifelse(read_time_days<365, read_time_days, as.numeric(NA)))



#create the dataset for the remainder of this analysis.
weekly_added_data<- all_books %>% group_by(first_day_of_week_added) %>% summarise(
  av_rating = mean(`user rating`, na.rm=TRUE), 
  av_days_per_book = mean(read_time_days, na.rm = TRUE),
  av_pages_per_day = mean(pages_per_day, na.rm=TRUE),
  perc_nostalgic = mean(nostalgic, na.rm=TRUE),
  perc_re_read = mean(read_count >1),
  perc_recent = mean(most_recent, na.rm=TRUE),
  av_pages_per_book = mean(num_pages, na.rm=TRUE),
  av_ratings_per_book = mean(num_ratings),
  mean_age_of_books = mean(age_of_book, na.rm=TRUE)
  )

#add the covid stringencies
weekly_added_data <- weekly_added_data %>% left_join(covid_stringency, by=c("first_day_of_week_added" = "first_day_of_week"))

#fill the NAs with 0
weekly_added_data<- weekly_added_data %>% replace(is.na(.), 0)

################################################################################
################################################################################
################################################################################
#average days per book (reading pace)
#first plot (all dates)
time <- weekly_added_data$first_day_of_week_added
y <- weekly_added_data$av_days_per_book
left_limits= c(5,20)
time_limits = c(as.Date("2014-01-01"), as.Date("2022-01-01"))
title = "Average days to finish a book compared to COVID-19 Stringency"
left_title = "Days per book"



COVID_19_stringency <- weekly_added_data$Average
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(time, type="l", y, xlab= "", ylab = "", main = title, at=seq(time_limits[1],time_limits[2], by="years") , xlim=time_limits, ylim=left_limits ) # initiate plot layout
axis(side=2, at = pretty(range(left_limits))) # add right ticks
abline(h=pretty(range(left_limits)), v=seq(time_limits[1],time_limits[2], by="years"), col="grey85") # add raster
mtext(left_title, side=2, line=3) # add title to the left 

par(new = TRUE) #add new line

plot(time, y, type = "l",lty=1,lwd=1, axes = FALSE, bty = "n", xlab = "", ylab = "", xlim=time_limits, ylim=left_limits) #add the covid line

mtext("Date (January 1st of year)", side=1, line=3)

par(new = TRUE) #add new line
plot(time, COVID_19_stringency, type = "l", lty=2,lwd=2, axes = FALSE, bty = "n", xlab = "", ylab = "", col="darkgrey" , xlim=time_limits, ylim=c(0,100)) #add the covid line
axis(side=4, at = pretty(range(COVID_19_stringency)))
mtext("COVID-19 stringency", side=4, line=3)

legend("topleft", inset = 0.03, legend = c(left_title, "COVID-19 Stringency"), lty=c(1,2), col=c("black","darkgrey"), lwd =2, bg="white")

#################################################################################

#Compare against base level 2018 & 2019
data_18_19<-weekly_added_data %>% filter(first_day_of_week_added > "2017-12-24" & first_day_of_week_added < "2020-01-01")
data_18_19 <- data_18_19 %>% mutate(week = week(first_day_of_week_added)) %>% group_by(week)%>% summarise(avg = mean(av_days_per_book))

data_20_21 <- weekly_added_data %>% filter(first_day_of_week_added > "2019-12-24" & first_day_of_week_added < "2022-01-01")
data_20_21$week<-week(data_20_21$first_day_of_week_added)

data_20_21<- data_20_21 %>% left_join(data_18_19, by='week')
data_20_21$perc_increase<- data_20_21$av_days_per_book/data_20_21$avg
data_20_21$perc_increase<- data_20_21$perc_increase*100-100


#plot
time <- data_20_21$first_day_of_week_added
number_of_books_added <- data_20_21$perc_increase
COVID_19_stringency20_21 <- data_20_21$Average
left_limits= c(-20, 20)
time_limits = c(as.Date("2020-01-01"), as.Date("2021-01-01"))
title = "Number of books added each week per active user compared to COVID-19 stringency index"
left_title = "% change days to finish a book compared to 2018-2019"


par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(time, type="n", number_of_books_added, xlab= "", ylab = "", main = title, at=seq(time_limits[1],time_limits[2], by="years") , xlim=time_limits, ylim=left_limits ) # initiate plot layout
axis(side=2, at = pretty(range(left_limits))) # add right ticks
abline(h=pretty(range(left_limits)), v=seq(time_limits[1],time_limits[2], by="years"), col="grey85") # add raster
mtext(left_title, side=2, line=3) # add title to the left 

par(new = TRUE) #add new line
plot(time, COVID_19_stringency20_21, type = "l", lty=2,lwd=2, axes = FALSE, bty = "n", xlab = "", ylab = "", col="darkgrey" , xlim=time_limits, ylim=c(0,100)) #add the covid line
axis(side=4, at = pretty(range(COVID_19_stringency20_21)))
mtext("COVID-19 stringency", side=4, line=3)

par(new = TRUE)
plot(time, type="l", lty=1, lwd=2, number_of_books_added, xlab= "", ylab = "", xlim=time_limits, ylim=left_limits)
mtext("Date (2020)", side=1, line=3)

legend("topleft", inset = 0.02, legend = c(left_title, "COVID-19 Stringency"), lty=1:2, col=c("black","darkgray"), lwd =2, bg="white")




#################################################################################
################################################################################
#################################################################################
#plot average number of pages per day

#first plot (all dates)
time <- weekly_added_data$first_day_of_week_added
y <- weekly_added_data$av_pages_per_day
left_limits= c(75,125)
time_limits = c(as.Date("2014-01-01"), as.Date("2022-01-01"))
title = "Average number of pages read per day read compared to COVID-19 Stringency"
left_title = "Pages per day"



COVID_19_stringency <- weekly_added_data$Average
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(time, type="l", y, xlab= "", ylab = "", main = title, at=seq(time_limits[1],time_limits[2], by="years") , xlim=time_limits, ylim=left_limits ) # initiate plot layout
axis(side=2, at = pretty(range(left_limits))) # add right ticks
abline(h=pretty(range(left_limits)), v=seq(time_limits[1],time_limits[2], by="years"), col="grey85") # add raster
mtext(left_title, side=2, line=3) # add title to the left 

par(new = TRUE) #add new line

plot(time, y, type = "l",lwd=1, axes = FALSE, bty = "n", xlab = "", ylab = "", xlim=time_limits, ylim=left_limits) #add the covid line

mtext("Date (January 1st of year)", side=1, line=3)

par(new = TRUE) #add new line
plot(time, COVID_19_stringency, type = "l", lty=2,lwd=2, axes = FALSE, bty = "n", xlab = "", ylab = "", col="darkgrey" , xlim=time_limits) #add the covid line
axis(side=4, at = pretty(range(COVID_19_stringency)))
mtext("COVID-19 stringency", side=4, line=3)

legend("topleft", inset = 0.05, legend = c(left_title, "COVID-19 Stringency"), lty=c(1,2), col=c("black","darkgrey"), lwd =2, bg="white")

#################################################################################
################################################################################
#################################################################################
#plot average rating per book

#first plot (all dates)
time <- weekly_added_data$first_day_of_week_added
y <- weekly_added_data$av_rating
left_limits= c(3.6,4)
time_limits = c(as.Date("2014-01-01"), as.Date("2022-01-01"))
title = "Average rating score per book compared to COVID-19 Stringency"
left_title = "Rating per book"



COVID_19_stringency <- weekly_added_data$Average
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(time, type="l", y, xlab= "", ylab = "", main = title, at=seq(time_limits[1],time_limits[2], by="years") , xlim=time_limits, ylim=left_limits ) # initiate plot layout
axis(side=2, at = pretty(range(left_limits))) # add right ticks
abline(h=pretty(range(left_limits)), v=seq(time_limits[1],time_limits[2], by="years"), col="grey85") # add raster
mtext(left_title, side=2, line=3) # add title to the left 

par(new = TRUE) #add new line

plot(time, y, type = "l",lwd=1, axes = FALSE, bty = "n", xlab = "", ylab = "", xlim=time_limits, ylim=left_limits) #add the covid line

mtext("Date (January 1st of year)", side=1, line=3)

par(new = TRUE) #add new line
plot(time, COVID_19_stringency, type = "l", lty=2,lwd=2, axes = FALSE, bty = "n", xlab = "", ylab = "", col="darkgrey" , xlim=time_limits, ylim=c(0,100)) #add the covid line
axis(side=4, at = pretty(range(COVID_19_stringency)))
mtext("COVID-19 stringency", side=4, line=3)

legend("topleft", inset = 0.03, legend = c(left_title, "COVID-19 Stringency"), lty=c(1,2), col=c("black","darkgrey"), lwd =2, bg="white")


################################################################################
#Compare against base level 2018 & 2019
data_18_19<-weekly_added_data %>% filter(first_day_of_week_added > "2017-12-24" & first_day_of_week_added < "2020-01-01")
data_18_19 <- data_18_19 %>% mutate(week = week(first_day_of_week_added)) %>% group_by(week)%>% summarise(avg = mean(av_rating))

data_20_21 <- weekly_added_data %>% filter(first_day_of_week_added > "2019-12-24" & first_day_of_week_added < "2022-01-01")
data_20_21$week<-week(data_20_21$first_day_of_week_added)

data_20_21<- data_20_21 %>% left_join(data_18_19, by='week')
data_20_21$perc_increase<- data_20_21$av_rating/data_20_21$avg
data_20_21$perc_increase<- data_20_21$perc_increase*100-100


#plot
time <- data_20_21$first_day_of_week_added
number_of_books_added <- data_20_21$perc_increase
COVID_19_stringency20_21 <- data_20_21$Average
left_limits= c(-0.5, 4)
time_limits = c(as.Date("2020-01-01"), as.Date("2022-01-01"))
title = "Number of books added each week per active user compared to COVID-19 stringency index"
left_title = "% change book rating compared to 2018-2019"


par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(time, type="n", number_of_books_added, xlab= "", ylab = "", main = title, at=seq(time_limits[1],time_limits[2], by="years") , xlim=time_limits, ylim=left_limits ) # initiate plot layout
axis(side=2, at = pretty(range(left_limits))) # add right ticks
abline(h=pretty(range(left_limits)), v=seq(time_limits[1],time_limits[2], by="years"), col="grey85") # add raster
mtext(left_title, side=2, line=3) # add title to the left 

par(new = TRUE) #add new line
plot(time, COVID_19_stringency20_21, type = "l", lty=2,lwd=2, axes = FALSE, bty = "n", xlab = "", ylab = "", col="darkgrey" , xlim=time_limits, ylim=c(0,100)) #add the covid line
axis(side=4, at = pretty(range(COVID_19_stringency20_21)))
mtext("COVID-19 stringency", side=4, line=3)

par(new = TRUE)
plot(time, type="l", lty=1, lwd=2, number_of_books_added, xlab= "", ylab = "", xlim=time_limits, ylim=left_limits)
mtext("Date (January 1st of year)", side=1, line=3)

legend("topleft", inset = 0.02, legend = c(left_title, "COVID-19 Stringency"), lty=1:2, col=c("black","darkgray"), lwd =2, bg="white")





















#################################################################################
################################################################################
#################################################################################
#plot average number of pages per day

#first plot (all dates)
time <- weekly_added_data$first_day_of_week_added
y <- weekly_added_data$av_pages_per_book
left_limits= c(280,340)
time_limits = c(as.Date("2014-01-01"), as.Date("2022-01-01"))
title = "Average pages per book compared to COVID-19 Stringency"
left_title = "Pages per book"



COVID_19_stringency <- weekly_added_data$Average
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(time, type="l", y, xlab= "", ylab = "", main = title, at=seq(time_limits[1],time_limits[2], by="years") , xlim=time_limits, ylim=left_limits ) # initiate plot layout
axis(side=2, at = pretty(range(left_limits))) # add right ticks
abline(h=pretty(range(left_limits)), v=seq(time_limits[1],time_limits[2], by="years"), col="grey85") # add raster
mtext(left_title, side=2, line=3) # add title to the left 

par(new = TRUE) #add new line

plot(time, y, type = "l",lwd=1, axes = FALSE, bty = "n", xlab = "", ylab = "", xlim=time_limits, ylim=left_limits) #add the covid line

mtext("Date (January 1st of year)", side=1, line=3)

par(new = TRUE) #add new line
plot(time, COVID_19_stringency, type = "l", lty=2,lwd=2, axes = FALSE, bty = "n", xlab = "", ylab = "", col="darkgrey" , xlim=time_limits, ylim=c(0,100)) #add the covid line
axis(side=4, at = pretty(range(COVID_19_stringency)))
mtext("COVID-19 stringency", side=4, line=3)

legend("topleft", inset = 0.05, legend = c(left_title, "COVID-19 Stringency"), lty=c(1,2), col=c("black","darkgrey"), lwd =2, bg="white")

################################################################################
#Compare against base level 2018 & 2019
data_18_19<-weekly_added_data %>% filter(first_day_of_week_added > "2017-12-24" & first_day_of_week_added < "2020-01-01")
data_18_19 <- data_18_19 %>% mutate(week = week(first_day_of_week_added)) %>% group_by(week)%>% summarise(avg = mean(av_pages_per_book))

data_20_21 <- weekly_added_data %>% filter(first_day_of_week_added > "2019-12-24" & first_day_of_week_added < "2022-01-01")
data_20_21$week<-week(data_20_21$first_day_of_week_added)

data_20_21<- data_20_21 %>% left_join(data_18_19, by='week')
data_20_21$perc_increase<- data_20_21$av_pages_per_book/data_20_21$avg
data_20_21$perc_increase<- data_20_21$perc_increase*100-100


#plot
time <- data_20_21$first_day_of_week_added
number_of_books_added <- data_20_21$perc_increase
COVID_19_stringency20_21 <- data_20_21$Average
left_limits= c(-5,10)
time_limits = c(as.Date("2020-01-01"), as.Date("2022-01-01"))
title = "Number of books added each week per active user compared to COVID-19 stringency index"
left_title = "% change book length compared to 2018-2019"


par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(time, type="n", number_of_books_added, xlab= "", ylab = "", main = title, at=seq(time_limits[1],time_limits[2], by="years") , xlim=time_limits, ylim=left_limits ) # initiate plot layout
axis(side=2, at = pretty(range(left_limits))) # add right ticks
abline(h=pretty(range(left_limits)), v=seq(time_limits[1],time_limits[2], by="years"), col="grey85") # add raster
mtext(left_title, side=2, line=3) # add title to the left 

par(new = TRUE) #add new line
plot(time, COVID_19_stringency20_21, type = "l", lty=2,lwd=2, axes = FALSE, bty = "n", xlab = "", ylab = "", col="darkgrey" , xlim=time_limits, ylim=c(0,100)) #add the covid line
axis(side=4, at = pretty(range(COVID_19_stringency20_21)))
mtext("COVID-19 stringency", side=4, line=3)

par(new = TRUE)
plot(time, type="l", lty=1, lwd=2, number_of_books_added, xlab= "", ylab = "", xlim=time_limits, ylim=left_limits)
mtext("Date (January 1st of year)", side=1, line=3)

legend("topleft", inset = 0.02, legend = c(left_title, "COVID-19 Stringency"), lty=1:2, col=c("black","darkgray"), lwd =2, bg="white")



























#################################################################################
################################################################################
#################################################################################
#plot average number of pages per day

#first plot (all dates)
time <- weekly_added_data$first_day_of_week_added
y <- weekly_added_data$av_ratings_per_book
left_limits= c(100000,300000)
time_limits = c(as.Date("2014-01-01"), as.Date("2022-01-01"))
title = "Average number of ratings per book compared to COVID-19 Stringency"
left_title = "Number of ratings"



COVID_19_stringency <- weekly_added_data$Average
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(time, type="l", y, xlab= "", ylab = "", main = title, at=seq(time_limits[1],time_limits[2], by="years") , xlim=time_limits, ylim=left_limits ) # initiate plot layout
axis(side=2, at = pretty(range(left_limits))) # add right ticks
abline(h=pretty(range(left_limits)), v=seq(time_limits[1],time_limits[2], by="years"), col="grey85") # add raster
mtext(left_title, side=2, line=3) # add title to the left 

par(new = TRUE) #add new line

plot(time, y, type = "l",lwd=1, axes = FALSE, bty = "n", xlab = "", ylab = "", xlim=time_limits, ylim=left_limits) #add the covid line

mtext("Date (January 1st of year)", side=1, line=3)

par(new = TRUE) #add new line
plot(time, COVID_19_stringency, type = "l", lty=2,lwd=2, axes = FALSE, bty = "n", xlab = "", ylab = "", col="darkgrey" , xlim=time_limits) #add the covid line
axis(side=4, at = pretty(range(COVID_19_stringency)))
mtext("COVID-19 stringency", side=4, line=3)

legend("topleft", inset = 0.05, legend = c(left_title, "COVID-19 Stringency"), lty=c(1,2), col=c("black","darkgrey"), lwd =2, bg="white")


#################################################################################
################################################################################
#################################################################################
#plot average number of pages per day

#first plot (all dates)
time <- weekly_added_data$first_day_of_week_added
y <- weekly_added_data$mean_age_of_books
left_limits= c(20,50)
time_limits = c(as.Date("2014-01-01"), as.Date("2022-01-01"))
title = "Average age of books added compared to COVID-19 Stringency"
left_title = "Age of book"



COVID_19_stringency <- weekly_added_data$Average
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(time, type="l", y, xlab= "", ylab = "", main = title, at=seq(time_limits[1],time_limits[2], by="years") , xlim=time_limits, ylim=left_limits ) # initiate plot layout
axis(side=2, at = pretty(range(left_limits))) # add right ticks
abline(h=pretty(range(left_limits)), v=seq(time_limits[1],time_limits[2], by="years"), col="grey85") # add raster
mtext(left_title, side=2, line=3) # add title to the left 

par(new = TRUE) #add new line

plot(time, y, type = "l",lwd=1, axes = FALSE, bty = "n", xlab = "", ylab = "", xlim=time_limits, ylim=left_limits) #add the covid line

mtext("Date (January 1st of year)", side=1, line=3)

par(new = TRUE) #add new line
plot(time, COVID_19_stringency, type = "l", lty=2,lwd=2, axes = FALSE, bty = "n", xlab = "", ylab = "", col="darkgrey" , xlim=time_limits) #add the covid line
axis(side=4, at = pretty(range(COVID_19_stringency)))
mtext("COVID-19 stringency", side=4, line=3)

legend("topleft", inset = 0.05, legend = c(left_title, "COVID-19 Stringency"), lty=c(1,2), col=c("black","darkgrey"), lwd =2, bg="white")


#################################################################################
################################################################################
#################################################################################






#plot percentages of types of books

#first plot (all dates)
time <- weekly_added_data$first_day_of_week_added
y <- weekly_added_data$perc_nostalgic
y2 <- weekly_added_data$perc_recent


left_limits= c(0.1,0.6)
time_limits = c(as.Date("2014-01-01"), as.Date("2022-01-01"))
title = "Fractile of nostalgic, recent and re-read books compared to COVID-19 stringency"
left_title = "Proportion of books"



COVID_19_stringency <- weekly_added_data$Average
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(time, type="l", y, xlab= "", ylab = "", main = title, at=seq(time_limits[1],time_limits[2], by="years") , xlim=time_limits, ylim=left_limits ) # initiate plot layout
axis(side=2, at = pretty(range(left_limits))) # add right ticks
abline(h=pretty(range(left_limits)), v=seq(time_limits[1],time_limits[2], by="years"), col="grey85") # add raster
mtext(left_title, side=2, line=3) # add title to the left 

par(new = TRUE) #add new line
plot(time, y, type = "l",lwd=2, axes = FALSE, bty = "n", xlab = "", ylab = "", xlim=time_limits, ylim=left_limits) #add the covid line
par(new = TRUE) #add new line
plot(time, y2, type = "l",lwd=2, axes = FALSE, bty = "n", xlab = "", ylab = "", xlim=time_limits, ylim=left_limits, col = "grey55") #add the covid line

mtext("Date (January 1st of year)", side=1, line=3)

par(new = TRUE) #add new line
plot(time, COVID_19_stringency, type = "l", lty=2,lwd=2, axes = FALSE, bty = "n", xlab = "", ylab = "", col="darkgrey" , xlim=time_limits, ylim=c(0,100)) #add the covid line
axis(side=4, at = pretty(range(COVID_19_stringency)))
mtext("COVID-19 stringency", side=4, line=3)

legend("topleft", inset = 0.02, legend = c("Nostalgic books", "Recent publishes", "COVID-19 Stringency"), lty=c(1,1,2), col=c("black","grey55", "darkgrey"), lwd =2, bg="white")


#################################################################################
#Compare against base level 2018 & 2019
data_18_19<-weekly_added_data %>% filter(first_day_of_week_added > "2017-12-24" & first_day_of_week_added < "2020-01-01")
data_18_19 <- data_18_19 %>% mutate(week = week(first_day_of_week_added)) %>% group_by(week)%>% summarise(avg_n = mean(perc_nostalgic))

data_20_21 <- weekly_added_data %>% filter(first_day_of_week_added > "2019-12-24" & first_day_of_week_added < "2022-01-01")
data_20_21$week<-week(data_20_21$first_day_of_week_added)

data_20_21<- data_20_21 %>% left_join(data_18_19, by='week')
data_20_21$perc_increase_n<- data_20_21$perc_nostalgic-data_20_21$avg_n
data_20_21$perc_increase_n<- data_20_21$perc_increase_n*100



data_18_19<-weekly_added_data %>% filter(first_day_of_week_added > "2017-12-24" & first_day_of_week_added < "2020-01-01")
data_18_19 <- data_18_19 %>% mutate(week = week(first_day_of_week_added)) %>% group_by(week)%>% summarise(avg_r = mean(perc_recent))


data_20_21<- data_20_21 %>% left_join(data_18_19, by='week')
data_20_21$perc_increase_r<- data_20_21$perc_recent-data_20_21$avg_r
data_20_21$perc_increase_r<- data_20_21$perc_increase_r*100







#plot
time <- data_20_21$first_day_of_week_added
number_of_books_added_n <- data_20_21$perc_increase_n
number_of_books_added_r <- data_20_21$perc_increase_r

COVID_19_stringency20_21 <- data_20_21$Average
left_limits= c(-5,8)
time_limits = c(as.Date("2020-01-01"), as.Date("2022-01-01"))
title = "Number of books added each week per active user compared to COVID-19 stringency index"
left_title = "% change what is read compared to 2018-2019"
left_title_r = "% change recent books compared to 2018-2019"
left_title_n = "% change nostalgic books compared to 2018-2019"

par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(time, type="n", number_of_books_added_n, xlab= "", ylab = "", main = title, at=seq(time_limits[1],time_limits[2], by="years") , xlim=time_limits, ylim=left_limits ) # initiate plot layout
axis(side=2, at = pretty(range(left_limits))) # add right ticks
abline(h=pretty(range(left_limits)), v=seq(time_limits[1],time_limits[2], by="years"), col="grey85") # add raster
mtext(left_title, side=2, line=3) # add title to the left 

par(new = TRUE) #add new line
plot(time, COVID_19_stringency20_21, type = "l", lty=2,lwd=2, axes = FALSE, bty = "n", xlab = "", ylab = "", col="darkgrey" , xlim=time_limits, ylim=c(0,100)) #add the covid line
axis(side=4, at = pretty(range(COVID_19_stringency20_21)))
mtext("COVID-19 stringency", side=4, line=3)

par(new = TRUE)
plot(time, type="l", lty=1, lwd=2, number_of_books_added_n, xlab= "", ylab = "", xlim=time_limits, ylim=left_limits)
mtext("Date (January 1st of year)", side=1, line=3)

par(new = TRUE)
plot(time, type="l", lty=1, lwd=2, number_of_books_added_r, xlab= "", ylab = "", xlim=time_limits, ylim=left_limits, col='darkgrey')
mtext("Date (January 1st of year)", side=1, line=3)




legend("topleft", inset = 0.02, legend = c(left_title_n,left_title_r, "COVID-19 Stringency"), lty=c(1,1,2), col=c("black","darkgray","darkgray"), lwd =2, bg="white")

































#################################################################################
#################################################################################
#################################################################################
#Who are reading
weekly_added_per_user <- all_books %>% group_by(`reader id`,first_day_of_week_added) %>% summarise(
  num_books = sum(dummy)
)

#add the info we miss
user_info_to_add<- user_info %>% select()



weekly_added_per_user2<- weekly_added_per_user %>% leftjoin(user_info_to_add, by = "reader id")

















#################################################################################
# days to finish books filtered for books finished within a year.

all_books_read_time<-all_books %>% filter(read_time_days<365)


all_books_read_time <- all_books_read_time %>% group_by(first_day_of_week_added) %>% summarise(mn= mean(read_time_days),md=median(read_time_days))


all_books_read_time<- all_books_read_time %>% filter(first_day_of_week_added>"2013-12-21" & first_day_of_week_added<"2021-01-01")


#add the covid stringencies
covid_stringency<-covid_stringency %>% select(first_day_of_week, Average)
all_books_read_time <- all_books_read_time %>% left_join(covid_stringency, by=c("first_day_of_week_added" = "first_day_of_week"))

# fill the NAs with 0
all_books_read_time<- all_books_read_time %>% replace(is.na(.), 0)


#################################################################################
#first plot (total number of books added against all dates)
time <- all_books_read_time$first_day_of_week_added
read_time <- all_books_read_time$mn
## second data set on a very different scale
COVID_19_stringency <- all_books_read_time$Average


par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis


plot(time, type="l", read_time, ylim=c(10.5,19), , xlab= "", ylab = "", main = "Average number of days before finishing book compared to COVID-19 stringency index",
     
     at=c(as.Date("2007-01-01"),as.Date("2008-01-01"),as.Date("2009-01-01"),as.Date("2010-01-01"),as.Date("2011-01-01"),as.Date("2012-01-01"),as.Date("2013-01-01"),as.Date("2014-01-01"),as.Date("2015-01-01"),as.Date("2016-01-01"),as.Date("2017-01-01"),
          as.Date("2018-01-01"), as.Date("2019-01-01"),as.Date("2020-01-01"),as.Date("2021-01-01"),as.Date("2022-01-01"))
) # first plot
axis(side=2, at = c(10,11,12,13,14,15,16,17,18,19))

abline(h=c(10,11,12,13,14,15,16,17,18,19), v=c(as.Date("2007-01-01"),as.Date("2008-01-01"),as.Date("2009-01-01"),as.Date("2010-01-01"),as.Date("2011-01-01"),as.Date("2012-01-01"),as.Date("2013-01-01"),as.Date("2014-01-01"),as.Date("2015-01-01"),as.Date("2016-01-01"),as.Date("2017-01-01"),
                                                   as.Date("2018-01-01"), as.Date("2019-01-01"),as.Date("2020-01-01"),as.Date("2021-01-01"),as.Date("2022-01-01"))
       , col="grey85")


mtext("# days to finish book", side=2, line=3)
par(new = TRUE)
plot(time, COVID_19_stringency, type = "l", lty=2,lwd=2, axes = FALSE, bty = "n", xlab = "", ylab = "", col="darkgrey")
axis(side=4, at = pretty(range(COVID_19_stringency)))
mtext("COVID-19 stringency", side=4, line=3)

par(new = TRUE)
plot(time, ylim=c(10.5,19),type="l", lwd=1.5, read_time, xlab= "", ylab = "")
mtext("Date (January 1st of year)", side=1, line=3)


legend("topleft", inset = 0.05, legend = c("Average number of days to finish book", "COVID-19 Stringency"), lty=1:2, col=c("black","darkgray"), lwd =2, bg="white")








