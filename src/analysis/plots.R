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

#remove the columns we do not need
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

# make sure we can save the plot
dir.create("../../gen/output")
png(file="../../gen/output/fig2_nr_books.png",
    width=800, height=400)


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
axis(side=4, at = pretty(range(COVID_19_stringency)))
mtext("COVID-19 stringency", side=4, line=3)

par(new = TRUE)
plot(time, type="l", lwd=2, number_of_books_added, xlab= "", ylab = "")
mtext("Date (January 1st of year)", side=1, line=3)


legend("topleft", inset = 0.05, legend = c("Number of books added", "COVID-19 Stringency"), lty=1:2, col=c("black","darkgray"), lwd =2, bg="white")

dev.off()


################################################################################
# compute number of books added per active user

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


all_books_weekly_added$week<- week(all_books_weekly_added$first_day_of_week_added)




#now compute for each week in our dataset the average number of books added per user
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
#plot books per user from 2014 onward
time <- all_books_weekly_added$first_day_of_week_added
number_of_books_added <- all_books_weekly_added$added_per_user
left_limits= c(0, 1)
time_limits = c(as.Date("2014-01-01"), as.Date("2022-01-01"))
title = "Number of books added each week per active user"
left_title = "# books added per active user"


png(file="../../gen/output/fig3_nr_books_per_user.png",
    width=500, height=400)

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

dev.off()

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
title = "Percentage Change in Number of books added per active user for each week"
left_title = "% more books per user compared to 2018-2019"


png(file="../../gen/output/fig4_percentage_change_nr_books_per_user.png",
    width=500, height=400)

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


dev.off()

################################################################################
################################################################################
################################################################################

#create the dataset for the remainder of this analysis.
all_books <- all_books %>% mutate(read_time_days_limit = ifelse(read_time_days<366, read_time_days, as.numeric(NA)))


weekly_added_data<- all_books %>% group_by(first_day_of_week_added) %>% summarise(
  av_rating = mean(`user rating`, na.rm=TRUE), 
  av_days_per_book = mean(read_time_days_limit, na.rm = TRUE),
  perc_nostalgic = mean(nostalgic, na.rm=TRUE),
  perc_recent = mean(recent, na.rm=TRUE),
  av_pages_per_book = mean(num_pages, na.rm=TRUE),
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


png(file="../../gen/output/fig5_days_per_book.png",
    width=500, height=400)

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


dev.off()
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

png(file="../../gen/output/fig6_percentage_change_days_per_book.png",
    width=500, height=400)


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

dev.off()


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


png(file="../../gen/output/fig9_rating.png",
    width=500, height=400)

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

dev.off()

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


png(file="../../gen/output/fig10_percentage_change_rating.png",
    width=500, height=400)


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

dev.off()


#################################################################################
################################################################################
#################################################################################
#plot average number of pages per book

#first plot (all dates)
time <- weekly_added_data$first_day_of_week_added
y <- weekly_added_data$av_pages_per_book
left_limits= c(280,340)
time_limits = c(as.Date("2014-01-01"), as.Date("2022-01-01"))
title = "Average pages per book compared to COVID-19 Stringency"
left_title = "Pages per book"

png(file="../../gen/output/fig7_pages_per_book.png",
    width=500, height=400)


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

dev.off()
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


png(file="../../gen/output/fig8_percentage_change_pages_per_book.png",
    width=500, height=400)

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

dev.off()



#################################################################################
################################################################################
#################################################################################
# plot percentages of types of books

# first plot 
time <- weekly_added_data$first_day_of_week_added
y <- weekly_added_data$perc_nostalgic
y2 <- weekly_added_data$perc_recent


left_limits= c(0.1,0.6)
time_limits = c(as.Date("2014-01-01"), as.Date("2022-01-01"))
title = "Fractile of nostalgic, recent and re-read books compared to COVID-19 stringency"
left_title = "Proportion of books"
COVID_19_stringency <- weekly_added_data$Average


png(file="../../gen/output/fig11_proportions_of_books.png",
    width=500, height=400)

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

dev.off()

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

png(file="../../gen/output/fig12_percentage_change_proportion_of_books.png",
    width=500, height=400)

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


dev.off()
