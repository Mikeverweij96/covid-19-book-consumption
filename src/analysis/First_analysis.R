

#################################################################################
#Check reliability of date added variable
date_table<-all_books2%>% select(date_added,date_started_month, date_read_month, dummy)

date_table$diff_read<- abs(difftime(date_table$date_added, date_table$date_read_month, units = "days"))
date_table$diff_started<- abs(difftime(date_table$date_added, date_table$date_started_month, units = "days"))
df<-date_table%>% select(diff_read, diff_started)
df$diff_read<-as.numeric(df$diff_read)
df$diff_started<-as.numeric(df$diff_started)
date_table$min_diff<- apply(df,1,FUN=min)

lessthan<- date_table %>% filter(min_diff<63)
morethan<- date_table %>% filter(min_diff>62)
nrow(lessthan)/(nrow(lessthan)+nrow(morethan))


date_table$min_diff<-ifelse(date_table$date_added <date_table$date_read_month & date_table$date_added>date_table$date_started_month, 0, date_table$min_diff)


lessthan<- date_table %>% filter(min_diff<63)
morethan<- date_table %>% filter(min_diff>62)
nrow(lessthan)/(nrow(lessthan)+nrow(morethan))



#################################################################################

#First plots:
books_per_date<- all_books2 %>% group_by(date_added_test) %>% summarise(total = sum(dummy))

books_per_date2 <- subset(books_per_date, date_added_test < "2022-01-01" & date_added_test > "2016-01-01")


ggplot(books_per_date2, aes(x = date_added_test, y = total)) +
  geom_point() +  geom_smooth()



#Per week:
books_per_date3 <- subset(books_per_date, date_added_test > "2017-01-01" & date_added_test < "2021-01-01")
books_per_date3$week <- format(books_per_date3$date_added_test, "%Y-%V")

books_per_week<- books_per_date3 %>% group_by(week) %>% summarise(tot = sum(total))


ggplot(books_per_week, aes(x = week, y = tot)) +
  geom_point() 


#Per month:
books_per_date4 <- subset(books_per_date, date_added_test > "2015-01-01" & date_added_test < "2022-01-01")
books_per_date4$month <- format(books_per_date4$date_added_test, "%Y-%m")

books_per_month<- books_per_date4 %>% group_by(month) %>% summarise(tot = sum(total))
books_per_month$dummy<-1:nrow(books_per_month)

ggplot(books_per_month, aes(x = dummy, y = tot)) +
  geom_point() + geom_line()

#################################################################################

#average reading time for month started with reading
overview_readtimes_started<- all_books2 %>% filter(read_time_months <20*30.42) %>% group_by(date_started_monthonly) %>% summarise(avgspeed_complete=mean(read_time_days, na.rm =  TRUE), avgspeed_monthonly = mean(read_time_months, na.rm =  TRUE))
overview_readtimes_started<- overview_readtimes_started %>% filter(date_started_monthonly >"2015-01-01" & date_started_monthonly < "2022-01-01")

ggplot(overview_readtimes_started, aes(x =date_started_monthonly , y = avgspeed_complete)) +
  geom_point() + geom_line()
#So, it looks like that books started in the first lockdown month are read on average as fast as books started in December (Holiday period)

#average reading time for month finished with reading
overview_readtimes_finished<- all_books2 %>% group_by(dateread_monthonly) %>% summarise(avgspeed_complete=mean(read_time_days, na.rm =  TRUE), avgspeed_monthonly = mean(read_time_months, na.rm =  TRUE))
overview_readtimes_finished<- overview_readtimes_finished %>% filter(dateread_monthonly >"2015-01-01" & dateread_monthonly < "2022-01-01")

ggplot(overview_readtimes_finished, aes(x = dateread_monthonly, y = avgspeed_complete)) +
  geom_point() + geom_line()
#It looks like people finish books they have been reading for longer in the december months, and also during the first lockdown period.

