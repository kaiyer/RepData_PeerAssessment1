library(dplyr)
library(ggplot2)
library(lattice)
library(foreach)
# load data
data <- read.table(unz("activity.zip", "activity.csv"), nrows=17570, header=T, quote="\"", sep=",")
data$date <- as.Date(data$date)
# drop the NA's
data.df <- data[!is.na(data$steps),]
# summary
summary(data.df)
# now summarize data
# apply group_by and compute daily total steps
by_date.df <- group_by(data.df, date)
by_date.summary <- summarise(by_date.df, totsteps=sum(steps),nr=length(interval))
# plot histogram of ata
hist(by_date.summary$totsteps,main="Histogram of total daily steps",xlab="total daily steps", ylab="count")
# mean and median of daily total steps
all_data.summary <- summarise(by_date.summary, mean.steps = mean(totsteps),median.steps=median(totsteps))
# now create dataframe for daily activity pattern and plot the data
by_interval.df <- group_by(data.df, interval)
by_interval.summary <- summarise(by_interval.df, mean.steps=mean(steps))
plot(by_interval.summary$interval,by_interval.summary$mean.steps, type='l', main="steps vs intervals", xlab= "interval" , ylab="mean steps")# find the max value of steps during the day and then find the interval at which it occurs
maxsteps <- max(by_interval.summary$mean.steps)
maxsteps
by_interval.summary[by_interval.summary$mean.steps==maxsteps,]$interval
maxinterval <- by_interval.summary[by_interval.summary$mean.steps==maxsteps,]$interval
maxinterval
intervalnum <- maxinterval/5 + 1
intervalnum
# find number of NA's
summary(data)
na.df <- data[is.na(data$steps),]
# now backfill NA's with mean of by_interval.df
interval2steps <- function(interval) {
  by_interval.summary[by_interval.summary$interval==interval,]$mean.steps
}
backfill_na <- function(interval) {
  na.df[na.df$interval==interval,]$steps <<- interval2steps(interval)
  0
}
intervals.df <- unique(by_interval.df$interval)

foreach(i=1:length(intervals.df)) %do% backfill_na(intervals.df[i])
backfilled.df <- rbind(na.df, data.df)
# now create a table by date
backfilled_by_date.df <- group_by(backfilled.df, date)
backfilled_by_date.summary <- summarise(backfilled_by_date.df, totsteps=sum(steps),nr=length(interval))
# plot histogram of data
hist(backfilled_by_date.summary$totsteps,xlab="total daily steps", ylab="count")
# mean and median of daily total steps
backfilled_all_data.summary <- summarise(backfilled_by_date.summary, mean.steps = mean(totsteps),median.steps=median(totsteps))
# now create dataframe for weekend/weekday activity pattern
backfilled_by_date.df$wend <- as.factor(ifelse(weekdays(backfilled_by_date.df$date) %in% c("Saturday","Sunday"), "weekend", "weekday")) 
# compute data frame with summary by interval 
wday_by_interval.df <- group_by(backfilled_by_date.df[backfilled_by_date.df$wend=="weekday",], interval)
wday_by_interval.summary <- summarise(wday_by_interval.df, mean.steps=mean(steps))
wday_by_interval.summary$wend <- as.factor("weekday")
mean(wday_by_interval.summary$mean.steps)

wend_by_interval.df <- group_by(backfilled_by_date.df[backfilled_by_date.df$wend=="weekend",], interval)
wend_by_interval.summary <- summarise(wend_by_interval.df, mean.steps=mean(steps))
wend_by_interval.summary$wend <- as.factor("weekend")
mean(wend_by_interval.summary$mean.steps)

activity_by_interval.summary <- rbind(wday_by_interval.summary,wend_by_interval.summary)

xyplot(mean.steps~interval|wend, data=activity_by_interval.summary, as.table=TRUE,ylab="Number of Steps", xlab="Interval", type='l')
