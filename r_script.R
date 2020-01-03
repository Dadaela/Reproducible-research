#  Step 1 - Code for reading in the dataset and/or processing the data
library(lubridate)
library(dplyr)
activity_data<-read.csv("C:/Users/daniela.dimichele/Desktop/coursera/corso_5/test_2/activity.csv",header=TRUE,sep=",")
summary(activity_data)
activity_data$date<-ymd(activity_data$date)


#INFORMATION ABOUT THE VARIABLES
str(activity_data)
head(activity_data)
str(activity_data)
data_1<-with(activity_data,tapply(steps,date,sum,na.rm=TRUE))


##Step 2 - Histogram of the total number of steps taken each day
hist(data_1,
     main = "Histogram of the Total Number of Steps Taken per Day", 
     xlab = "Number of Steps", 
     ylab = "Number of days",
     col  = "Green",
     breaks=10)
abline(v = mean(data_1), lty = 1, lwd = 2, col = "orange")
abline(v = median(data_1), lty = 2, lwd = 2, col = "black")
legend(x = "topleft", c("Mean", "Median"), col = c("orange", "black"), 
       lty = c(1, 2), lwd = c(2, 2))

##Step 3 - Mean and median number of steps taken each day
mean(data_1)
median(data_1)
summary(data_1)

##Step 4 - Time series plot of the average number of steps taken
Group_Activity <- group_by(activity_data, interval)
Activity_mean<-summarize(Group_Activity, steps= mean(steps, na.rm = TRUE ))
Activity_mean

plot(Activity_mean$interval, steps,
     col="green",
     xlab = "Time interval", 
     ylab = "Mean Steps Every day", 
     main = "Mean Steps by Date")

##Step 5 - The 5-minute interval that, on average, contains the maximum number of steps
Activity_mean[which.max(Activity_mean$steps), ]$interval
summary(activity_data)

##Step 6 - Code to describe and show a strategy for imputing missing data
sum(is.na(activity_data$steps))

index<-which(is.na(activity_data$steps))
l<-length(index)
steps_avg<-with(activity_data,tapply(steps,date,mean,na.rm=TRUE))
na<-mean(steps_avg,na.rm = TRUE)
for (i in 1:l) {
  activity_data[index[i],1]<-na
}
sum(is.na(activity_data$steps))

str(activity_data)

##Step 7 - Histogram of the total number of steps taken each day after missing values are imputed

data_fi<-with(activity_data,tapply(steps,date,sum,na.rm=TRUE))
hist(data_fi,breaks=10,
     col = "red",
     xlab = "Total Steps",
     ylab = "Frequency",
     main = "Total Number of Steps per Day")
abline(v = mean(data_fi), lty = 1, lwd = 2, col = "orange")
abline(v = median(data_fi), lty = 2, lwd = 2, col = "black")
legend(x = "topleft", c("Mean", "Median"), col = c("orange", "black"), 
       lty = c(1, 2), lwd = c(2, 2))

print(mean_steps<-mean(data_fi))
print(median_steps<-median(data_fi))


##Step 8 - Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

library(dplyr)

activity_day<- mutate(activity_data, day = if_else(weekdays(activity_data$date) == "sabato" | weekdays(activity_data$date) == "domenica", "weekend", "weekday"))
activity_day$day<-as.factor(activity_day$day)
str(activity_day)


act_w_end<-subset(activity_day,as.character(activity_day$day)=="weekend")
act_w_day<-subset(activity_day,as.character(activity_day$day)=="weekday")

steps_w_end<-with(act_w_end,tapply(steps,interval,mean,na.rm=TRUE))
steps_w_day<-with(act_w_day,tapply(steps,interval,mean,na.rm=TRUE))

int_w_end<-unique(act_w_end$interval)
int_w_day<-unique(act_w_day$interval)

new_w_end<-data.frame(cbind(steps_w_end,int_w_end))
new_w_day<-data.frame(cbind(steps_w_day,int_w_day))

par(mfrow=c(2,1),mar=c(4,4,2,1))
plot(new_w_end$int_w_end,new_w_end$steps_w_end,
     type = "l",
     xlab = "Intervals",
     ylab = "Average Steps",
     main = "Weekend")
plot(new_w_day$int_w_day,new_w_day$steps_w_day,
     type = "l",
     xlab = "Intervals",
     ylab = "Average Steps",
     main = "Weekday")
