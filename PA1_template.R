## Loading and PreProcessing the data

data1 <- "E:Projects/Reproducible-Research-peer-Assessment-1/Reproducible-Research-Peer-Assessment-1/activity.csv"
data <- read.csv(data1)
str(data)

## What is the total number of steps taken per day?

# The following histogram displays the total number of steps taken per day,ignoring missing values

library(ggplot2)
data.date <- aggregate(data[1],by=data[2],FUN = sum,na.rm=TRUE)
hist(data.date$steps, breaks=20, col ="red",main = "Histogram of Total Number of Steps Taken Each Day",xlab="Steps Taken Each Day")
# Mean
mean(data.date$steps)
#Median
median(data.date$steps)
 
## What is the average daily activity pattern?

#Following is a time-series plot of the average number of steps taken during each 5-minute interval during the day 

library(ggplot2)
averages <- aggregate(x=list(steps=data$steps),by=list(interval=data$interval),FUN=mean,na.rm=TRUE)
ggplot(data=averages,aes(x=interval,y=steps))+ geom_line()+xlab("5-minute interval")+ylab("Average number of steps taken")
averages[which.max(averages$steps),]


## Imputing missing values

# Number of missing values

nrow(data)
sum(is.na(data$steps))

# Replacing missing values and creating a new dataset

library(plyr)
impute.mean <- function(x) replace(x,is.na(x), mean(x,na.rm = TRUE))
data.impute <- ddply(data,~interval,transform,steps=impute.mean(steps))

# Histogram,Mean and Median

data.impute.date <- aggregate(data.impute[1],by=data.impute[2],FUN=sum,na.rm=TRUE)
hist(data.impute.date$steps,breaks=20,col = "steelblue", main="Histogram of Total NO.Of Steps Taken Each Day(Imputed Data)",xlab="Steps Taken Each Day")
# Mean
mean(data.impute.date$steps)
# Median 
median(data.impute.date$steps)

## Are there differences in activity patterns between weekdays and weekends?

# New factor variable

data.impute$dateP <- as.POSIXlt(data.impute$date,format="%Y-%m-%d")
data.impute$day <- "Weekday"
data.impute$day [weekdays(data.impute$dateP) %in% c("Saturday","Sunday")] <- "Weekend"

# Weekend/Weekday comparision

data.impute.interval <- aggregate(data.impute[1],
                                   by=data.impute[c(3,5)],
                                   FUN=mean,
                                   na.rm=TRUE)
library(ggplot2)
plot <- ggplot(data = data.impute.interval,aes(x=interval,y=steps))+ geom_line()+xlab("5-minute interval")+ylab("Number Of Steps")   
plot + geom_line() + facet_wrap(~day,nrow = 2)



     