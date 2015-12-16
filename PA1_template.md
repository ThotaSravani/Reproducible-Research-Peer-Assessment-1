# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

The data for this assignment is contained in a zipped file on the course Github repository.  It can be accessed  at the time of analysis.  However, to ensure that the data is available for analysis even in the absence of an internet connection, this implementation has downloaded the file to be accessed locally at any time.  The structure of the loaded dataframe is shown below.

```r
data1 <- "E:Projects/Reproducible-Research-peer-Assessment-1/Reproducible-Research-Peer-Assessment-1/activity.csv"
data <- read.csv(data1)
str(data)

```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is the total number of steps taken per day?

The following histogram displays the total number of steps taken per day, ignoring missing values.

```r
library(ggplot2)

data.date <- aggregate(data[1],by=data[2],FUN = sum,na.rm=TRUE)

hist(data.date$steps, breaks=20, col ="red",main = "Histogram of Total Number of Steps Taken Per Day",xlab="Steps Per Day")

```

![plot of  unnamed-chunk-1](Figure/unnamed-chunk-1.png)

```r
mean(adata.date$steps)   
```

```
## [1] 9354
```

```r
median(adata.date$steps) 
```

```
## [1] 10395
```

**Mean:** Ignoring missing values, the mean number of steps taken per day is 9,354.

**Median:** Ignoring missing values, the median number of steps taken per day is 10,395.

## What is the average daily activity pattern?
Following is a time-series plot of the average number of steps taken during each 5-minute interval during the day.

```r
# The following data set contains the sample data aggregated by interval.

library(ggplot2)

averages <- aggregate(x=list(steps=data$steps),by=list(interval=data$interval),FUN=mean,na.rm=TRUE)

ggplot(data=averages,aes(x=interval,y=steps))+ geom_line()+xlab("5-minute interval")+ylab("Average number of steps taken")


```

![plot of chunk unnamed-chunk-2](Figure/unnamed-chunk-2.png)

**On average across all the days in the dataset,the 5-minute interval contains the maximum number of steps?**


```r
averages[which.max(averages$steps),]
```

```
##    interval steps
## 104  835 206.2
```

The maximum average steps per period occur at interval 835, with an average of 206.2 steps per 5 minutes (calculated inline).

## Imputing missing values

**1) Number of Missing Values**

```r
# Total records
nrow(data)
```

```
## [1] 17568
```

```r
# Missing records
sum(is.na(data$steps))
```

```
## [1] 2304
```
The total number of records in the dataset is 17,568.  Of these, 2,304 contain missing values for number of steps taken.

**2) & 3) - Replacing Missing Values and Creating a New Dataset**

Missing values for steps per interval are replaced by the mean number of steps for that interval, calculated on the non-missing rows.  A new dataset is created that contains these imputed values.

```r
library(plyr)

impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

data.impute <- ddply(data, ~interval, transform, steps = impute.mean(steps))
```

**4) Histogram, Mean and Median**

The following is a histogram of the total number of steps per day using imputed values for missing values.

```r
# The following data set contains the sample data aggregated by date.

data.impute.date <- aggregate(data.impute[1],by=data.impute[2],FUN=sum,na.rm=TRUE)

hist(data.impute.date$steps,breaks=20,col = "steelblue",main="Histogram of Total Number of Steps Taken Per Day(Imputed Data)",xlab="Steps Per Day")
```

![plot of chunk unnamed-chunk-3](Figure/unnamed-chunk-3.png) 

```r
mean(data.impute.date$steps)  
```

```
## [1] 10766
```

```r
median(data.impute.date$steps) 
```

```
## [1] 10766
```

**Mean:** Ignoring missing values, the mean number of steps taken per day, rounded to the nearest step, is 10,766.

**Median:** Ignoring missing values, the median number of steps taken per day, rounded to the nearest step, is 10,766.

Mean and median values are higher after imputing missing data. The reason is that in the original data, there are some days with steps values NA for any interval. The total number of steps taken in such days are set to 0s by default. However, after replacing missing steps values with the mean steps of associated interval value, these 0 values are removed from the histogram of total number of steps taken each day.



## Are there differences in activity patterns between weekdays and weekends?

**1. New Factor Variable**

A new factor variable, Day, is created, indicating whether a record pertains to a weekday or to a weekend.

```r

data.impute$dateP <- as.POSIXlt(data.impute$date,format="%Y-%m-%d")

data.impute$day <- "Weekday"

data.impute$day [weekdays(data.impute$dateP) %in% c("Saturday","Sunday")]

 <- "Weekend"

```

**2. Weekday/Weekend Comparison**

The following time series plots display the weekday and weekend data separately.  


```r

data.impute.interval <- aggregate(data.impute[1],
                                   by=data.impute[c(3,5)],
                                   FUN=mean,
                                   na.rm=TRUE)

library(ggplot2)

plot <- ggplot(data = data.impute.interval,aes(x=interval,y=steps))+ geom_line()+xlab("5-minute interval")+ylab("Number Of Steps") 
  
plot + geom_line() + facet_wrap(~day,nrow = 2)



```

![plot of chunk unnamed-chunk-4](Figure/unnamed-chunk-4.png) 
