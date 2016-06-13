# Week 2: Assignment - Course Project 1
Brian O'Donnell  
June 12, 2016  



##Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

* Dataset: Activity monitoring data [52K] (https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The variables included in this dataset are:

* steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
* date: The date on which the measurement was taken in YYYY-MM-DD format
* interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.


##Loading and preprocessing the data
Load the data

```r
data = read.csv('activity.csv', stringsAsFactors=FALSE)
```
NOTE: The data file is assumed to be in the current working directory.

Pre-process the data

```r
data$date <- as.Date(data$date, format = "%Y-%m-%d")
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

##What is mean total number of steps taken per day?
###1 Calculate the total number of steps taken per day

```r
dailyTotal <- aggregate(steps ~ date, data, sum)
```
Example data:

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

###2 A histogram of the total number of steps taken each day

```r
hist(dailyTotal$step,
     col="blue", 
     xlab="Total number of steps", 
     main="Histogram of the total number of steps taken per day\n(NA removed)")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

###3 Calculate and report the mean and median of the total number of steps taken per day

```r
meanTotalStepsPerDay <- mean(dailyTotal$steps)
```
The mean of the total number of step taken per day is: 

```
## [1] 10766.19
```


```r
medianTotalStepsPerDay <- median(dailyTotal$steps, na.rm=TRUE)
```
The median of the total number of step taken per day is: 

```
## [1] 10765
```

##What is the average daily activity pattern?
###1 Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
mean_data <- aggregate(data$steps, 
                       by=list(data$interval), 
                       FUN=mean, 
                       na.rm=TRUE)
names(mean_data) <- c("interval", "mean")
plot(mean_data$interval, 
     mean_data$mean, 
     type="l", 
     col="blue", 
     lwd=2, 
     xlab="Interval [minutes]", 
     ylab="Average number of steps", 
     main="Time-series of the average number of steps per intervals\n(NA removed)")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->



###2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_pos <- which(mean_data$mean == max(mean_data$mean))
max_interval <- mean_data[max_pos, 1]
```



The max_interval value is:

```
## [1] 835
```
and has the maximum of 

```
## [1] 104
```
steps.



##Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

###1 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
NA_count <- sum(is.na(data$steps))
```
The number of rows with NAs:

```
## [1] 2304
```

###2 Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
na_pos <- which(is.na(data$steps))
mean_vec <- rep(mean(data$steps, na.rm=TRUE), times=length(na_pos))
```

###3 Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
data[na_pos, "steps"] <- mean_vec
```

###4 Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
dailyTotal <- aggregate(data$steps, 
                       by=list(data$date), 
                       FUN=sum)
names(dailyTotal) <- c("date", "total")
hist(dailyTotal$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="blue", 
     xlab="Total number of steps", 
     ylim=c(0, 30), 
     main="Histogram of the total number of steps taken each day\n(NA replaced by mean value)")
```

![](PA1_template_files/figure-html/unnamed-chunk-18-1.png)<!-- -->


```r
meanTotalStepsPerDay <- mean(dailyTotal$total)
```
The mean of the total number of step taken per day is: 

```
## [1] 10766.19
```


```r
medianTotalStepsPerDay <- median(dailyTotal$total)
```
The median of the total number of step taken per day is: 

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

###1 Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
data <- data.frame(date=data$date, 
                weekday=tolower(weekdays(data$date)), 
                steps=data$steps, 
                interval=data$interval)

data <- cbind(data, 
                daytype=ifelse(data$weekday == "saturday" | 
                data$weekday == "sunday", "weekend", 
                "weekday"))
```
###2 Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
library(lattice)

# Compute the average number of steps taken, averaged across all daytype variable
mean_data <- aggregate(data$steps, 
                       by=list(data$daytype, 
                               data$weekday, data$interval), mean)

# Rename the attributes
names(mean_data) <- c("daytype", "weekday", "interval", "mean")

xyplot(mean ~ interval | daytype, mean_data, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Number of steps", 
       layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-24-1.png)<!-- -->
