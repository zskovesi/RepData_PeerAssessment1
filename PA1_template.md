---
title: 'Reproducible Research: Peer Assessment 1'
author: "zskovesi"
date: "Friday, October 19, 2014"
output: html_document
---
---

####Introduction

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

####Data

The data for this assignment can be downloaded from the course web site: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K].
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

---

### Document setup 

1. Making codes readable `echo = TRUE`.
2. Adjusting scientific notations of numeric items.
3. Set library.

```{r}
echo = TRUE
options(scipen = 1)
library(lattice)
```

### Loading and processing the data

1. Download, unzip and read downloaded file, (i.e. `read.csv()`)
2. Remove zip file.
3. Process/transform the data (if necessary) into a format suitable for the analysis.

```{r}
file.url <- 'http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
file.destination <- 'repdata-data-activity.zip'
download.file(file.url, file.destination)
source.file <- unzip(file.destination)
file.remove(file.destination)
data <- read.csv("activity.csv", colClasses = c("numeric", "character", "numeric"))
head(data,10)
```

### What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day.
2. Calculate and report the mean and median total number of steps taken per day.

```{r} 
# sum of steps taken per day
steps.day <- aggregate(steps ~ date, data=data, sum, na.rm = TRUE)         
# draw histogram
hist(steps.day$steps, main = paste("Total Number of Steps Taken per Day"), col="lightblue", xlab="Number of Steps")         
# draw a blue line for the mean 
abline(v=mean(steps.day$steps), lty=3, col="blue")                        
# draw a red line for the median  
abline(v=median(steps.day$steps), lty=4, col="red")                       
# label the mean  
text(mean(steps.day$steps),25,labels="mean", pos=4, col="blue")           
# label the median  
text(median(steps.day$steps),23,labels="median", pos=4, col="red")         
# draw rug
rug(steps.day$steps, col="blue")                                          
# calculate mean
mean_1 <- round(mean(steps.day$steps), digits=0)                          
# calculate median
median_1 <- round(median(steps.day$steps), digits=0)                        
```

The `mean`  is  `r mean_1`. The  `median`  is `r median_1`.

### What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
# average steps for each interval for all of the days
steps.interval <- aggregate(steps ~ interval, data, mean)
# plot average number of steps of day intervals
plot(steps.interval$interval,steps.interval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="AVG Number of Steps per Day by Interval")
# design histogram and label maximum number of steps
abline(v=steps.interval [which.max(steps.interval$steps),1], lty=3, col="blue")          
text(which.max(steps.interval$steps),max(steps.interval$steps), labels=paste("max = ",as.character(round(max(steps.interval$steps)))), pos=4, col="blue")                 
# which 5-minute interval has the maximum avg.steps, what is the max. of steps
max.interval <- steps.interval[which.max(steps.interval$steps),1]
max.steps <- round(max(steps.interval$steps))
```

Averaging all of the days in the data set, the exact 5-minute interval, that has the maximum number of steps is `r max.interval`. The maximum number of steps in that interval is `r max.steps`.

### Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)

```{r}
data.NA <- sum(is.na(data))
```

The number of rows with `NA`s we ignored until this point is `r data.NA`.

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or 
the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
# use mean in 5-minute intervals to replase NAs
steps.AVG <- aggregate(steps ~ interval, data = data, FUN = mean)
adjust.NAs <- numeric() 
for (i in 1:nrow(data)) {
    observations <- data[i, ]  
if (is.na(observations$steps)) {
    steps <- subset(steps.AVG, interval == observations$interval)$steps} 
else {
    steps <- observations$steps}
    adjust.NAs <- c(adjust.NAs, steps)}
# create new dataset with the missing data filled in 
data.new <- data
data.new$steps <- adjust.NAs
```

4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates 
from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
 
```{r}
# recalculate total steps by day
steps.day_2 <- aggregate(steps ~ date, data = data.new, sum, na.rm = TRUE)
# create histogram on the adjusted observations
hist(steps.day_2$steps, main = paste("Total Steps Taken per Day"), col="blue", xlab="Number of Steps")
# create histogram to show difference
hist(steps.day$steps, main = paste("Total Steps Taken per Day"), col="lightblue", xlab="Number of Steps", add=TRUE)
legend("topright", c("Imputed", "Non-Imputed"), col=c("blue", "lightblue"), lwd=3)
# calculate new mean
mean_2 <- round((mean(steps.day_2$steps)), digits=2)
# calculate new median
median_2 <- round((median(steps.day_2$steps)), digits=2)
# calculate delta between imputed and non-imputed data
mean_delta <- round((mean_2 - mean_1), digits=2)
median_delta <- round((median_2 - median_1), digits=2)
# show total delta
total_delta <- round((sum(steps.day_2$steps) - sum(steps.day$steps)), digits=0)
```


* The new/imputed data mean is `r mean_2`.
* The new/imputed data median is `r median_2`.
* The difference between the non-imputed mean and imputed mean is `r mean_delta`.
* The difference between the non-imputed mean and imputed mean is `r median_delta`.
* By the replacement of NA values we added `r total_delta` extra steps in the new/imputed data.
After imputing NA rows the mean is the same but the median is a slightly different.

### Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

``` {r}
# new factor variable
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
data.new$day_of_week = as.factor(ifelse(is.element(weekdays(as.Date(data.new$date)),weekdays), "Weekday", "Weekend"))
steps.interval_2 <- aggregate(steps ~ interval + day_of_week, data.new, mean)
# draw planel plot
xyplot(steps.interval_2$steps ~ steps.interval_2$interval|steps.interval_2$day_of_week, main="AVG Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```

Histogram shows higher peaks at the beginning of weekdays. Overall we can observe more steps average on weekends. 