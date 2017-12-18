---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading the data

```r
library(ggplot2) # package required for plotting
```

```
## Warning: package 'ggplot2' was built under R version 3.4.2
```

```r
library(Hmisc) # package required for impute function used in the second part of the assignment
```

```
## Warning: package 'Hmisc' was built under R version 3.4.3
```

```
## Loading required package: lattice
```

```
## Loading required package: survival
```

```
## Loading required package: Formula
```

```
## Warning: package 'Formula' was built under R version 3.4.1
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```
#### Load the data (i.e. read.csv())

```r
if(!file.exists('activity.csv')){
        unzip('activity.zip')
}

activityData <- read.csv('activity.csv')
```
## What is mean total number of steps taken per day?

#### 1. Calculate the total number of steps taken per day

```r
stepsByDay <- tapply(activityData$steps, activityData$date, sum, na.rm=TRUE)
```

#### 2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
qplot(stepsByDay, xlab='Total steps per day', ylab='Frequency using binwith 500', binwidth=500)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

#### 3. Calculate and report the mean and median of the total number of steps taken per day


```r
stepsByDayMean <- mean(stepsByDay)
stepsByDayMedian <- median(stepsByDay)
```
* Mean:   9354.23
* Median: 10395

## What is the average daily activity pattern?


```r
avgStepsPerTimeInt <- aggregate(x=list(meanSteps=activityData$steps), by=list(interval=activityData$interval), FUN=mean, na.rm=TRUE)
```

#### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
ggplot(data=avgStepsPerTimeInt, aes(x=interval, y=meanSteps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("Average number of steps taken") 
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

#### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxSteps <- which.max(avgStepsPerTimeInt$meanSteps)
timeMaxSteps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", avgStepsPerTimeInt[maxSteps,'interval'])
```
* Max steps is at: 8:35

## Imputing missing values

#### 1. Calculate and report the total number of missing values in the dataset

```r
missingValues <- length(which(is.na(activityData$steps)))
```
* Number of missing values: 2304  

#### 2. Devise a strategy for filling in all of the missing values in the dataset
#### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
# Both questions clubbed
# Replace each missing value with the mean value

impActivityData <- activityData
impActivityData$steps <- impute(impActivityData$steps, fun = mean)
```
  
#### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
impStepsByDay <- tapply(impActivityData$steps, impActivityData$date, sum)

qplot(impStepsByDay, xlab='Total steps per day (Imputed)', ylab='Frequency using binwith 500', binwidth=500)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
impMeanStepsByDay <- mean(impStepsByDay)
impMedianStepsByDayMedian <- median(impStepsByDay)
```
* Imputed Mean:   1.0766189\times 10^{4}
* Imputed Median: 1.0766189\times 10^{4}

####### Mean and median values are higher after imputing missing data. The reason is that in the original data, there are some days with steps values NA for any interval. The total number of steps taken in such days are set to 0s by default. However, after replacing missing steps values with the mean steps, these 0 values are removed from the histogram of total number of steps taken each day.  

## Are there differences in activity patterns between weekdays and weekends?

#### 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
impActivityData$dateType <-  ifelse(as.POSIXlt(impActivityData$date)$wday %in% c(0,6), 'weekend', 'weekday')
```

#### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
impAveragedActivityData <- aggregate(steps ~ interval + dateType, data=impActivityData, mean)

ggplot(impAveragedActivityData, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(dateType ~ .) +
    xlab("5-minute interval") + 
    ylab("Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
