*"This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012 and
include the number of steps taken in 5 minute intervals each day."*

Below are varying analyses conducted on the data starting with loading
the data into R.

Loading and preprocessing the data
----------------------------------

#### 1. Load the data

#### 2. Process/transform the data

    library(knitr)
    setwd("~/Desktop/Coursera/ReproducibleResearch")
    data<-read.csv("activity.csv")
    data2<- na.omit(data)
    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    data3<-tbl_df(data2)

What is mean total number of steps taken per day?
-------------------------------------------------

#### 1. Make a histogram of the total number of steps taken each day

    library(ggplot2)
    teststeps <- tapply(data3$steps, data3$date, sum)
    hist(teststeps, col = "lightblue", xlab = "Total Steps per Day", ylab = "Frequency", 
         main = "Histogram of Total Steps taken per day")

![](Week2_PA1_files/figure-markdown_strict/unnamed-chunk-2-1.png)

#### 2. Calculate and report the mean and median total of steps taken per day

    date_median<-aggregate(data3["steps"], by=data3["date"], median)
    date_mean<-aggregate(data3["steps"], by=data3["date"], mean)
    dmm<-inner_join(date_median, date_mean, by = "date")
    names(dmm)[2:3] <- c("median","mean")
    print(dmm)

    ##          date median       mean
    ## 1  2012-10-02      0  0.4375000
    ## 2  2012-10-03      0 39.4166667
    ## 3  2012-10-04      0 42.0694444
    ## 4  2012-10-05      0 46.1597222
    ## 5  2012-10-06      0 53.5416667
    ## 6  2012-10-07      0 38.2465278
    ## 7  2012-10-09      0 44.4826389
    ## 8  2012-10-10      0 34.3750000
    ## 9  2012-10-11      0 35.7777778
    ## 10 2012-10-12      0 60.3541667
    ## 11 2012-10-13      0 43.1458333
    ## 12 2012-10-14      0 52.4236111
    ## 13 2012-10-15      0 35.2048611
    ## 14 2012-10-16      0 52.3750000
    ## 15 2012-10-17      0 46.7083333
    ## 16 2012-10-18      0 34.9166667
    ## 17 2012-10-19      0 41.0729167
    ## 18 2012-10-20      0 36.0937500
    ## 19 2012-10-21      0 30.6284722
    ## 20 2012-10-22      0 46.7361111
    ## 21 2012-10-23      0 30.9652778
    ## 22 2012-10-24      0 29.0104167
    ## 23 2012-10-25      0  8.6527778
    ## 24 2012-10-26      0 23.5347222
    ## 25 2012-10-27      0 35.1354167
    ## 26 2012-10-28      0 39.7847222
    ## 27 2012-10-29      0 17.4236111
    ## 28 2012-10-30      0 34.0937500
    ## 29 2012-10-31      0 53.5208333
    ## 30 2012-11-02      0 36.8055556
    ## 31 2012-11-03      0 36.7048611
    ## 32 2012-11-05      0 36.2465278
    ## 33 2012-11-06      0 28.9375000
    ## 34 2012-11-07      0 44.7326389
    ## 35 2012-11-08      0 11.1770833
    ## 36 2012-11-11      0 43.7777778
    ## 37 2012-11-12      0 37.3784722
    ## 38 2012-11-13      0 25.4722222
    ## 39 2012-11-15      0  0.1423611
    ## 40 2012-11-16      0 18.8923611
    ## 41 2012-11-17      0 49.7881944
    ## 42 2012-11-18      0 52.4652778
    ## 43 2012-11-19      0 30.6979167
    ## 44 2012-11-20      0 15.5277778
    ## 45 2012-11-21      0 44.3993056
    ## 46 2012-11-22      0 70.9270833
    ## 47 2012-11-23      0 73.5902778
    ## 48 2012-11-24      0 50.2708333
    ## 49 2012-11-25      0 41.0902778
    ## 50 2012-11-26      0 38.7569444
    ## 51 2012-11-27      0 47.3819444
    ## 52 2012-11-28      0 35.3576389
    ## 53 2012-11-29      0 24.4687500

What is the average daily activity pattern?
-------------------------------------------

#### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

    mean_steps <- tapply(data3$steps, data3$interval, mean, na.rm = TRUE)
    plot(row.names(mean_steps), mean_steps, type = "l", xlab = "Time Intervals (5-minute)", 
         ylab = "Mean of steps (all days)", 
         main = "Average Steps Taken at 5 minute Intervals", 
         col = "purple", lwd=2)

![](Week2_PA1_files/figure-markdown_strict/unnamed-chunk-4-1.png)

#### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

    whichmax <- which.max(mean_steps)
    maxsteps <- names(whichmax)
    print("The interval with the highest number of steps is 835")

    ## [1] "The interval with the highest number of steps is 835"

Input missing values
--------------------

#### 1. Calculate and report the total number of missing values in the data set.

    sum(is.na(data))

    ## [1] 2304

#### 2. Devise a strategy for filling in all of the missing values in the dataset.

#### 3. Create a new dataset that is equal to the original data set but with the missing values filled in.

    mean_steps <- tapply(data3$steps, data3$interval, mean, na.rm = TRUE)
    head(data)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

    data[is.na(data)] <- mean_steps
    newdata<- data
    data<-read.csv("activity.csv")
    head(newdata)

    ##       steps       date interval
    ## 1 1.7169811 2012-10-01        0
    ## 2 0.3396226 2012-10-01        5
    ## 3 0.1320755 2012-10-01       10
    ## 4 0.1509434 2012-10-01       15
    ## 5 0.0754717 2012-10-01       20
    ## 6 2.0943396 2012-10-01       25

#### 4a. Make a histogram of the total number of steps taken each day

    nonas_teststeps <- tapply(newdata$steps, newdata$date, sum)
    hist(nonas_teststeps, col = "darkblue", xlab = "Total Steps per Day", ylab = "Frequency", 
         main = "Histogram of Total Steps taken per day")

![](Week2_PA1_files/figure-markdown_strict/unnamed-chunk-8-1.png)

#### 4b. Calculate and report the mean/median total number of steps taken per day

    nonas_date_median<-aggregate(newdata["steps"], by=newdata["date"], median)
    nonas_date_mean<-aggregate(newdata["steps"], by=newdata["date"], mean)
    nonas_dmm<-inner_join(date_median, date_mean, by = "date")
    names(nonas_dmm)[2:3] <- c("median","mean")
    print(nonas_dmm)

    ##          date median       mean
    ## 1  2012-10-02      0  0.4375000
    ## 2  2012-10-03      0 39.4166667
    ## 3  2012-10-04      0 42.0694444
    ## 4  2012-10-05      0 46.1597222
    ## 5  2012-10-06      0 53.5416667
    ## 6  2012-10-07      0 38.2465278
    ## 7  2012-10-09      0 44.4826389
    ## 8  2012-10-10      0 34.3750000
    ## 9  2012-10-11      0 35.7777778
    ## 10 2012-10-12      0 60.3541667
    ## 11 2012-10-13      0 43.1458333
    ## 12 2012-10-14      0 52.4236111
    ## 13 2012-10-15      0 35.2048611
    ## 14 2012-10-16      0 52.3750000
    ## 15 2012-10-17      0 46.7083333
    ## 16 2012-10-18      0 34.9166667
    ## 17 2012-10-19      0 41.0729167
    ## 18 2012-10-20      0 36.0937500
    ## 19 2012-10-21      0 30.6284722
    ## 20 2012-10-22      0 46.7361111
    ## 21 2012-10-23      0 30.9652778
    ## 22 2012-10-24      0 29.0104167
    ## 23 2012-10-25      0  8.6527778
    ## 24 2012-10-26      0 23.5347222
    ## 25 2012-10-27      0 35.1354167
    ## 26 2012-10-28      0 39.7847222
    ## 27 2012-10-29      0 17.4236111
    ## 28 2012-10-30      0 34.0937500
    ## 29 2012-10-31      0 53.5208333
    ## 30 2012-11-02      0 36.8055556
    ## 31 2012-11-03      0 36.7048611
    ## 32 2012-11-05      0 36.2465278
    ## 33 2012-11-06      0 28.9375000
    ## 34 2012-11-07      0 44.7326389
    ## 35 2012-11-08      0 11.1770833
    ## 36 2012-11-11      0 43.7777778
    ## 37 2012-11-12      0 37.3784722
    ## 38 2012-11-13      0 25.4722222
    ## 39 2012-11-15      0  0.1423611
    ## 40 2012-11-16      0 18.8923611
    ## 41 2012-11-17      0 49.7881944
    ## 42 2012-11-18      0 52.4652778
    ## 43 2012-11-19      0 30.6979167
    ## 44 2012-11-20      0 15.5277778
    ## 45 2012-11-21      0 44.3993056
    ## 46 2012-11-22      0 70.9270833
    ## 47 2012-11-23      0 73.5902778
    ## 48 2012-11-24      0 50.2708333
    ## 49 2012-11-25      0 41.0902778
    ## 50 2012-11-26      0 38.7569444
    ## 51 2012-11-27      0 47.3819444
    ## 52 2012-11-28      0 35.3576389
    ## 53 2012-11-29      0 24.4687500

#### 4c. Do these values differ from the estimates from the

#### first part of the assignment? What is the impact of

#### imputing missing data on the estimates of the total

#### daily number of steps?

##### As seen in the median and mean calculations for each individual day, these values do not change when missing values are replaced with the mean of the recorded steps for that interval.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

#### 1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

    library(dplyr)
    library(lubridate)

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

    testdata2 <- data3
    # testdata2<-mutate(data3, day = weekdays(data3$date))

    testdata2$date<-as.Date(data3$date)

    ## Warning in strptime(xx, f <- "%Y-%m-%d", tz = "GMT"): unknown timezone
    ## 'zone/tz/2017c.1.0/zoneinfo/America/New_York'

    testdata2$Weekend <- weekdays(testdata2$date)
    testdata2$Weekend <- grepl("S.+",testdata2$Weekend)


    testdata2$date <- as.Date(testdata2$date)
    weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
    testdata2$day <- factor((weekdays(testdata2$date) %in% weekdays1), 
                       levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))
    testdata2

    ## # A tibble: 15,264 x 5
    ##    steps       date interval Weekend     day
    ##  * <int>     <date>    <int>   <lgl>  <fctr>
    ##  1     0 2012-10-02        0   FALSE weekday
    ##  2     0 2012-10-02        5   FALSE weekday
    ##  3     0 2012-10-02       10   FALSE weekday
    ##  4     0 2012-10-02       15   FALSE weekday
    ##  5     0 2012-10-02       20   FALSE weekday
    ##  6     0 2012-10-02       25   FALSE weekday
    ##  7     0 2012-10-02       30   FALSE weekday
    ##  8     0 2012-10-02       35   FALSE weekday
    ##  9     0 2012-10-02       40   FALSE weekday
    ## 10     0 2012-10-02       45   FALSE weekday
    ## # ... with 15,254 more rows

#### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

    wdaydata<-subset(testdata2, testdata2$day=="weekday")
    wdaymean<-tapply(wdaydata$steps, wdaydata$interval, mean, na.rm = TRUE)

    wenddata<-subset(testdata2, testdata2$day=="weekend")
    wendmean<-tapply(wenddata$steps, wenddata$interval, mean, na.rm = TRUE)


    par(mfrow=c(2,1))
    plot(names(wdaymean),wdaymean, type = "l",
         xlab = "Time Intervals (5-minute)",
         ylab = "Mean of steps",
         main = "Weekdays",
         col = "red", lwd=2)

    plot(names(wendmean),wendmean, type = "l",
         xlab = "Time Intervals (5-minute)",
         ylab = "Mean of steps",
         main = "Weekends",
         col = "green", lwd=2)

![](Week2_PA1_files/figure-markdown_strict/unnamed-chunk-11-1.png)
