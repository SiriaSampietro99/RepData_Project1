---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: true
  word_document: default
---

## Introduction

This is an R Markdown document, created for the Coursera course "Reproducible Research", in completion of "Peer Assessment 1". The assignment requires students to write an R markdown document evidencing literate programming, using markdown and R programming techniques. There are 5 primary questions to be answered, dealing with processing and analysing data. The data provided to be worked upon, is called "activity monitoring data".

### The Data

The data provided for use, is derived from a study whereupon a single individual wore a "personal activity monitoring device". The study says that:

The device used in this particular data set collects data on the number of steps taken by an individual, in 5 minute intervals. Two months of data, October/November 2012 are included within the data set. The variables measured include steps (the number of steps taken), date (the day on which the steps measurement was taken) and interval, (the interval in which the steps measurement was taken.) The data is stored in csv format, with 17,598 observations and the aforementioned 3 variables recorded.

## Completing the Assignment

### Question 1: Loading and preprocessing the data

The data must be in the user's current working directory for the code to run correctly. The unzip function extracts the data from the zip file, before it is read into R. The object classes contained within each of the variables are defined, so as to speed up the reading process.


``` r
dataset <- read.csv("/Users/siriasampietro/Desktop/activity.csv", sep=",", header=TRUE)
```

An initial look at the data confirms its dimensions and contents.


``` r
head(dataset)
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


``` r
str(dataset)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

### Question 2: What is mean total number of steps taken per day?

1- Calculate the total number of steps taken per day


``` r
library(openxlsx)
library(readxl)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

``` r
library(ggplot2)
library(ggthemes)
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```


``` r
totalStepsdf <- dataset %>%
  rename(day = date) %>%
  group_by(day) %>%
  summarise(steps = sum(steps, na.rm = TRUE)) %>%
  ungroup()
```

2- Make a histogram of the total number of steps taken each day


``` r
plot1 <- ggplot(totalStepsdf, aes(x = steps)) + 
  geom_histogram(breaks = seq(0, 25000, by = 2500), fill = "#83CAFF", col = "black") + 
  ylim(0, 30) + 
  xlab("Total Steps Taken Per Day") + 
  ylab("Frequency") + 
  ggtitle("Total Number of Steps Taken on a Day") + 
  theme_calc(base_family = "serif")

print(plot1)
```

![](Project-1_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

3- Calculate and report the mean and median of the total number of steps taken per day


``` r
meansteps <- mean(totalStepsdf$steps)
print(paste0("MEAN: ", meansteps))
```

```
## [1] "MEAN: 9354.22950819672"
```

``` r
mediansteps <- median(totalStepsdf$steps)
print(paste0("MEDIAN: ", mediansteps))
```

```
## [1] "MEDIAN: 10395"
```

### Question 3: What is the average daily activity pattern?

1- Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


``` r
averageDailyActivity <- dataset %>% 
  rename(day = date) %>%
  group_by(interval) %>%
  summarise(steps = mean(steps, na.rm = TRUE)) %>%
  ungroup()

plot2 <- ggplot(averageDailyActivity, mapping = aes(interval, steps)) + 
  geom_line(col = "blue") +
  xlab("Interval") + 
  ylab("Average Number of Steps") + 
  ggtitle("Average Number of Steps Per Interval") +
  theme_calc(base_family = "serif")

print(plot2)
```

![](Project-1_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

2- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


``` r
maxinterval <- averageDailyActivity[which.max(averageDailyActivity$steps), ]$interval
print(maxinterval)
```

```
## [1] 835
```

### Question 4: Imputing missing values

1- Calculate and report the total number of missing values in the dataset


``` r
missing <- nrow(dataset %>% filter(is.na(steps)))
print(paste0("Number of Missing Values: ",missing))
```

```
## [1] "Number of Missing Values: 2304"
```

2- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


``` r
imputedSteps <- averageDailyActivity$steps[match(dataset$interval, averageDailyActivity$interval)]
```

3- Create a new dataset that is equal to the original dataset but with the missing data filled in.


``` r
activityImputed <- transform(dataset, 
                             steps = ifelse(is.na(dataset$steps), yes = imputedSteps, no = dataset$steps))

head(activityImputed)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

4- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


``` r
totalActivityImputed <- activityImputed %>%
  rename(day = date) %>%
  group_by(day) %>%
  summarise(steps = sum(steps, na.rm = TRUE)) %>%
  ungroup()

plot3 <- ggplot(totalActivityImputed, aes(x = steps)) + 
  geom_histogram(breaks = seq(0, 25000, by = 2500), fill = "#83CAFF", col = "black") + 
  ylim(0, 30) + 
  xlab("Total Steps Taken Per Day") + 
  ylab("Frequency") + 
  ggtitle("Total Number of Steps Taken on a Day") + 
  theme_calc(base_family = "serif")

print(plot3)
```

![](Project-1_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


``` r
meansteps2 <- mean(totalActivityImputed$steps)
print(paste0("MEAN: ",meansteps2))
```

```
## [1] "MEAN: 10766.1886792453"
```

``` r
mediansteps2 <- median(totalActivityImputed$steps)
print(paste0("MEDIAN: ", mediansteps2))
```

```
## [1] "MEDIAN: 10766.1886792453"
```

### Question 5: Are there differences in activity patterns between weekdays and weekends?

1- Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


``` r
activityImputed <- activityImputed %>%
  mutate(dayType = ifelse(wday(date) %in% c(1, 7), "weekend", "weekday")) %>%
  mutate(dayType = factor(dayType, levels = c("weekday", "weekend")))

table(activityImputed$dayType)
```

```
## 
## weekday weekend 
##   12960    4608
```

2- Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


``` r
activityByDay <- activityImputed %>% 
  group_by(dayType,interval) %>% 
  summarise(steps = mean(steps, na.rm = TRUE), .groups = "drop") %>%
  ungroup() %>% 
  arrange(interval,dayType)

plot4 <-  ggplot(activityByDay, aes(x = interval , y = steps, color = dayType)) + 
  geom_line() + ggtitle("Average Daily Steps by Day Type") + 
  xlab("Interval") + 
  ylab("Average Number of Steps") +
  facet_wrap(~dayType, ncol = 1, nrow=2) +
  scale_color_discrete(name = "Day Type") +
  theme_calc(base_family = "serif")

print(plot4)
```

![](Project-1_files/figure-html/unnamed-chunk-16-1.png)<!-- -->
