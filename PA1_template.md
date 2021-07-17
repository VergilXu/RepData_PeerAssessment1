---
title: "PA1_template"
author: "Steven"
date: "2021/7/4"
output:
  html_document: default
  pdf_document: default
---


```r
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
```

Loading packages


```r
library(ggplot2)
library(dplyr)
library(lattice)
```

<font color = red> 1. Code for reading in the dataset and/or processing the data </font>


```r
activity <- read.csv("./activity.csv")
```

<font color = red> 2. Histogram of the total number of steps taken each day </font>


```r
activity <- activity[which(!is.na(activity$steps)), ]
sum_steps <- aggregate(steps~date, activity, sum, na.rm = TRUE)
g <- ggplot(sum_steps, aes(x = date, y = steps))
g + geom_histogram(stat='identity') + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) + labs(title = "The total number of steps taken each day") +  theme(plot.title = element_text(hjust = 0.5, size = 15))
```

![plot of chunk histgram](figure/histgram-1.png)

<font color = red> 3. Mean and median number of steps taken each day </font>
 

```r
group_activity <- group_by(activity, date)
mean_steps <- data.frame(summarize(group_activity, mean(steps)))
median_steps <- data.frame(summarize(group_activity, median(steps)))
mean_median_steps <- merge(mean_steps, median_steps, by.x = "date", by.y = "date", all.x = TRUE)
mean_median_steps
```

```
##          date mean.steps. median.steps.
## 1  2012-10-02   0.4375000             0
## 2  2012-10-03  39.4166667             0
## 3  2012-10-04  42.0694444             0
## 4  2012-10-05  46.1597222             0
## 5  2012-10-06  53.5416667             0
## 6  2012-10-07  38.2465278             0
## 7  2012-10-09  44.4826389             0
## 8  2012-10-10  34.3750000             0
## 9  2012-10-11  35.7777778             0
## 10 2012-10-12  60.3541667             0
## 11 2012-10-13  43.1458333             0
## 12 2012-10-14  52.4236111             0
## 13 2012-10-15  35.2048611             0
## 14 2012-10-16  52.3750000             0
## 15 2012-10-17  46.7083333             0
## 16 2012-10-18  34.9166667             0
## 17 2012-10-19  41.0729167             0
## 18 2012-10-20  36.0937500             0
## 19 2012-10-21  30.6284722             0
## 20 2012-10-22  46.7361111             0
## 21 2012-10-23  30.9652778             0
## 22 2012-10-24  29.0104167             0
## 23 2012-10-25   8.6527778             0
## 24 2012-10-26  23.5347222             0
## 25 2012-10-27  35.1354167             0
## 26 2012-10-28  39.7847222             0
## 27 2012-10-29  17.4236111             0
## 28 2012-10-30  34.0937500             0
## 29 2012-10-31  53.5208333             0
## 30 2012-11-02  36.8055556             0
## 31 2012-11-03  36.7048611             0
## 32 2012-11-05  36.2465278             0
## 33 2012-11-06  28.9375000             0
## 34 2012-11-07  44.7326389             0
## 35 2012-11-08  11.1770833             0
## 36 2012-11-11  43.7777778             0
## 37 2012-11-12  37.3784722             0
## 38 2012-11-13  25.4722222             0
## 39 2012-11-15   0.1423611             0
## 40 2012-11-16  18.8923611             0
## 41 2012-11-17  49.7881944             0
## 42 2012-11-18  52.4652778             0
## 43 2012-11-19  30.6979167             0
## 44 2012-11-20  15.5277778             0
## 45 2012-11-21  44.3993056             0
## 46 2012-11-22  70.9270833             0
## 47 2012-11-23  73.5902778             0
## 48 2012-11-24  50.2708333             0
## 49 2012-11-25  41.0902778             0
## 50 2012-11-26  38.7569444             0
## 51 2012-11-27  47.3819444             0
## 52 2012-11-28  35.3576389             0
## 53 2012-11-29  24.4687500             0
```

<font color = red> 4. Time series plot of the average number of steps all the days </font>


```r
group_activity <- group_by(activity, interval)
mean_interval_steps <- data.frame(summarize(group_activity, mean(steps)))
mean_interval_steps <- rename(mean_interval_steps, steps = "mean.steps.")
g <- ggplot(mean_interval_steps, aes(x = interval, y = steps))
g + geom_line(size = 1) + labs(title = "The average number of steps per 5 minutes") + theme(plot.title = element_text(hjust = 0.5, size = 15))
```

![plot of chunk average number of steps](figure/average number of steps-1.png)

<font color = red> 5. The 5-minute interval that, on average, contains the maximum number of steps </font>


```r
max_steps_interval <- filter(mean_interval_steps, steps == max(mean_interval_steps$steps))$interval
```

The maximum number of steps in a 5-minute interval is 835.

<font color = red> 6. Code to describe and show a strategy for imputing missing data </font>


```r
activity <- read.csv("./activity.csv")
missing_value <- activity[is.na(activity$steps), ]
# length(missing_value$steps)
integer_interval_steps <- round(mean_interval_steps)
Imputing_missing_value <- merge(missing_value, integer_interval_steps, by.x = "interval", by.y = "interval", all.x = TRUE)
missing_value$id <- 1:nrow(missing_value)
Imputing_missing_value <- arrange(merge(missing_value, integer_interval_steps, by.x = "interval", by.y = "interval", all.x = TRUE), id)
Imputing_missing_value <- Imputing_missing_value[ ,c(-2,-4)]
noNa_activity <- activity
noNa_activity[is.na(noNa_activity$steps), ]$steps <- Imputing_missing_value$steps.y
noNa_activity <- noNa_activity
```

<font color = red> 7. Histogram of the total number of steps taken each day after missing values are imputed </font>


```r
noNa_sum_steps <- aggregate(steps~date, noNa_activity, sum)
g <- ggplot(noNa_sum_steps, aes(x = date, y = steps))
g + geom_histogram(stat='identity') + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) + labs(title = "The total number of steps taken each day after imputing missing value") +  theme(plot.title = element_text(hjust = 0.5, size = 15))
```

![plot of chunk histgram for total numer os steps after missing values are imputed](figure/histgram for total numer os steps after missing values are imputed-1.png)

```r
noNA_group_activity <- group_by(noNa_activity, date)
noNA_mean_steps <- data.frame(summarize(noNA_group_activity, mean(steps)))
noNA_median_steps <- data.frame(summarize(noNA_group_activity, median(steps)))
```

<font color = red> 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends </font>


```r
temp <- merge(noNA_mean_steps, mean_steps, by.x = "date", by.y = "date", all.x = TRUE)
temp <- merge(temp, noNA_median_steps, by.x = "date", by.y = "date", all.x = TRUE)
temp <- merge(temp, median_steps, by.x = "date", by.y = "date", all.x = TRUE)
Imputation_contrast <- rename(temp, meansteps_afterImputation = mean.steps..x, meansteps_beforeImputation = mean.steps..y, mediansteps_afterImputation = median.steps..x, mediansteps_beforeImputation= median.steps..y)
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
noNa_activity$date <- as.Date(noNa_activity$date)
noNa_activity$label <- weekdays(noNa_activity$date, abbreviate = TRUE)
noNa_activity[which(grepl("S(at|un)", noNa_activity$label)), ]$label <- "weekend"
noNa_activity[(noNa_activity$label != "weekend"), ]$label <- "weekday"
noNa_activity$label <- as.factor(noNa_activity$label)
plot <- aggregate(noNa_activity[,1], by = list(interval = noNa_activity$interval, label = noNa_activity$label), FUN = mean)
plot <- rename(plot, steps = x)
xyplot(steps ~ interval| label, data = plot, layout = c(1, 2), type = "l", main = "The average number of steps taken per 5-minute interval across weekdays and weekends")
```

![plot of chunk comparing the average number of steps taken per 5-minute interval across weekdays and weekends](figure/comparing the average number of steps taken per 5-minute interval across weekdays and weekends-1.png)
