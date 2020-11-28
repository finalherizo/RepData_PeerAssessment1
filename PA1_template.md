---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---




## Loading and preprocessing the data

```r
library(dplyr)

if (!file.exists("./activity.csv")) {
        unzip("./activity.zip")
}

dataset <- read.csv("activity.csv", colClasses = c("numeric", "Date", "numeric"))
print(head(dataset))
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



## What is mean total number of steps taken per day?

### Computation of the number of steps taken each day


```r
total_steps_per_day <- dataset %>%
  as_tibble %>%
  group_by(date) %>%
  summarize(total_steps = sum(steps, na.rm = TRUE))

print(head(total_steps_per_day))
```

```
## # A tibble: 6 x 2
##   date       total_steps
##   <date>           <dbl>
## 1 2012-10-01           0
## 2 2012-10-02         126
## 3 2012-10-03       11352
## 4 2012-10-04       12116
## 5 2012-10-05       13294
## 6 2012-10-06       15420
```

### Histogram of the total number of steps taken each day


```r
library(lattice)

histogram(~ total_steps, data = total_steps_per_day, xlab = "Total Steps",
          main = "Histogram of the total numbers of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

### Mean and median of the total number of steps taken each day


```r
mean <- mean(total_steps_per_day$total_steps)
median <- median(total_steps_per_day$total_steps)
```
The mean of the total number of steps taken each day is **9354.2295082**.

The median of the total number of steps taken each day is **1.0395\times 10^{4}**.

## What is the average daily activity pattern?

### Computation of the average steps for each interval


```r
average_steps_by_interval <- dataset %>%
  as_tibble %>%
  group_by(interval) %>%
  summarize(mean_steps = mean(steps, na.rm = TRUE))
```

### Time series plot of the 5-minute interval

```r
library(lattice)
xyplot(mean_steps ~ interval, type = "l", data = average_steps_by_interval,
       main = "Time series plot of the 5-minute interval",
       xlab = "Interval",
       ylab = "Average steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


```r
max_steps <- max(average_steps_by_interval$mean_steps)
interval_for_max_steps <- average_steps_by_interval %>%
  filter(mean_steps == max(max_steps)) %>%
  .$interval
```

The interval **835** has the maximum numbers of steps **206.1698113**

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?

