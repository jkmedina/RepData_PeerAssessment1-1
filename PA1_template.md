# Reproducible Research: Peer Assessment 1


```r
opts_chunk$set(echo = TRUE)
```


The data for this report ([activity.zip](https://github.com/rdpeng/RepData_PeerAssessment1/commit/9abcd99566ad027022e4aafbffb5768f9ee11182#diff-d732846a7761c0c1ec91a3ba65791c60)) was obtained from the Coursera "Reproducible Research" Data Science course.  The 'activity.zip' file was extracted as: 'activity.csv'.

## Loading and preprocessing the data


```r
data <- read.csv(unz("activity.zip", "activity.csv"), header = TRUE, quote = "\"", 
    sep = ",", na.strings = c("NA"), colClasses = c(steps = "integer", date = "character", 
        interval = "integer"))
data$date <- as.POSIXct(data$date, format = "%Y-%m-%d", tz = "US/Pacific")
```




## The mean total number of steps taken per day


```r

# Calculate and report the mean and median total number of steps taken per
# day
removed_steps_na_data <- subset(data, !is.na(steps))
sum_steps_per_day <- aggregate(removed_steps_na_data$steps, by = list(removed_steps_na_data$date), 
    sum)

# Make a histogram of the total number of steps taken each day
library(ggplot2)
ggplot(sum_steps_per_day, aes(Group.1, x)) + geom_histogram(stat = "identity", 
    binwidth = 24 * 60 * 60) + xlab("Day") + ylab("# Steps") + ggtitle("# Steps Per Day")
```

![plot of chunk part2_1](figure/part2_1.png) 

```r

mean_steps_per_day <- mean(sum_steps_per_day$x)
median_steps_per_day <- median(sum_steps_per_day$x)

mean_steps_per_day_str <- sprintf("%.4f", mean_steps_per_day)
median_steps_per_day_str <- sprintf("%d", median_steps_per_day)
```



## The mean and median of the total number of steps taken per day

The **mean** total number of steps taken per day is **10766.1887 steps**.

The **median** total number of steps taken per day is **10765 steps**.


## The average daily activity pattern


```r

# Make a time series plot (i.e. type = 'l') of the 5-minute interval
# (x-axis) and the average number of steps taken, averaged across all days
# (y-axis)

ggplot(data = removed_steps_na_data, mapping = aes(x = interval, y = steps)) + 
    stat_summary(fun.y = mean, geom = "line", mapping = aes(group = 1)) + xlab("5 Minute Interval") + 
    ylab("Avg. # of Steps Over All Days") + ggtitle("Average Daily Activity Pattern")
```

![plot of chunk part3_1](figure/part3_1.png) 



```r

# Which 5-minute interval, on average across all the days in the dataset,
# contains the maximum number of steps?

mean_steps_per_5min <- aggregate(removed_steps_na_data$steps, by = list(removed_steps_na_data$interval), 
    mean)

max_steps_on_average_in_5min_interval <- max(mean_steps_per_5min$x)

interval_with_max_steps_on_average <- subset(mean_steps_per_5min, x == max_steps_on_average_in_5min_interval)$Group.1
```


The interval with the maximum number of steps, on average (across all days) is **835**.



## Imputing missing values


```r

# Calculate and report the total number of missing values in the dataset
# (i.e. the total number of rows with NAs)
total_number_of_missing_values <- nrow(subset(data, is.na(steps) | is.na(date) | 
    is.na(interval)))
```


There are **2304** rows with NAs in the dataset.


```r

# Devise a strategy for filling in all of the missing values in the dataset.

# There are no missing days (i.e. 61 days in October and November combined)
num_unique_days <- length(unique(data$date))
stopifnot(num_unique_days == 61)

# There are no missing intervals (i.e. 288 each day)
num_intervals_per_day <- aggregate(data$interval, by = list(data$date), length)
stopifnot(num_intervals_per_day$x == 288)

stopifnot(nrow(mean_steps_per_5min) == 288)

# Only steps are left to fill-in.  Create a new dataset that is equal to the
# original dataset but with the missing data filled in.  Use mean of the 5
# minute interval for all other days.

impute <- function(steps, interval) {
    ifelse(is.na(steps), mean_steps_per_5min$x[mean_steps_per_5min$Group.1 == 
        interval], steps)
}
data$imputed_steps <- mapply(impute, data$steps, data$interval)

stopifnot(nrow(data) == (288 * 61))
```




```r

# Make a histogram of the total number of steps taken each day and Calculate
# and report the mean and median total number of steps taken per day. Do
# these values differ from the estimates from the first part of the
# assignment? What is the impact of imputing missing data on the estimates
# of the total daily number of steps?

sum_steps_per_day_imputed <- aggregate(data$imputed_steps, by = list(data$date), 
    sum)
```


For the following plot, the data was **imputed** by taking the mean of the number of steps for that particular 5 minute interval from the other days where it was not missing.


```r
ggplot(sum_steps_per_day_imputed, aes(Group.1, x)) + geom_histogram(stat = "identity", 
    binwidth = 24 * 60 * 60) + xlab("Day") + ylab("# Steps") + ggtitle("# Steps Per Day (Imputed)")
```

![plot of chunk part4_4](figure/part4_4.png) 



```r

# Calculate and report the mean and median total number of steps taken per
# day
mean_steps_per_day_imputed <- mean(sum_steps_per_day_imputed$x)
mean_steps_per_day_imputed_str <- sprintf("%.4f", mean_steps_per_day_imputed)
median_steps_per_day_imputed <- median(sum_steps_per_day_imputed$x)
median_steps_per_day_imputed_str <- sprintf("%.4f", median_steps_per_day_imputed)
```


The **mean** total number of steps taken per day (from imputed dataset) is **10766.1887 steps**.

The **median** total number of steps taken per day (from imputed dataset) is **10766.1887 steps**.

Using an imputed dataset (using the mean of intervals), the overall mean remained unchanged frmo the non-imputed dataset.  This is expected since adding more 'mean' values will not change the center.  The median has come closer (exact) to the mean in the imputed dataset.  This is also not surprising since there are more 'central' (mean) numbers in the middle of the dataset where the median could be found.

The impact of imputing missing data on the estimates of the total daily number of steps shows an overall increase compared to non-imputation (as shown in the histogram).



## Are there differences in activity patterns between weekdays and weekends?


```r

data$day_type <- factor(ifelse(weekdays(data$date) %in% c("Saturday", "Sunday"), 
    "weekend", "weekday"))

ggplot(data = data, mapping = aes(x = interval, y = imputed_steps)) + stat_summary(fun.y = mean, 
    geom = "line", mapping = aes(group = 1)) + xlab("Interval") + ylab("Number of steps") + 
    ggtitle("Average Weekday/Weekend Activity Pattern") + facet_wrap(~day_type, 
    ncol = 1)
```

![plot of chunk part5_1](figure/part5_1.png) 


As the plot above shows, there are differences in the weekend vs. weekday activity patterns.
