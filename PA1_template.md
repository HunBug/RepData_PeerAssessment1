# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
data <- read.csv("activity.csv", header = TRUE, sep = ",", quote = "\"")
data$date <- as.Date(data$date, format = "%Y-%m-%d")
data2 <- data[!is.na(data$steps), ]
```




## What is mean total number of steps taken per day?

```r
hist(aggregate(data2$steps, by = list(data2$date), FUN = sum)$x, xlab = "Steps each day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
mean(aggregate(data2$steps, by = list(data2$date), FUN = sum)$x)
```

```
## [1] 10766
```

```r
median(aggregate(data2$steps, by = list(data2$date), FUN = sum)$x)
```

```
## [1] 10765
```




## What is the average daily activity pattern?

```r
daily <- aggregate(data2$steps, by = list(data2$interval), FUN = mean)
plot(daily$Group.1, daily$x, type = "l")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
daily[which.max(daily[, 2]), ]$Group.1
```

```
## [1] 835
```




## Imputing missing values

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
# i don't understand but it has som incorrect results: data4 <-
# apply(data3,1, function (x) ifelse( is.na(x[1]), daily[daily[1] ==
# x[3],2], x[3]))
data3 <- data
for (i in 1:nrow(data3)) {
    if (is.na(data3[i, 1])) {
        data3[i, 1] = daily[daily[1] == data3[i, 3], 2]
    }
}
mean(aggregate(data3$steps, by = list(data3$date), FUN = sum)$x)
```

```
## [1] 10766
```

```r
median(aggregate(data3$steps, by = list(data3$date), FUN = sum)$x)
```

```
## [1] 10766
```

```r
# meain is the same because it is filled with means median has changed
# because new values were added
```




## Are there differences in activity patterns between weekdays and weekends?

```r
library(lattice)
data3$weekday <- as.POSIXlt(data3[, 2])$wday < 6
data4 <- aggregate(steps ~ interval + weekday, data = data3, FUN = mean)
data4$weekday <- as.factor(data4$weekday)
levels(data4$weekday) <- c("weekday", "weekend")
xyplot(steps ~ interval | weekday, layout = c(1, 2), data = data4, type = "l", 
    ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


