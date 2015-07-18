---
title: 'PA1_template.Rmd '
author: 'kevin wang '
date: "Saturday, July 18, 2015"
output: html_document
---

---
title: 'Reproducible Research: Peer Assessment 1'
author: "kevin wang"
date: "Saturday, July 18, 2015"
output: html_document
---


Basic settings

```r
echo = TRUE 
```

## Load data into R

```r
P1<-read.csv("C:/Users/Xi/Documents/Reproducible Research/activity.csv",sep=",",header=TRUE)
library(ggplot2)
```

##What is mean total number of steps taken per day?
###1.Calculate the total number of steps taken per day

```r
aggregatedperday <- aggregate(steps~date,P1,sum)
aggregatedperdaymean <- aggregate(steps~interval,P1,mean)
```
###2.If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
barplot(aggregatedperday$steps, names.arg =aggregatedperday $date, xlab = "date", ylab = "steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 


###3.Calculate and report the mean and median of the total number of steps taken per day

```r
mean(aggregatedperday$steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(aggregatedperday$steps, na.rm=TRUE)
```

```
## [1] 10765
```
##What is the average daily activity pattern?
###1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
P1$interval <- as.factor(P1$interval)
P1Interval <- aggregate(steps~interval,P1,mean)

library(ggplot2)
pp1<-ggplot(aggregatedperdaymean ,aes(x=interval,y=steps))+
  geom_line()+xlab("Interval [min]")+ylab("Steps")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
pp1
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 
###2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
P1Interval$interval[which.max(P1Interval$steps)]
```

```
## [1] 835
## 288 Levels: 0 5 10 15 20 25 30 35 40 45 50 55 100 105 110 115 120 ... 2355
```
##Imputing missing values
###1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(P1$steps))
```

```
## [1] 2304
```
###2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
datamerge= merge(P1, P1Interval, by="interval")
```
###3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
datamerge$steps.x[is.na(datamerge$steps.x)]=datamerge$steps.y[is.na(datamerge$steps.x)]

aggregatedmerge1 <- aggregate(steps.x~interval,datamerge,sum)
aggregatedmerge2 <- aggregate(steps.x~date,datamerge,sum)
```
###4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
barplot(aggregatedmerge2$step,names.arg=aggregatedmerge2$date,xlab = "date", ylab = "steps")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

```r
mean(aggregatedmerge2$steps.x,na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(aggregatedmerge2$steps.x,na.rm=TRUE)
```

```
## [1] 10766.19
```
##Are there differences in activity patterns between weekdays and weekends?
###1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
datamerge$day<-as.POSIXlt(datamerge$date)$wday
datamerge$day[datamerge$day %in% c(0, 6) ] <- "weekend"
datamerge$day[datamerge$day %in% c(1:5) ] <- "weekday"

averageStepsByIntervalWeekday =  tapply(subset(datamerge, day=="weekday")$steps.x, subset(datamerge, day=="weekday")$interval, mean)
averageStepsByIntervalWeekend =  tapply(subset(datamerge, day=="weekend")$steps.x, subset(datamerge, day=="weekend")$interval, mean)
```
###2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
par(mfrow = c(2, 1))
plot(averageStepsByIntervalWeekday, type="l", xlab="interval", ylab="Number of steps", main="weekdays")
plot(averageStepsByIntervalWeekend, type="l", xlab="interval", ylab="Number of steps", main="weekend")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 





