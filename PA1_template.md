# Reproducible Research: Peer Assessment 1
chris-kwas  
## Loading and preprocessing the data

```r
##library for working with data.frames in a more natural way
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.2.1
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
## name of file in current working directory containing data
fileName <- "activity.csv" 
##If file does not exist then leave error message and stop
if(!file.exists(fileName)){ 
    stop("Unable to find file ",fileName,call. = FALSE)
}
##colClasses to apply to data
activityDataColClasses = c("integer","character","integer")
activityData <- read.csv(file = fileName, sep = ",", colClasses = activityDataColClasses)
##activityData <- activityData %>% 
    ##mutate(datetime = paste(date,sprintf("%04d",interval))) %>%
    ##mutate(interval2 = paste(sprintf("%04d",interval)))
##activityData$datetime <- strptime(activityData$datetime,"%Y-%m-%d %H%M")
##activityData$interval2 <- strptime(activityData$interval2,"%H%M")
##activityData <- activityData %>% 
##    select(datetime,date,steps)
```
## What is mean total number of steps taken per day?

```r
by_date <- activityData %>% 
    select(date,steps) %>%
    group_by(date)  %>%
    summarise(sum(steps))
par(mfrow = c(1,1))
hist(by_date$`sum(steps)`,ylim=c(0,30),breaks=7,col="green",xlab="Daily Steps",main = "Histogram of Daily Steps")
dataMean <- mean(na.omit(by_date$`sum(steps)`))
dataMedian  <- median(na.omit(by_date$`sum(steps)`))
abline(v=dataMean,col="red")
abline(v=dataMedian,col="blue")
legendData <- c(paste("   Mean",round(dataMean,0)),paste("Median",round(dataMedian,0)))
legend("topright", bty = "n", cex = .875, lwd = .5, col = c("red", "blue"), xjust = 0, legend = legendData)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

## What is the average daily activity pattern?

```r
by_minute <- activityData %>%
    select(interval,steps) %>%
    group_by(interval)  %>%
    summarise(mean(steps,na.rm=TRUE))
xAxisLabel <- strptime(sprintf("%04d",by_minute$interval), "%H%M")
maxSteps <- max(by_minute$`mean(steps, na.rm = TRUE)`)
maxStepsTime <- by_minute$interval[by_minute$`mean(steps, na.rm = TRUE)` == maxSteps]
maxStepsTime <- sprintf("%04d", maxStepsTime)
xText <- strptime(maxStepsTime, "%H%M")
timeMessage <- substr(xText,12,16)
yText <- maxSteps 
par(mfrow = c(1,1))
plot(by_minute$`mean(steps, na.rm = TRUE)`,type="l", x=xAxisLabel, xlab="Time", ylab="Steps", main="Average Daily Activity")
meanMessage <- sprintf ("High at %s of %s mean steps", timeMessage, round(maxSteps))
text(x=xText,y=yText,pos=4,col="blue",labels = meanMessage)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 



Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in.

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?




## Imputing missing values

```r
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 3.2.1
```

```r
##Missing rows
missingValues <- (c(sum(is.na(activityData$steps)),sum(is.na(activityData$interval)),sum(is.na(activityData$date))))
data.frame(missingValues,row.names = names(activityData))
```

```
##          missingValues
## steps             2304
## date                 0
## interval             0
```

```r
by_mandd <- activityData %>%
    select(date,interval,steps) %>%
    mutate(steps = as.numeric(is.na(steps))) %>%
    group_by(date) %>%
    summarise(sum(steps)) %>%
    filter(`sum(steps)` > 0) %>%
    mutate(weekday = wday(date,label = TRUE)) %>%
    arrange(weekday) %>% print
```

```
## Source: local data frame [8 x 3]
## 
##         date sum(steps) weekday
## 1 2012-11-04        288     Sun
## 2 2012-10-01        288     Mon
## 3 2012-10-08        288     Mon
## 4 2012-11-14        288     Wed
## 5 2012-11-01        288   Thurs
## 6 2012-11-09        288     Fri
## 7 2012-11-30        288     Fri
## 8 2012-11-10        288     Sat
```

```r
## for missing value, use the mean for same interval and weekday
by_mandd <- activityData %>%
    mutate(day = wday(date)) %>%
    select(interval,steps,day) %>%
    group_by(day,interval)  %>%
    summarise(mean(steps,na.rm=TRUE))



newData <- activityData %>%
        mutate(day = wday(date)) ##%>%
        ##filter(date == "2012-10-01" & interval < 35)

adjData <- function(x){
    steps <- x[1]
    if(is.na(steps)){
        day <- as.numeric(x[4])
        interval <- as.numeric(x[3])
        y <- by_mandd[by_mandd$day == day & by_mandd$interval == interval,3]$`mean(steps, na.rm = TRUE)`
        ##print("Hello")
        ##print(day)
        ##print(interval)
        ##print(y)
        ##print("Bye")
        y <- as.numeric(y)
        ##print(y)
        
        y
    }
    else{
        steps;
    }
##by_mandd[by_mandd$day == day & by_mandd$interval == interval,]$`mean(steps, na.rm = ##TRUE)`
}
##head(newData)
newData$steps <- as.numeric(apply(newData,1,adjData))

##apply(newData,1,function(x){a <- x[1]; b <- x[2]; print(a); print(b)})
##newData$steps[is.na(newData$steps)] <- adjData(newData$day,newData$interval)
##head(newData)    
##par(mfrow = c(2,1))

by_date <- newData %>% 
    select(date,steps) %>%
    group_by(date)  %>%
    summarise(sum(steps))
hist(by_date$`sum(steps)`,ylim=c(0,35),breaks=7,col="green",xlab="Daily Steps",main = "Histogram of Daily Steps IMPUTTED")
dataMeanImputted <- mean(na.omit(by_date$`sum(steps)`))
dataMedianImputted  <- median(na.omit(by_date$`sum(steps)`))
abline(v=dataMeanImputted,col="red")
abline(v=dataMedianImputted,col="blue")
legendData <- c(paste("   Mean",round(dataMeanImputted,0)),paste("Median",round(dataMedianImputted,0)))
legend("topright", bty = "n", cex = .875, lwd = .5, col = c("red", "blue"), xjust = 0, legend = legendData) 
text(2500,34.5,cex = .875,paste("   Original Mean",round(dataMean,0)))
text(2500,33,cex = .875,paste("Original Median",round(dataMedian,0)))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

## Are there differences in activity patterns between weekdays and weekends?


```r
library(grid)
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.1
```

```r
a <- cbind(
    c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),
    c("weekday","weekday","weekday","weekday","weekday","weekend","weekend"))
by_minute <- activityData %>%
    select(date,interval,steps) %>%
    ##mutate(day = as.factor(weekdays(as.Date(date)))
    mutate(day = as.factor(a[match(weekdays(as.Date(date)),a),2])) %>%
    group_by(interval,day)  %>%
    summarise(mean(steps,na.rm=TRUE)) %>%
    rename(`Number of steps` = `mean(steps, na.rm = TRUE)`, Interval = interval) 
by_minute$day <- factor(by_minute$day, levels = c("weekend","weekday"))
ggplot(data = by_minute,aes(Interval,`Number of steps`)) + 
    geom_line()+facet_wrap(~day,ncol=1)+
    theme(panel.margin = unit(c(0,0,0,0),"cm"), strip.background = element_rect(fill="lightpink"))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 


##Appendix

by_weekday <- by_minute %>% filter(day == "weekday")
by_weekend <- by_minute %>% filter(day == "weekend")

par(mfrow = c(4,1),mar = c(2,3,2,2), oma = c(3, 3, 1, 1))##B L T R

xAxisLabel <- strptime(sprintf("%04d",by_minute$interval), "%H%M")
plot(1:10,1:10,bg="red"); ##polygon(0,0,bg="red")
plot(by_weekend$`mean(steps, na.rm = TRUE)`,
     type="l",ylab="",xlab="",xaxt="n")
##axis(1, col="dodgerblue", col.ticks="green", col.axis="orange", cex.axis=2)
##title(main = "weekend",bg="blue")
plot(by_weekday$`mean(steps, na.rm = TRUE)`,
     type="l",ylab="",xlab="Interval",axes=FALSE)
mtext("yaxis",2,outer=TRUE,bg="red")
##legend(0,0,"weekend",3,text.col="green",bg="pink")

##plot(by_minute$`mean(steps, na.rm = TRUE)`,type="l", x=xAxisLabel, xlab="Time", ##ylab="Steps", main="Average Daily Activity")