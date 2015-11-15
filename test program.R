

setwd("C:/Users/CLT/Google Drive/Data Science/course5/Project1")


## Read data
activity<- read.csv("activity.csv")

activity$date<-as.Date(activity$date)

#### What is mean total number of steps taken per day?
## Calculate the total number of steps taken per day
steps.total <- aggregate(steps ~ date, activity, sum)

## Histogram of the total number of steps taken each day
hist(steps.total$steps, col='blue', breaks=10, xlab = "Number of steps", main="Total number of steps taken per day")

## Mean and median of the total number of steps taken per day
mean(steps.total$steps)
median(steps.total$steps)

#### What is the average daily activity pattern?

## Calculate the average number of steps taken on avery 5-minute interval, averaged across all days
steps.average <- aggregate(steps ~ interval, activity, mean)

## Make a time series plot
xyplot(steps.average$steps~steps.average$interval, type="l", 
       xlab="5-minute interval", ylab='Average number of steps across all days',
       main='The time series plot of the average number of steps taken on avery 5-minute interval', col="blue",
       panel = function(x, y,...) {
         panel.xyplot(x, y, type='l') 
         panel.abline(v=subset(steps.average, steps==max(steps))$interval, lty="dotted",col="red")
       })

## The 5-minute interval with the maximum average steps
subset(steps.average, steps==max(steps))$interval

#### Imputing missing values
## Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA
sum(!is.na(activity$steps))

activityx<-merge(activity, steps.average, by="interval")
activityx1<-activityx[is.na(activityx$steps.x),]
activityx2<-activityx[!is.na(activityx$steps.x),]

## impute the missing values by the 5-minute average steps 
steps<-as.integer(round(activityx1$steps.y))
activityx1$steps<-steps
activityx1$steps.x<-NULL
activityx1$steps.y<-NULL

## Retain the non-missing values
activityx2$steps.y<-NULL
colnames(activityx2)[2]<-"steps"

## Combine two dataframes to obtain a new dataset that is equal to the original dataset but with the missing data filled in.
activity.impute<-rbind(activityx1, activityx2)
activity.impute<-activity.impute[order(activity.impute$interval, activity.impute$date),]

## Calculate the total number of steps taken per day
steps.total.impute <- aggregate(steps ~ date, activity.impute, sum)

## Histogram of the total number of steps taken each day
hist(steps.total.impute$steps, col='blue', breaks=10, xlab = "Number of steps", 
     main="Total number of steps taken per day for the imputted data")

## Mean and median of the total number of steps taken per day
mean(steps.total.impute$steps)
median(steps.total.impute$steps)

### Are there differences in activity patterns between weekdays and weekends?

## Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating 
## whether a given date is a weekday or weekend day.
newvar<-weekdays(activity.impute$date)
newvarx<-as.factor(ifelse(newvar %in% c("Saturday","Sunday"), "Weekend","Weekday"))
activity.impute$index.weekday<-newvarx

## Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, 
## averaged accross all weekday days or weekend days.
steps.avg <- aggregate(steps ~ interval + index.weekday, activity.impute, mean)

xyplot(steps~interval | index.weekday, data=steps.avg, type="l", layout=c(1,2), 
       xlab="5-minute interval", ylab='Average number of steps across all days',
       main='The time series plot of the average number of steps taken on avery 5-minute interval', col="blue",
       )

#### The end of the code #####

