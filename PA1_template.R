# Reproducible Research: Peer Assessment 1

AcMon <- read.csv("activity.csv")

#2.  Process/transform the data (if necessary) into a format suitable for your analysis

## What is mean total number of steps taken per day?
#For this part of the assignment, you can ignore the missing values in the dataset.
#1.  Make a histogram of the total number of steps taken each day
sumsAcMon <- aggregate(steps ~ date, data = AcMon, FUN = sum, na.rm=T)
#hist(sumAcMon, breaks=4, col="#CCCCFF", main= "Histogram of total steps per Day", 
#'  xlab="Sums of steps per day")
library(ggplot2)

ggplot(data=sumsAcMon, aes(x=steps)) + 
  xlab("Sums of Steps Per Day")+ 
  geom_histogram(fill= 'lightgreen', colour='black', binwidth=2500) +
  labs(title = "Histogramm of Total Steps Measured Each Day")
### Achsen anpassen

#2.	Calculate and report the mean and median total number of steps taken per day
mean(sumsAcMon[,2])

median(sumsAcMon[,2])

## What is the average daily activity pattern?
#1.  Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
AcTimes <- aggregate(steps ~ interval, data = AcMon, FUN = mean)

ggplot(data=AcTimes, aes(x=interval, y=steps)) + 
  xlab("5 Minute Interval")+ 
  geom_line(colour= 'lightgreen', size=1.1) +
  ylab("Mean of Steps (averaged across all days)") + 
    labs(title = "Average Daily Activity Pattern")
### Achsen ändern, maximum einzeichnen

#2.	Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
AcTimes[which(AcTimes[,2]==max(AcTimes$steps)),]
AcTimes$interval[which(AcTimes[,2]==max(AcTimes$steps))]

## Imputing missing values
#Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
#1.  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
any(is.na(AcMon[,1]))
any(is.na(AcMon[,2]))
any(is.na(AcMon[,3]))
sum(is.na(AcMon[,1]))
#2.	Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
#3.	Create a new dataset that is equal to the original dataset but with the missing data filled in.
AcMon2 <- AcMon
AcMon2[is.na(AcMon2)] <- mean(AcMon2$steps, na.rm=T)

#4.	Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
sumsAcMon2 <- aggregate(steps ~ date, data = AcMon2, FUN = sum)

ggplot(data=sumsAcMon2, aes(x=steps)) + 
  xlab("Sums of Steps Per Day")+ 
  geom_histogram(fill= 'lightgreen', colour='black', binwidth=2500) +
  labs(title = "Histogramm of Total Steps Measured Each Day") +
  theme_bw(base_family = "Avenir", base_size = 15)
### Achsen anpassen

mean(sumsAcMon2[,2])

median(sumsAcMon2[,2])

## Are there differences in activity patterns between weekdays and weekends?
#For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
#1.	Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
AcMon2$date <- as.Date(AcMon2$date)
AcMon2$weekday <- weekdays(AcMon2$date)
AcMon2$typeoday[AcMon2$weekday== "Samstag" | AcMon2$weekday== "Sonntag"] <- 'weekend'
AcMon2$typeoday[AcMon2$weekday!= "Samstag" & AcMon2$weekday!= "Sonntag"] <- 'weekday'
AcMon2$typeoday <- as.factor(AcMon2$typeoday)
### international

#2.	Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

AcTimes2 <- aggregate(AcMon2$steps, by=list(daytype=AcMon2$typeoday, interval= AcMon2$interval), FUN = mean)

head(AcTimes2)

ggplot(data=AcTimes2, aes(x=interval, y=x)) + 
  xlab("5 Minute Interval")+ 
  geom_line(colour= 'lightgreen', size=1) +
  facet_wrap(~ daytype, nrow = 2) + ylab("Mean of Steps (averaged across all days)")+ 
  labs(title = "Average Daily Activity Pattern") +
  theme_bw(base_family = "Avenir", base_size = 15)
### Achsen ändern, maximum einzeichnen