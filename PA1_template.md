
--- 
title: "Untitled" 
output: 
  html_document: 
    keep_md: true 
---


###Loading and preprocessing the data
library(ggplot2)

activityData <- read.csv(file="activity.csv", header=TRUE)
dim(activityData)


### WHAT IS MEAN TOTAL NUMBER OF STEPS TAKEN PER DAY?

# 1. Calculate the total number of steps taken per day
totalSteps <- aggregate(steps ~ date, activityData, FUN=sum)

# 2. Make a histogram of the total number of steps taken each day
hist(totalSteps$steps,
     main = "Total Steps per Day",
     xlab = "Number of Steps")

# 3. Calculate and report the mean and median of the total number of steps taken per day
meanSteps <- mean(totalSteps$steps, na.rm = TRUE)
medSteps <- median(totalSteps$steps, na.rm = TRUE)
#Mean of steps = 10766.19
#median of steps =10765



### What is the average daily activity pattern?

# 1. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute 
#interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

meanStepsByInt <- aggregate(steps ~ interval, activityData, mean)
ggplot(data = meanStepsByInt, aes(x = interval, y = steps)) +
  geom_line() +
  ggtitle("Average Daily Activity Pattern") +
  xlab("5-minute Interval") +
  ylab("Average Number of Steps") +
  theme(plot.title = element_text(hjust = 0.5))

# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

maxInt <- meanStepsByInt[which.max(meanStepsByInt$steps),]
maxInt
#interval    steps
#104      835 206.1698


### Imputing missing values

#1. Calculate and report the total number of missing values in the datase
missingVals <- is.na(activityData$steps)
summary(missingVals)
#Mode     FALSE     TRUE 
#logical   15264    2304 

#2. Devise a strategy for filling in all of the missing values in the dataset.
#mean for that 5-minute interval will be used for filling the NA's

#3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
imp_activityData <- transform(activityData,
                              steps = ifelse(is.na(activityData$steps),
                                             meanStepsByInt$steps[match(activityData$interval, 
                                                                        meanStepsByInt$interval)],
                                             activityData$steps))
imp_activityData

#3. Make a histogram of the total number of steps taken each day and report the mean and median.

impStepsByInt <- aggregate(steps ~ date, imp_activityData, FUN=sum)
hist(impStepsByInt$steps,
     main = "Imputed Number of Steps Per Day",
     xlab = "Number of Steps")

impMeanSteps <- mean(impStepsByInt$steps, na.rm = TRUE)
impMedSteps <- median(impStepsByInt$steps, na.rm = TRUE)
diffMean = impMeanSteps - meanSteps
diffMed = impMedSteps - medSteps
diffTotal = sum(impStepsByInt$steps) - sum(totalSteps$steps)
#diffMean=0 #diffMed =1.188679  #diffTotal=86129.51


## Are there differences in activity patterns between weekdays and weekends?

# 1. Create a new factor variable in the dataset with two levels

DayType <- function(date) {
  day <- weekdays(date)
  if (day %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))
    return ("weekeday")
  else if (day %in% c('Saturday', 'Sunday'))
    return ("weekend")
  else
    stop ("Invalid Date Format.")
}
imp_activityData$date <- as.Date(imp_activityData$date)
imp_activityData$day <- sapply(imp_activityData$date, FUN = DayType)


# 2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) 

meanStepsByDay <- aggregate(steps ~ interval + day, imp_activityData, mean)
ggplot(data = meanStepsByDay, aes(x = interval, y = steps)) + 
  geom_line() +
  facet_grid(day ~ .) +
  ggtitle("Average Daily Activity Pattern") +
  xlab("5-minute Interval") +
  ylab("Average Number of Steps") +
  theme(plot.title = element_text(hjust = 0.5))
