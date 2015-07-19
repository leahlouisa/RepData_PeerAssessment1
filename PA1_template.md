# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
First the data must be loaded into R, and the dates must be interpreted as dates.

'''{r loadingAndProcessing}
library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
fitnessData <- read.csv("activity.csv")
fitnessData$date <- as.Date(fitnessData$date, "%Y-%m-%d")
'''


## What is mean total number of steps taken per day?
'''{r meansAndMedians}
totalSteps <- sum(fitnessData$steps, na.rm=TRUE)
stepsPerDay = summarize(group_by(fitnessData, date), steps = sum(steps, na.rm=TRUE))
meanStepsPerDay <- mean(stepsPerDay$steps)
medianStepsPerDay <- median(stepsPerDay$steps)
hist(stepsPerDay$steps)

'''
The total number of steps taken is 'r totalSteps'.  The mean number of steps per day is 'r meanStepsPerDay'.  The median number of steps per day is 'r medianStepsPerDay'.

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
