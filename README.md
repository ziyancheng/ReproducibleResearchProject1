# Analyzing Fitbit Data for Reproducible Reseach

This is the first project as Coursera's Data Science specialization courses of Reproducible Research. The goals of this project are as the following:

  1. load and prepare the data
  2. imputing the missing value in the data
  3. answer all the research questions
  
## Data

The data for this assignment can be downloaded from the course web site:

    Dataset: Activity monitoring data [52K]

The variables included in this dataset are:

    1. steps: Number of steps taking in a 5-minute interval (missing values are coded as NA\color{red}{\verb|NA|}NA)
    2. date: The date on which the measurement was taken in YYYY-MM-DD format
    3. interval: Identifier for the 5-minute interval in which measurement was taken
    
 ## Question 1: Load and prepare the data
 
 Assume the data got downloaded in the same current working directory
 ```
 setwd("/Users/chengya@us.ibm.com//coursera/reproducibleResearch/week2")
 columns <- c("integer", "character", "integer")
 df <- read.csv("activity.csv", header = TRUE, colClasses = columnss, na.strings = "NA")
 names(df)
 str(df)
 head(df[which(!is.na(df$steps)), ])
 summary(df)

```
## Question 2: Histogram of the total number steps each day
```
hist(total_steps$steps,
     xlab = "The total number of steps per day",
     ylab = "The number of the dates",
     breaks = 10,
     col = "blue",
     main = "The distribution of total number steps per day")
```
## Question 3: Mean and median number of steps taken each day showing as abline 
```
abline(v = mean(total_steps$steps), lty=1, lwd =2, col="red")
abline(v = median(total_steps$steps), lty =2, lwd = 2, col="black")
legend( x= "topright", c("Mean", "Median"), col =c("red", "black"), lty = c(1,2), lwd =c(2,2))

meanSteps <- mean(total_steps$steps)
medianSteps <- median(total_steps$steps)
```

## Question 4: Time series plot of the average number of steps taken
```
avgStepsByInterval <- aggregate(steps ~ interval, df, mean)
plot(avgStepsByInterval$interval, 
     avgStepsByInterval$steps,
     type = "l",
     xlab = "Interval",
     ylab = "Number of Steps",
     main = "Average Steps taken daily by interval")
```

## Question 5: The 5-minute interval that, on average, contains the maximum number of steps

```
maxSteps <- avgStepsByInterval[which.max(avgStepsByInterval$steps),1]

```

## Question 6: Code to describe and show a strategy for imputing missing data
  check the total of missing data
```
MissedDF <- sum(!complete.cases(df))
```

  Note: 2304 is missed and filled with NA

 using the mean to fill in the missing data, create a new dataset is equal to the original data but using the mean to fill in the missing data
```
imputed_df <- transform(df, steps = ifelse(is.na(df$steps), avgStepsByInterval$steps[match(df$interval, avgStepsByInterval$interval)], df$steps))

summary(imputed_df)
```
 Zeros are imputed for 10-1-2012, since it is the min value of the dates and would have 9,000 steps higher than the following day
```
imputed_df[as.character(imputed_df$date) == "2012-10-01", 1 ] <- 0
```
 Recount the total steps per day and create histogram

```
total_steps_imputed <- aggregate(steps ~ date, imputed_df, sum)
names(total_steps_imputed) <- c("date", "daily_steps")
```

## Question 7: Histogram of the total number of steps taken each day after missing values are imputed

```
hist(total_steps_imputed$daily_steps, col="purple", xlab = " Class of Total of Steps per day", ylab = "Number of days", main = "Daily Steps taken after the missing data are imputed")
```


## Question 8: Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

 1: create a new vector as dateType and alter the data from
```
library(dplyr)
df$date <- as.Date(df$date)

df2 <- df %>% mutate(dateType =ifelse(weekdays(df$date)=="Saturday" | weekdays(df$date)=="Sunday", "Weekends","Weekday" ))

#avgStepsByDateTypeAndInterval <- df2 %>% 
                              #group_by(dateType, interval) %>%
                              #summarize(avgStepsByDay = mean(steps))
avgStepbyDate <- aggregate(steps ~ interval +dateType, df2, mean, na.rm=TRUE)

```
2: make the plot

```
plot<- ggplot(avgStepbyDate, aes(x = interval , y = steps, color = dateType)) +
  geom_line() +
  labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
  facet_wrap(~dateType, ncol = 1, nrow=2)
print(plot)

```

 
