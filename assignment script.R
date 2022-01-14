# Initialization
install.packages("dplyr")
install.packages("tidyverse")
install.packeges("readr")
install.packeges("base")
install.packeges("ggplot2")
library("dplyr")
library("tidyverse")
library("readr")
library("base")
library("ggplot2")
setwd("~/develop/datasciencecoursera/Reproducible Research2")

#read data
data <- read.csv("./data/activity.csv")

#convert date format
data$date <- as.Date(data$date, format = "%Y-%m-%d")

#aggregate sum of steps per day and avg steps per interval
aggday <- aggregate(data["steps"], by=data["date"], sum, na.rm = TRUE)
aggmeaninterval <- aggregate(data["steps"], by=data["interval"], mean, na.rm = TRUE)
agginterval <-aggregate(data["steps"], by=data["interval"], mean, na.rm = TRUE)

#Histogram of the total number of steps taken each day
phist <- aggday %>%
  ggplot( aes(x=steps)) +
  geom_histogram(bins = 30)

#Mean and median number of steps taken each day

datamean <- mean(aggday$steps, na.rm = TRUE)
datamedian <- median(aggday$steps, na.rm = TRUE)

#Time series plot of the average number of steps taken
ptimeseries <- agginterval %>%
  ggplot( aes(x=interval, y=steps)) +
  geom_line()

#Code to describe and show a strategy for imputing missing data by mean of interval
dataimputed <- data %>%
  group_by(interval) %>%
  mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))

aggimputedday <- aggregate(dataimputed["steps"], by=dataimputed["date"], sum, na.rm = TRUE)

dataimputedmean <- format(mean(aggimputedday$steps, na.rm = TRUE), scientific = FALSE)
dataimputedmedian <- format(median(aggimputedday$steps, na.rm = TRUE), scientific = FALSE)
stepmean <- mean(data$steps, na.rm = TRUE)
datanona<- data %>% replace(is.na(.), stepmean)

#aggregate sum of steps per day and avg steps per interval without missing data
aggdatanona <- aggregate(datanona["steps"], by=datanona["date"], sum)
aggintervalnona <- aggregate(datanona["steps"], by=datanona["interval"], mean)

#The 5-minute interval that, on average, contains the maximum number of steps without missing data
maxinterval <- slice_max(aggintervalnona, steps)

#Histogram of the total number of steps taken each day after missing values are imputed
phistnona <- aggdatanona %>%
  ggplot( aes(x=steps)) +
  geom_histogram(bins = 30)

#Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
dataimputedweek <- cbind(dataimputed, weekday = !(weekdays(dataimputed$date) %in% c('Samstag' , 'Sonntag'))) %>%
  filter(weekday == TRUE)
dataimputedwknd <- cbind(dataimputed, weekday = !(weekdays(dataimputed$date) %in% c('Samstag' , 'Sonntag'))) %>%
  filter(weekday == FALSE)
intervalweek <- aggregate(dataimputedweek["steps"], by=dataimputedweek["interval"], mean)
intervalweek <- cbind(intervalweek, day = "weekday")
intervalwknd <- aggregate(dataimputedwknd["steps"], by=dataimputedwknd["interval"], mean)
intervalwknd <- cbind(intervalwknd, day = "weekend")
compareinterval <- merge(intervalweek, intervalwknd, all = TRUE)
compareplot <- ggplot(data=compareinterval, aes(x=interval, y=steps)) + 
  geom_line() +
  facet_wrap(~day, dir = "v") +
  labs(title = 'dir = "v"',
       y = "Number of steps")


