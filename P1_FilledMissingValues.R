#The script is used to read and process personal movement activity data collected from  
#from mobile moitoring devices.Data were collected between October and NOvember, 2012
#Code below is used to answer a series of questions pertinent to exploratory analysis 

library(dplyr)
library(ggplot2)

##1.Code for reading in the dataset and/or processing the data
mainData <- read.csv("activity.csv")

##6.1 Calculate and report the total number of missing values in the dataset 
##(i.e. the total number of rows with NAs)
cat("Total number of missing values: ", sum(is.na(mainData)), "\n")

##6.2 Code to describe and show a strategy for imputing missing data
##Strategy is to imput missing data with the mean of corresponding 5-min interval  
imputData <- group_by(mainData, interval) %>%
          mutate(steps = replace(steps, is.na(steps), mean(steps, na.rm = TRUE)))

##Ensuring data frame structure and integer values after prior grouping and averaging
imputData <- as.data.frame(imputData)
imputData$steps <- as.integer(imputData$steps)

##7.Make a histogram of the total number of steps taken each day 
png(filename = "Histogram2.png")
par(mfrow = c(1,2))

with(mainData, {
  hist(steps, col = "red", main = "Initial Dataset",
       xlab = "Steps Taken Each Day",
       ylab = "Count of Occurences in Dataset",
       ylim = c(0, 16000))
})

with(imputData, {
  hist(steps, col = "green", main = "Imput Dataset",
       xlab = "Steps Taken Each Day",
       ylab = "Count of Occurences in Dataset",
       ylim = c(0, 16000))
})

dev.off()

cat("Summary for IMPUTTED dataset:", "\n")  
print(summary(imputData$steps))

cat("Summary for INITIAL dataset:", "\n")  
print(summary(mainData$steps))

##8.1 Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
##indicating whether a given date is a weekday or weekend day

#Convert the date variable to R class (POSIXlt) and create factor
imputData$date <- as.Date(imputData$date, "%Y-%m-%d")
weekend <- c("Saturday", "Sunday")
dayVar <- factor((weekdays(imputData$date) %in% weekend), 
                    levels = c(TRUE, FALSE), 
                    labels = c("weekend day", "weekday"))

##Add the variable to dataset and average steps by day type
imputData <- mutate(imputData, daytype = dayVar)
stepsImput <- group_by(imputData, interval, daytype) %>%
  summarize(avgSteps = mean(steps))

##8.2 Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
library(lattice)
png(filename = "Time_Serie_Plot2.png")
xyplot(avgSteps ~ interval | daytype, data = stepsImput, type = "l", layout = c(1, 2))

dev.off()

