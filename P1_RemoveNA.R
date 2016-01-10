##The script is used to read and process personal movement activity data collected from  
##from mobile moitoring devices.Data were collected between October and NOvember, 2012
##Code below is used to answer a series of questions pertinent to exploratory analysis 

library(dplyr)
library(ggplot2)

filename <- "repdata_data_activity.zip"

## Download and unzip the dataset:
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(fileURL, filename)
}  
if (!file.exists("activity.csv")) { 
  unzip(filename) 
}

##1.Code for reading in the dataset and/or processing the data
mainData <- na.omit(read.csv("activity.csv"))

#Convert the date variable to R class (POSIXlt)
mainData$date <- as.Date(mainData$date, "%Y-%m-%d")

##2.Histogram of the total number of steps taken each day
png(filename = "Histogram.png")

qplot(mainData$steps, geom="histogram",
      binwidth = 10,
      main = "Total Number of Steps Taken Each Day",
      ylab = "Count of Occurences in Dataset",
      xlab = "Number of Steps", fill=I("red"), col=I("blue"), ylim = c(0, 16000))

dev.off()

##3.Calculate and report the mean and median of the total number of steps taken per day
reportTable <- summary(mainData$steps)

##4.Time series plot of the average number of steps taken per 5-minute interval
stepIntervals <- group_by(mainData, interval) %>%
              summarize(avgSteps = mean(steps))

#Create the time plot 
png(filename = "Time_Series_Plot.png")

with(stepIntervals, {
  plot(interval, avgSteps, type = "l", 
       ylab = "Average Number of Steps",
       xlab = "5-min Intevals")
})

dev.off()

##5.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
maxStepsInterval <- filter(stepIntervals, avgSteps == max(avgSteps))
