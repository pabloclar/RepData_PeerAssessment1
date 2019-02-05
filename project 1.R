#reproducible research code
# initial setup
setwd("C:/Users/clarp/Documents/Coursera/Reproducible Research/repdata_data_activity")
library ("data.table")
library ("ggplot2")
library ("plyr")
library("gridExtra", lib.loc="~/R/R-3.4.0/library")
#1 getting the file
URL_file <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file (URL_file, destfile = "./repdata.Fdata.Factivity.zip", method = "auto")
unzip ("./repdata.Fdata.Factivity.zip")
rep_research_file <- data.table::fread(input = "./activity.csv")
#Calculate total number of steps per day
Total_Steps_Day <- rep_research_file [, c(lapply(.SD, sum, na.rm = FALSE)), .SDcols = c("steps"), by = .(date)] 
# show first 10 values:
head (Total_Steps_Day, 10)
# Histogram of the Total Number of Steps per day
p1 <- ggplot (Total_Steps_Day,  aes(x = steps))
p1 <- p1 + geom_histogram (binwidth = 2000, boundary = 0) 
p1 <- p1 + labs (title = "Total Daily Steps", subtitle = "2000 steps' interval", x = "Steps", y = "Frequency (Days)")
print (p1)
# Mean and Median of Total Number of Steps per Day
Mean_Steps <- mean (Total_Steps_Day [,steps], na.rm = TRUE)
Median_Steps <- median (Total_Steps_Day [,steps], na.rm = TRUE)
print (Mean_Steps, digits = 1)
print (Median_Steps, digits = 1)
# Average daily activity pattern (average of steps in each interval across days)
Avg_Steps_Int <- rep_research_file [, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval)]
# Time Series of the avg steps per 5 minute interval
q1 <- ggplot (Avg_Steps_Int, aes (x = interval, y = steps))
q1 <- q1 + geom_line  (size = 1)
q1 <- q1 + labs (title = "Avg.Daily Steps", subtitle = "per 5 minutes interval", x = "Interval", y = "Avg.Steps")
print (q1)
# Interval with maximum number of average steps
Max_Interval <- Avg_Steps_Int [steps == max(steps)]
print(Max_Interval, digits = 1)
#Number of missing values in original dataset
qty_NA <- sum(is.na(rep_research_file$steps))
print (qty_NA, digits = 1)
#Filling missing values (NA) with average for the interval
rep_research_file <- within (rep_research_file, steps <- ifelse (is.na(steps),Avg_Steps_Int$steps,steps))
#Create new dataset with the missing data filled in
data.table::fwrite(rep_research_file, file = "./activity_cleared.csv", quote = TRUE)
# Histogram, mean and median using new file without NA
# Total steps per day, new calculation
NTotal_Steps_Day <- rep_research_file [, c(lapply(.SD, sum, na.rm = FALSE)), .SDcols = c("steps"), by = .(date)] 
# Histogram of the Total Number of Steps per day
p2 <- ggplot (NTotal_Steps_Day,  aes(x = steps))
p2 <- p2 + geom_histogram (binwidth = 2000, boundary = 0) 
p2 <- p2 + labs (title = "Total Daily Steps wo NA", subtitle = "2000 steps' interval", x = "Steps", y = "Frequency (Days)")
print (p2)
# Mean and Median of Total Number of Steps per Day
NMean_Steps <- mean (NTotal_Steps_Day [,steps], na.rm = TRUE)
NMedian_Steps <- median (NTotal_Steps_Day [,steps], na.rm = TRUE)
print (NMean_Steps, digits = 1)
print (NMedian_Steps, digits = 1)
# Activity pattern differs between weekdays and weekend?
# First we need to identify weekdays/weekends and for that we need to adjust date format
rep_research_file$date <- as.Date (rep_research_file$date)
# Now we identify weekday/weekend
rep_research_file <- rep_research_file [, 'WEEKDAY' := weekdays(x = date)]
rep_research_file$WEEKDAY <- revalue(rep_research_file$WEEKDAY,c("Monday"="YES","Tuesday"="YES","Wednesday"="YES","Thursday"="YES","Friday"="YES"))
rep_research_file$WEEKDAY <- revalue(rep_research_file$WEEKDAY,c("Saturday"="NO","Sunday"="NO"))
# We calculate the average steps per interval in weeekdays and weekends to graphic with then
WD_Avg_Steps_Int <- rep_research_file [WEEKDAY == 'YES', c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval)]
WE_Avg_Steps_Int <- rep_research_file [WEEKDAY == 'NO', c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval)]
# Graphic comparison 
q3 <- ggplot (WD_Avg_Steps_Int, aes (x = interval, y = steps))
q3 <- q3 + geom_line  (size = 1)
q3 <- q3 + labs (title = "Avg.Daily Steps - Weekdays", subtitle = "per 5 minutes interval", x = "Interval", y = "Avg.Steps")
q4 <- ggplot (WE_Avg_Steps_Int, aes (x = interval, y = steps))
q4 <- q4 + geom_line  (size = 1)
q4 <- q4 + labs (title = "Avg.Daily Steps - Weekends", subtitle = "per 5 minutes interval", x = "Interval", y = "Avg.Steps")
grid.arrange(q3, q4)

