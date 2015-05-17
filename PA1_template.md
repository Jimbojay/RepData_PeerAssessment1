output:  PA1_template.md

#Peer Assessment 1

*Imre Dekker*



##Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

##Data
The data for this assignment can be downloaded from the course web site:

- Dataset: Activity monitoring data [52K]

The variables included in this dataset are:

- **steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)

- **date**: The date on which the measurement was taken in YYYY-MM-DD format

- **interval**: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

##Assignment
This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a **single R markdown** document that can be processed by knitr and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code. **This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.**

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the GitHub repository created for this assignment. You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

NOTE: The GitHub repository also contains the dataset for the assignment so you do not have to download the data separately.

###Loading and preprocessing the data
Show any code that is needed to

1. Load the data (i.e. read.csv())

```{r extract, echo = TRUE}
# create source data folder if neccesary
if (!file.exists('PA1 Source Data')) {
        dir.create('PA1 Source Data')
}

# download the data file and unpack the zip-tile
if (!file.exists(paste('PA1 Source Data/activity.csv'))) {
        file.url<-'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
        download.file(file.url,destfile='PA1 Source Data/repdata-data-activity.zip')
        unzip('PA1 Source Data/repdata-data-activity.zip',exdir='PA1 Source Data',overwrite=TRUE)
}

PA1Data <- read.csv(file='PA1 Source Data/activity.csv', header=TRUE, sep=",")


```


2. Process/transform the data (if necessary) into a format suitable for your analysis

```{r transform}
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
PA1Data$date <- ymd(PA1Data$date)


```


###What is mean total number of steps taken per day?



For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

```{r question1.1}
# generate df2 with complete cases only
PA1DataSample <- na.omit(PA1Data)

# aggregate steps as per date to get total number of steps in a day
StepsPerDayAggr <- aggregate(steps ~ date, PA1DataSample, sum)
StepsPerDayAggr


```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```{r question1.2}
ggplot(StepsPerDayAggr, aes(x=date, y=steps))+geom_histogram(stat="identity")+ xlab("Dates")+ ylab("Steps")+ labs(title= "Total numbers of Steps per day")


```

3. Calculate and report the mean and median of the total number of steps taken per day

```{r question1.3}
StepsPerDay <- tapply(PA1Data$steps, PA1Data$date, FUN = sum, na.rm = TRUE)
mean(StepsPerDay)
median(StepsPerDay)


```


###What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r quesion2.1}
StepsPerInterval <- aggregate(steps ~ interval, PA1DataSample, mean)
plot(StepsPerInterval$interval, StepsPerInterval$steps, type = "l")

```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r quesion2.2}
StepsPerInterval[which.max(StepsPerInterval$steps), ]$interval

```


###Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.


1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r quesion3.1}
missing <- sum(is.na(PA1Data))
missing

```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

*N/A's are filled with the mean of the specific interval as was calculated in the previous section*


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```{r quesion3.3}
PA1DataNonNA <- PA1Data
for (i in 1:nrow(PA1DataNonNA)) {
  if (is.na(PA1DataNonNA$steps[i])) {
    interval_value <- PA1DataNonNA$interval[i]
    steps_value <- StepsPerInterval[
      StepsPerInterval$interval == interval_value,]
    PA1DataNonNA$steps[i] <- steps_value$steps
  }
}


```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r question3.4}
StepsPerDayAggrNonNa <- aggregate(steps ~ date, PA1DataNonNA, sum)

hist(StepsPerDayAggrNonNa$steps, main="Histogram of total number of steps per day (non NA)", 
     xlab="Total number of steps in a day")

mean(StepsPerDayAggr$steps)
mean(StepsPerDayAggrNonNa$steps)


median(StepsPerDayAggr$steps)
median(StepsPerDayAggrNonNa$steps)


```


The mean remains the same, while the median increases sligtly


###Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r question4.1}
# add variable to the data
PA1DataNonNA['type_of_day'] <- weekdays(as.Date(PA1DataNonNA$date))
PA1DataNonNA$type_of_day[PA1DataNonNA$type_of_day  %in% c('zaterdag','zondag') ] <- "weekend"
PA1DataNonNA$type_of_day[PA1DataNonNA$type_of_day != "weekend"] <- "weekday"
# convert type_of_day from character to factor
PA1DataNonNA$type_of_day <- as.factor(PA1DataNonNA$type_of_day)
```



2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```{r question4.2}
# calculate average steps by interval across all days
StepsPerIntervalNonNa <- aggregate(steps ~ interval + type_of_day, PA1DataNonNA, mean)

# creat a plot
qplot(interval, 
      steps, 
      data = StepsPerIntervalNonNa, 
      type = 'l', 
      geom=c("line"),
      xlab = "Interval", 
      ylab = "Number of steps", 
      main = "") +
  facet_wrap(~ type_of_day, ncol = 1)

```