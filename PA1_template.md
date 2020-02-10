Following steps to be performed as part of submission
-----------------------------------------------------

1.  Code for reading in the dataset and/or processing the data
2.  Histogram of the total number of steps taken each day
3.  Mean and median number of steps taken each day
4.  Time series plot of the average number of steps taken
5.  The 5-minute interval that, on average, contains the maximum number
    of steps
6.  Code to describe and show a strategy for imputing missing data
7.  Histogram of the total number of steps taken each day after missing
    values are imputed
8.  Panel plot comparing the average number of steps taken per 5-minute
    interval across weekdays and weekends
9.  All of the R code needed to reproduce the results (numbers, plots,
    etc.) in the report

step 1 - Code for reading in the dataset and/or processing the data
-------------------------------------------------------------------

    library(ggplot2)

    ## Warning: package 'ggplot2' was built under R version 3.6.2

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    activity_data<-read.csv("activity.csv")
    names(activity_data)

    ## [1] "steps"    "date"     "interval"

    str(activity_data)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

step 2 - Histogram of the total number of steps taken each day
--------------------------------------------------------------

    Daily_steps<-summarise(group_by(activity_data,date),steps = sum(steps,na.rm=TRUE))
    head(Daily_steps)

    ## # A tibble: 6 x 2
    ##   date       steps
    ##   <fct>      <int>
    ## 1 2012-10-01     0
    ## 2 2012-10-02   126
    ## 3 2012-10-03 11352
    ## 4 2012-10-04 12116
    ## 5 2012-10-05 13294
    ## 6 2012-10-06 15420

    #Histogram of total steps
    qplot(Daily_steps$steps,geom="histogram",xlab="Total Steps",ylab="Counts",main="Total Daily Steps Historgram", bins = 45)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-1-1.png)

step 3 - Mean and Median number of steps taken each day
-------------------------------------------------------

    Daily_steps_mean<-summarise(group_by(activity_data,date),Mean = mean(steps,na.rm=TRUE))
    Daily_steps_median<-summarise(group_by(activity_data,date),Median = median(steps,na.rm=TRUE))
    Daily_steps_mean_median <- cbind(Daily_steps_mean, Daily_steps_median$Median)
    names(Daily_steps_mean_median)[3]<- "Median"
    Daily_steps_mean_median_final<- filter(Daily_steps_mean_median, Daily_steps_mean_median$Median != "NA")
    head(Daily_steps_mean_median_final)

    ##         date     Mean Median
    ## 1 2012-10-02  0.43750      0
    ## 2 2012-10-03 39.41667      0
    ## 3 2012-10-04 42.06944      0
    ## 4 2012-10-05 46.15972      0
    ## 5 2012-10-06 53.54167      0
    ## 6 2012-10-07 38.24653      0

step 4 - Time series plot of the average number of steps taken
--------------------------------------------------------------

    Daily_steps_mean_median_final$date <- as.Date(Daily_steps_mean_median_final$date, format = "%Y-%m-%d")
    ggplot(Daily_steps_mean_median_final, aes(y = Mean, x = date), na.rm = TRUE)+geom_bar(stat = "Identity") + scale_x_date() + ylab("Average Steps")+ xlab("Date") + ggtitle("Time series plot of the daily average steps taken")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

step 5 - The 5-minute interval that, on average, contains the maximum number of steps
-------------------------------------------------------------------------------------

    Intervalwise_mean<-summarise(group_by(activity_data,interval),Mean = mean(steps,na.rm=TRUE))
    Intervalwise_mean

    ## # A tibble: 288 x 2
    ##    interval   Mean
    ##       <int>  <dbl>
    ##  1        0 1.72  
    ##  2        5 0.340 
    ##  3       10 0.132 
    ##  4       15 0.151 
    ##  5       20 0.0755
    ##  6       25 2.09  
    ##  7       30 0.528 
    ##  8       35 0.868 
    ##  9       40 0     
    ## 10       45 1.47  
    ## # ... with 278 more rows

    max1 <- max(Intervalwise_mean$Mean)
    filter(Intervalwise_mean, Intervalwise_mean$Mean == max1)

    ## # A tibble: 1 x 2
    ##   interval  Mean
    ##      <int> <dbl>
    ## 1      835  206.

    plot(Intervalwise_mean$interval, Intervalwise_mean$Mean, type = "l", col="darkblue", lwd = 2, xlab="Interval", ylab="Average number of steps", main="Average number of steps per intervals")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

step 6 - Code to describe and show a strategy for imputing missing data
-----------------------------------------------------------------------

There are multiple ways missing data can be imputed like 1. by replacing
‘0’  
2. by replacing Mean/Median values  
3. Interpolation/Extrapolation - for simplicity, I will describe
imputing missing values with Mean values.  
4. Find out the missing values (represented by NA) in steps column  
5. Match the interval with the interval\_wise mean data in step no 5  
6. Write a function to identify the mean data from the
intervalwise\_mean data to replace the NA value in the original
dataset  
7. Apply this to the entire dataset each row-wise  
8. check the steps column if there’s any NA present or not

    getMeanSteps<-function(interval){
    Intervalwise_mean[Intervalwise_mean$interval==interval,]$Mean
    }
    activity_data_clean <- activity_data
    for(i in 1:nrow(activity_data_clean)){
    if(is.na(activity_data_clean[i,]$steps)){
    activity_data_clean[i,]$steps <- getMeanSteps(activity_data_clean[i,]$interval)
    }
    }
    unique(is.na(activity_data_clean$steps))

    ## [1] FALSE

step 7 - Histogram of the total number of steps taken each day after missing values are imputed
-----------------------------------------------------------------------------------------------

1.  This is similar to step no 2 but the post imputation dataset will be
    used

<!-- -->

    Daily_steps_clean<-summarise(group_by(activity_data_clean,date),steps = sum(steps))
    qplot(Daily_steps_clean$steps,geom="histogram",xlab="Total Steps",ylab="Counts",main="Total Daily Steps Histogram Post Imputation", bins = 45)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)

step 8 - Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
------------------------------------------------------------------------------------------------------------------

1.  This is similar to step no 2 but the post imputation dataset will be
    used  
2.  identify which dates are weekdays and weekends  
3.  add a new column to identify each row as weekday/weekend  
4.  plot the interval wise comparison chart between weekdays and
    weekends

<!-- -->

    library(timeDate)

    ## Warning: package 'timeDate' was built under R version 3.6.2

    activity_data_clean_weekdays <- activity_data_clean[isWeekday(as.Date(activity_data_clean$date, format = "%Y-%m-%d")),]
    activity_data_clean_weekends <- activity_data_clean[!isWeekday(as.Date(activity_data_clean$date, format = "%Y-%m-%d")),]
    activity_data_clean_weekdays$weekdayflag <- "Weekday"
    activity_data_clean_weekends$weekdayflag <- "Weekend"
    activity_data_clean_weekdayflag <- rbind(activity_data_clean_weekdays, activity_data_clean_weekends)
    Intervalwise_mean_weekdayflag <- summarise(group_by(activity_data_clean_weekdayflag,interval,weekdayflag),Mean = mean(steps))
    ggplot(Intervalwise_mean_weekdayflag, aes(x = interval, y = Mean, col = weekdayflag))+geom_line(stat="identity")  + labs(x="Interval", y=expression("Average steps")) + ggtitle(expression("Interval Wise Average Steps With Weekday Flag")) + facet_grid(scales="free", space="free", .~weekdayflag )

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-7-1.png)
