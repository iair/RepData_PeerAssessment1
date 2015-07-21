#This part install the pachages needed to the assignment
if (!require("kernlab")) {
  install.packages("kernlab",repos="http://cran.rstudio.com/")
}
if (!require("plyr")) {
  install.packages("plyr",repos="http://cran.rstudio.com/")
  library(plyr)
}  
if (!require("dplyr")) {
  install.packages("dplyr",repos="http://cran.rstudio.com/")
  library(dplyr)
}
if (!require("data.table")) {
  install.packages("data.table",repos="http://cran.rstudio.com/")
  library(data.table)
}
#Load the data
activity <- read.csv("~/Documents/Formacion/MOOCS/Data Science Certificate/Reproducible Research/activity.csv")
activity <- activity[,1:3]
#Claear the NA
activity$date<-as.POSIXct(activity$date)
good<-complete.cases(activity)
activity_no_NA<-activity[good,]
#Get the mean of steps by date
dt<-as.data.table(activity_no_NA)
setkey(dt,date)
act_no_NA_mean<-dt[,list(mean=mean(steps)),by=date]
act_no_NA_mean$mean<-round(act_no_NA_mean$mean,1)
#Show the result
act_no_NA_mean
#Histogram of the total number of steps taken per day
setkey(dt,date)
act_no_NA_sum<-dt[,list(sum=sum(steps)),by=date]
hist(act_no_NA_sum$sum,main="Histogram of total steps by date",
     xlab = "Total steps by day",ylab = "Count",col = "red",breaks = "FD",
     ylim=c(0,25))
#This the mean and median of the total number of steps taken per day
data.frame("Mean"=mean(act_no_NA_sum$sum),"Median"=median(act_no_NA_sum$sum))

#Question 2
#The average daily activity pattern
step_by_interval_noNA<-group_by(activity_no_NA,interval)%>%summarise(mean_steps=round(mean(steps),1))
plot(y = step_by_interval_noNA$mean_steps, x = step_by_interval_noNA$interval, 
     type = "l", xlab = "5-Minute-Interval", 
     main = "Average Daily Activity Pattern", ylab = "Average number of steps")

#Question 3
#The sum of the NAs
na<-apply(activity, 1, function(x) sum(is.na(x)))
sum(na)
#Get the index of the rows in activity which has steps with NA values
indx_steps <- which(is.na(activity$steps))
#Get the interval of the rows in activity matrix which has steps with NA values 
indx_interval<-activity$interval[indx_steps]
#Get the index of the rows in steps_by_interval matrix which has the interval 
#which match with the steps with NA values 
indx_interval_steps<-match(indx_interval,step_by_interval_noNA$interval)
#Imput NA using the mean and create the new matrix with the imputed NA 
activity_imp<-activity
for(i in 1:length(indx_interval))
{
        activity_imp$steps[indx_steps[i]]<-step_by_interval_noNA[indx_interval_steps[i],2] 
}
activity_imp$steps<-as.double(activity_imp$steps)

#Histogram of the total number of steps taken per day using the new matrix
act_imp_sum<-group_by(activity_imp,date)%>%summarise(sum_steps=round(sum(steps),0))
hist(act_imp_sum$sum,
     main="total steps by date after imput the NA with the maean",
     xlab = "Total steps by day",ylab = "Count",col = "red",breaks = "FD",
     ylim=c(0,25))
#The mean and median of the total nª of steps taken p/day using the new matrix
data.frame("Mean"=mean(act_imp_sum$sum),"Median"=median(act_imp_sum$sum))
#Plot both graphics so we can see the difference between each other
par(mfrow=c(1,2),mar=c(4,4,2,2),oma=c(0.5,0.5,0.5,0))
hist(act_no_NA_sum$sum,main="Total steps by date",
     xlab = "Total steps by day",ylab = "Count",col = "red",breaks = "FD",
     ylim=c(0,25))
abline(v = median(act_no_NA_sum$sum), col = 4, lwd = 4)
hist(act_imp_sum$sum,
     main="Total steps by day after imput NAs",
     xlab = "Total steps by day",ylab = "Count",col = "red",breaks = "FD",
     ylim=c(0,25))
abline(v = median(act_imp_sum$sum), col = 4, lwd = 4)


#Question4:Difference in acitivity patterns between weekdays and weekends

#Creating a new factor variable in the dataset with two levels -- 
#"weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
activity_imp$nameday_aux<-weekdays(activity_imp$date)
activity_imp$nameday<-apply(activity_imp[,"nameday_aux",drop=FALSE],1,
                            function(x){
        ifelse(x=="Sunday","Weekend",ifelse(x=="Saturday","Weekend","Weekday"))
})
#plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, 
#averaged across all weekday days or weekend days (y-axis)

act_imp_weekday<-subset(activity_imp,nameday=="Weekday")
act_imp_weekend<-subset(activity_imp,nameday=="Weekend")
daily_act_weekday<-tapply(act_imp_weekday$steps,act_imp_weekday$interval,mean)
daily_act_weekend<-tapply(act_imp_weekend$steps,act_imp_weekend$interval,mean)
par(mfrow=c(2,1),mar=c(5,4.1,3,2))
plot(y = daily_act_weekend, x = names(daily_act_weekend), type = "l", 
     xlab = "5-Minute Interval", main = "Daily Activity Pattern on Weekends", 
     ylab = "Average number of steps",
     ylim =c(0, 250), xlim=c(0,max(as.integer(names(daily_act_weekend)))))
plot(y = daily_act_weekday, x = names(daily_act_weekday), type = "l", 
     xlab = "5-Minute Interval", main = "Daily Activity Pattern on Weekdays", 
     ylab = "Average number of steps", ylim =c(0, 250),
     xlim=c(0,max(as.integer(names(daily_act_weekday)))))

