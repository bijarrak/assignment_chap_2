########downloading the data file containing number of steps taken per 5 min and reading the same 
download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",destfile = "Steps.zip")
unzip("Steps.zip")
act<-read.csv("activity.csv")
####################################exploring the CSV##################################

head(act)
summary(act)
str(act)
nrow(act)

######################################file description#######################################
#file contains 3 fiels,total steps taken in 5 min interval, date and time.
#there ar 17568 observations, where out of which 2304 are missing.
#here date field is a factor variable hence need to convert it to date.
##############################################################################################
act$date<-as.Date(as.character(act$date),"%Y-%m-%d")
class(act$date)###date now
############################################exploration starts here##########################
###What is mean total number of steps taken per day?
###Calculate the total number of steps taken per day

library(dplyr)

act_total_steps<-act%>%group_by(date)%>%summarise(total_steps=sum(steps))

print(act_total_steps)

with(act_total_steps,hist(total_steps))

act_M_steps<-act%>%group_by(date)%>%summarise(mean_steps=mean(steps),median_step=median(steps))

head(act_M_steps)



########What is the average daily activity pattern?
#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#and the average number of steps taken, averaged across all days (y-axis)
#
#Which 5-minute interval, on average across all the days in the dataset, 
#contains the maximum number of steps?

act_avg_steps<-act%>%group_by(interval)%>%summarise(avg_steps=round(mean(steps,na.rm=TRUE)))

with(act_avg_steps,plot(x=c(1:288),y=avg_steps,type = "l",xlab = "Time interval",ylab = "Average steps",main ="Average Steps per Interval" ))

#Imputing missing values
#Note that there are a number of days/intervals where there are missing values (coded as NA). 
#The presence of missing days may introduce bias into some calculations or summaries of the data.

#Calculate and report the total number of missing values in the dataset 
#(i.e. the total number of rows with NAs)
#Devise a strategy for filling in all of the missing values in the dataset. 
#The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, 
#or the mean for that 5-minute interval, etc.
#Create a new dataset that is equal to the original dataset but with the missing data filled in.
#Make a histogram of the total number of steps taken each day and Calculate 
#and report the mean and median total number of steps taken per day. 
#Do these values differ from the estimates from the first part of the assignment? What is 
#the impact of imputing missing data on the estimates of the total daily number of steps?
###########################################################################################
#####number of missing values######################
table(is.na(act$steps))####2304 missing values
act_new<-act####reatting new dataframe

for(i in 1:nrow(act_new))
{
 if(is.na(act_new[i,]$steps))
 {
   int<-act_new[i,]$interval
   act_new[i,]$steps<-subset(act_avg_steps,interval==int)$avg_steps
 }
}

table(is.na(act_new$steps))
####################plotting the histogram and calculating the mean and median again#################

library(dplyr)

act_total_steps_new<-act_new%>%group_by(date)%>%summarise(total_steps=sum(steps))

print(act_total_steps_new)

with(act_total_steps_new,hist(total_steps,xlab = "Total steps"))

act_M_steps_new<-act_new%>%group_by(date)%>%summarise(mean_steps=mean(steps),median_step=median(steps))

head(act_M_steps_new)
#################################################################
#Are there differences in activity patterns between weekdays and weekends?

#For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

#Create a new factor variable in the dataset with two levels - "weekday"
#and "weekend" indicating whether a given date is a weekday or weekend day.
#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
#See the README file 
#in the GitHub repository to see an example of what this plot should look like using simulated data.
###########################################################
########################################calculating the weekdays
act_new$Wkday<-"Weekday"
act_new[weekdays(act_new$date)%in% c("Sunday","Saturday"),]$Wkday<-"Weekend"
act_new$Wkday<-as.factor(act_new$Wkday)
table(act_new$Wkday)
act_avg_steps_new<-act_new%>%group_by(interval,Wkday)%>%summarize(avg_steps=mean(steps))

###############plotting the data#######
library(ggplot2)
g<-ggplot(act_avg_steps_new,aes(x=interval,y=avg_steps))

g+geom_line(aes(col=Wkday))+facet_grid(.~Wkday)+labs(xlab("Interval"))+labs(ylab("Average Steps"))


library(ggplot2)

ggplot(act_total_steps,aes(x=date,y=total_steps))+
  geom_histogram(stat = "identity",binwidth = 30)
+ggtitle("Total number of steps taken per day") + 
  xlab("Date") + 
  ylab("Steps")
