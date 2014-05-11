#1 Loading and preprocessing the data
#1.1 Load the data (i.e. read.csv())
Sys.setlocale("LC_TIME","English United States")
Sys.setlocale("LC_TIME", "en_US")
unzip("activity.zip")
data <- read.csv("activity.csv")
#1.2 Process/transform the data (if necessary) into a format suitable for your analysis
dataV = data[!is.na(data$steps),]
str(data)
names(data)

#2. What is mean total number of steps taken per day?
#2.1 Make a histogram of the total number of steps taken each day
days=levels(dataV$date)
totalSteps=c()
for (day in days) {
  totalSteps=c(totalSteps,sum(dataV[dataV$date==day,"steps"]))
}
names(totalSteps)=days
#plot(as.factor(days),totalSteps,type="h",, freq=TRUE)
barplot(totalSteps,las=2,cex.names=.5)
#hist(dataV$steps, xlab="Steps",)
#2.2 Calculate and report the mean and median total number of steps taken per day
meanSteps=mean(totalSteps)
meanSteps
medianSteps=median(totalSteps)
medianSteps

#3. What is the average daily activity pattern?
#3.1 Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and
#the average number of steps taken, averaged across all days (y-axis)
intervalF=factor(dataV$interval)
means=c()
for (i in levels(intervalF)) {
  print(i)
  means=c(means, mean(dataV[dataV$interval==as.numeric(i),"steps"]))
}
plot(levels(intervalF), means, type="l")
#3.2 Which 5-minute interval, on average across all the days in the dataset, contains 
#the maximum number of steps?
maxInterval=levels(intervalF)[means==max(means)]

#4. Imputing missing values
#4.1 Calculate and report the total number of missing values in the dataset
# (i.e. the total number of rows with NAs)
flags=is.na(data$steps)
rowsWithNA=sum(flags)
print(rowsWithNA)
#4.2 Devise a strategy for filling in all of the missing values in the dataset. 
#The strategy does not need to be sophisticated. For example, you could use the 
#mean/median for that day, or the mean for that 5-minute interval, etc.
#4.3 Create a new dataset that is equal to the original dataset but with the 
#missing data filled in.
data2=data
index=0
# for each values ...
for (flag in flags) {
#print(flag)
  # Calculate index
  index=index+1
#index=9158
  # Is it na?
  if (flag) {
    # Yes! Replace na value
print(paste("correcting index",index))
print(paste("before: ",data2[index,"steps"]))
#day="2012-11-02"
    data2[index,"steps"]=mean(dataV[dataV$interval==data2[index,"interval"],"steps"])
print(paste("after: ",data2[index,"steps"]))
  }
}
#4.4 Make a histogram of the total number of steps taken each day and Calculate 
#and report the mean and median total number of steps taken per day. Do these values 
#differ from the estimates from the first part of the assignment? What is the impact 
#of imputing missing data on the estimates of the total daily number of steps?
days2=levels(data2$date)
totalSteps2=c()
for (day2 in days2) {
  totalSteps2=c(totalSteps2,sum(data2[data2$date==day2,"steps"]))
}
names(totalSteps2)=days2
par(mfrow=c(1,2),mar=c(4,4,1,1))
barplot(totalSteps,las=2,cex.names=.5,main="hist(deleting na)")
barplot(totalSteps2,las=2,cex.names=.5,main="hist(changing na)")
meanSteps2=mean(totalSteps2)
print(paste("mean(deleting na): ",meanSteps,", mean(changing na):",meanSteps2,sep=""))
medianSteps2=median(totalSteps2)
print(paste("median(deleting na): ",medianSteps,", median(changing na):",medianSteps2,sep=""))

#5. Are there differences in activity patterns between weekdays and weekends?
#5.1 Create a new factor variable in the dataset with two levels -- "weekday" 
#and "weekend" indicating whether a given date is a weekday or weekend day.
isweekend = function (x) {
  wday=as.POSIXlt(x)$wday+1
#print(wday)
  (wday==1 | wday==7)  
}
data2$type=as.factor(sapply(data2$date,isweekend))
levels(data2$type)=c("weekday","weekend")
#5.2Make a panel plot containing a time series plot (i.e. type = "l") of the 
# 5-minute interval (x-axis) and the average number of steps taken, averaged 
# across all weekday days or weekend days (y-axis). The plot should look 
# something like the following, which was creating using simulated data:
means2Labor=c()
means2WeekEnd=c()
for (i in levels(intervalF)) {
#print(i)
  means2Labor=c(means2Labor, mean(data2[data2$interval==as.numeric(i) & data2$type=="weekday","steps"]))
  means2WeekEnd=c(means2WeekEnd, mean(data2[data2$interval==as.numeric(i) & data2$type=="weekend","steps"]))
}
par(mfrow=c(2,1),mar=c(4,4,1,1))
plot(levels(intervalF), means2Labor, type="l",main="weekday",xlab="")
plot(levels(intervalF), means2WeekEnd, type="l",main="weekend",xlab="Interval")


#Final conversion
library(knitr)
list.files()
knit2html(input="PA1_template.Rmd")