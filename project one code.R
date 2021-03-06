A2010<- read.csv('/Users/yuehanxiao/Desktop/MA415/project/BP Apprehensions 2010.csv', header=TRUE, stringsAsFactors = FALSE)
A2017<- read.csv('/Users/yuehanxiao/Desktop/MA415/project/PB Apprehensions 2017(2).csv', header=TRUE, stringsAsFactors = FALSE)


#find the sum of each sector without the first column in 2010
sector_sum_2010<-rowSums(A2010[,-1])
#find the sector with the most apprehensions
max(sector_sum_2010)
#Tucson is the sector with the most apprehensions. 

#find the sum of each sector in 2017 
sector_sum_2017<-rowSums(A2017[,-1])
#find the sector with the most apprehensions in 2010 
max(sector_sum_2017)
#Rio Grande Valley is the sector wiht the most apprehensions. 

#Two samples t-test
#Extracting tucson's monthly apprehensions from the data set and set as vector. 
row.names(A2010) <- A2010[,1]
A2010<-A2010[,2:13]
A2010_tucson <- as.vector(A2010[8,],mode='numeric')
#Extracting Rio Grande Valley monthly apprehensions from the data set and set as vector. 
row.names(A2017) <- A2017[,1]
A2017 <- A2017[,2:13]
A2017_RGV <- as.vector(A2017[6,],mode='numeric')
#Two Sample T-test
intake.A2010_sector <- A2010_tucson
intake.A2017_sector <- A2017_RGV
t.test(intake.A2010_sector, intake.A2017_sector)
#Since the p-value is 0.06346 which is greater than 0.05, so we fail to reject null hypothesis. It indiates 
#that there are no significant difference in the mean apprehensions of 2010 Tucson and 2017 Rio Grand Valley. 
#So there is no change in the sector'smaximum. 

#Through bar plots, we observed the 3 month periods with the most apprehsions in 2010 is March, April and May
#And the 3 month periods with the most apprehensions in 2017 are October, Novermber and December. 
#extract March, Aprial and May's apprehensions from 2010
A2010_3 <- as.vector(A2010[,6], mode='numeric' )
A2010_4 <- as.vector(A2010[,7], mode='numeric' )
A2010_5 <- as.vector(A2010[,8], mode='numeric' )
#combines them into a matrix and then into a vector 
A2010_345<-as.vector(cbind(A2010_3,A2010_4,A2010_5))

#extract October, Novermber and December's apprehensions from 2017
A2017_10 <- as.vector(A2017[,1], mode='numeric' )
A2017_11 <- as.vector(A2017[,2], mode='numeric' )
A2017_12 <- as.vector(A2017[,3], mode='numeric' )
#combines them into a matrix and then into a vector 
A2017_101112 <- as.vector(cbind(A2017_10,A2017_11,A2017_12))
#two sample t-test
intake.A2010_month <- A2010_345
intake.A2017_month <- A2017_101112
t.test(intake.A2010_month, intake.A2017_month)
#Since the p-value is 0.2075 which is greater than 0.05, so we fail to reject null hypothesis. It indiates 
#that there are no significant difference in the mean apprehensions of 2010 March, April and May,  and 2017 October, Novermber and December
#So there is no change in the month's maximum. 



#Time Series

A2010 <- read.csv('/Users/mariaren/Desktop/MA 615 first project/PB Apprehensions 2010.csv',header=TRUE, stringsAsFactors = FALSE)
A2017 <- read.csv('/Users/mariaren/Desktop/MA 615 first project/PB Apprehensions 2017.csv',header=TRUE, stringsAsFactors = FALSE)
Monthly <- read.csv('/Users/mariaren/Desktop/MA 615 first project/PB monthly summaries.csv',header=TRUE, stringsAsFactors = FALSE)

# Monthly Summary BP Apprehensions
# Time series chart for the monthly summary in BP Apprehensions across the different sectors
ts8 <- as.vector(t(Monthly[,-1]))
ts9 <- ts(ts8, start= c(2000,10), frequency=12)
ts10 <- ts.plot(ts9, gpars=list(xlab="year", ylab="Apprehensions", lty=c(1:3)),col='purple')
#From the time series plot, we can see that the average monthly BP Apprehensions increases across the years between 2000 and 2017. 

#the following is wrong 
#Additional barplots of both 2010 and 2017 apprehensions 

#Barplot of 2010 BP Apprehensions

A2010 <- read.csv('/Users/mariaren/Desktop/MA 615 first project/PB Apprehensions 2010.csv',header=TRUE,row.names=1, stringsAsFactors = FALSE)
A2017 <- read.csv('/Users/mariaren/Desktop/MA 615 first project/PB Apprehensions 2017.csv',header=TRUE,row.names=1, stringsAsFactors = FALSE)

barplot(A2010[1:9,13], names.arg = rownames(A2010)[1:9], 
        las=2,
        axisnames=TRUE,
        main="2010 Border Patrol Apprehensions by Sector",
        border="blue",
        col="yellow")

#Barplot of 2017 BP Apprehensions
barplot(A2017[1:9,13], names.arg = rownames(A2017)[1:9], 
        las=2,
        axisnames=TRUE,
        main="2017 Border Patrol Apprehensions by Sector",
        border="black",
        col="green")




