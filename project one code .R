A2010<- read.csv('/Users/yuehanxiao/Desktop/MA415/project/BP Apprehensions 2010.csv', header=TRUE, stringsAsFactors = FALSE)
A2017<- read.csv('/Users/yuehanxiao/Desktop/MA415/project/PB Apprehensions 2017(2).csv', header=TRUE, stringsAsFactors = FALSE)

A2010 <- read.csv('/Users/mariaren/Desktop/MA 615 first project/BP Apprehensions 2010.csv')
A2017 <- read.csv('/Users/mariaren/Desktop/MA 615 first project/PB Apprehensions 2017.csv')

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
A2010_matrix<-as.matrix(A2010[,-1])
is.matrix(A2010_matrix)
df[A2010_matrix[1,]]
is.vector(A2010_matrix)

#Time Series chart 
ts1 <- as.vector(A2010[,-1])
ts2 <- ts(ts1, start = c(2000,1), frequency=0.4)
ts.plot(ts2, gpars=list(xlab="year", ylab="Apprehensions", lty=c(1:3)))


ts2 <- as.vector(A2017[,-1])
ts3 <- ts(ts2, start = c(2000,1), frequency=0.4)
ts.plot(ts3, gpars=list(xlab="year", ylab="Apprehensions", lty=c(1:3)))







