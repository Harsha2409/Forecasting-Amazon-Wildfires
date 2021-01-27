library(data.table)
library(ggplot2)
library(forecast)
library(tseries)
library(urca)
library(dplyr)
library(GGally)

# read the data and make a copy of it
amazon <- read.csv('amazon1.csv')
str(amazon)
View(amazon)
amazon= data.table(amazon)

amazon_sorted = amazon[order(amazon$year)]


amazon_fire = amazon_sorted[which(amazon_sorted$state=="Amazonas")]

# convert it to time-series data
ts_amazon=ts(amazon_fire$number, start=c(1998,1) ,frequency=12 )
str(ts_amazon)
View(ts_amazon)

#check for NA
sum(is.na(ts_amazon))

#plot for time and seasonality
autoplot(ts_amazon)+ ggtitle('Amazon Forest Fires in Brazil')+ xlab('Year')+ ylab('Number of Fires')

#decomposition
autoplot(decompose(ts_amazon,type='additive')) + xlab ('Year')
+ ggtitle('Decomposition Forest Fires')


#boxplot for each month
boxplot(ts_amazon~cycle(ts_amazon), xla='Month',
  ylab='Number of Fires', main='Forest Fires in Brazil')

# seasonal plot
ggseasonplot(ts_amazon, year.labels=TRUE, year.labels.left=TRUE)
+ylab('Number of fires') +ggtitle("Seasonal plot for Forest Fires in Brazil")

#subseriesplot
ggsubseriesplot(ts_amazon) + ylab('Number of fires') 
+ggtitle('Seasonal Subseries plot for Forest Fires in Brazil')

#acf: lag 1 positive, use AR model. Lag 1 -ve, MA.
autoplot(acf(ts_amazon,plot=TRUE))
+labs(title="Correlogram of Amazon Forest fires")


#our time series data shows ACF, which is why no white noise


#Simple Forcasting Methods


train=window(ts_amazon, start=c(1998,1), end=c(2012,12))
test=window(ts_amazon, start=c(2013,1))



#mean

mmean=meanf(train, h=100)
autoplot(train) + autolayer(mmean, series="Mean", PI=FALSE) 
+xlab('Year') +ylab('Number of fires') 
+ggtitle('Average method')+ guides(colour=guide_legend(title='Forecast'))

#naive

nnaive=rwf(train, h=100)
autoplot(train)+ autolayer(nnaive, series="Naive", PI=FALSE) 
+xlab('Year') +ylab('Number of fires') 
+ggtitle('Naive method')+guides(colour=guide_legend(title='Forecast'))

#seasonal naive

seanaive=snaive(train, h=100)
autoplot(train)+ autolayer(seanaive, series="Seasonal Naive", PI=FALSE) 
+xlab('Year') +ylab('Number of fires') 
+ggtitle('Seasonal Naive method')+guides(colour=guide_legend(title='Forecast'))

#drift

drift=rwf(train, h=100, drift = TRUE)
autoplot(train)+ autolayer(drift,series="Drift",PI=FALSE) 
+xlab('Year')+ylab('Number of fires')
+ggtitle('Drift method')+guides(colour=guide_legend(title='Forecast'))


#all

autoplot(train)+
  autolayer(mmean, series="Mean", PI=FALSE)+
  autolayer(nnaive, series="Naive", PI=FALSE)+
  autolayer(seanaive, series="Seasonal Naive", PI=FALSE)+
  autolayer(drift,series="Drift",PI=FALSE) + xlab('Year')+ylab('Number of fires')+
  ggtitle('Forecasts for Amazon forest fires')+
  guides(colour=guide_legend(title='Forecast'))


# accuracy test

accuracy(mmean,test)
accuracy(nnaive,test)  
accuracy(seanaive,test)  
accuracy(drift,test)  

#lowest MAE and ACF1 values for seasonal naive, so that's good


#Check for stationarity: mean and variance constant over time.
#1st diff = current - previous

#AUGMENTED DICKEY-FULLER TEST

adf.test(ts_amazon)

#P-VALUE IS 0.01, which is not more than 1%, so we reject null hypothesis of not stationarity


#KPSS UNIT ROOT TEST

summary(ur.kpss(ts_amazon))

ndiffs(ts_amazon)

#seasonal differencing

nsdiffs(ts_amazon)


#p<- AR
#d-diff
#q<-MA
