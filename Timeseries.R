#Get the directory
getwd()
#Set Directory
setwd("C:/Users/Dell/Downloads")

#Loading data
dataset <- read.csv("Dogecoin Historical Data - Investing.com India.csv")
head(dataset)

install.packages("xts")
library(xts)

# Extract the closing prices
price <- dataset[ , "Price"]
head(price)


install.packages("tseries")
library(tseries)



# Arima model
acf(price)
pacf(price)


#Test of Stationarity
plot.ts(price)
adf.test(price)
PP.test(price)


# making price stationary
new_price = diff(log(price))

#checking after making it stationary
plot(new_price)
plot.ts(new_price)
adf.test(new_price)
PP.test(new_price)

acf(new_price)
pacf(new_price)


install.packages("forecast")  # install the forecast package
library(forecast) 

# TO get best model of ARIMA
auto.arima(new_price)

# Fitting the model
model1 <- arima(new_price, order = c(1, 0, 3))
model1



# TO test the model1
res1 = residuals(model1)

box_pierce_test <- Box.test(res1, lag=10, type="Box-Pierce")
ljung_box_test <- Box.test(res1, lag=10, type="Ljung-Box")
print(box_pierce_test)
print(ljung_box_test)

# check model summary
summary(model1)

coef(model1)

# check forecasted prices
forecasted_prices <- forecast(model1, h = 10)
forecasted_prices

#check model accuracy
accuracy(model1)

plot(model1)


# 1st variation of ARIMA

# Fitting the model
model2 <- arima(new_price, order = c(1, 1, 2))
model2



# TO test the model2
res2 = residuals(model2)

box_pierce_test2 <- Box.test(res2, lag=10, type="Box-Pierce")
ljung_box_test2 <- Box.test(res2, lag=10, type="Ljung-Box")
print(box_pierce_test2)
print(ljung_box_test2)

# check model summary
summary(model2)

coef(model2)

# check forecasted prices
forecasted_prices2 <- forecast(model2, h = 10)
forecasted_prices2

#check model accuracy
accuracy(model2)

plot(model2)


# 2nd variation of ARIMA

# Fitting the model
model3 <- arima(new_price, order = c(2, 0, 2))
model3



# TO test the model3
res3 = residuals(model3)

box_pierce_test3 <- Box.test(res3, lag=10, type="Box-Pierce")
ljung_box_test3 <- Box.test(res3, lag=10, type="Ljung-Box")
print(box_pierce_test3)
print(ljung_box_test3)

# check model summary
summary(model3)

coef(model3)

# check forecasted prices
forecasted_prices3 <- forecast(model3, h = 10)
forecasted_prices3

#check model accuracy
accuracy(model3)



plot(model3)



# GARCH
install.packages("rugarch")
library(rugarch)

install.packages("FinTS")
library(FinTS)

install.packages("e1071")
library(e1071)

new_price = diff(log(price))


plot.ts(new_price)
adf.test(new_price)
# this means that our data is stationary

# Checking arch effect
ArchTest(new_price)

garch(new_price,grad="numerical",trace= FALSE)

# Fitting the model
price_garch = ugarchspec(variance.model = list(garchOrder=c(1,1)), mean.model = list(armaOrder=c(1,1)))
price_garch_fit = ugarchfit(price_garch, data = new_price)
price_garch_fit

#plotting news impact
news_garch = newsimpact((price_garch_fit))

plot(news_garch$zx,news_garch$zy,ylab=news_garch$yexpr,xlab=news_garch$xexpr, main="News Impact Curve")

# TO test the model
res4 = residuals(price_garch_fit)

# CHecking our model
box_pierce_test4 <- Box.test(res4, lag=10, type="Box-Pierce")
ljung_box_test4 <- Box.test(res4, lag=10, type="Ljung-Box")
print(box_pierce_test4)
print(ljung_box_test4)

# check model summary
summary(price_garch_fit)

coef(price_garch_fit)

# Forecasting prices
price_vol = ugarchforecast(price_garch_fit,n.head=10)
price_vol



plot(price_garch_fit, which='all')

plot(price_garch_fit)
     

# EGARCh

# Fitting the model
price_egarch = ugarchspec(variance.model = list(model="eGARCH",garchOrder=c(1,1)), mean.model = list(armaOrder=c(1,1)))
price_egarch_fit = ugarchfit(price_egarch, data = new_price)
price_egarch_fit

# TO test the model
res5 = residuals(price_garch_fit)

# CHecking our model
box_pierce_test5 <- Box.test(res5, lag=10, type="Box-Pierce")
ljung_box_test5 <- Box.test(res5, lag=10, type="Ljung-Box")
print(box_pierce_test5)
print(ljung_box_test5)

# plotting news impact
news_egarch = newsimpact((price_egarch_fit))

plot(news_egarch$zx,news_egarch$zy,ylab=news_egarch$yexpr,xlab=news_egarch$xexpr, main="News Impact Curve")

# forecasting prices
price_fore = ugarchforecast(price_egarch_fit,n.head=10)
price_fore 


plot(price_egarch_fit, which='all')

plot(price_egarch_fit)


# GJRGARCh

# fitting the model
price_gjrgarch = ugarchspec(variance.model = list(model="gjrGARCH",garchOrder=c(1,1)), mean.model = list(armaOrder=c(1,1)))
price_gjrgarch_fit = ugarchfit(price_gjrgarch, data = new_price)
price_gjrgarch_fit



# TO test the model
res6 = residuals(price_gjrgarch_fit)

# CHecking our model
box_pierce_test6 <- Box.test(res6, lag=10, type="Box-Pierce")
ljung_box_test6 <- Box.test(res6, lag=10, type="Ljung-Box")
print(box_pierce_test6)
print(ljung_box_test6)


# plotting news impact
news_gjrgarch = newsimpact((price_gjrgarch_fit))

plot(news_gjrgarch$zx,news_gjrgarch$zy,ylab=news_gjrgarch$yexpr,xlab=news_gjrgarch$xexpr, main="News Impact Curve")

# forecasting
price_fore3 = ugarchforecast(price_gjrgarch_fit,n.head=10)
price_fore3 


plot(price_gjrgarch_fit, which='all')

plot(price_gjrgarch_fit)

y=fitted(price_gjrgarch_fit)
y















