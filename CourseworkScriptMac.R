#the ten year period from 01/2010 to 12/2020, the term structure has been computed using the spread between 15-year
#My analysis is to measure the relationship between the BSE SENSEX index prices and the change in the term structure over 
#and 1-year bond yields. My intuition is supported by the the fact that as the term structure increases this represents
#a widening in yields, implying that longer term bonds yield more than short term bonds and as such an increase in yield
#is driven by a decrease in their price and vice versa. During times of uncertainty and/or low confidence bond yields should
#drop as investors substitute their equity for safer investments and market forces drive the prices of bonds up and again vice versa
#This analysis will show the relationship (or lack of) between these two variables using VAR methods.

#DATA SOURCES
  #https://uk.investing.com/rates-bonds/india-1-year-bond-yield
  #https://uk.investing.com/rates-bonds/india-15-year-bond-yield
  
#PRELIMINARIES
rm(list = ls())
library(vars)
library(zoo)
library(xts)
library(tidyverse)
library(urca)
library(vars)
library(tsDyn)
library(coefplot)
library(lubridate)
options(scipen = 999)

setwd("/Users/nishaldave/OneDrive - University of Bristol/TB2/Applied Financial Econometrics/Coursework")
# setwd("C:/Users/Nish/OneDrive - University of Bristol/TB2/Applied Financial Econometrics/Coursework")
bse<-read.csv("S&P BSE SENSEX.csv")
sr<-read.csv("1yr Historical Bond Data India.csv")
lr<-read.csv("15yr Historical Bond Data India.csv")
  
###############################################################################
###############################################################################
#DATA PROCESSING/FORMATTING
bse$Close<-as.numeric(as.character(bse$Close))
bse$Date<-as.Date(bse$Date,"%d/%m/%Y")
sr$Date<-as.Date(sr$Date,"%B %d, %Y")
lr$Date<-as.Date(lr$Date,"%B %d, %Y")
  
#PROCESSING BONDS 
data <- merge(sr, lr, join = "right")
data <- data.frame(sr=data$X1yr,
                 lr=data$X15yr,
                 Date = data$Date,
                 year = as.numeric(format(data$Date, format = "%Y")),
                 month = as.numeric(format(data$Date, format = "%m")),
                 day = as.numeric(format(data$Date, format = "%d")))
data$ym <- as.yearmon(paste(data$year, data$month), "%Y %m")
sr_xts <- xts(data$sr, data$ym)
lr_xts <-xts(data$lr, data$ym)
sr_xts<-apply.monthly(sr_xts,mean)
lr_xts<-apply.monthly(lr_xts,mean)
bonds_xts <- merge(sr_xts, lr_xts, join = "right")
  
#PROCESSING INDEX VALUES
data2 <- data.frame(bse=bse$Close,
                  Date = bse$Date,
                  year = as.numeric(format(bse$Date, format = "%Y")),
                  month = as.numeric(format(bse$Date, format = "%m")),
                  day = as.numeric(format(bse$Date, format = "%d")))

data2$ym <- as.yearmon(paste(data2$year, data2$month), "%Y %m")
data2_xts <- xts(data2$bse, data2$ym)
#TAKING A DAILY AVERAGE FOR EACH MONTH TO CONVERT INTO MONTHLY DATA FOR 10 YEARS
bse_xts<-apply.monthly(data2_xts,mean)

bseND <- merge.xts(bse_xts, bonds_xts,join = "right", fill = NA)
colnames(bseND)[1:3] <-c("bse","sr","lr")

rm(bse_xts,bse,data2,data2_xts,lr_xts,sr_xts,sr,lr,data,bonds_xts)
#Now that the data is cleaned - any further transformations can be applied.
#Before any transformation is applied - taking logs provides a monotonic transformation to our variables of interest.
lbseND <- log(bseND)
#Generate returns to BSE index by taking the first difference - since this is our variable of interest
lbseND$rlbse <- diff(lbseND$bse)
colnames(lbseND)[1:3] <-c("lbse","lsr","llr")
#Generate slope variable which characterizes the slope of the yield curve
lbseND$slope <- lbseND$llr-lbseND$lsr
#Combining variables of interest
dataND <- na.omit(cbind(lbseND$rlbse,lbseND$slope))
colnames(dataND)[1] <- c("returns")
rm(bseND,lbseND)

#SOME BASIC PLOTS
(ggplot(dataND,aes(x=Index,y=returns))
  +geom_line()
  +xlab("Year")
  +ylab("Returns")
)

(ggplot(dataND,aes(x=Index,y=slope))
  +geom_line()
  +xlab("Year")
  +ylab("Yield Curve Slope")
)

plot(dataND$returns, col="red",ylim=c(-0.5,0.7)) 
lines(dataND$slope, col="blue")

###############################################################################
###############################################################################
#RUNNING UNIT ROOT TESTS
#lags are determined by T^{0.25}, where T=131, this is approximately 3 lags.
adfreturns<-ur.df(dataND$returns,type="drift",lags=3)
adfslope<-ur.df(dataND$slope,type="drift",lags=3)
adfreturns<-ur.df(dataND$returns,type="drift",selectlags="AIC")
adfslope<-ur.df(dataND$slope,type="drift",selectlags="AIC")

#ADF for returns
summary(adfreturns)
#ADF for slope
summary(adfslope)
#Results show that returns are stationary, slope non-stationary

rm(adfreturns,adfslope)

#Unit root test on first difference of slope
adfslope<-ur.df(na.omit(diff(dataND$slope)),type="drift",lags=3)
adfslope<-ur.df(na.omit(diff(dataND$slope)),type="drift",selectlags="AIC")
summary(adfslope)

rm(adfslope)
###############################################################################
###############################################################################
#We have now established that the series' for returns and slope are I(0) and I(1) respectively - this means we can take the
#first difference of slope, which gives us two stationary variables to model a VAR with.
VARND <- na.omit(cbind(dataND$returns,diff(dataND$slope)))
  
#VAR MODELLING
VARselect(VARND, lag.max=12, type = "both")
#Based on the VARselect function the minimum lag length suggested by the AIC is 3 lags


VARmodel <- VAR(y = VARND, lag.max=12, ic="AIC")
summary(VARmodel)
coef(VARmodel)

#Forecast
varfc <- predict(VARmodel, n.ahead = 12, ci = 0.95, dumvar = NULL)
plot(varfc)
#Impulse Response Function
irf <- irf(VARmodel, impulse="returns", response=c("slope"))
plot(irf)
#Variance Decomposition
vardec <- fevd(VARmodel, n.ahead=12)
vardec
#Granger Causality Test
causality(VARmodel, cause="slope")$Granger
causality(VARmodel, cause="returns")$Granger
#The conclusions drawn from the granger causality test tell us that whilst the change in the slope of the yield curve do not granger cause
#returns, the opposite is a different case, there is a strong statistical significance in the impact of returns upon the change in the slope 
#of the yield curve.

#VAR DIAGNOSTICS
serial.test(VARmodel, type = "BG")
#The Breusch-Godfrey serial correlation test suggests that there is no serial correlation in residuals at all levels
serial.test(VARmodel, type = "PT.adjusted")
#Adjusting for a small sample, the Portmanteau test confirms the result above albeit with stronger significance of the null

normality.test(VARmodel, multivariate.only = TRUE)
#The null of normality is rejected for all levels - which implies that the VAR error term is highly likely to be non-normally distributed.
#As a result there are likely to be some misspecification in the VAR model.


VARparamstab <- stability(VARmodel)
plot(VARparamstab)
  
arch.test(VARmodel,lags.multi=1, multivariate.only=TRUE)

#Results of the ARCH test show that the null of no ARCH effect is rejected, so there is likely some heteroscedasticity in the data.
  
#The results from the analysis above show that the VAR model may not be the best model for this situation, this is due to the heteroskedasticity
#in the error term and the subsequential non-normality that arises from that.

  
  
  
  
  
  


  
  




    