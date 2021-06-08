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
library(lmtest)
library(scales)
options(scipen = 999)


setwd("C:/Users/Nish/OneDrive - University of Bristol/TB2/Applied Financial Econometrics/Coursework")
bse<-read.csv("S&P BSE SENSEX.csv")
sr<-read.csv("1yr Historical Bond Data India.csv")
lr<-read.csv("15yr Historical Bond Data India.csv")
  
###############################################################################
###############################################################################
#DATA PROCESSING/FORMATTING
bse$Close<-as.numeric(as.character(bse$Close))
bse$Date<-as.Date(bse$Date,"%d/%m/%Y")
sr$Date<-as.Date(sr$ï..Date,"%B %d, %Y")
lr$Date<-as.Date(lr$ï..Date,"%B %d, %Y")
  
#PROCESSING BONDS 
NDdata <- merge(sr, lr, join = "right")
NDdata <- data.frame(sr=NDdata$X1yr,
                 lr=NDdata$X15yr,
                 Date = NDdata$Date,
                 year = as.numeric(format(NDdata$Date, format = "%Y")),
                 month = as.numeric(format(NDdata$Date, format = "%m")),
                 day = as.numeric(format(NDdata$Date, format = "%d")))
NDdata$ym <- as.yearmon(paste(NDdata$year, NDdata$month), "%Y %m")
sr_xts <- xts(NDdata$sr, NDdata$ym)
lr_xts <-xts(NDdata$lr, NDdata$ym)
sr_xts<-apply.monthly(sr_xts,mean)
lr_xts<-apply.monthly(lr_xts,mean)
bonds_xts <- merge(sr_xts, lr_xts, join = "right")
  
#PROCESSING INDEX VALUES
NDdata2 <- data.frame(bse=bse$Close,
                  Date = bse$Date,
                  year = as.numeric(format(bse$Date, format = "%Y")),
                  month = as.numeric(format(bse$Date, format = "%m")),
                  day = as.numeric(format(bse$Date, format = "%d")))

NDdata2$ym <- as.yearmon(paste(NDdata2$year, NDdata2$month), "%Y %m")
NDdata2_xts <- xts(NDdata2$bse, NDdata2$ym)
#TAKING A DAILY AVERAGE FOR EACH MONTH TO CONVERT INTO MONTHLY DATA FOR 10 YEARS
bse_xts<-apply.monthly(NDdata2_xts,mean)

bseND <- merge.xts(bse_xts, bonds_xts,join = "right", fill = NA)
colnames(bseND)[1:3] <-c("bse","sr","lr")

rm(bse_xts,bse,NDdata2,NDdata2_xts,lr_xts,sr_xts,sr,lr,NDdata,bonds_xts)
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

bseND$slope <- (bseND$lr-bseND$sr) 

dataND <- dataND["/2019-12"]


##Johansen##
vecmdata <- cbind(bseND$bse, bseND$slope)
VARselect(vecmdata, lag.max=12, type = "both")
#2 lags have been determined to minimise the AIC

#case 1
Jtest1 <- ca.jo(vecmdata, type = "trace", ecdet = "none", K = 2, spec = "transitory")
summary (Jtest1)
#no cointegration

#case 2
Jtest1 <- ca.jo(vecmdata, type = "trace", ecdet = "const", K = 2, spec = "transitory")
summary (Jtest1)
#no cointegration

#case 3
Jtest1 <- ca.jo(vecmdata, type = "trace", ecdet = "trend", K = 2, spec = "transitory")
summary (Jtest1)
#no cointegration?

rm(vecmdata)

#SOME BASIC PLOTS
(ggplot(bseND,aes(x=Index,y=bse,color=''))
  +geom_line()
  +xlab("")
  +ylab("BSE Price")
  +theme(legend.position = "none")
  +scale_y_continuous(label = comma, name ="BSE SENSEX: Monthly Average (INR)", 
                     limits=c(15000,50000))
)

(ggplot() + 
  geom_line(data = bseND, aes(x = Index, y = sr), color = "red") +
  geom_line(data = bseND, aes(x = Index, y = lr), color = "blue") +
  xlab('') +
  ylab('% Yield') +
  scale_y_continuous(label = comma, name ="Bond Yields: Monthly Average (%)", 
                      limits=c(-1,10))
)


(ggplot(dataND,aes(x=Index,y=returns,color=''))
  +geom_line()
  +xlab("")
  +ylab("BSE SENSEX: Monthly Returns (%)")
  +theme(legend.position = "none") 
)

(ggplot(dataND,aes(x=Index,y=slope,color = "blue" ))
  +geom_line()
  +xlab("")
  +ylab("Yield Curve Slope")
  +scale_y_continuous(name ="Yield Curve Slope (%)", 
                     limits=c(-0.1,0.6))
  +theme(legend.position = "none") 
)

###############################################################################
###############################################################################
#RUNNING UNIT ROOT TESTS
#lags are determined by T^{0.25}, where T=131, this is approximately k=3 lags.
#lags determined by the minimum AIC yield a result of k=1 lags.

### STEP 1 ###
adfreturns<-ur.df(dataND$returns,type="none",selectlags="AIC")
adfreturns<-ur.df(dataND$returns,type="drift",selectlags="AIC")
adfreturns<-ur.df(dataND$returns,type="trend",selectlags="AIC")

adfslope<-ur.df(dataND$slope,type="none",selectlags="AIC")
adfslope<-ur.df(dataND$slope,type="drift",selectlags="AIC")
adfslope<-ur.df(dataND$slope,type="trend",selectlags="AIC")
#All do not reject null for slope - AIC has selected 1 lag for both variables

#ADF for returns
summary(adfreturns)
#ADF for slope
summary(adfslope)


#VAR MODELLING
VARselect(VARND, lag.max=12, type = "both")
#Based on the VARselect function the minimum lag length suggested by the AIC is 3 lags


#Forecast
varfc <- predict(VARmodel, n.ahead = 12, ci = 0.95, dumvar = NULL)
plot(varfc)

#Impulse Response Function
irf <- irf(VARmodel, impulse="returns", response=c("slope"), n.ahead = 12)
plot(irf)
irf <- irf(VARmodel, impulse="slope", response=c("returns"), n.ahead = 12)
plot(irf)

#Variance Decomposition
vardec <- fevd(VARmodel, n.ahead=12)
vardec

#Granger Causality Test
causality(VARmodel, cause="slope")$Granger
causality(VARmodel, cause="returns")$Granger

#VAR DIAGNOSTICS
serial.test(VARmodel,lags.bg = 12, type = "BG")
#The Breusch-Godfrey serial correlation test suggests that there is no serial correlation in residuals at all levels
serial.test(VARmodel,lags.bg = 12, type = "PT.adjusted")
#Adjusting for a small sample, the Portmanteau test confirms the result above albeit with stronger significance of the null

normality.test(VARmodel, multivariate.only = TRUE)
#The null of normality is rejected for all levels - which implies that the VAR error term is highly likely to be non-normally distributed.

#Shapiro-Wilks Test
VARresid <- resid(VARmodel)
shapiro.test(as.vector(VARresid))

#ARCH-LM test
arch.test(VARmodel,lags.multi=12, multivariate.only=TRUE)

  
  
  
  




  
  




    