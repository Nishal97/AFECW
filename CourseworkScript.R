#My analysis is to measure the relationship between the BSE SENSEX index prices and the change in the term structure over 
#the ten year period from 01/2010 to 12/2020, the term structure has been computed using the spread between 15-year
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
  library(zoo)
  library(xts)
  library(tidyverse)
  library(urca)
  
  setwd("C:/Users/Nish/OneDrive - University of Bristol/TB2/Applied Financial Econometrics/Coursework")
  bse<-read.csv("S&P BSE SENSEX.csv")
  sr<-read.csv("1yr Historical Bond Data India.csv")
  lr<-read.csv("15yr Historical Bond Data India.csv")
  


#DATA PROCESSING/FORMATTING
  bse$Close<-as.numeric(as.character(bse$Close))
  bse$Date<-as.Date(bse$Date,"%d/%m/%Y")
  sr$ï..Date<-as.Date(sr$ï..Date,"%B %d, %Y")
  lr$ï..Date<-as.Date(lr$ï..Date,"%B %d, %Y")
  
#PROCESSING BONDS 
  data <- merge(sr, lr, join = "right")
  data <- data.frame(sr=data$X1yr,
                   lr=data$X15yr,
                   Date = data$ï..Date,
                   year = as.numeric(format(data$ï..Date, format = "%Y")),
                   month = as.numeric(format(data$ï..Date, format = "%m")),
                   day = as.numeric(format(data$ï..Date, format = "%d")))
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
  
  bselr <- merge.xts(bse_xts, bonds_xts,join = "right", fill = NA)
  colnames(bselr)[1:3] <-c("bse","sr","lr")
  
  rm(bse_xts,bse,data2,data2_xts,lr_xts,sr_xts,sr,lr,data,bonds_xts)
  
#THIS FUNCTION TURNS THE SHORT RUN AND LONG RUN YIELDS INTO A SPREAD WHICH CHARACTERISES THE SLOPE OF THE YIELD CURVE. 
  bselr$term<-bselr$lr-bselr$sr

#SOME BASIC PLOTS
  plot(index(bselr),bselr$bse,type="l")
  plot(index(bselr),bselr$sr,type="l")
  plot(index(bselr),bselr$lr,type="l")
  plot(index(bselr),bselr$term,type="l")

#STEP 1
#RUNNING UNIT ROOT TESTS ON THE TREND 
  adfbse<-ur.df(bselr$bse,type="trend",selectlags="AIC")
  adfsr<-ur.df(bselr$sr,type="trend",selectlags="AIC")
  adflr<-ur.df(bselr$lr,type="trend",selectlags="AIC")
  adfterm<-ur.df(bselr$term,type="trend",selectlags="AIC")
  
#ADF for bse
  summary(adfbse)
#ADF for sr
  summary(adfsr)
#ADF for lr
  summary(adflr)
#ADF for term
  summary(adfterm)
  #Results show that all are non-stationary at the 5% level except term structure, which is stationary at the 5% level.

#STEP 2
#RUNNING A REGRESSION ON THE TREND
  Dbse <- na.omit(diff(bselr$bse, differences=1))
  Dsr <- na.omit(diff(bselr$sr, differences=1))
  Dlr <- na.omit(diff(bselr$lr, differences=1))
  Dterm <- na.omit(diff(bselr$term, differences=1))

  Lbse <- na.omit(stats::lag(bselr$bse, k=1))
  Lsr <- na.omit(stats::lag(bselr$sr, k=1))
  Llr <- na.omit(stats::lag(bselr$lr, k=1))
  Lterm <- na.omit(stats::lag(bselr$term, k=1))
  
  LDbse <- na.omit(stats::lag(Dbse, k=1))
  LDsr <- na.omit(stats::lag(Dsr, k=1))
  LDlr <- na.omit(stats::lag(Dlr, k=1))
  LDterm <- na.omit(stats::lag(Dterm, k=1))
  
  ttdata<-cbind(Dbse,Dsr,Dlr,Dterm,Lbse,Lsr,Llr,Lterm,LDbse,LDsr,LDlr,LDterm)
  ttdata<-na.omit(ttdata)
  
  adfbse<-lm(ttdata$bse ~ c(1:length(ttdata$bse))+ttdata$bse.2)
  adfsr<-lm(ttdata$sr ~ c(1:length(ttdata$sr))+ttdata$sr.2)
  adflr<-lm(ttdata$lr ~ c(1:length(ttdata$lr))+ttdata$lr.2)
  adfterm<-lm(ttdata$term ~ c(1:length(ttdata$term))+ttdata$term.2)
  
  #ADF for bse
  summary(adfbse)
  #ADF for sr
  summary(adfsr)
  #ADF for lr
  summary(adflr)
  #ADF for term
  summary(adfterm)
    #The results show that the time trend cannot be rejected at the 1% level for the variables - this is necessary as financial time series data
    #has been empirically proven to be DS and therefore needs strong evidence otherwise

#STEP 3 
  adfbse <- ur.df(bselr$bse, type="drift", selectlags="AIC")
  adfsr <- ur.df(bselr$sr, type="drift", selectlags="AIC")
  adflr <- ur.df(bselr$lr, type="drift", selectlags="AIC")
  adfterm <- ur.df(bselr$term, type="drift", selectlags="AIC")
  #ADF for bse
  summary(adfbse)
  #ADF for sr
  summary(adfsr)
  #ADF for lr
  summary(adflr)
  #ADF for term
  summary(adfterm)
    #Non-stationary for all variables at 5% except for term variable. 
  
#STEP 4
  adfbse <- ur.df(bselr$bse, type="none")
  adfsr <- ur.df(bselr$sr, type="none")
  adflr <- ur.df(bselr$lr, type="none")
  adfterm <- ur.df(bselr$term, type="none")
  #ADF for bse
  summary(adfbse)
  #ADF for sr
  summary(adfsr)
  #ADF for lr
  summary(adflr)
  #ADF for term
  summary(adfterm)
    #It is not possible to reject any of the variables at the 1% level in this situation, therefore it is concluded that, both relevant variables
    #bse and term are difference stationary. Individually, the short run and long run yields are difference stationary, which would support
    #the spread to being difference stationary as well, even though it showed less evidence of such compared to the two individually.
  
  
  adfbse <- ur.df(ttdata$bse, type="drift", selectlags="AIC")
  adfterm <- ur.df(ttdata$term, type="drift", selectlags="AIC")
  #ADF for First difference bse
  summary(adfbse)
  #ADF for First different term
  summary(adfterm)
    #Based on the conclusions seen in this test - we can reject the null for a unit root and conclude difference stationarity with an
    #integrated order of 1.
  
#CALCULATING THE RETURNS FOR THE VARIABLES
  #Take logs for percentage form
  bselr$rbse <- diff(log(bselr$bse))
  bselr$rsr <- diff(log(bselr$sr))
  bselr$rlr <- diff(log(bselr$lr))
  bselr$rterm <- bselr$rlr-bselr$rsr
  
  plot(bselr$rbse, col="red") 
  lines(bselr$rterm, col="blue")
  
  vardata <- cbind(bselr$rbse, bselr$rsr, bselr$rlr, bselr$rterm)
  vardata <- na.omit(vardata)
  
  rm(adfbse,adflr,adfsr,adfterm,Dbse,Dlr,Dsr,Dterm,Lbse,LDbse,LDlr,LDsr,LDterm,Llr,Lsr,Lterm,ttdata)
  
#So far we have concluded that the data are I(1), and as a result
  
  #Engle Granger First step
  
  EG1 <- lm(vardata$rbse ~ vardata$rterm + c(1:length(vardata$rbse)))
  summary(EG1)




  
  
  
  
  
  
  
  
  


  
  



