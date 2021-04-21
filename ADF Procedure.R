###############################################################################
###############################################################################
#STEP 1
#RUNNING UNIT ROOT TESTS ON THE TREND
#4 lags are determined by T^{0.25}, where T=119, this is approximately 3 lags.
adfreturns<-ur.df(dataND$returns,type="trend",lags=3)
adfslope<-ur.df(dataND$slope,type="trend",lags=3)
adfreturns<-ur.df(dataND$returns,type="trend",selectlags="AIC")
adfslope<-ur.df(dataND$slope,type="trend",selectlags="AIC")

#ADF for returns
summary(adfreturns)
#ADF for slope
summary(adfslope)
#Results show that returns are stationary at all levels, however the slope variable is non-stationary at 5% and 10%
#using 4 lags and lags selected by the AIC respectively.

#STEP 2
#RUNNING A REGRESSION ON THE TREND
Dslope <- na.omit(diff(dataND$slope, differences=1))
Lslope <- na.omit(stats::lag(dataND$slope, k=1))
LDslope <- na.omit(stats::lag(Dslope, k=1))

ttdata<-cbind(Dslope,lslope,LDslope)
ttdata<-na.omit(ttdata)

adfslope<-lm(ttdata$term ~ c(1:length(ttdata$term))+ttdata$term.2)

summary(adfslope)
#The results show that the null still cannot be rejected - so the trend coefficient is not significant.  

#STEP 3 
adfslope <- ur.df(dataND$slope, type="drift", lags=4)
adfslope <- ur.df(dataND$slope, type="drift", selectlags="AIC")
summary(adfslope)
#Cannot reject the null at any level - continue

#STEP 4
adfslope <- ur.df(dataND$slope, type="none")
summary(adfslope)

#Cannot reject the null and therefore conclude that the series is difference stationary
