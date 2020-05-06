setwd("/Users/sunshaoyang/Desktop/")
Citi=read.csv("/Users/sunshaoyang/Desktop/Citi.csv")

#California
CalData = Citi[Citi$State=="California",]

fire_data = ts(data = Citi[,27], start = 2007, frequency = 1)
temp_data_jan = ts(data = Citi[,3], start = 2007, frequency = 1)
temp_data_feb = ts(data = Citi[,4], start = 2007, frequency = 1)
temp_data_march = ts(data = Citi[,5], start = 2007, frequency = 1)
temp_data_apr = ts(data = Citi[,6], start = 2007, frequency = 1)
temp_data_may = ts(data = Citi[,7], start = 2007, frequency = 1)
temp_data_june = ts(data = Citi[,8], start = 2007, frequency = 1)
temp_data_july = ts(data = Citi[,9], start = 2007, frequency = 1)
temp_data_aug = ts(data = Citi[,10], start = 2007, frequency = 1)
temp_data_sept = ts(data = Citi[,11], start = 2007, frequency = 1)
temp_data_oct= ts(data = Citi[,12], start = 2007, frequency = 1)
temp_data_nov = ts(data = Citi[,13], start = 2007, frequency = 1)
temp_data_dec = ts(data = Citi[,14], start = 2007, frequency = 1)
insurance_data = ts(data = Citi[,28], start = 2007, frequency = 1)


reg = tslm(fire_data~trend, data=CalData)
S(reg)
plot(fire_data)
lines(reg$fitted.values, col = "red")

y = cbind(reg$residuals,temp_data,rain_data)
S(VAR(y, p = 1))

tsdisplay(reg$residuals)
auto.arima(reg$residuals)

names(CalData)[27] = "Acres"
ss=regsubsets( Acres~ .,method=c("exhaustive"),nbest=1,data = CalData[,3:39], nvmax = 10,force.in = "RXBURNS" )
summary(ss)

cpoutput = lm(Acres ~  TempJuly + TempSeptember+ RainJanuary+RainMay +RainJune +DryApril + DryJuly , data= CalData)
summary(cpoutput)

vif(cpoutput)
#vif is small
gqtest(cpoutput)
#p-value in gqtest is not significant, which is good.

#Colorado
CoData=Citi[Citi$State=="Colorado",]
names(CoData)[27] = "Acres"
ss_co=regsubsets( Acres~ .,method=c("exhaustive"),nbest=1,data = CoData[,3:39], nvmax = 10,force.in = "RXBURNS" )
summary(ss_co)

#Check multilinearity
vif(Colo_cpoutput)
#DryOctober has a 30 vif which is large

#Check heteroskedasciticity
gqtest(Colo_cpoutput)
ibrary(leaps)
UtahData = Citi[Citi$State=="Utah",]
ss=regsubsets( Acres~ .,method=c("exhaustive"),nbest=1,data = UtahData[,3:39] )
ss.summary=summary(ss)
ss.summary
plot(ss,scale = "adjr2")

#final model
cpoutput = lm(Acres ~RXBURNS+ TempJanuary +TempApril +TempDecember +RainApril+RainSeptember+RainDecember, data= CoData)
S(cpoutput)
grangertest(Acres ~RXBURNS,order=1)
grangertest( RXBURNS~Acres,order=1)
a=var(CoData)
causality(a)


#Utah
library(car)
modelw8 = lm(Acres ~  TempJanuary + RainJune +RainSeptember + RainOctober + DryJanuary+DryMarch+DryMay+DryAugust, data= UtahData)
summary(modelw8)
vif(modelw8)
gqtest(modelw8)

The multicollinearity between variables are too high and it is hard to test the heteroskedascticity also. Then modify the model.

modelw6 = lm(Acres ~  TempMarch + RainJuly +RainAugust + RainNovember +RainDecember+ DrySeptember, data= UtahData)
summary(modelw6)
vif(modelw6)
gqtest(modelw6)
#p-value in gqtest is not significant, which is good.