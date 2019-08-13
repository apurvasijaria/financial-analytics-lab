setwd("C:/Users/DELL-PC/Desktop/2017-18/FA Lab/Assignment 4")
options(warn=-1)
##Time Period  : 1 Feb 2013 - 4 Feb 2018

##read the merged files
goldbees = read.csv("goldbees.csv")
silv = read.csv("silver_fut.csv")
gol = read.csv("gold_fut.csv")
plat = read.csv("plat_fut.csv")
usdinr = read.csv("usdinr.csv")

##Drop unecesarry cols and cleaning the data
goldbees = goldbees[,c(4,5,10)]
colnames(goldbees) <- c("Date","lastspot","spot")
silv = silv[,c(1,2)]
colnames(silv) <- c("Date","silver_fut")
gol = gol[,c(1,2)]
colnames(gol) <- c("Date","gold_fut")
plat = plat[,c(1,2)]
colnames(plat) <- c("Date","plat_fut")
usdinr = usdinr[,c(1,2)]
colnames(usdinr) <- c("Date","usdinr")
plat = plat[1:1555,]
silv = silv[1:1335,]
gol=gol[1:1322,]
usdinr = usdinr[1:1306,]

##Make date formats even for both files for merging
silv$Date = as.Date(silv$Date, format = '%b %d,%Y')
gol$Date = as.Date(gol$Date, format = '%b %d,%Y')
plat$Date = as.Date(plat$Date, format = '%b %d,%Y')
usdinr$Date = as.Date(usdinr$Date, format = '%b %d,%Y')
goldbees$Date = as.Date(goldbees$Date, format = '%d-%b-%Y')


##merge the spot and futures by date
fut1 = merge(x=silv,y=gol, by.x = "Date", by.y = "Date")
fut2 = merge(x=plat,y=usdinr, by.x = "Date", by.y = "Date")
fut = merge(x=fut2,y=fut1, by.x = "Date", by.y = "Date")

##Converting all the privces in INR
fut$plat_fut = as.numeric(gsub(",", "", fut$plat_fut))
fut$gold_fut = as.numeric(gsub(",", "", fut$gold_fut))
fut$usdinr = as.numeric(gsub(",", "", fut$usdinr))
fut$plat_fut = fut$plat_fut*fut$usdinr
fut$gold_fut = fut$gold_fut*fut$usdinr


vecm= merge (x = goldbees , y= fut , by.x = "Date", by.y = "Date" )

##calculating returns for spot and futures

##For gold BeES spot prices
vecm$retspot = log(vecm$spot/vecm$lastspot)

##silver futures
vecm$silver_last = 0
vecm$silver_fut =as.numeric(gsub(",", "", vecm$silver_fut))
vecm$silver_last =as.numeric(gsub(",", "", vecm$silver_last))
vecm$silver_last[2:nrow(vecm)] = vecm$silver_fut[1:nrow(vecm)-1]
vecm$silver_ret = log(vecm$silver_fut/vecm$silver_last)
vecm$silver_ret[1]=0

##gold futures
vecm$gold_last = 0
vecm$gold_fut =as.numeric(gsub(",", "", vecm$gold_fut))
vecm$gold_last =as.numeric(gsub(",", "", vecm$gold_last))
vecm$gold_last[2:nrow(vecm)] = vecm$gold_fut[1:nrow(vecm)-1]
vecm$gold_ret = log(vecm$gold_fut/vecm$gold_last)
vecm$gold_ret[1]=0

##platinium futures
vecm$plat_last = 0
vecm$plat_fut =as.numeric(gsub(",", "", vecm$plat_fut))
vecm$plat_last =as.numeric(gsub(",", "", vecm$plat_last))
vecm$plat_last[2:nrow(vecm)] = vecm$plat_fut[1:nrow(vecm)-1]
vecm$plat_ret = log(vecm$plat_fut/vecm$plat_last)
vecm$plat_ret[1]=0


#installing packages
#install.packages("vars")
library(vars)
library(forecast)
library(tseries)


## checking stationarity
# alpha = 0.05
adf.test(vecm$silver_fut, k = 5)[4]$p.value
adf.test(vecm$gold_fut, k = 5)[4]$p.value
adf.test(vecm$plat_fut, k = 5)[4]$p.value
adf.test(vecm$spot, k = 5)[4]$p.value    
adf.test(vecm$silver_ret, k = 5)[4]$p.value 
adf.test(vecm$gold_ret, k = 5)[4]$p.value 
adf.test(vecm$plat_ret, k = 5)[4]$p.value 
adf.test(vecm$retspot, k = 5)[4]$p.value 

adf.test(vecm$silver_fut, k = 5)[3]$alternative
adf.test(vecm$gold_fut, k = 5)[3]$alternative
adf.test(vecm$plat_fut, k = 5)[3]$alternative
adf.test(vecm$spot, k = 5)[3]$alternative
adf.test(vecm$silver_ret, k = 5)[3]$alternative
adf.test(vecm$gold_ret, k = 5)[3]$alternative
adf.test(vecm$plat_ret, k = 5)[3]$alternative
adf.test(vecm$retspot, k = 5)[3]$alternative

###Silver ------------------------------------------------------
##VAR Select
dat = cbind(retf = vecm$silver_ret,rets = vecm$retspot)

##A conventional method of finding an optimal hedge ratio is using simple ordinary least
##square (OLS) estimation of the following linear regression model:
##  st ft t r = ?? + ?? r + ?? (1)
##where rst and rft are the spot and futures returns for period t. ?? provides an estimate of the
##optimal hedge ratio.

#Linear Regression Method
model1 = lm(vecm$retspot ~ vecm$silver_ret)
summary(model1)
hratio_silver_1 = model1$coefficients[2]
hratio_silver_1

map = VARselect(dat,lag.max = 10, type = "both")
map$selection[1]

##Varm
varm = VAR(dat,p = map$selection[1], type = "const") 


##granger's causality
causality(varm, cause = "rets")$Granger
#in our case, spot price weakly/no significances futures prices

##Predict
pred = predict(varm,n.ahead = 1)

##covariance matrix of the var model
cov = summary(varm)[3]

#calculating hedge ratio
covsf=cov$covres[2]
covff=cov$covres[1]
hratio_silver = covsf/covff 
hratio_silver


##Johansen-Procedure test
#install.packages("urca")
library(urca)
john_test = ca.jo(dat, type = "eigen", ecdet = "const", K = map$selection[1], spec = "longrun" )
print (john_test)
summary(john_test)


##OLS regression of VECM
jc_ols = cajorls(john_test, r = 1, reg.number = NULL)
print (jc_ols)
summary(jc_ols)
##Silver end -----------------------------------------


###Gold ------------------------------------------------------
##VAR Select
dat = cbind(retf= vecm$gold_ret,rets = vecm$retspot)

#Linear Regression Method
model2 = lm(vecm$retspot ~ vecm$gold_ret)
summary(model2)
hratio_gold_1 = model2$coefficients[2]
hratio_gold_1

map = VARselect(dat,lag.max = 10, type = "both")
map$selection[1]

##Varm
varm = VAR(dat,p = map$selection[1], type = "const") 

##granger's causality
causality(varm, cause = "rets")$Granger
#in our case, spot price weakly/no significances futures prices

##Predict
pred = predict(varm,n.ahead = 1)

##covariance matrix of the var model
cov = summary(varm)[3]

#calculating hedge ratio
covsf=cov$covres[2]
covff=cov$covres[1]
hratio_gold = covsf/covff 
hratio_gold

##Johansen-Procedure test
#install.packages("urca")
library(urca)
john_test = ca.jo(dat, type = "eigen", ecdet = "const", K = map$selection[1], spec = "longrun" )
print (john_test)
summary(john_test)

##OLS regression of VECM
jc_ols = cajorls(john_test, r = 1, reg.number = NULL)
print (jc_ols)
summary(jc_ols)
##Gold end -----------------------------------------

###platnium ------------------------------------------------------
##VAR Select
dat = cbind(retf= vecm$plat_ret,rets = vecm$retspot)

#Linear Regression Method
model3 = lm(vecm$retspot ~ vecm$plat_ret)
summary(model3)
hratio_plat_1 = model3$coefficients[2]
hratio_plat_1

map = VARselect(dat,lag.max = 10, type = "both")
map$selection[1]

##Varm
varm = VAR(dat,p = map$selection[1], type = "const") 

##granger's causality
causality(varm, cause = "rets")$Granger
#in our case, spot price weakly/no significances futures prices

##Predict
pred = predict(varm,n.ahead = 1)

##covariance matrix of the var model
cov = summary(varm)[3]

#calculating hedge ratio
covsf=cov$covres[2]
covff=cov$covres[1]
hratio_plat = covsf/covff 
hratio_plat

##Johansen-Procedure test
john_test = ca.jo(dat, type = "eigen", ecdet = "const", K = map$selection[1], spec = "longrun" )
print (john_test)
summary(john_test)

##OLS regression of VECM
jc_ols = cajorls(john_test, r = 1, reg.number = NULL)
print (jc_ols)
summary(jc_ols)
##platnium end -----------------------------------------



##Hedge ratio by different futures of precious metals for Gold BeES ETF

##Silver 
hratio_silver
hratio_silver_1
##Gold
hratio_gold
hratio_gold_1
##Platinium
hratio_plat
hratio_plat_1