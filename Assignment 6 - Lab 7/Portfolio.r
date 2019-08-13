#install.packages("quadprog")
setwd("C:/Users/Venkatesh Atul Pai/Desktop/leada/Financial Analytics Lab/Financial Analytics Lab/Lab 7/lab7")
library(quadprog)

#Portfolio of 5 companies of different sectors
Apollo = read.csv("15-03-2016-TO-14-03-2018APOLLOTYREALLN.csv")
Bata = read.csv("15-03-2016-TO-14-03-2018BATAINDIAALLN.csv")
Indigo = read.csv("15-03-2016-TO-14-03-2018INDIGOALLN.csv")
Lupin = read.csv("15-03-2016-TO-14-03-2018LUPINALLN.csv")
SBI = read.csv("15-03-2016-TO-14-03-2018SBINALLN.csv")

SBI = SBI[SBI$Series=='EQ',]
Lupin = Lupin[Lupin$Series=='EQ',]
Bata = Bata[Bata$Series=='EQ',]
Apollo = Apollo[Apollo$Series=='EQ',]
Indigo = Indigo[Indigo$Series=='EQ',]

#Calculating returns
Apollo$Apolloreturns = log(Apollo$Close.Price[1:nrow(Apollo)]/Apollo$Prev.Close[1:nrow(Apollo)])
Bata$Batareturns = log(Bata$Close.Price[1:nrow(Bata)]/Bata$Prev.Close[1:nrow(Bata)])
Indigo$Indigoreturns = log(Indigo$Close.Price[1:nrow(Indigo)]/Indigo$Prev.Close[1:nrow(Indigo)])
Lupin$Lupinreturns = log(Lupin$Close.Price[1:nrow(Lupin)]/Lupin$Prev.Close[1:nrow(Lupin)])
SBI$SBIreturns = log(SBI$Close.Price[1:nrow(SBI)]/SBI$Prev.Close[1:nrow(SBI)])

#Combine all returns in one vector
ret = cbind(Apollo$Apolloreturns, Bata$Batareturns, Indigo$Indigoreturns, Lupin$Lupinreturns, SBI$SBIreturns)

#Mean of historical stock returns
retn  = colMeans(ret)

#Variance Covariance Matrix
varcov = cov(ret)
n = ncol(ret)
Dmat = varcov
Amat = cbind(retn, rep(1,n))
dvec = rep(0,n)
bvec = c(0.0005, 1) # 0.0005 logic - take portfolio returns value as value between the 5 returns values
weights = solve.QP(Dmat, dvec, Amat, bvec, meq=2) #negative weights - shorting of stocks
wts = weights$solution
portf_ret = retn%*%wts
portf_risk = sqrt(t(wts)%*%varcov%*%wts)


# In many countries shorting not allowed. Add additional constraint wi > 0. In R, always put equality constraints at top and only greater than constraints are considered.
Amat = cbind(Amat, diag(x=1,n,n))
bvec = c(0.0005, 1, rep(0,n))
weights2 = solve.QP(Dmat, dvec, Amat, bvec, meq=2)
wts2 = weights2$solution
portf_ret2 = retn%*%wts2
portf_risk2 = sqrt(t(wts2)%*%varcov%*%wts2)


# Plotting of efficient frontier
del = (max(retn)-min(retn))/100
ret_seq = seq(min(retn), max(retn), del)
for(i in 1:101){
  bvec = c(ret_seq[i],1,rep(0,n))
  weights = solve.QP(Dmat, dvec, Amat, bvec, meq=2)
  wts = weights$solution
  portf_risk[i] = sqrt(t(wts)%*%varcov%*%wts)
}

plot(portf_risk, ret_seq, col="red")


# ASsIGNMENT  - Minimise portfolio variance and minimize transaction costs
rf = 0.06/252
max_sr = 0
max_i = 0

#Transaction costs 

# Assuming total amount invested as Rs. 10000

# SBI being in BSE Sensex 30 has lower transaction costs i.e. 0.1%
# STT = 0.1% of trade amount
# Transaction Costs - 0.00325% of trade amount for stocks other than in Sensex 30
# 18% GST on Transaction Costs
# 0.00104  = 0.001 + 1.18*0.0000325

dvec = c(-0.00104, -0.00104, -0.00104, -0.00104, -0.001) 

for(i in 1:101){
  bvec = c(ret_seq[i],1,rep(0,n))
  weights = solve.QP(Dmat, dvec, Amat, bvec, meq=2)
  wts = weights$solution
  portf_risk[i] = sqrt(t(wts)%*%varcov%*%wts)
  sr = (ret_seq[i] - rf)/portf_risk[i]
  if(sr > max_sr){
    max_sr = sr
    max_i = i
    sr_risk = portf_risk[i]
    sr_ret = ret_seq[i]
    sr_wts = wts
   } 
}

x = c(0,portf_risk[max_i], 1.2*portf_risk[max_i] )
y = c(rf,ret_seq[max_i], 1.2*ret_seq[max_i] )
plot(portf_risk, ret_seq, col="red") # As we can see no significant change in Plot as transaction costs are too small compared to variances 
lines(x,y, col="green")

# Assuming portfolio of Rs. 10,000
#For portfolio having maximum Sharpe ratio
A = 10000
Portfolio_Risk = A* sr_risk
Portfolio_Risk # in rupee terms
Portfolio_Returns = A*(sr_ret)
Portfolio_Returns # in rupee terms
Transaction_Costs = -A*(t(wts)%*%dvec) # in rupee terms
