#setwd("C:/Users/User_2/Desktop")
#setwd("C:/Users/user.user-PC/Downloads")
setwd("C:/Users/jasper/Desktop/FA/Lab2 14MT3FP03")


# loading the data
bonds = read.csv("cbm_trd20171215.csv")
bond_info = read.csv("cbm_security_master.csv", skip = 5)
zcyc = read.csv("zcyc15dec.csv")

# Apurva's Birthday
bday = as.Date("2017-12-15")

# merging the data by taking an intersection of the two datasaets
bonds$ISIN = as.character(bonds$ISIN)
bond_info$ISIN = as.character(bond_info$ISIN)
bonds$ISIN = trimws(bonds$ISIN, which = "right")
master= merge (x = bonds , y= bond_info , by.x = "ISIN",by.y = "ISIN" )

# Out of the 113 bonds traded on 15th December 2017, 
# 51 bonds have expired on or before 14th January 2018

## levels(master$Coupon.Frequency)
#[1] ""            "0"           "4"           "Half Yearly" "Monthly"     "Quarterly"  
#[7] "Yearly"

# creating the numeric coupon frequency column to be used further
temp = data.frame(freq=c(0,1,2,4,12),x=c("0", "Yearly","Half Yearly", "Quarterly", "Monthly"   ))
master$cpn_freq= temp$freq[match(master$Coupon.Frequency,temp$x)]
master = master[-c(54, 58, 59),]

# standardizing the date formats
master$Next.Coupon.Date = as.Date(master$Next.Coupon.Date, format = "%d-%b-%Y")
master$Maturity.Date = as.Date(master$Maturity.Date, format = "%d-%b-%Y")

# calculating the no. of days for which the accrued interest is to be calculated
master$acr_days[master$cpn_freq == 1] = abs(365 - (master$Next.Coupon.Date[master$cpn_freq == 1] - bday))/365
master$acr_days[master$cpn_freq == 2] = abs(182 - (master$Next.Coupon.Date[master$cpn_freq == 2] - bday))/182
master$acr_days[master$cpn_freq == 12] = abs(31 - (master$Next.Coupon.Date[master$cpn_freq == 12] - bday))/31

# calculating the accrued interest
master$cpn_rate = as.numeric(unlist(strsplit(as.character(master$Issue.Name), "%")))
master$acr_int = master$cpn_rate * master$acr_days

# calculating the dirty price
master$dirty_price = master$Last.Trade.Price..in.Rs.. + master$acr_int


# calculating YTM
# defining various bond parameters 
c = master$cpn_rate        # the coupon rate
f = master$cpn_freq        # the coupon frequency

# the no. of days till the next coupon(as on 15th Dec 2017) as a fraction of coupon frequency
w = abs(master$Next.Coupon.Date - bday)/(365/f)
master$Next.Coupon.Date[w>1] = master$Next.Coupon.Date[w>1] - (365/f[w>1])
w = abs(master$Next.Coupon.Date - bday)/(365/f)
w = as.numeric(w)

# remaining no. of coupons (as on 15th Dec 2017)
t = as.numeric(round((master$Maturity.Date - master$Next.Coupon.Date)/(365/f) + 1))

fv = master$Face.Value      # face value of the bond
price = master$dirty_price  # dirty price of the bond

tol = 0.001                 # the tolerance value for breaking out of the loop

# initializing the YTM vector
y0 = rep(10, nrow(master))
ytm = rep(0, nrow(master))

# using Newton - Raphson method for calculating YTM
# y(k+1) = y(k) - f(y)/f'(y)
# f(y) = Dirty Price - [(c/f)/(1 + y/f)^(w) + (c/f)/(1 + y/f)^(w + 1) + .... + (c/f)/(1 + y/f)^(w + T - 1) + (fv)/(1 + y/f)^(w + T - 1)]
# f'(y) = [(c/f^2)*(w)/(1 + y/f)^(w + 1) + (c/f^2)*(w + 1)/(1 + y/f)^(w + 2) + ... + (c/f^2)*(w + T - 1)/(1 + y/f)^(w + T) 
#          + (fv/f)*(w + T - 1)/(1 + y/f)^(w + T)]
for (n in c(1:nrow(master))){
  error = 1000
  while(error > tol){
    P = 0
    df = 0
    for (i in c(1:t[n])){
      P = P + (c[n]/f[n])/((1 + y0[n]/(100*f[n]))^(w[n] + i - 1))
    }
    P = P + fv[n]/((1 + y0[n]/(100*f[n]))^(w[n] + t[n] - 1))
    funct = price[n] - P
    for (j in c(1:t[n])){
      df = df + (c[n]/(100*(f[n])^2))*(w[n]+j-1)/((1+y0[n]/(100*f[n]))^(w[n]+j))
    }
    df = df + (fv[n]/(f[n]*100))*(w[n]+t[n]-1)/((1+y0[n]/(100*f[n]))^(w[n]+t[n]))
    
    ytm[n] = y0[n] - funct/df
    error = abs(ytm[n] - y0[n])
    y0[n] = ytm[n]
  }
}

# Writing data to an excel file for curves
zcyc$Time_To_Maturity = 0
zcyc$Calculated_zcyc = 0
zcyc$Time_To_Maturity[1:length(ytm)] = (master$Maturity.Date - bday)/365
zcyc$Calculated_zcyc[1:length(ytm)] = ytm
write.csv(zcyc, "curve.csv")
