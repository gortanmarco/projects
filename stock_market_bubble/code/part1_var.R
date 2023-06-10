#######################
### Import the packages 
#######################

library(vars) # Vector Autoregression Functions 
library(calibrate) # Useful for plotting 
library(pracma) # Useful for time-series data manipulation
library(stats) # To perform statistical tests
library(olsrr) # Useful for OLS diagnostics
library(xtable) # To create output for Latex
library(tseries) # For the KPSS test
library(RcppRoll) # To compute rolling sum

##########################à
# Set the working directory in the folder with the codes
###########################
setwd("C:\\Users\\gorta\\OneDrive\\Documenti\\uni\\Tesi\\supplementary_thesis_Gortan\\code")

### Import the functions from the file funzioni.R
source("functions.R")

####################
# Create objects for the level of the market index and of the dividends (in log) 
# and also the object for the term spread
##################
mktlevel = df_part0$mkt_value_def_log
divlevel = df_part0$div_def_log
term = df_part0$term_spread

####################
# Create objects for the first differences of the log market index and the log dividends 
##################
mktdiff = df_part0$mkt_diff
divdiff = df_part0$div_diff

####################
# Compute the total return of the market index (including dividends)
##################
rtot = c(NA, log((lagna(df_part0$mkt_value_def,1) + df_part0$div_def %>% na.omit())
                 /leadna(df_part0$mkt_value_def,1)))

####################
# Average of the log dividends over the trailing twelve months
# and compute the monthly dividend-to-price ratio
##################
divlevelsmooth = c(rep(NA,11),rollmean(divlevel,12))
dpm = divlevelsmooth - mktlevel 

###################
# Estimate the (monthly) parameter rho
#################
mktbar = mean(mktlevel, na.rm = TRUE) # mean of the log market index
divbar = mean(divlevelsmooth, na.rm = TRUE) # mean of the log dividends
rhom = exp(mktbar)/(exp(mktbar)+exp(divbar)) # compute the log as shown in Shi (2017)

#######################
# Estimate the parameter kappa
#####################
kappa = -log(rhom) + (1-rhom)*(mktbar - divbar)

########################
# Compute the monthly real risk free interest rate by subtracting
# the monthly y-o-y inflation from the treasury bill rate.
# Then, the parameter phi is computed as the difference between
# the real total return to the stock market and the monthly real risk free
# interest rate
#######################
rfrm = (df_part0$ytm_90+1)^(1/12) -1 - c(rep(NA,12), diff(log(df_part0$cpi),12)/12)
phi = mean((rtot-rfrm), na.rm = TRUE)

###################
# With kappa, phi, and rho we can compute the constant leg
# of the fundamental component of the log price-to-dividend ratio  
####################
fundamental_constant = (kappa - phi)/(1-rhom)

###################
# Setup of the variables to include in the VAR
# only the dividend growth rate needs the seasonality adjustment 
###################

# Real dividend growth rate
numerator = c(rep(NA,11), roll_sum(df_part0$div_def,12))
denominator = c(rep(NA,11), roll_sum(lag(df_part0$div_def),12))
div_growth = log(numerator/denominator)


##########################
# Stationarity check
# We use different unit root tests to investigate the stationarity
# of our variables
#############################


station = tibble("Variable"  = c(),"ADF" = c(), "KPSS" = c(), "Phillips-Perron" = c())
variables = c("div_growth", "rfrm", "dpm", "term")
for (i in 1:4){
  pp = PP.test(get(variables[i])[!is.na(get(variables[i]))], lshort = FALSE)$statistic
  kp = kpss.test(get(variables[i]), null = "T", lshort = FALSE)$statistic
  adf = adf.test(get(variables[i]) %>% na.omit())$statistic
  dfstat = tibble("Variable"  = variables[i],"ADF" = adf, "KPSS" = kp, "Phillips-Perron" = pp)
  station = rbind(station, dfstat)
}

xtable(station, type = "latex")


################
# Number of lags check
# We use the partial autocorrelation function to study the persistence
# of the variables
#################

variables = c("div_growth", "rfrm", "dpm", "term")
mains = c("Real dividend growth rate", "Real risk free interest rate", "Log dividend-price ratio", "Term spread")

par(mfrow=c(2,2), mai = c(0.7, 0.7, 0.7, 0.7), 
    oma = c(0.7, 0.7, 0.7, 0.7))
for (i in 1:4){
  pacf(get(variables[i]) %>% na.omit(), main = mains[i], font.main = 1)
}


#############################
# We now want to pick the coefficients that are significant
# at the 5% level of significance when running the VAR with all 
# the historical data we have
#############################
start = 14 # First non-NA elements in the dividend growth rate
len = length(mktlevel) # Length of the variables we use
nlags = 24 # As from the partial autocorellation functions

vardf = cbind(
  div_growth[start:len], 
  rfrm[start:len],
  dpm[start:len],
  term[start:len]
  )

vardf = as.data.frame(vardf)
modelvar <- VAR(vardf, p = nlags, type = "const")
modelvar = restrict(modelvar, method = "ser", thresh = 2)
restrvar = modelvar$restrictions


#########################
# We set up the initial conditions for computing the rolling vector
# autoregressions
#############################
winvar = 300 +12 # First window length
fund2 = c() # Second addend of the fundamental component
            # (which depends on the constant term of the VAR)
fund3 = c() # Third addend of the fundamental component
            # (which depends on the values of the variables
            # at time t)
nlags= 24
nvars = 4
hg = matrix(c(1,rep(0,nvars-1), rep(0, nvars*(nlags-1))),ncol=1) #vector of zeros with
                              #value 1 for the position of the dividend growth rate 
hr = matrix(c(0,1,rep(0,nvars-2), rep(0, nvars*(nlags-1))), ncol=1)#vector of zeros with
                              #value 1 for the position of the risk free rate
h = hg - hr

for (t in winvar:len){ 
  gvar = div_growth[start:t]
  rvar = rfrm[start:t]
  dpvar = dpm[start:t]
  termvar = term[start:t]
  
  vardf = cbind(gvar, rvar, dpvar, termvar)
  colnames(vardf)=cbind("Dividend growth", "Risk-free", "D/P ratio", "Term")
  vardf = as.data.frame(vardf)
  modelvar <- VAR(vardf, p = nlags, type = "const")
  modelvar <- restrict(modelvar, method = "man", resmat = restrvar)
  
  coeffs = Bcoef(modelvar)
  
  A0 = matrix(coeffs[,(nvars*nlags+1)], nrow = nvars) %>%
    rbind(zeros((nlags-1)*nvars,1))
  A1 = zeros(nvars*nlags)
  A1[1:nvars,1:(nvars*nlags)] = coeffs[, 1:(nvars*nlags)]
  A1[(nvars+1):(nvars*nlags), 1:((nlags-1)*nvars)] = diag((nlags-1)*nvars)
  A1[is.na(A1)] = 0
  
  prod1 =(t(h))*((1-rhom)^(-1))
  prod2 <- solve(diag(nvars*nlags)- A1*rhom)%*%A0
            
  fund2 = append(fund2,prod1%*%prod2) 
  
  Zt = vardf[order(nrow(vardf):1),] %>%
    data.matrix() %>%
    t() %>%
    matrix(ncol = 1)
  Zt = matrix(Zt[1:(nvars*(nlags)),1], ncol = 1)
  inverseprob <-  solve(diag(nvars*nlags)- A1*rhom)
  
  fund3 = append(fund3, (t(h))%*%A1%*%inverseprob%*%Zt)
  print(t)
}

##################
# Addends of the fundamental component
##################
fund1 = rep(((kappa-phi)/(1-rhom)), length(fund2))
fund = fund1 + fund2 + fund3


###################
# Retrieving the non-fundamental component by subtracting the 
# estimated fundamental component from the monthly price-to-dividend
# ratio, where the denominator is the mean of the dividends in the previous
# twelve months
##################
p = mktlevel[winvar:len]
d = divlevel[winvar:len] 
dav = divlevel[(winvar-11):length(divlevel)] 
dav = mav(dav, 12)
dav = dav[!is.na(dav)]
pdav = p - dav
nonfund = pdav - fund

#################
# Date time object with the dates of the 
# period analyzed
##################
datesmon = df_part0$date[winvar:len]


####################
# Plotting the components of the price-to-dividend ratio
####################
dev.off()
{plot(datesmon, fund, type = "l", lty=1, ylim = c(-1,8),
      ylab = "Log price dividend ratio",
      xlab = "Year")
  lines(datesmon, pdav, lty=2)
  lines(datesmon, nonfund, lty = 3)
  legend("bottomright", legend=c("Fundamental", "Observed", "Non - fundamental"),
         lty=1:3, cex=0.8)}

