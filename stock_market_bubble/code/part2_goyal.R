##########################à
# Set the working directory in the folder with the datasets
###########################
setwd("C:\\Users\\gorta\\OneDrive\\Documenti\\uni\\Tesi\\supplementary_thesis_Gortan\\data")

##########################
# Load the dataframe downloaded from Amit Goyal's website
#########################
goyal = as_tibble(read.csv(file ="goyal.csv")) 

##########################
# Adjust the column values of the Index (S&P 500) to make it 
# numeric
#########################
goyal$Index = as.numeric(
  gsub(",", "",goyal$Index, fixed = TRUE))

##########################
# Compute the variables we used for the OOS forecast of the equity premium
# as shown in Rapach et al (2008)
#########################
dp = log(goyal$D12) - log(goyal$Index) # dividend price ratio
dy = log(goyal$D12) - log(lag(goyal$Index)) # dividend yield
ep = log(goyal$E12) - log(goyal$Index) # earnings price ratio
de = log(goyal$D12) - log(goyal$E12) # dividend payout ratio
svar = goyal$svar # stock variance
bm = goyal$b.m # book-to-market ratio
ntis = goyal$ntis # net equity expansion
tbl = goyal$tbl # treasury bill rate
lty = goyal$lty # long-term government bond yield
ltr = goyal$ltr # long-term government bond return
tms = lty - tbl # term spread
dfy = goyal$BAA - goyal$AAA # default yield spread
dfr = goyal$corpr - ltr  # default return spread
infl = goyal$infl # monthly inflation

#####################
# Remove the rows with na values
##################
goyaldf = tibble("date" = goyal$yyyymm,
                 dp, dy, ep, de, svar, bm, ntis, tbl, lty,
                 ltr, tms, dfy, dfr, infl,
                 "retexc" = goyal$CRSP_SPvwx - goyal$Rfree) %>% na.omit()

lengoyal = nrow(goyaldf) # object to save the number of rows of the dataframe
                         # with the predictors

windgoyal = 120 # minimum number of observations to use to compute the 
                # OLS regression to predict the equity premium

###################
# Create a dataframe with two columns: the date and the excess return on the date.
# It will be populated with the estimate values of the equity premium (r hat) from 
# the economic variables
##################

rhats = tibble("date" = goyaldf$date[(windgoyal+1):lengoyal], 
               "retexc" = goyaldf$retexc[(windgoyal+1):lengoyal])
               #Note: the '+1' element comes by the fact that we performing OOS forecasts

###################
# Create a dataframe with NA values. At each iteration, the (m,n) entry will be
# populated with the corresponding estimate of the equity premium. 'm' is the 
# date and 'n' is the economic variable
##################

rfits = matrix(NA, nrow = nrow(rhats), ncol = 14)

###################
# Create a dataframe with NA values. At each iteration, the (m,n) entry will be
# populated with the corresponding estimate of the out of the sample R squared
##################
ros = matrix(NA, nrow = nrow(rhats), ncol = 14)

cols = names(goyaldf)

#################
# For loop on the economic variables. The idea is to take one variable at time,
# estimating the expanding window regressions of the excess return (one time ahead)
# on the values of the economic variable
#############
for (i in 2:15){
  for (l in (windgoyal+1):lengoyal){
      sub = data.frame( # sub is a datframe with two columns: the first one is the lagged
                        # value of the economic variables, the second one is the excess return
        "predictor" = lag(goyaldf[1:l,c(i)])%>% na.omit(), # note the lag function, 
                                              # as the excess return at time t+1
                                              # is regressed on the values of the economic
                                              # variable at time t
        "retexc" = goyaldf[2:l,c(16)]) # one-step ahead
      names(sub)[1] <- cols[i]
      names(sub)[2] <- "retexc"
      reg = lm(retexc ~ get(cols[i]), data = sub)
      ret = sub[,2]
      fit = predict(reg)[l-1] # 
      rfits[(l-windgoyal), (i-1)] = fit
     if (l >= windgoyal+60){ # The Ros is computed only from when we have at least 60 
                             # forecasts of the equity premium
       histav = (cumsum(ret)/length(ret))[(windgoyal):(length(ret))] # historical average excess return
       histret = ret[(windgoyal):(length(ret))] # historical excess return
       histfits = rfits[(1:(l-windgoyal)), (i-1)] # historical predicted values
       num = sum((histfits- histret)^2) # numerator of the second component of the OOS R2
       den = sum((histav - histret)^2) # denominator of the second component of the OOS R2
       ros[(l-windgoyal), (i-1)] =  1 - num/den
     }
    }
  print(i)
}

rfits2 = rowMeans(rfits) # taking the averages of the predicted values of the excess return from
                          # each predictor
rhats$fit = rfits2 # add a column with the averages

rosm = c() # initiating the OOS R2 of the average values 

####################
# Computing the OOS R2 of the average of the forecasts.
# The procedure is the same as that for the OOS R2 of the
# individual predictors
#################################
for (i in windgoyal:nrow(rhats)){
  histfits = rfits2[1:(i)]
  ret = goyaldf[1:(i+windgoyal),c(16)]
  ret = ret$retexc
  histret = ret[(windgoyal+1):length(ret)]
  histav = (cumsum(ret)/length(ret))[(windgoyal+1):length(ret)]
  num = sum((histfits - histret )^2)
  den = sum((histav - histret)^2)
  rost=  1- num/den
  rosm = append(rosm, rost)
}
  
plot(rosm)



########################
# I have a dataframe called 'rhats' which has three columns:
# (i) date column (ii) excess return (iii) average of the forecasts [fit]
# The noise at each date is computed by taking the last residual from the expanding
# regression of the excess return on the fitted value, with intercept
############################
noise = c()
for (i in 60:nrow(rhats)){
  sub = rhats[c(1:i),c(2,3)]
  a0 = lm(sub$retexc ~ sub$fit)
  a = a0$residuals[i]
  noise = append(noise, a)
}

dfnoise = tibble("date" = rhats$date[60:nrow(rhats)], "noise" = noise)
rhats2 = rhats %>% merge(dfnoise)
rhats2 = rhats2 %>% na.omit()
rhats2$date = rhats2$date %>% as.character() %>% paste("01", sep="") %>% as.Date("%Y%m%d") %>% as.yearmon("%Y-%m-%d")

########################
# Create an ad-hoc dataframe with just the date and the values
# of the non-fundamental component
#########################
nfunddf = tibble("date" = datesmon, "nonfund" =  nonfund)

finalGW = merge(rhats2, nfunddf) %>% tibble()

finalGW$nfunddiff = c(NA, diff(finalGW$nonfund)) # first differences of the nonfund component
finalGW$noisel = lag(finalGW$noise) # add a column with the lag of the noise

###########################
# The following is to initiate some objects to 
# investigate the regression we estimate when retrieving the 
# bubble shock
##############################
b1s = c() # estimate of the coefficient on noise t
b2s = c() # estimate of the coefficient on noise t-1
s1s = c() # std error of b1s
s2s = c() # std error of b2s

bubshock = c()
for(i in 60:nrow(finalGW)){
  sub = finalGW[1:i,] %>% na.omit()
  regshock = lm(sub$nfunddiff ~ sub$noise + sub$noisel)
  b1 = regshock$coefficients[2] 
  b2 = regshock$coefficients[3] 
  s1 = sqrt(diag(NeweyWest(regshock, lag = 12)))[2] # the first element would the intercept
  s2 = sqrt(diag(NeweyWest(regshock, lag = 12)))[3]
  bshock = regshock$residuals[i-1] # i-1 because the object is of length i-1
  bubshock = append(bubshock,bshock)
  b1s = append(b1s, b1)
  b2s = append(b2s, b2)
  s1s = append(s1s, s1)
  s2s = append(s2s, s2)
}

raptot = regshock # the regression with all the elements is given by the last iteration 

coeftest(raptot, vcov.=NeweyWest(raptot, lag=12, prewhite=FALSE, adjust=TRUE, verbose=TRUE))
# FIRST HALF
sub = finalGW[1:((nrow(finalGW)+1)/2),] %>% na.omit()
regshock1 = lm(sub$nfunddiff ~ sub$noise + sub$noisel)
raphalf1 = regshock1
coeftest(raphalf1, vcov.=NeweyWest(raphalf1, lag=12, prewhite=FALSE, adjust=TRUE, verbose=TRUE))
# SECOND HALF
sub = finalGW[((nrow(finalGW)+2)/2):nrow(finalGW),] %>% na.omit()
regshock2 = lm(sub$nfunddiff ~ sub$noise + sub$noisel)
raphalf2 = regshock2
coeftest(raphalf2, vcov.=NeweyWest(raphalf2, lag=12, prewhite=FALSE, adjust=TRUE, verbose=TRUE))

#####################
# Finally, create the dataframe with the bubble shock
# to use in the asset pricing analysis
#####################

datebub = finalGW$date[60:842]
bubt = tibble("date" = datebub, "bub_shock" = bubshock*100)
df_bubble_fin = bubt %>%
  mutate(date = as.Date(date))
