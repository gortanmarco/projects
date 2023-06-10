##################
# Create and populate a dataframe that has as columns the excess returns of the 
# decile pfolios and the return of the long-short pfolio
##################

pdeciles = tibble(date = bub1$date)
pdeciles$p1 = bub1$p1
pdeciles$p2 = bub2$p2
pdeciles$p3 = bub3$p3
pdeciles$p4 = bub4$p4
pdeciles$p5 = bub5$p5
pdeciles$p6 = bub6$p6
pdeciles$p7 = bub7$p7
pdeciles$p8 = bub8$p8
pdeciles$p9 = bub9$p9
pdeciles$p10 = bub10$p10
pdeciles$p101 = bub101$p101

factors = ff5_umd %>% merge(df_bubble_fin)
pdeciles = pdeciles %>% merge(factors)

excesses = matrix(NA, nrow=2,ncol=11)

# Excess return 
for (i in 2:12){
  fits = lm(pdeciles[,i] ~ 1) # Set up the model
  results = coeftest(fits, vcov.=NeweyWest(fits, lag=12, adjust=TRUE, verbose=TRUE)) # Newey-West's HAC errors
  mean = results[,1]
  tstat = results[,3]
  excesses[1, i-1] = mean
  excesses[2, i-1] = tstat
}
print(xtable(excesses, type = "latex"))

excesses = matrix(NA, nrow=2,ncol=11)

# CAPM alpha
for (i in 2:12){
  fits = lm(pdeciles[,i] ~ pdeciles[,13]) # Set up the model
  results = coeftest(fits, vcov.=NeweyWest(fits, lag = 12, adjust=TRUE, verbose=TRUE)) # Newey-West's HAC errors
  mean = results[1,1]
  tstat = results[1,3]
  excesses[1, i-1] = mean
  excesses[2, i-1] = tstat
}
print(xtable(excesses, type = "latex"))

excesses = matrix(NA, nrow=2,ncol=11)

# 3-factor alpha
for (i in 2:12){
  fits = lm(pdeciles[,i] ~ pdeciles[,13] + pdeciles[,14]+ pdeciles[,15]) # Set up the model
  results = coeftest(fits, vcov.=NeweyWest(fits, lag = 12, adjust=TRUE, verbose=TRUE)) # Newey-West's HAC errors
  mean = results[1,1]
  tstat = results[1,3]
  excesses[1, i-1] = mean
  excesses[2, i-1] = tstat
}
print(xtable(excesses, type = "latex"))

excesses = matrix(NA, nrow=2,ncol=11)

# 4-factor alpha
for (i in 2:12){
  fits = lm(pdeciles[,i] ~ pdeciles[,13] + pdeciles[,14]+ pdeciles[,15] + pdeciles[,19]) # Set up the model
  results = coeftest(fits, vcov.=NeweyWest(fits, lag = 12, adjust=TRUE, verbose=TRUE)) # Newey-West's HAC errors
  mean = results[1,1]
  tstat = results[1,3]
  excesses[1, i-1] = mean
  excesses[2, i-1] = tstat
}
print(xtable(excesses, type = "latex"))

# 5-factor alpha
for (i in 2:12){
  fits = lm(pdeciles[,i] ~ pdeciles[,13] + pdeciles[,14] + pdeciles[,15] + pdeciles[,16] + pdeciles[,17]) # Set up the model
  results = coeftest(fits, vcov.=NeweyWest(fits, lag = 12, adjust=TRUE, verbose=TRUE)) # Newey-West's HAC errors
  mean = results[1,1]
  tstat = results[1,3]
  excesses[1, i-1] = mean
  excesses[2, i-1] = tstat
}
print(xtable(excesses, type = "latex"))

excesses = matrix(NA, nrow=2,ncol=11)

# Post-ranking beta
for (i in 2:12){
  fits = lm(pdeciles[,i] ~ pdeciles[,13] + pdeciles[,14]+ pdeciles[,15] + pdeciles[,16] + pdeciles[,17] + pdeciles[,20]) # Set up the model
  results = coeftest(fits, vcov.=NeweyWest(fits, lag = 12, adjust=TRUE, verbose=TRUE)) # Newey-West's HAC errors
  mean = results[7,1]
  tstat = results[7,3]
  excesses[1, i-1] = mean
  excesses[2, i-1] = tstat
}
print(xtable(excesses, type = "latex"))

