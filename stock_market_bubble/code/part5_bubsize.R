####################
# Add a column with the size quantile for each individual stock
####################

sfbbna = sfbbna %>% group_by(date) %>% 
  mutate(portsize = assign_portfolio_size(data = cur_data(), n_portfolios = 2, exchanges = "NYSE"))

####################
# Compute the portfolio returns of the bivariate sorted portfolios
###################

pretbi = compute_portfolio_returns_bi(n_portfolios = 10,
                                         exchanges = c("NYSE"),
                                         value_weighted = TRUE,
                                         data = sfbbna %>% filter(altprc >5 & altprc <1000))


pretbi = pretbi %>% ungroup()

# Small and High bubble beta
pretbiSBUB = pretbi %>% filter(portsize == 1 & portfolio == 10) %>% dplyr::select(date, ret) %>%
  rename(SBUB = ret)

# Small and Low bubble beta
pretbiSANTI = pretbi %>% filter(portsize == 1 & portfolio == 1) %>% dplyr::select(date, ret) %>%
  rename(SANTI = ret)

# Big and High bubble beta
pretbiBBUB = pretbi %>% filter(portsize == 2 & portfolio == 10) %>% dplyr::select(date, ret) %>%
  rename(BBUB = ret)

# Big  and Low bubble beta
pretbiBANTI = pretbi %>% filter(portsize == 2 & portfolio == 1) %>% dplyr::select(date, ret) %>%
  rename(BANTI = ret)

# Bubsize portfolio
BUBSIZE = tibble("date" = pretbiSBUB$date, "BUBSIZE" = 0.5*pretbiSBUB$SBUB+0.5*pretbiBBUB$BBUB - 0.5*pretbiSANTI$SANTI - 0.5*pretbiBANTI$BANTI)


pretbibs = tibble(date = pretbiBANTI$date)
pretbibs$SBUB = pretbiSBUB$SBUB
pretbibs$BBUB = pretbiBBUB$BBUB
pretbibs$SANTI = pretbiSANTI$SANTI
pretbibs$BANTI = pretbiBANTI$BANTI
pretbibs$BUBSIZE = BUBSIZE$BUBSIZE

factors = ff5_umd %>% merge(df_bubble_fin)
pretbibs = pretbibs %>% merge(factors)
excesses = matrix(NA, nrow=2,ncol=5)

# Excess return
for (i in 2:6){
  sub = pretbibs[1:nrow(pretbibs),]
  fits = lm(sub[,i] ~ 1) # Set up the model
  results = coeftest(fits, vcov.=NeweyWest(fits, lag=12, adjust=TRUE, verbose=TRUE)) # Newey-West's HAC errors
  mean = results[,1]
  tstat = results[,3]
  excesses[1, i-1] = mean
  excesses[2, i-1] = tstat
}
print(xtable(excesses, type = "latex"))

excesses = matrix(NA, nrow=2,ncol=5)

# CAPM alpha
for (i in 2:6){
  sub = pretbibs[1:nrow(pretbibs),]
  fits = lm(sub[,i] ~ sub[,7]) # Set up the model
  results = coeftest(fits, vcov.=NeweyWest(fits, lag=12, adjust=TRUE, verbose=TRUE)) # Newey-West's HAC errors
  mean = results[,1][1]
  tstat = results[,3][1]
  excesses[1, i-1] = mean
  excesses[2, i-1] = tstat
}
print(xtable(excesses, type = "latex"))

excesses = matrix(NA, nrow=2,ncol=5)

# 3-Factor alpha 
for (i in 2:6){
  sub = pretbibs[1:nrow(pretbibs),]
  fits = lm(sub[,i] ~ sub[,7]+sub[,8]+sub[,9]) # Set up the model
  results = coeftest(fits, vcov.=NeweyWest(fits, lag=12, adjust=TRUE, verbose=TRUE)) # Newey-West's HAC errors
  mean = results[,1][1]
  tstat = results[,3][1]
  excesses[1, i-1] = mean
  excesses[2, i-1] = tstat
}
print(xtable(excesses, type = "latex"))

# 4-Factor alpha
for (i in 2:6){
  sub = pretbibs[163:nrow(pretbibs),]
  fits = lm(sub[,i] ~ sub[,7]+sub[,8]+sub[,9]+sub[,12]) # Set up the model
  results = coeftest(fits, vcov.=NeweyWest(fits, lag=12, adjust=TRUE, verbose=TRUE)) # Newey-West's HAC errors
  mean = results[,1][1]
  tstat = results[,3][1]
  excesses[1, i-1] = mean
  excesses[2, i-1] = tstat
}
print(xtable(excesses, type = "latex"))

# 5-Factor alpha
for (i in 2:6){
  sub = pretbibs[163:nrow(pretbibs),]
  fits = lm(sub[,i] ~ sub[,7]+sub[,8]+sub[,9]+ sub[,10]+sub[,11]) # Set up the model
  results = coeftest(fits, vcov.=NeweyWest(fits, lag=12, adjust=TRUE, verbose=TRUE)) # Newey-West's HAC errors
  mean = results[,1][1]
  tstat = results[,3][1]
  excesses[1, i-1] = mean
  excesses[2, i-1] = tstat
}
print(xtable(excesses, type = "latex"))
