library(broom)
library(purrr)
####################
# Load the dataframe with the individual stock characteristics
# from Compustat
####################
setwd("C:\\Users\\gorta\\OneDrive\\Documenti\\uni\\Tesi\\supplementary_thesis_Gortan\\data")
comp = read.csv("compustat.csv")
comp <- comp %>%
  mutate(date = lubridate::ymd(as.character(datadate))) %>%
  mutate(date = as.yearmon(date, "%Y-%m-%d")) %>%
  mutate(date = as.Date(date)) %>%
  rename(permno = LPERMNO) %>%
  arrange(permno,date)

##### CONTROL 0 
# Bubble beta. Reconstruct the dataframe with the bubble beta of the individual stocks 
est_window = 60
beta_bub <- sfb[,c("date","permno","ret_exc", "bub_shock",
                   "mktrf", "smb", "hml", "rmw", "cma")] %>% drop_na() %>% data.table()
beta_bub[, count := (.N), by = permno]
beta_bub <- beta_bub[beta_bub$count >= est_window,] 
beta_bub <- beta_bub[order(beta_bub$permno,beta_bub$date),]
beta_bub <- beta_bub %>%
  group_by(permno) %>%
  dplyr::do(.,mutate(.,b_bub =roll_regres(ret_exc ~ bub_shock+mktrf + smb + hml + rmw + cma,data = .,
                                          width= est_window,
                                          do_downdates = TRUE)$coef[,2]))
beta_bub <- beta_bub %>% data.table()
beta_bub <- na.omit(beta_bub[,list(date,permno,b_bub, ret_exc, bub_shock)])

##### CONTROL 1 
# Market beta. Computed in a rolling fashion with the most recent 60 months
est_window = 60
beta <- sfb[,c("date","permno","ret_exc", "bub_shock",
               "mktrf", "smb", "hml", "rmw", "cma")]%>% drop_na() %>% data.table()
beta[, count := (.N), by = permno]
beta <- beta[beta$count >= est_window,] 
beta <- beta[order(beta$permno,beta$date),]
beta <- beta %>%
  group_by(permno) %>%
  dplyr::do(.,mutate(.,beta =roll_regres(ret_exc ~ mktrf,data = ., 
                                         width= est_window,
                                         do_downdates = TRUE)$coef[,2]))
beta <- beta %>% data.table()
beta <- na.omit(beta[,list(date,permno,beta)])

##### CONTROL 2 
# Market capitalization (log levels)
mkcap<- sfb[,c("date","permno","mktcap")]%>% drop_na() %>% tibble() %>%
  mutate(mktcap = log(mktcap))

##### CONTROL 3
# BM Ratio
bmdf = comp %>% dplyr::select(permno, date, 
                              txditcq, # deferred taxes and investment tax credit
                              seqq,  # stockholders equity
                              pstkrq) # redemption value of preferred stocks
bmdf = bmdf %>%
  mutate(numerator = txditcq + seqq - pstkrq,
         year = year(date))
bmdf = bmdf %>% merge(mkcap, by = c("date","permno"))
bmdf2 = bmdf %>% replace(is.na(.), 0) %>% mutate(numerator = txditcq + seqq - pstkrq)
bmdf3 = bmdf2 %>% 
  group_by(permno, year) %>% 
  arrange(permno, date) %>%  
  slice(n()) # take the last value of each year

bmdf4 = bmdf3 %>% ungroup() %>% mutate(yearref = year + 1, monthref = '06') # the ratio is from 
                                                                          # the june of the following year

# Combine year and month columns into a new date column
bmdf4 <- unite(bmdf4, dateref, yearref, monthref, sep = "-")
# Convert the combined date column from character to Date format
bmdf4$dateref <- lubridate::ymd(parse_date_time(bmdf4$dateref, orders = "%Y-%m"))

bmdf4 = bmdf4 %>% mutate(dateref = lubridate::ymd(dateref)) %>%
  mutate(dateref = as.yearmon(dateref)) %>%
  mutate(dateref = as.Date(dateref))
bmdf5 = bmdf4 %>% mutate(bm = log(numerator/mktcap)) %>% na.omit()
bmfinal = bmdf5 %>% dplyr::select(permno, dateref, bm)
bmfinal = bmfinal %>% rename(date = dateref)


##### CONTROL 4
#  Assets annual growth
anngro = comp %>% dplyr::select(permno, date, atq) 
anngro2 = anngro %>% mutate(year = year(date))
anngro3 = anngro2 %>%  group_by(permno, year) %>% 
  arrange(permno, date) %>%  
  slice(n()) %>% 
  ungroup()

anngro4 = anngro3 %>%
  group_by(permno) %>%
  replace(is.na(.), 0) %>%
  mutate(atql = lag(atq, n=1, order_by=permno)) %>%
  ungroup()
anngro4 = anngro4 %>% mutate(anngrow = (atq-atql)/atql)
assetfin = anngro4 %>% dplyr::select(permno,date, anngrow)


##### CONTROL 5
#  Profitability
profit = comp %>% dplyr::select(permno, date, ibq, atq) 
profit2 = profit %>% group_by(permno) %>%
  replace(is.na(.), 0) %>%
  mutate(atql = lag(atq, n=1, order_by=permno)) %>%
  ungroup() %>% na.omit() %>% filter(atql != 0)
profit2$prof = profit2$ibq/profit2$atql
proffin = profit2 %>% dplyr::select(date, permno, prof)

##### CONTROL 6 
# Momentum
moment = sfb %>% tibble() %>% dplyr::select(date, permno, ret_exc) %>% na.omit()
moment2 = moment %>% mutate(ret_log = log(ret_exc/100+1))
moment2$ret_log = ifelse(is.finite(moment2$ret_log), moment2$ret_log, NA)
moment3 = moment2 %>% group_by(permno) %>%
  replace(is.na(.), 0) %>%
  mutate(ret_logl = lag(ret_log, n=1, order_by=permno)) %>%
  group_by(permno) %>%
  arrange(permno, date) %>%
  filter(n()>11) %>%
  mutate(moment = c(c(rep(NA,10), rollsum(ret_logl,11)))) %>%
  ungroup() %>% na.omit() 
moment3$mom = moment3$moment*100
momnet4 = moment3 %>% mutate(momfin = (exp(moment) -1)*100)
momfin = momnet4 %>% dplyr::select(date, permno, momfin)

##### CONTROL 7
## Short-term reversal
rev = sfb %>% tibble() %>% dplyr::select(date, permno, ret_exc) %>% na.omit()
rev = rev %>% group_by(permno) %>%
  mutate(rever = lag(ret_exc, n=1, order_by=permno)) %>%
  ungroup()
rev = rev %>% dplyr::select(date, permno, rever)

##### CONTROL 8
## Conditional skewness
cosk <- sfb[,c("date","permno","ret_exc", "mktrf")]%>% drop_na() %>% data.table()
cosk[, count := (.N), by = permno]
cosk <- cosk[cosk$count >= est_window,] 
cosk <- cosk[order(cosk$permno,cosk$date),]
# Save the intercept
cosk <- cosk %>%
  group_by(permno) %>%
  dplyr::do(.,mutate(.,interc =roll_regres(ret_exc ~ mktrf,data = .,min_obs=est_window, 
                                           width= est_window,
                                           do_downdates = TRUE)$coef[,1]))
# Save the beta coefficient
cosk <- cosk %>%
  group_by(permno) %>%
  dplyr::do(.,mutate(.,beta =roll_regres(ret_exc ~ mktrf,data = .,min_obs=est_window, 
                                         width= est_window,
                                         do_downdates = TRUE)$coef[,2]))
cosk2 = cosk %>% 
  group_by(permno) %>% 
  fill(interc, beta, .direction = "up")
cosk2 <- cosk2 %>% mutate(residual= ret_exc - (interc+ beta*mktrf)) %>%
  mutate(epsR2 = residual*mktrf^2) %>%
  mutate(esp2 = residual^2) %>%
  mutate(r2 = mktrf ^2)
cosk3 = cosk2 %>% group_by(permno) %>%
  arrange(permno, date) %>%
  filter(n()>59) %>%
  mutate(numerator = c(c(rep(NA,59), rollsum(epsR2/60,60))),
         den1 = sqrt(c(c(rep(NA,59), rollsum(esp2/60, 60)))) ,
         den2 = c(c(rep(NA,59), rollsum(r2/60,60))))
cosk4 = cosk3 %>% mutate(coskew = numerator/(den1*den2))
coskfin = cosk4 %>% dplyr::select(date, permno, coskew) %>% na.omit()

###############################################


dfcontrol = beta_bub %>%
  merge(beta, all.x = TRUE) %>%
  merge(mkcap, all.x = TRUE) %>%
  merge(bmfinal, all.x = TRUE) %>%
  merge(assetfin, all.x = TRUE) %>%
  merge(proffin, all.x = TRUE) %>%
  merge(momfin, all.x = TRUE) %>%
  merge(rev, all.x = TRUE)  %>%
  merge(coskfin, all.x =TRUE)

dfcontrol$bm = ifelse(is.finite(dfcontrol$bm), dfcontrol$bm, NA)
dfcontrol2 = dfcontrol %>% 
  group_by(permno) %>% 
  arrange(permno, date) %>%
  fill(bm, anngrow, prof, .direction = "down")
dfcontrol3 = dfcontrol2 %>% na.omit() %>% ungroup()
dfcontrol4 = dfcontrol3 %>% filter(anngrow != -1) %>% # checking only numeric columns:
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))

datecontro = dfcontrol4$date
b_bub = dfcontrol4$b_bub
dfcontrol5 = dfcontrol4 %>% dplyr::select(-c(date, permno, b_bub)) %>% 
  sapply(function(x) (x-mean(x))/sd(x)) %>% # stabdardize the variables
  as.data.frame() 
dfcontrol5$date = datecontro
dfcontrol5$b_bub = b_bub

stocks_cross = list()
variables = c("beta", "mktcap", "bm", "anngrow", "prof", "momfin", "rever", "coskew")


########
# Perform the two-step procedure to see which stock characteristics are linked
# to the bubble beta 
#########

for (i in 1:8){
  stockchar <- dfcontrol5[, c(11, 12, i+2)] %>% data.frame()
  colnames(stockchar) <- c("date", "b_bub", "variab")
  riskpremia = stockchar %>% 
    group_by(date) |>
    do(model =  tidy(lm(b_bub ~ variab,  data = .)))
  stocks_cross[[i]] = riskpremia
}

resultfmcross = list()
for (i in 1:8){
  resultfmcross[[i]] = stocks_cross[[i]] %>% unnest() %>%
    filter(date >= as.Date("1972-12-01")) |>
    dplyr::select(date, factor = term, estimate) |>
    nest(data = c(date, estimate)) |>
    mutate(
      model = map(data, ~ lm(estimate ~ 1, .)),
      mean = map(model, tidy)
    ) |>
    mutate(newey_west_se = map_dbl(model, ~ sqrt(NeweyWest(.,lag = 12, prewhite = FALSE)))) |>
    unnest(mean) |>
    mutate(t_statistic_newey_west = estimate / newey_west_se) |>
    dplyr::select(factor,
           risk_premium = estimate,
           t_statistic_newey_west
    )}

#################
# Using all the stock characteristics contemporaneously
################
fmbtot = dfcontrol5 %>% 
  group_by(date) |>
  do(model =  tidy(lm(b_bub ~ beta+ mktcap + bm + anngrow + prof+ momfin + rever + coskew,  data = .)))
fmbtot2 = fmbtot %>% unnest() %>%
  filter(date >= as.Date("1972-12-01")) |>
  dplyr::select(date, factor = term, estimate) |>
  nest(data = c(date, estimate)) |>
  mutate(
    model = map(data, ~ lm(estimate ~ 1, .)),
    mean = map(model, tidy)
  ) |>
  mutate(newey_west_se = map_dbl(model, ~ sqrt(NeweyWest(.,lag = 12, prewhite = FALSE)))) |>
  unnest(mean) |>
  mutate(t_statistic_newey_west = estimate / newey_west_se) |>
  dplyr::select(factor,
         risk_premium = estimate,
         t_statistic_newey_west
  )

