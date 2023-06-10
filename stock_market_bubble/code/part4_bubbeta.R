library(rollRegres) # Package to rolling regressions
library(tidyr)

###############
# Final dataframe with also the bubble shock
###############
stocks_final_bub <- stocks_final %>% merge(df_bubble_fin, by = c("date"), all.x = TRUE)
stocks_final_bub <- stocks_final_bub %>%
  mutate(ret_exc = ret - rf)

# Dataframe with the factors and bubble shock
chars = stocks_final_bub %>%
  dplyr::select(mktrf, smb, hml, rmw, cma, umd, bub_shock, rf, date) %>%
  distinct() %>%
  na.omit() %>%
  tibble() 

# Correlation matrix
cor(chars %>% dplyr::select(-date))

#################################
# Compute the bubble beta
# Note: the followning lines of code are based on 
# https://journal.r-project.org/archive/2021/RJ-2021-047/RJ-2021-047.pdf
#################################
est_window = 60

sfb = data.table(stocks_final_bub) # copy of the dataframe, with a shorter name

#############
# We initiate a dataframe that will be populated with the bubble betas of the stocks
#############
beta_bub <- sfb[,c("date","permno","ret_exc", "bub_shock",
                   "mktrf", "smb", "hml", "rmw", "cma")] %>%  
  drop_na() %>% 
  data.table()
beta_bub[, count := (.N), by = permno]
beta_bub <- beta_bub[beta_bub$count >= est_window,] # keep stocks with at least 60 observations
beta_bub <- beta_bub[order(beta_bub$permno,beta_bub$date),] # order by permno and date

#################
# Next, we perform the rolling regressions controlling for the 
# fama-french five factors 
################
beta_bub <- beta_bub %>%
  group_by(permno) %>%
  dplyr::do(.,mutate(.,b_bub =roll_regres(ret_exc ~ bub_shock + mktrf + smb + hml + rmw + cma,
                                          data = .,
                                          width= est_window,
                                          do_downdates = TRUE # rolling window regression
                                          )$coef[,2])) # take the beta coefficient of the bubble shock
beta_bub <- beta_bub %>%
  mutate(date_reference = date) %>%  # we want that the bubble beta that we compute
                                     # with the returns at month t
                                     # is the one used for creating the portfolios 
                                     # at the beginning of the following month 
  dplyr::select(-date) %>%
  data.table()

beta_bub <- na.omit(beta_bub[,list(date_reference,permno,b_bub)]) # subset of variables

###################
# We can now add the information of the bubble beta to the dataframe with 
# the individual stock information
###################

sfbb <- merge(sfb, beta_bub, by = c("permno","date_reference"), all.x = TRUE) 
sfbb <- sfbb[order(sfbb$permno,sfbb$date),]


################################################
# Construction of portfolios
# Note: the functions for the constructions of the portfolios
# are in the file funzioni.R
###################################################

sfbbna = sfbb %>% dplyr::select(permno, date, date_reference, 
                                mktcap_reference, exchange, ret_exc, b_bub, industry, altprc) %>% 
  drop_na() %>% #remove nas 
  tibble()

####################
# Create dataframe with columns:
#  - date
#  - portfolio: it is the identifier (1 = low bubble beta pfolio, 10 = high bubble beta pfolio)
#  - ret: excess return of the portfolio
pret = compute_portfolio_returns(data = sfbbna %>% 
                                 filter(altprc>5 & altprc <1000), #filter for stocks with price between 5 and 1000
                                 exchanges = "NYSE", # quantiles based on NYSE
                                 n_portfolios = 10, # create 10 portfolios
                                 value_weighted = TRUE) # value weighted portfolio returns

bub1 = pret %>% filter(portfolio == 1) %>% rename("p1" = ret)
bub2 = pret %>% filter(portfolio == 2) %>% rename("p2" = ret)
bub3 = pret %>% filter(portfolio == 3) %>% rename("p3" = ret)
bub4 = pret %>% filter(portfolio == 4) %>% rename("p4" = ret)
bub5 = pret %>% filter(portfolio == 5) %>% rename("p5" = ret)
bub6 = pret %>% filter(portfolio == 6) %>% rename("p6" = ret)
bub7 = pret %>% filter(portfolio == 7) %>% rename("p7" = ret)
bub8 = pret %>% filter(portfolio == 8) %>% rename("p8" = ret)
bub9 = pret %>% filter(portfolio == 9) %>% rename("p9" = ret)
bub10 = pret %>% filter(portfolio == 10)%>%rename("p10" = ret)

bub101 = tibble("date" = bub1$date, "p101" = bub10$p10 - bub1$p1) # long short portfolio returns

################
# Dataframe with individual stock characteristics, adding also the 
# portfolio to which the security is part of
################

bubports = sfbbna %>%
  filter(altprc >5 & altprc <1000) %>%
  group_by(date) %>%
  mutate(portfolio = assign_portfolio(
    n_portfolios = 10,
    exchanges = "NYSE",
    data = cur_data()
  )) %>%
  ungroup() 

sfbbport = sfbb %>% merge(bubports %>% dplyr::select(date, permno, portfolio), all.x = TRUE, by = c("permno","date"))
sfbbport2 = sfbbport %>% merge(bub101, by = c("date"), all.x=TRUE)

# Dataframe with the factors, bubble shock, and long-short
charsn = sfbbport2 %>%
  dplyr::select(mktrf, smb, hml, rmw,cma, bub_shock, rf, p101,date) %>%
  distinct() %>%
  na.omit() %>%
  tibble() 

# Correlation matrix
cor(charsn %>% dplyr::select(-date))



