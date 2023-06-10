library(reshape2)
library(texreg)
setwd("C:\\Users\\gorta\\OneDrive\\Documenti\\uni\\Tesi\\supplementary_thesis_Gortan\\data")


######################
# Load the 202 portfolios from the supplementary material of Feng et al. (2020)
######################
port202 =  read.csv("port202.csv")%>%
  tibble() %>%
  mutate(date = lubridate::ymd(as.character(date)%>%paste0("01"))) %>%
  mutate(date = as.yearmon(date, "%Y-%m-%d")) %>%
  mutate(date = as.Date(date))

factors = ff5_umd %>% merge(df_bubble_fin) %>% merge(bub101)
port202long = gather(port202, pfolio, measurement, X1:X202, factor_key=TRUE)%>%
  merge(factors)
port202long$measurementf = port202long$measurement - port202long$rf

# BUB
multibeta_pf <- port202long %>%
  nest(data = -pfolio) %>%
  mutate(model = map(data,~lm(measurementf ~ p101 , data = . )),
         tidied = map(model, tidy)) %>% unnest(tidied) %>%
  dplyr::select(pfolio, term, estimate, statistic) %>%
  dcast(pfolio ~ term, value.var = c("estimate"))

avg_exc = port202long %>% 
  group_by(pfolio) %>%
  summarize("avg_ret" = mean(measurementf)) %>%
  ungroup()

multibeta_pf$avg_exc = avg_exc$avg_ret
linb = lm(avg_exc ~ p101, data = multibeta_pf)
coeftest(linb, vcov.=NeweyWest(linb, lag=12, prewhite=FALSE, adjust=TRUE, verbose=TRUE))
summary(linb)

# CAPM
multibeta_pf <- port202long %>%
  nest(data = -pfolio) %>%
  mutate(model = map(data,~lm(measurementf ~ mktrf , data = . )),
         tidied = map(model, tidy)) %>% unnest(tidied) %>%
  dplyr::select(pfolio, term, estimate, statistic) %>%
  dcast(pfolio ~ term, value.var = c("estimate"))

avg_exc = port202long %>% 
  group_by(pfolio) %>%
  summarize("avg_ret" = mean(measurementf)) %>%
  ungroup()

multibeta_pf$avg_exc = avg_exc$avg_ret
lincapm = lm(avg_exc ~ mktrf, data = multibeta_pf)
coeftest(lincapm, vcov.=NeweyWest(lincapm, lag=12, prewhite=FALSE, adjust=TRUE, verbose=TRUE))
summary(lincapm)

# CAPM, BUB
multibeta_pf <- port202long %>%
  nest(data = -pfolio) %>%
  mutate(model = map(data,~lm(measurementf ~ mktrf + p101 , data = . )),
         tidied = map(model, tidy)) %>% unnest(tidied) %>%
  dplyr::select(pfolio, term, estimate, statistic) %>%
  dcast(pfolio ~ term, value.var = c("estimate"))

avg_exc = port202long %>% 
  group_by(pfolio) %>%
  summarize("avg_ret" = mean(measurementf)) %>%
  ungroup()

multibeta_pf$avg_exc = avg_exc$avg_ret
lincapmb = lm(avg_exc ~ mktrf + p101, data = multibeta_pf)
coeftest(lincapmb, vcov.=NeweyWest(lincapmb, lag=12, prewhite=FALSE, adjust=TRUE, verbose=TRUE))
summary(lincapmb)



# FF3
multibeta_pf <- port202long %>%
  nest(data = -pfolio) %>%
  mutate(model = map(data,~lm(measurementf ~ mktrf + smb + hml , data = . )),
         tidied = map(model, tidy)) %>% unnest(tidied) %>%
  dplyr::select(pfolio, term, estimate, statistic) %>%
  dcast(pfolio ~ term, value.var = c("estimate"))

avg_exc = port202long %>% 
  group_by(pfolio) %>%
  summarize("avg_ret" = mean(measurementf)) %>%
  ungroup()


multibeta_pf$avg_exc = avg_exc$avg_ret
linff3 = lm(avg_exc ~ mktrf + smb + hml, data = multibeta_pf)
coeftest(linff3, vcov.=NeweyWest(linff3, lag=12, prewhite=FALSE, adjust=TRUE, verbose=TRUE))
summary(linff3)

# FF3 , BUB
multibeta_pf <- port202long %>%
  nest(data = -pfolio) %>%
  mutate(model = map(data,~lm(measurementf ~ mktrf + smb + hml + p101 , data = . )),
         tidied = map(model, tidy)) %>% unnest(tidied) %>%
  dplyr::select(pfolio, term, estimate, statistic) %>%
  dcast(pfolio ~ term, value.var = c("estimate"))

avg_exc = port202long %>% 
  group_by(pfolio) %>%
  summarize("avg_ret" = mean(measurementf)) %>%
  ungroup()

multibeta_pf$avg_exc = avg_exc$avg_ret
linff3b = lm(avg_exc ~ mktrf + smb + hml +p101, data = multibeta_pf)
coeftest(linff3b, vcov.=NeweyWest(linff3b, lag=12, prewhite=FALSE, adjust=TRUE, verbose=TRUE))
summary(linff3b)

# FF4 + BUB
multibeta_pf <- port202long %>%
  nest(data = -pfolio) %>%
  mutate(model = map(data,~lm(measurementf ~ mktrf + smb + hml + umd + p101, data = . )),
         tidied = map(model, tidy)) %>% unnest(tidied) %>%
  dplyr::select(pfolio, term, estimate, statistic) %>%
  dcast(pfolio ~ term, value.var = c("estimate"))

avg_exc = port202long %>% 
  group_by(pfolio) %>%
  summarize("avg_ret" = mean(measurementf)) %>%
  ungroup()

multibeta_pf$avg_exc = avg_exc$avg_ret
linumdb = lm(avg_exc ~ mktrf + smb + hml +umd + p101, data = multibeta_pf)
coeftest(linumdb, vcov.=NeweyWest(linumdb, lag=12, prewhite=FALSE, adjust=TRUE, verbose=TRUE))
summary(linumdb)


# FF5 + BUB
multibeta_pf <- port202long %>%
  nest(data = -pfolio) %>%
  mutate(model = map(data,~lm(measurementf ~ mktrf + smb + hml + rmw+ cma + p101, data = . )),
         tidied = map(model, tidy)) %>% unnest(tidied) %>%
  dplyr::select(pfolio, term, estimate, statistic) %>%
  dcast(pfolio ~ term, value.var = c("estimate"))

avg_exc = port202long %>% 
  group_by(pfolio) %>%
  summarize("avg_ret" = mean(measurementf)) %>%
  ungroup()

multibeta_pf$avg_exc = avg_exc$avg_ret
linff5 = lm(avg_exc ~ mktrf + smb + hml +rmw+ cma + p101, data = multibeta_pf)
coeftest(linff5, vcov.=NeweyWest(linff5, lag=12, prewhite=FALSE, adjust=TRUE, verbose=TRUE))
summary(linff5)
texreg(list(linb, lincapm, linff3,linff3b, linumdb, linff5))

