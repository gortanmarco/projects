library(dplyr)
library(tidyverse)
library(knitr)
library(kableExtra)
library(tidyquant)
library(directlabels)
library(tibble)
library(httr)
library(xml2)
library(rvest)
library(tidyr)
library(quantmod)
library(Quandl)
library(tidyquant)
library(lubridate)
library(tidyselect)
library(namespace)
library(reshape)
library(pryr)
library(rjson)
library(tseries)
library(plm)
library(Hmisc)
library(usethis)
library(latticeExtra)
library(survival)
library(gtable)
library(grid)
library(ggstatsplot)
library(highfrequency)
library(alphavantager)
library(QuantTools)
#import tickers. They have been downloaded from the holdings of'iShares Russell 3000 ETF' from BlackRock Website
russell3000 <- read_csv("~/University/UNIPI/Thesis/russell3000.csv")
russell3000 <- russell3000 %>% drop_na()
tickers <- russell3000$ticker

#get stock prices for tickers. Some values gave errors (and so needed to be handled) but I preferred to keep this script as much clean as possible.
thesis.data<- tickers %>%
  tq_get(get  = "stock.prices",
         from = "2020-01-01",
         to   = TODAY()) %>%
  group_by(symbol) 

cast(stock_prices, symbol~date, value = "adjusted")

#get market capitalization. Scraped data from macrotrends.net with python
market_data <- fromJSON(file = "file:///C:/Users/Marco/Documents/University/UNIPI/Thesis/marketcap.json")

mark_cap <- list()

for (tck in ticker){
  mark_cap[[tck]] <- data.frame("date"=c(1), "cap" = c(1))
  for (i in 1:length(market_data[[tck]])){
  mark_cap[[tck]][i,1] = market_data[[tck]][[i]]$date
  mark_cap[[tck]][i,2] = market_data[[tck]][[i]]$v1
    }
  }



for(tck in tickers){
  mark_cap[[tck]][["ticker"]] <- tck
  }

#final dataset of market capitalisation
cap_set <- bind_rows(mark_cap)

rm(market_data)

#perform the merge of prices and market cap separately for a matter of computation

final1 <- merge(thesis.data %>% filter(symbol %in% tickers[1:1000]),
                cap_set %>% filter(ticker %in% tickers[1:1000]),
                by.x=c("symbol", "date"),
                by.y=c("ticker", "date"))
final2 <- merge(thesis.data %>% filter(symbol %in% tickers[1001:2000]),
                cap_set %>% filter(ticker %in% tickers[1001:2000]),
                by.x=c("symbol", "date"),
                by.y=c("ticker", "date"))
final3 <- merge(thesis.data %>% filter(symbol %in% tickers[2001:length(tickers)]),
                cap_set %>% filter(ticker %in% tickers[2001:length(tickers)]),
                by.x=c("symbol", "date"),
                by.y=c("ticker", "date"))
final <- bind_rows(final1, final2, final3)

tickers <- tickers[-c(528,1159)]

#get sector

business <- data.frame("ticker" = tickers)
for (tck in tickers){
  business[which(tickers == tck), 2] = (read_html(
    paste0("https://finance.yahoo.com/quote/",
           tck, 
           "/profile?p=",
            tck)) %>%
      html_nodes(xpath = "//span[@class = 'Fw(600)']") %>%
    html_text(trim = TRUE))[1]
  print(which(tickers==tck))
}

#get industry
for (tck in tickers){
  business[which(tickers == tck), 3] = (read_html(
    paste0("https://finance.yahoo.com/quote/",
           tck, 
           "/profile?p=",
           tck)) %>%
      html_nodes(xpath = "//span[@class = 'Fw(600)']") %>%
      html_text(trim = TRUE))[2]
  print(which(tickers==tck))
 }

business <- as_tibble(business)
business <- business %>%
  dplyr::rename(sector = V2,
                industry = V3)

final <- drop_na(final)

#needed to perform the merges separately, for a matter of computation
final_bus1 <- merge(drop_na(final1),
      business,
      by.x=c("symbol"),
      by.y=c("ticker"))

final_bus2 <- merge(drop_na(final2),
                   business,
                   by.x=c("symbol"),
                   by.y=c("ticker"))

final_bus3 <- merge(drop_na(final3),
                   business,
                   by.x=c("symbol"),
                   by.y=c("ticker"))

final_bus <- bind_rows(final_bus1, final_bus2, final_bus3)

final_bus <- drop_na(final_bus)

#get dataset with weekly performance
dataset_week <- final_bus %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted, 
            mutate_fun = periodReturn, 
            period = "weekly", 
            type = "log",
            col_rename = "returns_week") %>%
  mutate(performance = 100 * cumprod(1 + returns_week))

dataset_week1 <-merge(dataset_week %>% filter(symbol %in% tickers[1:1000]),
                              cap_set %>% filter(ticker %in% tickers[1:1000]),
                              by.x=c("symbol", "date"),
                              by.y=c("ticker", "date"))
dataset_week2 <-merge(dataset_week %>% filter(symbol %in% tickers[1001:2000]),
                     cap_set %>% filter(ticker %in% tickers[1001:2000]),
                     by.x=c("symbol", "date"),
                     by.y=c("ticker", "date"))

dataset_week3 <-merge(dataset_week %>% filter(symbol %in% tickers[2001:length(tickers)]),
                     cap_set %>% filter(ticker %in% tickers[2001:length(tickers)]),
                     by.x=c("symbol", "date"),
                     by.y=c("ticker", "date"))

dataset_week = bind_rows(dataset_week1, dataset_week2, dataset_week3)

dates <- dataset_week$date %>%
  factor() %>%
  levels()


dataset_week_wide <-  cast(dataset_week, date ~ symbol, value = 'cap') 
dataset_week_wide_cap <- as_tibble(dataset_week_wide)

#####weights
russ_cap <- dataset_week_wide %>%
  mutate(date = NULL) %>%
  rowSums(na.rm = TRUE)

dataset_week_wide_cap <- dataset_week_wide_cap %>%
  mutate(russ_cap)

russ_capitalization <- select(dataset_week_wide_cap, date, russ_cap)

dataset_week_I <- merge(dataset_week,
                   russ_capitalization,
                   by.x=c("date"),
                   by.y=c("date"))

dataset_week_II <- as_tibble(dataset_week_I %>%
  mutate(weights = cap/russ_cap))

dataset_week_sector_industry <- dataset_week_II %>%
  group_by(date, sector, industry) %>%
  transmute(cap = sum(cap, na.rm = TRUE),
            russ_cap = russ_cap, weights = sum(weights, na.rm = TRUE), 
            returns = weighted.mean(returns_week, weights)) %>%
  distinct() %>%
  ungroup()

##specification for sector only in order to obtain returns by sector  
dataset_week_sector <- dataset_week_II %>%
  group_by(date, sector) %>%
  transmute(cap = sum(cap, na.rm = TRUE),
            russ_cap = russ_cap, weights = sum(weights, na.rm = TRUE), 
            returns = weighted.mean(returns_week, weights)) %>%
  distinct() %>%
  ungroup()

dataset_week_sector <- dataset_week_sector %>%
  group_by(sector) %>%
  mutate(performance = 100 * cumprod(1 + returns)) %>%
  ungroup()

###dividends from macrotrends.net
dividends = list()
for (sim in tickers){
  dividends[[sim]] = ((read_html(paste0("https://www.macrotrends.net/assets/php/dividend_yield.php?t=",
                                      sim)) %>%
                       html_nodes(xpath = "//script") %>%
                       html_text(trim = TRUE))[12] %>%
                      str_extract_all("\\[.+?\\]"))[[1]][1] %>%
    fromJSON()
  print(which(tickers ==sim))
}

for (sim in tickers){
  for (i in 1:length(dividends[[sim]])){
    if (is.null(dividends[[sim]][[i]]$ttm_d)==TRUE){
      dividends[[sim]][[i]]$ttm_d = 0
      print(sim)
    }
    else {
      dividends[[sim]][[i]]$ttm_d = dividends[[sim]][[i]]$ttm_d
    }
  }
  print(sim)
}

dividends_data = list()
for (sim in tickers){
  dividends_data[[sim]] <- data.frame("date"=c(1), "dividend" = c(1))
  for (i in 1:length(dividends[[sim]])){
    dividends_data[[sim]][i,1] = dividends[[sim]][[i]]$date
    dividends_data[[sim]][i,2] = dividends[[sim]][[i]]$ttm_d
  }
  print(which(tickers == sim))
}



for(sim in tickers){
  dividends_data[[sim]][["sim"]] <- sim
}


dividends_dataset <- bind_rows(dividends_data)

###covid period

#log returns

covid_dates = seq(as.Date("2020/02/19"), by = "day", length.out = 87)
dates_rebound = seq(as.Date("2020/03/23"), by = "day", length.out = 54)


final_bus_covid<- final_bus %>% filter(date %in% covid_dates)

final_bus_covid_1 <- final_bus_covid %>%
  group_by(symbol) %>%
  tq_mutate(select = adjusted, 
               mutate_fun = periodReturn, 
               period = "daily", 
               type = "log",
               col_rename = "returns") %>%
  mutate(performance = 100 * cumprod(1 + returns))



final_bus_covid_2 <- final_bus_covid_1 %>%
  group_by(date, sector) %>%
  transmute(sector_cap = sum(cap, na.rm = TRUE),
            performance_sec = weighted.mean(performance, sector_cap)) %>%
  distinct() %>%
  ungroup()

russel_covid <- final_bus_covid_prova %>%
  group_by(date) %>%
  transmute(cap_russ = sum(cap, na.rm = TRUE),
            performance_russell = weighted.mean(performance, cap_russ)) %>%
  distinct() %>%
  ungroup()

#arithmenticreturns

final_bus_covid_prova_ar <- final_bus_covid %>%
  group_by(symbol) %>%
  tq_mutate(select = adjusted, 
            mutate_fun = periodReturn, 
            period = "daily", 
            type = "arithmetic",
            col_rename = "returns") %>%
  mutate(performance = 100 * cumprod(1 + returns))

final_bus_covid_2_ar <- final_bus_covid_prova_ar %>%
  group_by(date, sector) %>%
  transmute(sector_cap = sum(cap, na.rm = TRUE),
            performance_sec = weighted.mean(performance, sector_cap)) %>%
  distinct() %>%
  ungroup()

final_bus_covid_2_ar <- final_bus_covid_2_ar %>%
  group_by(date) %>%
  mutate(cap_tot = sum(sector_cap, na.rm = TRUE)) %>%
  distinct() %>%
  ungroup()

final_bus_covid_2_ar <- final_bus_covid_2_ar %>% mutate(weight = sector_cap/cap_tot)

russel_covid_ar <- final_bus_covid_prova_ar %>%
  group_by(date) %>%
  transmute(cap_russ = sum(cap, na.rm = TRUE),
            performance_russell = weighted.mean(performance, cap_russ)) %>%
  distinct() %>%
  ungroup()

russel_covid_ar <- russel_covid_ar %>%
  tq_mutate(select = performance_russell, 
            mutate_fun = periodReturn, 
            period = "daily", 
            type = "arithmetic",
            col_rename = "returns")


##examples with panel analysis
final_bus_covid_prova_ar <- ungroup(final_bus_covid_prova_ar)
covid_rebound<- final_bus %>% filter(date %in% dates_rebound)

covid_rebound <- covid_rebound%>%
  group_by(symbol) %>%
  tq_mutate(select = adjusted, 
            mutate_fun = periodReturn, 
            period = "daily", 
            type = "arithmetic",
            col_rename = "returns") %>%
  mutate(performance = 100 * cumprod(1 + returns))
covid_rebound <- ungroup(covid_rebound)

##difference 
dates_in_end <- as.Date(c("2020/03/23", "2020/05/15"))
covid_rebound_inend<- covid_rebound %>% filter(date %in% as.Date("2020/05/15"))
fit <- lm(I(performance-100) ~ I(cap^0.5)*sector, data=covid_rebound_inend)
summary(fit)
par(mfrow = c(1, 1))
plot(fit)

###current ratio
current_ratio = list()
for (sim in tickers){
  current_ratio[[sim]] = ((read_html(paste0("https://www.macrotrends.net/assets/php/fundamental_iframe.php?t=", sim, "&type=current-ratio&statement=ratios&freq=Q")) %>%
                         html_nodes(xpath = "//script") %>%
                         html_text(trim = TRUE))[8] %>%
                         str_extract_all("\\[.+?\\]"))[[1]][1] %>%
    fromJSON()
  print(which(tickers ==sim))
}

for (sim in tickers){
  for (i in 1:length(current_ratio[[sim]])){
    if (is.null(current_ratio[[sim]][[i]]$v3)==TRUE){
      current_ratio[[sim]][[i]]$v3 = 0
      print(sim)
    }
    else {
      current_ratio[[sim]][[i]]$ttm_d = current_ratio[[sim]][[i]]$v3
    }
  }
  print(sim)
}

current_ratio_data = list()
for (sim in tickers){
  current_ratio_data[[sim]] <- data.frame("date"=c(1), "c_ratio" = c(1))
  for (i in 1:length(current_ratio[[sim]])){
    current_ratio_data[[sim]][i,1] = current_ratio[[sim]][[i]]$date
    current_ratio_data[[sim]][i,2] = current_ratio[[sim]][[i]]$v3
  }
  print(which(tickers == sim))
}



for(sim in tickers){
  current_ratio_data[[sim]][["sim"]] <- sim
}


current_ratio_dataset <- bind_rows(current_ratio_data)
as_tibble(current_ratio_dataset)

final_bus_covid_prova_ar <- final_bus_covid_prova_ar %>% filter(symbol %in% tickers)

dates_current_ratio <- seq(as.Date("2020/1/1"), as.Date("2020/04/01"), "days")
current_ratio_dataset_dates <- current_ratio_dataset %>% filter(date %in% dates_current_ratio)

current_ratios <- current_ratio_dataset_dates %>%
  group_by(sim) %>%
  transmute(cu_ratio = last(c_ratio))%>%
  distinct() %>%
  ungroup()

##data for the covid evlution in Italy taken from github.com/pcm-dpc/COVID-19
covid <- read.csv("~/University/UNIPI/Thesis/covid.txt")
covid_percentage <- covid %>% select(data, tamponi, nuovi_positivi)
covid_percentage <- covid_percentage %>% mutate(percentage = nuovi_positivi/tamponi)
covid_perc_dates <- seq(from = as.Date("2020-02-24"), to = as.Date("2020-06-01"), by = "days")
covid_percentage = covid_percentage %>% mutate(date = covid_perc_dates)
covid_percentage = covid_percentage %>% mutate(data = NULL)

ggplot() +
  geom_line(data = covid_percentage, aes(x = date, y= percentage)) +
  geom_line(data = final_bus_covid_prova_ar, aes(x = date, y =symbol))

dates_covid <- russel_covid_ar$date
dates_covid <- dates_covid[dates_covid %in% covid_percentage$date]
russel_covid_ar_tests <- russel_covid_ar %>% filter(date %in% dates_covid)
covid_perc_russ <- covid_percentage %>% filter(date %in% dates_covid)
#-------------------------------------------

