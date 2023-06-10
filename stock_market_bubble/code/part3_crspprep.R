#######################
### Import the packages 
#######################

library(stringr) # character type manipulation
library(lubridate) # to better deal with dates
library(data.table) # type of table useful for doing some computations

###########################
# Set the working directory in the folder with the datasets
###########################
setwd("C:\\Users\\gorta\\OneDrive\\Documenti\\uni\\Tesi\\supplementary_thesis_Gortan\\data")


##########################
# Load the Fama-French Five factors and the momentum factor
#########################
ff5_umd = read.csv("ff5_umd.csv") %>%
  as_tibble() %>%
  mutate(date = lubridate::ymd(dateff)) %>%
  mutate(date = as.yearmon(date, "%Y-%m-%d")) %>%
  mutate(date = as.Date(date)) %>%
  dplyr::select(-dateff)




##########################
# Load the permno-level information from CRSP
# Note: the following lines of code are based on the instructions provided 
# in https://www.tidy-finance.org/wrds-crsp-and-compustat.html
#########################
crsp = read.csv("crsp.csv")
crsp = as_tibble(crsp)
colnames(crsp) <- str_to_lower(colnames(crsp))
  
# Parse only relevant variables
crsp <- crsp %>%
  transmute(permno = as.integer(permno),     # security identifier
            date = lubridate::ymd(date),     # month identifier
            ret = as.numeric(ret) * 100,     # return (converted to percent)
            shrout = as.numeric(shrout),     # shares outstanding (in thousands)
            altprc = as.numeric(altprc),     # last traded price in a month
            exchcd = as.integer(exchcd),     # exchange code
            siccd = as.integer(siccd),       # industry code
            dlret = as.numeric(dlret) * 100, # delisting return (converted to percent)
            dlstcd = as.integer(dlstcd),     # delisting code
            shrcd = as.integer(shrcd),       # share code
            comnam = comnam                  # company name
  ) 


# Keep only US-based common stocks (10 and 11)
crsp <- crsp %>%
  filter(shrcd %in% c(10, 11)) %>%
  dplyr::select(-shrcd)

# Keep only distinct observations to avoid multiple counting
crsp <- crsp %>%
  distinct(permno, date, .keep_all = TRUE) # remove duplicates 

# Compute market cap
# Note: altprc is the negative of average of bid and ask from last traded price
#       for which the data is available if there is no last traded price
crsp <- crsp %>%
  mutate(mktcap = abs(shrout * altprc) / 1000, # in millions of dollars
         mktcap = if_else(mktcap == 0, as.numeric(NA), mktcap)) # consider as missing when market cap
                                                                # is zero

# Define exchange labels and keep only NYSE, AMEX and NASDAQ stocks
crsp <- crsp %>%
  mutate(exchange = case_when(exchcd %in% c(1, 31) ~ "NYSE",
                              exchcd %in% c(2, 32) ~ "AMEX",
                              exchcd %in% c(3, 33) ~ "NASDAQ",
                              TRUE ~ "Other")) %>%
  filter(exchange != "Other")

# Adjust delisting returns (see Shumway, 1997)
crsp <- crsp %>%
  mutate(ret_adj = case_when(is.na(dlstcd) ~ ret,
                             !is.na(dlstcd) & !is.na(dlret) ~ dlret,
                             dlstcd %in% c(500, 520, 580, 584) | 
                               (dlstcd >= 551 & dlstcd <= 574) ~ -30,
                             TRUE ~ -100)) %>%
  dplyr::select(-c(dlret, dlstcd))

# Create a copy of the crsp data with the format we need
stocks <- crsp %>%
  dplyr::select(permno, date, exchange, ret = ret_adj, mktcap, comnam, shrout, altprc,
                industry = siccd) %>%
  mutate(date = as.yearmon(date, "%Y-%m-%d")) %>%
  mutate(date = as.Date(date)) %>%
  arrange(permno,date) %>%
  data.table()

# Important: create a column with lagged values of the date. Needed for the empirical
# analysis when we compute the bubble beta at a certain month and compute the return in the
# following month
stocks = stocks %>%
  group_by(permno) %>%
  mutate(date_reference = lag(date, n=1, order_by=permno)) %>%
  ungroup()

# Important: create a column with lagged values of the market capitalization Needed for the empirical
# analysis when we compute the bubble beta at a certain month and compute the value-weighted
# return of the portfolios created at month t ans assessed in the following month
stocks = stocks %>% data.table
stocks = stocks[, mktcap_reference:=c(NA, mktcap[-.N]), by=permno]
stocks <- stocks %>% tibble()

# Multiply the factor return by 100 to have the return in percentage
ff5_umddate = ff5_umd$date
ff5_umd = ff5_umd %>% dplyr::select(-date)
ff5_umd = ff5_umd*100
ff5_umd = ff5_umd %>%
  mutate(date = ff5_umddate)

# Create the final dataframe that is used for the cross-sectional analysis
stocks_final = stocks %>%
  merge(ff5_umd, by = c("date"), all.x = TRUE) %>%
  as_tibble() 

# Remove the last date available as the returns are all indicated as -100
stocks_final <- filter(stocks_final, date != as.Date("2022-03-31"))
stocks_final <- filter(stocks_final, date != as.Date("2022-03-01"))
