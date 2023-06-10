#######################
### Import the packages 
#######################
library(magrittr) # Use the %>% (pipe) operator
library(dplyr) # Tidy data handler
library(xlsx) # Import excel files 
library(zoo) # Handle time series objects

##########################à
# Set the working directory in the folder with the datasets
###########################
setwd("C:\\Users\\gorta\\OneDrive\\Documenti\\uni\\Tesi\\supplementary_thesis_Gortan\\data")

##########################
# Load the value weighted index from CRSP
#########################

mkt_index = as_tibble(read.csv(file ="crsp_vw.csv"))

###############
# Create two distinct dataframes, one for the market index and one for the
# dividend series
################
mkt_ts = tibble("mkt_value" = mkt_index$vwindx, "date" = mkt_index$caldt %>% 
                  as.character() %>% 
                  as.yearmon("%Y%m%d")) 

div_ts = tibble("div_monthly" = lag(mkt_index$vwindx) * (mkt_index$vwretd - mkt_index$vwretx),
                "date" = mkt_index$caldt %>% 
                  as.character() %>% 
                  as.yearmon("%Y%m%d"))

##################
# Load the dataframe with the inflation index (CPI-U) from Robert Shiller's website
##################

shiller = as_tibble(read.xlsx(file ="shiller.xls", sheetName = 'Data'))
inflation = tibble("date" = shiller$Date 
                   %>% sprintf("%.2f", .) 
                   %>% as.yearmon("%Y.%m"), "cpi" = shiller$cpi %>% as.numeric())

###############################
# Load the dataframe with the economic variables available from Amit Goyal's website
# and create a dataframe with the long-term yield and the treasury bill rate. Afterwards,
# compute the term spread
##################################

goyal = as_tibble(read.csv(file ="goyal.csv")) 

ytm = tibble("date" = goyal$yyyymm %>% as.character() %>% paste("01", sep="") %>% as.Date("%Y%m%d") %>% as.yearmon("%Y-%m-%d"),
             "ytm_10" = goyal$lty, "ytm_90" = goyal$tbl) %>% na.omit()

ytm = ytm %>%
  as_tibble() %>%
  mutate(term_spread = ytm_10 - ytm_90)

########################
# Merge the dataframe to obtain the dataset which will be the base
# for retrieving the fundamental and non-fundamental components of
# log price-to-dividend ratio
###########################

df_part0 = merge(mkt_ts, inflation) %>%
  merge(div_ts) %>%
  merge(ytm) %>%
  as_tibble()

# Deflating market values
df_part0$mkt_value_def = (df_part0$mkt_value / df_part0$cpi)
# Deflating market values -- LOG
df_part0$mkt_value_def_log = (df_part0$mkt_value / df_part0$cpi) %>% log()
# Deflating dividends 
df_part0$div_def = (df_part0$div_monthly / df_part0$cpi)
# Deflating dividends -- LOG
df_part0$div_def_log = (df_part0$div_monthly / df_part0$cpi) %>% log()

# Market returns
df_part0$mkt_diff = c(NA, diff(df_part0$mkt_value_def_log))
# Dividends changes - monthly
df_part0$div_diff = c(NA, diff(df_part0$div_def_log))




