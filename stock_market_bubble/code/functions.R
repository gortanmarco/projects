#####################
# Assign stocks to bubble beta quantiles
#####################
assign_portfolio <- function(n_portfolios,
                             exchanges,
                             data
                             ) {
  breakpoints <- data |>
    filter(exchange %in% exchanges) |>
    summarize(breakpoint = quantile(
      b_bub,
      probs = seq(0, 1, length.out = n_portfolios+1 ),
      na.rm = TRUE
    )) |>
    pull(breakpoint) |>
    as.numeric()
  
  assigned_portfolios <- data |>
    mutate(portfolio = findInterval(b_bub,
                                    breakpoints,
                                    all.inside = TRUE
    )) |>
    pull(portfolio)
  return(assigned_portfolios)
}


#####################
# Compute deciles portfolio returns
#####################
compute_portfolio_returns <- function(n_portfolios = 10,
                                      exchanges = c("NYSE"),
                                      value_weighted = TRUE,
                                      data = sfbbna) {
  data %>%
    group_by(date) |>
    mutate(portfolio = assign_portfolio(
      n_portfolios = n_portfolios,
      exchanges = exchanges,
      data = cur_data()
    )) |>
    group_by(date, portfolio) |>
    summarize(
      ret = if_else(value_weighted,
                    weighted.mean(ret_exc, mktcap_reference),
                    mean(ret_exc)
      ),
      .groups = "drop_last"
    ) #|>
    # summarize(size_premium = ret[portfolio == min(portfolio)] -
    #             ret[portfolio == max(portfolio)]) |>
    # summarize(size_premium = mean(size_premium))
}

#####################
# Assign stocks to size quantiles
#####################

assign_portfolio_size <- function(n_portfolios,
                             exchanges,
                             data) {
  breakpoints <- data |>
    filter(exchange %in% exchanges) |>
    summarize(breakpoint = quantile(
      mktcap_reference,
      probs = seq(0, 1, length.out = n_portfolios+1 ),
      na.rm = TRUE
    )) |>
    pull(breakpoint) |>
    as.numeric()
  
  assigned_portfolios <- data |>
    mutate(portsize = findInterval(mktcap_reference,
                                    breakpoints,
                                    all.inside = TRUE
    )) |>
    pull(portsize)
  return(assigned_portfolios)
}

#####################
# Compute the bivariate portfolios, based on size
#####################

compute_portfolio_returns_bi <- function(n_portfolios = 10,
                                      exchanges = c("NYSE"),
                                      value_weighted = TRUE,
                                      data = sfbbna) {
  data %>%
    group_by(date,portsize) |>
    mutate(portfolio = assign_portfolio(
      n_portfolios = n_portfolios,
      exchanges = exchanges,
      data = cur_data()
    )) |>
    group_by(date, portsize, portfolio) |>
    summarize(
      ret = if_else(value_weighted,
                    weighted.mean(ret_exc, mktcap_reference),
                    mean(ret_exc)
      ),
      .groups = "drop_last"
    ) #|>
  # summarize(size_premium = ret[portfolio == min(portfolio)] -
  #             ret[portfolio == max(portfolio)]) |>
  # summarize(size_premium = mean(size_premium))
}


# Remove the first N values when there are NAs
lagna <- function(ts, num){
  ts_lagged = ts[!is.na(lag(ts, num))]
  return(ts_lagged)
}

# Remove the last N values when there are NAs
leadna <- function(ts, num){
  ts_lead = ts[!is.na(lead(ts, num))]
  return(ts_lead)
}

# Moving average of time series
mav <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 1)}



