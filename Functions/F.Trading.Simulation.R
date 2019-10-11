# -------------------------------------------------------------------------
# install.packages(c("doSNOW", "foreach", "IBrokers", "BatchGetSymbols", "TTR", 
#                    "timeDate", "dplyr", "zoo", "lubridate", "tidyr", "data.table", 
#                    "MASS", "profvis", "plotly", "htmlwidgets", "xlsx", "openxlsx"))

# -------------------------------------------------------------------------
# Initialization
# -------------------------------------------------------------------------
options(scipen = 4)
set.seed(1024)

library(dplyr)
library(foreach)
library(plotly)
library(ggplot2)

library(doSNOW)
closeAllConnections()
cl <- makeCluster(4, outfile="dopar_log.txt")
registerDoSNOW(cl)

# -------------------------------------------------------------------------
# Supporting Functions
# -------------------------------------------------------------------------
AF.roll <- function(df, var, width)
{
  # df = y
  # width = 4
  # var = "buy.window"
  
  df$op <- NA
  
  for(i in 0:width)
  {df$op <- pmax(lag(df[[var]], i), df$op, na.rm = TRUE)}
  
  df <- df %>% select(-var) %>% rename(!!var := op)
  
  rm(var, width, i)
  return(df)
}

AF.Signal.Strength <- function(window, Type)
{
  # window = s$sell.window
  # Type = s$Type
  N <- length(window)
  strength <- rep(NA, N)
  op <- 0
  for(i in 1:N)
  {
    if(is.na(window[i])) {op <- 0}
    else if(i == 1) {op <- 1}
    else if(!is.na(Type[i-1]) & !is.na(Type[i]) & Type[i-1] != Type[i]) {op <- 1}
    else {op <- op + 1}
    
    if(op > 0) {strength[i] <- op}
    rm(i)
  }
  
  rm(N, op, window, Type)
  return(strength)
}

# -------------------------------------------------------------------------
# AF.simulate trade for long and short trades
# -------------------------------------------------------------------------
AF.simulate.ts <- function(df, Process = "Development")
{
  # df = sim2
  # Process = "Development"
  df <- df %>% ungroup() %>% 
    arrange(ID, ds) %>%
    mutate(buy.signal = case_when(Type == "LONG" & buy.price >= low ~ TRUE,
                                  Type == "SHRT" & buy.price <= high ~ TRUE,
                                  TRUE ~ FALSE),
           sell.signal = case_when(Type == "LONG" & sell.price <= high ~ TRUE,
                                   Type == "SHRT" & sell.price >= low ~ TRUE,
                                   TRUE ~ FALSE),
           stop.signal = case_when(Type == "LONG" & stop.price >= low ~ TRUE,
                                   Type == "SHRT" & stop.price <= high ~ TRUE,
                                   TRUE ~ FALSE) )
  
  buy.signal <- df$buy.signal
  sell.signal <- df$sell.signal
  stop.signal <- df$stop.signal
  
  clean.x <- function(x) 
  {
    x[is.na(x) | x < 0] <- 0
    return(x)
  }
  
  buy.price <- clean.x(df$buy.price)
  sell.price <- clean.x(df$sell.price)
  stop.price <- clean.x(df$stop.price)
  last.sell <- clean.x(df$last.sell)

  cost <- 0
  type <- ""
  holding.days <- 0
  trade.val <- 0
  continue <- 1 # To Prevent Buying After Stoploss in a series
  buy.bin <- NA
  
  action <- rep(NA, nrow(df))
  capacity <- rep(NA, nrow(df))
  ROI <- rep(NA, nrow(df))
  invest.period <- rep(NA, nrow(df))
  continue.trading <- rep(NA, nrow(df))
  bin <- rep(NA, nrow(df))
  
  for(i in 1:nrow(df))
  {
    if(sell.price[i] == 0) {continue <- 1}
    continue.trading[i] <- continue
    
    # Buy Or Sell. If buy, don't sell (Because we don't know the sequence of low and high)
    # Buy only once in a series
    # Do Not Stop Loss on Purchase Day
    
    if(buy.signal[i] & cost == 0 & continue > 0)        # BUY
    {
      action[i] <- "BUY"
      type <- df$Type[i]
      cost <- buy.price[i]
      holding.days <- 1
      trade.val <- round(df$volume[i]*(df$open[i] + df$close[i])/2, 0)
      capacity[i] <- trade.val
      
      buy.bin <- df$R[i]
      bin[i] <- buy.bin
      
    } else
      if(sell.signal[i] & cost > 0)                     # Sell
      {
        action[i] <- "TARGET SELL"
        capacity[i] <- trade.val
        ROI[i] <- case_when(type =="LONG" ~ sell.price[i]/cost,
                            type =="SHRT" ~ 2 - sell.price[i]/cost)
        invest.period[i] <- holding.days + 1
        bin[i] <- buy.bin
        
        cost <- 0
        type <- ""
        holding.days <- 0
        trade.val <- 0
        buy.bin <- NA
        
      } else
        if(i < nrow(df) & cost > 0 & stop.signal[i])    # Sell @ stop loss
        {
          action[i] <- "STOP SELL"
          capacity[i] <- trade.val
          ROI[i] <- case_when(type =="LONG" ~ stop.price[i]/cost,
                              type =="SHRT" ~ 2 - stop.price[i]/cost)
          
          invest.period[i] <- holding.days
          bin[i] <- buy.bin
          
          cost <- 0
          type <- NA
          holding.days <- 0
          trade.val <- 0
          continue <- 0
          buy.bin <- NA
        } else
          if(i < nrow(df) & cost > 0 & last.sell[i] > 0) # Sell @ Close if still to be sold
          {
            action[i] <- "EOD SELL"
            capacity[i] <- trade.val
            ROI[i] <- last.sell[i]/cost
            ROI[i] <- case_when(type =="LONG" ~ last.sell[i]/cost,
                                type =="SHRT" ~ 2 - last.sell[i]/cost)
            invest.period[i] <- holding.days
            bin[i] <- buy.bin
            
            cost <- 0
            type <- NA
            holding.days <- 0
            trade.val <- 0
            buy.bin <- NA
            
          } else
            if(cost > 0) # Hold
            {
              action[i] <- "HOLD"
              holding.days <- holding.days + 1
              if(trade.val > 0) {capacity[i] <- trade.val}
              bin[i] <- buy.bin
            }

  }
  
  if(Process == "Development")
  {
    op <- df %>% 
          bind_cols(data.frame(action, ROI, invest.period, capacity, stringsAsFactors = FALSE)) %>% 
          select(ID, R, buy.bin, ticker, ds, DP.Method, MA.Type, Period, Type,
                 signal, buy.price, sell.price, stop.price, last.sell, 
                 action, ROI, invest.period, capacity,
                 open, low, high, close) %>%
          mutate(ROI = ifelse(!is.na(ROI) & ROI < 0, 0, ROI), 
                 action = ifelse(!is.na(ROI) & ROI < 0, "STOP SELL", action)) %>%
          arrange(ds)
  } 
  
  if(Process == "Production")
  {
    op <- df %>% 
          select(-c(buy.window, sell.window, R.low, R.high, R.buy, R.sell, R.stop, ROI.l, ROI.h,
                    buy.signal, sell.signal, stop.signal)) %>%
          bind_cols(data.frame(continue.trading, action, capacity, ROI, invest.period
                               , stringsAsFactors = FALSE)) %>% arrange(ds) %>%
      # Provisions for Missed Sell
      mutate(missed.sell = case_when(!is.na(in.hand) & 
                                       max(which(grepl("SELL", action))) > max(which(grepl("BUY", action))) &
                                       row_number() > max(which(grepl("BUY", action))) ~ 1,
                                     TRUE ~ 0)
             , action = ifelse(missed.sell == 1, "MISSED SELL", action)
             , Type = ifelse(missed.sell == 1, zoo::na.locf(Type), Type)
             , signal = ifelse(missed.sell == 1 & is.na(signal), zoo::na.locf(signal), signal)
             , sell.price = ifelse(missed.sell == 1 & is.na(sell.price), zoo::na.locf(sell.price), sell.price)
             , stop.price = ifelse(missed.sell == 1 & is.na(stop.price), zoo::na.locf(stop.price), stop.price)
             , last.sell = ifelse(missed.sell == 1 & row_number() > max(which(!!last.sell > 0)),
                                  last(last.sell[!!last.sell > 0]),
                                  last.sell)
             , continue.trading = ifelse(missed.sell == 1, 1, continue.trading)
             , ROI = ifelse(missed.sell == 1, NA, ROI)
             , invest.period = ifelse(missed.sell == 1, NA, invest.period)
      ) %>%
      select(-missed.sell) %>%
      
      # Provisions for Missed Buy
      mutate(missed.buy = case_when(is.na(in.hand) & 
                                      !is.na(last(action)) & last(action) == "HOLD" &
                                      row_number() >= max(which(grepl("BUY", action))) ~ 1,
                                    TRUE ~ 0)
      , action = ifelse(missed.buy == 1, "MISSED BUY", action)
      , capacity = ifelse(missed.buy == 1, NA, capacity)
      ) %>% 
      select(-missed.buy) %>%
      
      # Removing Redundant Price Points
      mutate(buy.price = case_when(continue.trading > 0 ~ buy.price)
             , sell.price = case_when(continue.trading > 0 ~ sell.price)
             , stop.price = case_when(continue.trading > 0 ~ stop.price)
             , last.sell = case_when(continue.trading > 0 ~ last.sell)
      ) %>%
      select(-continue.trading) %>%
      
      # Provisions for Last Day
      mutate(action = case_when(row_number() == n() & is.na(action) & !is.na(buy.price) & is.na(in.hand) ~ "CAN BUY", 
                                row_number() == n() & !is.na(action) & action == "HOLD" ~ "CAN SELL",
                                TRUE ~ action))
    
    
  }

  rm(buy.signal, sell.signal, stop.signal, buy.price, sell.price, stop.price, last.sell
     , cost, type, holding.days, trade.val, continue, bin, buy.bin
     , action, capacity, ROI, invest.period, continue.trading, df, i, clean.x, Process)
  gc()
  
  return(op)
}

# -------------------------------------------------------------------------
Simulate.Trading <- function(sim, Process = "Development")
{
  if(nrow(sim) == 0) {return(data.frame())}
  
  sim2 <- foreach(i = unique(sim$ID), .errorhandling = 'remove', .combine = bind_rows) %do%
  {
    # i = 1
    sim2 <- sim %>%
          filter(ID == i) %>% group_by(ID) %>% arrange(ID, ds) %>%
          mutate(buy.window = case_when(R.buy > 0 ~ 1)) %>%
          AF.roll(df = ., var = "buy.window", width = 3) %>%
          mutate(buy.bin = case_when(!is.na(R.buy) ~ R)
                 , buy.bin = zoo::na.locf(buy.bin, na.rm = FALSE)
                 , sell.window = ifelse(!is.na(lag(buy.window)) | !is.na(buy.window), 1, NA)
                 , Type = zoo::na.locf(Type, na.rm = FALSE)
                 , Type = case_when(sell.window == 1 ~ Type)
                 , signal = 1.0*AF.Signal.Strength(window = sell.window, Type)

                 , buy.price  = round(R.buy*lag(close), 2)
                 , buy.price  = buy.window*zoo::na.locf(buy.price, na.rm = FALSE)

                 , sell.price = round(R.sell*lag(close), 2)
                 , sell.price = sell.window*zoo::na.locf(sell.price, na.rm = FALSE)
                 
                 , stop.price = ifelse(Type == "LONG",
                                       buy.price*(1 - R.stop),
                                       buy.price*(1 + R.stop))
                 , stop.price = sell.window*zoo::na.locf(stop.price, na.rm = FALSE)
                 , stop.price = round(stop.price, 2)

                 , last.sell = case_when(sell.window == 1 & is.na(buy.window) & !is.na(close) ~ close,
                                         sell.window == 1 & is.na(buy.window) & is.na(close) ~ 0,
                                         (Type == "LONG" & lag(Type) == "SHRT") |
                                           (Type == "SHRT" & lag(Type) == "LONG") ~ open )
          ) %>%
          AF.simulate.ts(., Process)
    
    rm(i)
    if(!all(is.na(sim2$ROI))) {return(sim2)}
  }
  
  return(sim2)
}

# -------------------------------------------------------------------------
Simulated.Performance.Bin <- function(sim, validation.period)
{
  x <- sim %>%
    arrange(ticker, ID, ds) %>%
    filter(!is.na(signal)) %>%
    mutate(ROR = ROI^(1/invest.period),
           bought.on = case_when(action == "BUY" ~ ds)) %>%
    tidyr::fill(bought.on) %>%
    rename(sold.on = ds, sell.type = action) %>%
    select(ticker, Type, bought.on, sold.on, invest.period, capacity, ROI, ROR,
           sell.type, ID, buy.bin, DP.Method, MA.Type, Period) %>%
    na.omit() %>% ungroup() %>% rename(R = buy.bin) %>%
    mutate(ds.Type = ifelse(bought.on <= Parms[["last.dev.date"]], "Dev",
                     ifelse(bought.on <= Parms[["last.dev.date"]] + validation.period, "Val", "XXX"))) %>%
    group_by(ticker, Type, ID, R, ds.Type) %>%
    summarise(Trades = n(), ROI = mean(ROI)) %>%
    data.table::setDT() %>%
    data.table::dcast(ticker + ID + R + Type ~ ds.Type, value.var = c("Trades", "ROI"), sep = "." )
  return(x)

}

Simulated.Performance.Model <- function(sim, validation.period)
{
  x <- sim %>%
    arrange(ticker, ID, ds) %>%
    filter(!is.na(signal)) %>%
    mutate(ROR = ROI^(1/invest.period),
           bought.on = case_when(action == "BUY" ~ ds)) %>%
    tidyr::fill(bought.on) %>%
    rename(sold.on = ds, sell.type = action) %>%
    select(ticker, Type, bought.on, sold.on, invest.period, capacity, ROI, ROR,
           sell.type, ID, buy.bin, DP.Method, MA.Type, Period) %>%
    na.omit() %>% ungroup() %>% rename(R = buy.bin) %>%
    mutate(ds.Type = ifelse(bought.on <= Parms[["last.dev.date"]], "Dev",
                            ifelse(bought.on <= Parms[["last.dev.date"]] + validation.period, "Val", "XXX"))) %>%
    group_by(ticker, ID, ds.Type) %>%
    summarise(Trades = n(), ROI = mean(ROI), Period = mean(invest.period)) %>%
    data.table::setDT() %>%
    data.table::dcast(ticker + ID ~ ds.Type, value.var = c("Trades", "ROI", "Period"), sep = "." )
  return(x)
  
}
