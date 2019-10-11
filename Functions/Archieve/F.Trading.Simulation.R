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

AF.Signal.Strength <- function(window)
{
  N <- length(window)
  strength <- rep(NA, N)
  op <- 0
  for(i in 1:N)
  {
    if(is.na(window[i])) {op <- 0}
    else {op <- op + 1}
    
    if(op > 0) {strength[i] <- op}
    rm(i)
  }
  
  rm(N, op)
  return(strength)
}

    
# -------------------------------------------------------------------------
# AF with stop loss
# Trading stops in a buy signal sequence after a stop loss
# For long trades only
# -------------------------------------------------------------------------
AF.simulate.trade <- function(df, Type = "Development")
{
  buy.signal <- ifelse(!is.na(df$buy.price) & !is.na(df$low) & df$buy.price >= df$low, TRUE, FALSE)
  sell.signal <- ifelse(!is.na(df$sell.price) & !is.na(df$high) & df$sell.price <= df$high, TRUE, FALSE)
  
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
  holding.days <- 0
  trade.val <- 0
  continue <- 1 # To Prevent Buying After Stoploss in a series

  action <- rep(NA, nrow(df))
  capacity <- rep(NA, nrow(df))
  ROI <- rep(NA, nrow(df))
  invest.period <- rep(NA, nrow(df))
  continue.trading <- rep(NA, nrow(df))

  for(i in 1:nrow(df))
  {
    if(buy.price[i] == 0) {continue <- 1}
    continue.trading[i] <- continue
    
    # Logic: Buy Or Sell. If buy, don't sell (Because we don't know the sequence of low and high)
    if(isTRUE(buy.signal[i]) & cost == 0 & continue > 0) # Buy only once in a series
    {
      action[i] <- "BUY"

      cost <- buy.price[i]
      holding.days <- 1
      trade.val <- round(df$volume[i]*(df$open[i] + df$close[i])/2, 0)
      capacity[i] <- trade.val

    } else
      if(isTRUE(sell.signal[i]) & cost > 0) # Sell
      {
        action[i] <- "SELL @ Model Price"
        capacity[i] <- trade.val
        ROI[i] <- sell.price[i]/cost
        invest.period[i] <- holding.days + 1
        
        cost <- 0
        holding.days <- 0
        trade.val <- 0
        
      } else 
        if(cost > 0) # Hold
        {
          action[i] <- "HOLD"
          holding.days <- holding.days + 1
          if(trade.val > 0) {capacity[i] <- trade.val}
        }
    
    if(i < nrow(df) & cost > 0) # Last Day of data (No Trading Yet)
    {
      # Sell @ stop loss
      # Rule: Do Not Stop Loss on Purchase Day
      if(cost > 0 & holding.days > 1 & df$low[i] <= stop.price[i]) 
      {
        action[i] <- "SELL @ Stop Loss"
        capacity[i] <- trade.val
        ROI[i] <- stop.price[i]/cost
        invest.period[i] <- holding.days
        
        cost <- 0
        holding.days <- 0
        trade.val <- 0
        continue <- 0
      }
  
      # Sell @ Close if still to be sold
      if(cost > 0 & last.sell[i] > 0)
      {
        action[i] <- "SELL @ Closing"
        capacity[i] <- trade.val
        ROI[i] <- last.sell[i]/cost
        invest.period[i] <- holding.days
  
        cost <- 0
        holding.days <- 0
        trade.val <- 0
      }
    }

  }
  
  if(Type == "Development")
  {op <- df %>% select(-c(buy.window, sell.window, volume, open, low, high, close, ROI.l, ROI.h))} 
  
  if(Type == "Production")
  {op <- df %>% select(-c(buy.window, sell.window, R.low, R.high, R.buy, R.sell, R.stop, ROI.l, ROI.h))}
  
  op <- bind_cols(op, data.frame(continue.trading, action, capacity, ROI, invest.period
                                 , stringsAsFactors = FALSE)) %>%
        mutate(buy.price = continue.trading*buy.price
               , sell.price = case_when(continue.trading > 0 ~ sell.price)
               , stop.price = case_when(continue.trading > 0 ~ stop.price)
               , last.sell = case_when(continue.trading > 0 ~ last.sell)
               ) %>%
        select(-continue.trading)

  rm(buy.signal, sell.signal, buy.price, sell.price, stop.price, last.sell
     , cost, holding.days, trade.val, continue
     , action, capacity, ROI, invest.period, continue.trading, df, i, clean.x)
  gc()

  return(op)
}

# -------------------------------------------------------------------------
# AF.simulate trade for long and short trades
# -------------------------------------------------------------------------
AF.simulate.hedge <- function(df, Process = "Development")
{
  # df = sim
  # Process = "Production"
  df <- df %>%
        arrange(ID, ds) %>%
        mutate(buy.signal = case_when(Type == "LONG" & buy.price >= low ~ TRUE,
                                      Type == "SHRT" & buy.price <= high ~ TRUE,
                                      TRUE ~ FALSE),
               sell.signal = case_when(Type == "LONG" & sell.price <= high ~ TRUE,
                                       Type == "SHRT" & sell.price >= low ~ TRUE,
                                       TRUE ~ FALSE),
               stop.signal = case_when(Type == "LONG" & stop.price >= low ~ TRUE,
                                       Type == "SHRT" & stop.price <= high ~ TRUE,
                                       TRUE ~ FALSE)
               )

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
  
  action <- rep(NA, nrow(df))
  capacity <- rep(NA, nrow(df))
  ROI <- rep(NA, nrow(df))
  invest.period <- rep(NA, nrow(df))
  continue.trading <- rep(NA, nrow(df))
  
  for(i in 1:nrow(df))
  {
    if(buy.price[i] == 0) {continue <- 1}
    continue.trading[i] <- continue
    
    # Logic: Buy Or Sell. If buy, don't sell (Because we don't know the sequence of low and high)
    # Buy only once in a series
    if(isTRUE(buy.signal[i]) & cost == 0 & continue > 0) 
    {
      action[i] <- "BUY"
      type <- df$Type[i]
      cost <- buy.price[i]
      holding.days <- 1
      trade.val <- round(df$volume[i]*(df$open[i] + df$close[i])/2, 0)
      capacity[i] <- trade.val
      
    } else
      if(isTRUE(sell.signal[i]) & cost > 0) # Sell
      {
        action[i] <- "TARGET SELL"
        capacity[i] <- trade.val
        ROI[i] <- case_when(type =="LONG" ~ sell.price[i]/cost,
                            type =="SHRT" ~ 2 - sell.price[i]/cost)
        invest.period[i] <- holding.days + 1
        
        cost <- 0
        type <- ""
        holding.days <- 0
        trade.val <- 0
        
      } else 
        if(cost > 0) # Hold
        {
          action[i] <- "HOLD"
          holding.days <- holding.days + 1
          if(trade.val > 0) {capacity[i] <- trade.val}
        }
    
    if(i < nrow(df) & cost > 0) # Last Day of data (No Trading Yet)
    {
      # Sell @ stop loss
      # Rule: Do Not Stop Loss on Purchase Day
      if(cost > 0 & holding.days > 1 & stop.signal[i]) 
      {
        action[i] <- "STOP SELL"
        capacity[i] <- trade.val
        ROI[i] <- case_when(Type =="LONG" ~ stop.price[i]/cost,
                            Type =="SHRT" ~ 2 - stop.price[i]/cost)
        
        invest.period[i] <- holding.days
        
        cost <- 0
        type <- NA
        holding.days <- 0
        trade.val <- 0
        continue <- 0
      }
      
      # Sell @ Close if still to be sold
      if(cost > 0 & last.sell[i] > 0)
      {
        action[i] <- "EOD SELL"
        capacity[i] <- trade.val
        ROI[i] <- last.sell[i]/cost
        ROI[i] <- case_when(type =="LONG" ~ last.sell[i]/cost,
                            type =="SHRT" ~ 2 - last.sell[i]/cost)
        invest.period[i] <- holding.days
        
        cost <- 0
        type <- NA
        holding.days <- 0
        trade.val <- 0
      }
    }
    
  }
  
  if(Process == "Development")
  {
    op <- df %>% select(-c(buy.window, sell.window, volume, open, low, high, close, ROI.l, ROI.h,
                           buy.signal, sell.signal, stop.signal))
  } 
  
  if(Process == "Production")
  {
    op <- df %>% select(-c(buy.window, sell.window, R.low, R.high, R.buy, R.sell, R.stop, ROI.l, ROI.h,
                           buy.signal, sell.signal, stop.signal))
  }
  
  op <- bind_cols(op, data.frame(continue.trading, action, capacity, ROI, invest.period
                                 , stringsAsFactors = FALSE)) %>%
        mutate(buy.price = continue.trading*buy.price
               , sell.price = case_when(continue.trading > 0 ~ sell.price)
               , stop.price = case_when(continue.trading > 0 ~ stop.price)
               , last.sell = case_when(continue.trading > 0 ~ last.sell)
        ) %>%
        select(-continue.trading)
  
  rm(buy.signal, sell.signal, stop.signal, buy.price, sell.price, stop.price, last.sell
     , cost, type, holding.days, trade.val, continue
     , action, capacity, ROI, invest.period, continue.trading, df, i, clean.x, Process)
  gc()
  
  return(op)
}

# -------------------------------------------------------------------------
AF.simulate.20190802 <- function(sim)
{
  if(nrow(sim) == 0) {return(data.frame())}
  
  sim <- sim %>%
          group_by(ID) %>% arrange(ID, ds) %>%
          mutate(buy.window = case_when(R.buy > 0 ~ 1)) %>%
          AF.roll(df = ., var = "buy.window", width = 3) %>%
          mutate(sell.window = ifelse(!is.na(lag(buy.window)) | !is.na(buy.window), 1, NA)
                 , signal = AF.Signal.Strength(window = sell.window)
                 
                 # Adjusted for Missed Buys
                 , buy.window = case_when(is.na(missed) ~ buy.window,
                                          !is.na(missed) & missed != "BUY" ~ buy.window)
                 
                 , sell.window = case_when(is.na(missed) ~ sell.window,
                                           !is.na(missed) & missed != "BUY" ~ sell.window)
                 
                 , buy.price  = round(R.buy*lag(close), 2)
                 , buy.price  = buy.window*zoo::na.locf(buy.price, na.rm = FALSE)
                 , sell.price = round(R.sell*lag(close), 2)
                 , sell.price = sell.window*zoo::na.locf(sell.price, na.rm = FALSE)
                 , stop.price = round(R.stop*lag(close), 2)
                 , stop.price = sell.window*zoo::na.locf(stop.price, na.rm = FALSE)
                 
                 , last.sell = case_when(sell.window == 1 & is.na(buy.window) & !is.na(close) ~ close,
                                         sell.window == 1 & is.na(buy.window) & is.na(close) ~ 0
                 )
          ) %>%
          select(-c(signal, missed)) %>%
          AF.simulate.trade(., Type = "Production") 
  
  return(sim)
}

# -------------------------------------------------------------------------
AF.simulate.20190821 <- function(sim)
{
  if(nrow(sim) == 0) {return(data.frame())}
  
  sim <- sim %>%
    group_by(ID) %>% arrange(ID, ds) %>%
    mutate(buy.window = case_when(R.buy > 0 ~ 1)) %>%
    AF.roll(df = ., var = "buy.window", width = 3) %>%
    mutate(sell.window = ifelse(!is.na(lag(buy.window)) | !is.na(buy.window), 1, NA)
           , signal = AF.Signal.Strength(window = sell.window)
           
           # Adjusted for Missed Buys
           , buy.window = case_when(is.na(missed) ~ buy.window,
                                    !is.na(missed) & missed != "BUY" ~ buy.window)
           
           , sell.window = case_when(is.na(missed) ~ sell.window,
                                     !is.na(missed) & missed != "BUY" ~ sell.window)
           
           , buy.price  = round(R.buy*lag(close), 2)
           , buy.price  = buy.window*zoo::na.locf(buy.price, na.rm = FALSE)
           , sell.price = round(R.sell*lag(close), 2)
           , sell.price = sell.window*zoo::na.locf(sell.price, na.rm = FALSE)
           # Prorated Stop Loss 
           , stop.price = R.stop*lag(close)
           , stop.price = sell.window*zoo::na.locf(stop.price, na.rm = FALSE)
           , stop.price = case_when(signal <= 5 ~ (0.94 + signal*0.01)*stop.price,
                                    signal >  5 ~ stop.price)
           , stop.price = round(stop.price, 2)
           , last.sell = case_when(sell.window == 1 & is.na(buy.window) & !is.na(close) ~ close,
                                   sell.window == 1 & is.na(buy.window) & is.na(close) ~ 0
           )
    ) %>%
    select(-c(signal, missed)) %>%
    AF.simulate.trade(., Type = "Production") 
  
  return(sim)
}