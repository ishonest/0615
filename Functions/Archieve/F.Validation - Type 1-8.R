# ########################################################################
# Validation in Out of Development Window
# Tried 3 different methods and selected on Type 3 (Most Practical)
# ########################################################################

# -------------------------------------------------------------------------
# Function to Plot Validation Results
# -------------------------------------------------------------------------
Type.01.Plot <- function(all.vals)
{
  library(plotly)

  sec.y <- list(overlaying = "y", side = "right", tickformat = '.1%')
  margins <- list(l = 60, r = 60, b = 25, t = 25)
  xfactor <- all.vals$factor[1]

  p1 <- plot_ly(data = all.vals, x = ~min.factor,
               y = ~Annual.Returns, name = "Annual.Returns", type = 'scatter', mode = 'lines') %>%
        add_trace(y = ~ROI, name = 'Mean ROI', yaxis = "y2") %>%
        layout(title = paste0("Financial Performance", ": By ", xfactor),
               xaxis = list(title = ""), yaxis = list(tickformat = '.1f'), yaxis2 = sec.y,
               legend = list(orientation = 'h'), hovermode = 'compare', margin = margins)


  p2 <- plot_ly(data = all.vals, x = ~min.factor,
          y = ~Holding.Period, name = "Holding.Period", type = 'scatter', mode = 'lines') %>%
        add_trace(y = ~SR.02, name = 'SR @ 2%', yaxis = "y2") %>%
        add_trace(y = ~SR.factor, name = paste('SR @', xfactor), yaxis = "y2") %>%
        layout(title = paste0("Operational Performance", ": By ", xfactor),
               xaxis = list(title = ""), yaxis = list(tickformat = '.1f'), yaxis2 = sec.y,
               legend = list(orientation = 'h'), hovermode = 'compare', margin = margins)

  p3 <- plot_ly(data = all.vals, x = ~min.factor,
          y = ~Trades, name = "Trades Since Feb", type = 'scatter', mode = 'lines') %>%
        add_trace(y = ~capacity, name = 'Weekly Capacity ($ MM)') %>%
        add_trace(y = ~ROI, name = 'Mean ROI', yaxis = "y2") %>%
        layout(title = paste0("Capacity Management", ": By ", xfactor),
               xaxis = list(title = ""), yaxis = list(tickformat = '.0f'), yaxis2 = sec.y,
               legend = list(orientation = 'h'), hovermode = 'compare', margin = margins)

  print(p1)
  print(p2)
  print(p3)
  rm(sec.y, margins, xfactor, p1, p2, p3)

}

# -------------------------------------------------------------------------
# Type 1 Validation: 
# Invest on every model equally (meeting the criterion)
# -------------------------------------------------------------------------
Val.Type.01 <- function(all.models,
                             xfactor = "ROR",
                             xbreaks = seq(0.95, 1.1, 0.002),
                             last.dev.date = as.Date("2019-01-31"))
{
  library(foreach)
  
  x3 <- foreach(min.factor = xbreaks
                , .combine = bind_rows, .packages = "dplyr"
                , .multicombine = TRUE, .inorder = FALSE, .errorhandling = 'remove') %dopar%
                {
                  x3 <- all.models %>% #filter(ticker == "MDB") %>%
                        filter(bought.on > last.dev.date) %>%
                        group_by(ticker, DP.Method, MA.Type, Period) %>%
                        arrange(ticker, DP.Method, MA.Type, Period, bought.on) %>%
                        mutate(wk = format(bought.on,"%Y-%V"),
                               H = lag(get(xfactor))
                        ) %>%
                        filter(H >= min.factor) %>%
                        group_by(wk) %>%
                        summarise(N = n()
                                  , capacity = sum(capacity)
                                  , Holding.Period = mean(invest.period)
                                  , SR.02 = mean(ROI >= 1.02)
                                  , SR.factor = mean(get(xfactor) >= min.factor)
                                  , ROI = mean(ROI)
                                  , ROR = mean(ROR)
                        ) %>%
                        ungroup() %>%
                        summarise(factor = xfactor
                                  , min.factor
                                  , Trades = sum(N)
                                  , capacity = median(capacity)
                                  , Holding.Period = weighted.mean(Holding.Period, w = N)
                                  , SR.02 = weighted.mean(SR.02, w = N)
                                  , SR.factor = weighted.mean(SR.factor, w = N)
                                  , ROI = mean(ROI)
                                  , ROR = mean(ROR)
                                  , Annual.Returns = ROR^250
                                  )
                  rm(min.factor)
                  return(x3)
                }
  
  x3 <- x3 %>% arrange(min.factor)
  Type.01.Plot(all.vals = x3)
  rm(xfactor, xbreaks, last.dev.date, x3)
}

# -------------------------------------------------------------------------
# Type 2 Validation: 
# Invest on the first trades of week equally (meeting the criterion)
# -------------------------------------------------------------------------
Val.Type.02 <- function(all.models,
                             xfactor = "ROR",
                             xbreaks = seq(0.95, 1.1, 0.002),
                             last.dev.date = as.Date("2019-01-31"))
{
  library(foreach)
  
  x3 <- foreach(min.factor = xbreaks
                , .combine = bind_rows, .packages = "dplyr"
                , .multicombine = TRUE, .inorder = FALSE, .errorhandling = 'remove') %dopar%
                {
                  # xfactor = "ROR"
                  # xbreaks = seq(0.95, 1.1, 0.002)
                  # last.dev.date = as.Date("2019-01-31")
                  # min.factor = xbreaks[1]
                  
                  x3 <- all.models %>% #filter(ticker == "MDB") %>%
                        filter(bought.on > last.dev.date) %>%
                        group_by(ticker, DP.Method, MA.Type, Period) %>%
                        arrange(ticker, DP.Method, MA.Type, Period, bought.on) %>%
                        mutate(wk = format(bought.on,"%Y-%V"),
                               H = lag(get(xfactor))
                        ) %>%
                        filter(H >= min.factor) %>%
                        group_by(ticker, wk) %>%
                        filter(bought.on == min(bought.on)) %>%
                        group_by(wk) %>%
                        summarise(N = n()
                                  , capacity = sum(capacity)
                                  , Holding.Period = mean(invest.period)
                                  , SR.02 = mean(ROI >= 1.02)
                                  , SR.factor = mean(get(xfactor) >= min.factor)
                                  , ROI = mean(ROI)
                                  , ROR = mean(ROR)
                        ) %>%
                        ungroup() %>%
                        summarise(factor = xfactor
                                  , min.factor
                                  , Trades = sum(N)
                                  , capacity = median(capacity)
                                  , Holding.Period = weighted.mean(Holding.Period, w = N)
                                  , SR.02 = weighted.mean(SR.02, w = N)
                                  , SR.factor = weighted.mean(SR.factor, w = N)
                                  , ROI = mean(ROI)
                                  , ROR = mean(ROR)
                                  , Annual.Returns = ROR^250
                        )
                  rm(min.factor)
                  return(x3)
                }
  
  x3 <- x3 %>% arrange(min.factor)
  Type.01.Plot(all.vals = x3)
  rm(xfactor, xbreaks, last.dev.date, x3)
}

# -------------------------------------------------------------------------
# Type 3 Validation: 
# Invest in all trades in a week until 10 trades (meeting the criterion)
# -------------------------------------------------------------------------
Val.Type.03 <- function(all.models,
                             maxtrades = 10,
                             xfactor = "ROR",
                             xbreaks = seq(0.95, 1.1, 0.002),
                             last.dev.date = as.Date("2019-01-31"))
{
  library(foreach)
  
  x3 <- foreach(min.factor = xbreaks
                , .combine = bind_rows, .packages = "dplyr"
                , .multicombine = TRUE, .inorder = FALSE, .errorhandling = 'remove') %dopar%
                {
                  # xfactor = "ROR"
                  # xbreaks = seq(0.95, 1.1, 0.002)
                  # last.dev.date = as.Date("2019-01-31")
                  # maxtrades = 10
                  # min.factor = xbreaks[1]
                  
                  x3 <- all.models %>% #filter(ticker == "MDB") %>%
                        filter(bought.on > last.dev.date) %>%
                        group_by(ticker, DP.Method, MA.Type, Period) %>%
                        arrange(ticker, DP.Method, MA.Type, Period, bought.on) %>%
                        mutate(wk = format(bought.on,"%Y-%V"),
                               H = lag(get(xfactor))
                        ) %>%
                        filter(H >= min.factor) %>%
                        group_by(wk) %>%
                        arrange(wk, bought.on) %>%
                        top_n(n = maxtrades, wt = bought.on) %>%
                        summarise(N = n()
                                  , capacity = sum(capacity)
                                  , Holding.Period = mean(invest.period)
                                  , SR.02 = mean(ROI >= 1.02)
                                  , SR.factor = mean(get(xfactor) >= min.factor)
                                  , ROI = mean(ROI)
                                  , ROR = mean(ROR)
                        ) %>%
                        ungroup() %>%
                        summarise(factor = xfactor
                                  , min.factor
                                  , Trades = sum(N)
                                  , capacity = median(capacity)
                                  , Holding.Period = weighted.mean(Holding.Period, w = N)
                                  , SR.02 = weighted.mean(SR.02, w = N)
                                  , SR.factor = weighted.mean(SR.factor, w = N)
                                  , ROI = mean(ROI)
                                  , ROR = mean(ROR)
                                  , Annual.Returns = ROR^250
                        )
                  
                  rm(min.factor)
                  return(x3)
                }
  
  x3 <- x3 %>% arrange(min.factor)
  Type.01.Plot(all.vals = x3)
  rm(xfactor, xbreaks, last.dev.date, x3, maxtrades)
}

# -------------------------------------------------------------------------
# Type 4 Validation:
# Invest with a fixed amount in bulks of max.lot
# -------------------------------------------------------------------------
Type.04.Plot <- function(df)
{
  library(plotly)
  
  df <- df %>%
        mutate(color = as.character(- min.Type)
               , text = paste0(Type, ": ", min.Type, 
                               " | Trades: ", Trades,
                               " | ROI: ", round(100*ROI, 0), "%",
                               " | Annual ROI: ", round(Annual.Returns, 2), "x")) %>%
        group_by(min.Type)
    
  
  p1 <- plot_ly(df, x = ~min.factor) %>%
        add_trace(y = ~Annual.Returns, name = "Annual.Returns", type = 'scatter', mode = 'lines+markers'
                  , text = ~text
                  , hoverinfo = 'text'
                  , marker = list(fill = ~color, colors = "RdBu")
                  , color = ~color, colors = "RdBu"
        ) %>%
        layout(title = paste("Future Potential Across Trades: By", df$Type[1]),
               xaxis = list(title = df$factor[1]), yaxis = list(tickformat = '.1f'), 
               showlegend = FALSE) 
  
  
  p2 <- plot_ly(df, x = ~min.factor) %>%
        add_trace(y = ~ROI, name = "ROI", type = 'scatter', mode = 'lines+markers'
                  , text = ~text
                  , hoverinfo = 'text'
                  , marker = list(fill = ~color, colors = "YlOrRd")
                  , color = ~color, colors = "YlOrRd"
                  ) %>%
        layout(title = paste("Historical Performance Across Trades: By", df$Type[1]),
               xaxis = list(title = df$factor[1]), yaxis = list(tickformat = '.0%'), 
               showlegend = FALSE)
  
  
  p3 <- plot_ly(df, x = ~min.factor) %>%
        add_trace(y = ~SR.02, name = "Strike Rate", type = 'scatter', mode = 'lines+markers'
                  , text = ~text
                  , hoverinfo = 'text'
                  , marker = list(fill = ~color, colors = "Greens")
                  , color = ~color, colors = "Greens"
                  ) %>%
        layout(title = paste("Strike Rate Across Trades: By", df$Type[1]),
               xaxis = list(title = df$factor[1]), 
               yaxis = list(tickformat = '.0%', title = "Strike Rate"), 
               showlegend = FALSE)
  
  
  print(p1)
  print(p2)
  print(p3)
  rm(p1, p2, p3, df)
  
}
# -------------------------------------------------------------------------
Val.Type.04 <- function(all.models,
                             xfactor = "ROR",
                             xbreaks = seq(0.85, 1.1, 0.005),
                             max.lot = 20,
                             last.dev.date = as.Date("2019-01-31"))
{
  library(foreach)

  x3 <- foreach(min.value = xbreaks, .combine = bind_rows) %do%
        {
          min.ROR = 0
          min.ROI = 0
          min.capacity = 0
          min.invest.period = 0
          assign(paste0("min.", xfactor), min.value)

          x3 <- all.models %>% #filter(ticker == "MDB") %>%
                filter(bought.on > last.dev.date) %>%
                group_by(ticker, DP.Method, MA.Type, Period) %>%
                arrange(ticker, DP.Method, MA.Type, Period, bought.on) %>%
                mutate(HROR = lag(ROR)
                       , HROI = lag(ROI)
                       , HCAP = lag(capacity)
                       , HDUR = lag(invest.period)
                ) %>%
                filter(HROR >= min.ROR, HROI >= min.ROI,
                       HCAP >= min.capacity, HDUR >= min.invest.period) %>%
                ungroup() %>%
                select(-c(DP.Method, MA.Type, Period, HROR, HROI, HCAP, HDUR)) %>%
                arrange(bought.on, sold.on)


          x4 <- foreach(maxtrades = seq(1, max.lot, 1)
                        , .combine = bind_rows, .packages = "dplyr"
                        , .multicombine = TRUE, .inorder = FALSE
                        , .errorhandling = 'remove') %dopar%
                  {

                    bank <- unique(c(all.models$bought.on, all.models$sold.on))
                    bank <- bank[bank > last.dev.date]
                    bank <- c(bank, max(bank) + 1)
                    bank <- data.frame(ds = bank, capital = 10000, trades = 0, missed = 0,
                                       capacity = 0, duration = 0, SR.02 = 0,
                                       stringsAsFactors = FALSE) %>% arrange(ds)

                    n <- nrow(bank)

                    for(i in 1:nrow(x3))
                    {
                      r.buy <- which(bank$ds == x3$bought.on[i])
                      r.sell <- which(bank$ds == x3$sold.on[i]) + 1

                      if(bank$capital[r.buy] > 0)
                      {
                        invest <- min(bank$capital[r.buy], 10000/maxtrades)
                        bank$capital[r.buy:n] <- bank$capital[r.buy:n] - invest
                        bank$capital[r.sell:n] <- bank$capital[r.sell:n] + invest*x3$ROI[i]

                        bank$trades[r.buy] <- bank$trades[r.buy] + 1
                        bank$capacity[r.buy] <- bank$capacity[r.buy] + x3$capacity[i]
                        bank$duration[r.buy] <- bank$duration[r.buy] + x3$invest.period[i]

                        bank$SR.02[r.buy] <- bank$SR.02[r.buy] + ifelse(x3$ROI[i] >= 1.02, 1, 0)

                        rm(invest)
                      } else
                      {
                        bank$missed[r.buy] <- bank$missed[r.buy] + 1
                      }

                      rm(r.buy, r.sell, i)
                    }

                    x4 <- bank %>%
                          summarise(factor = "Max Trades"
                                    , min.factor = maxtrades
                                    , Trades = sum(trades)
                                    , Missed = sum(missed)
                                    , capacity = sum(capacity)/Trades
                                    , Holding.Period = sum(duration)/Trades
                                    , SR.02 = sum(SR.02)/Trades
                                    , SR.factor = Trades/(Trades + Missed)
                                    , Total.ROI = last(capital)/10000
                                    , ROI = Total.ROI
                                    , ROR = Total.ROI^(1/(n()-1))
                                    , Annual.Returns = ROR^250) %>%
                          mutate(Type = xfactor, min.Type = min.value)

                    rm(maxtrades, bank, n)
                    return(x4)
                  }

          rm(min.ROR, min.ROI, min.capacity, min.invest.period)
          return(x4)
        }

  x3 <- x3 %>% arrange(min.Type, min.factor)
  rm(last.dev.date, max.lot, xfactor, xbreaks)
  Type.04.Plot(x3)
  rm(x3)
}

# -------------------------------------------------------------------------
# Type 5 Validation:
# Invest with a on.hand / N
# -------------------------------------------------------------------------
Val.Type.05 <- function(all.models,
                             xfactor = "ROR",
                             xbreaks = seq(0.85, 1.1, 0.005),
                             max.lot = 20,
                             last.dev.date = as.Date("2019-01-31"))
{
  library(foreach)

  x3 <- foreach(min.value = xbreaks, .combine = bind_rows) %do%
        {
          min.ROR = 0
          min.ROI = 0
          min.capacity = 0
          min.invest.period = 0
          assign(paste0("min.", xfactor), min.value)

          x3 <- all.models %>% #filter(ticker == "MDB") %>%
                filter(bought.on > last.dev.date) %>%
                group_by(ticker, DP.Method, MA.Type, Period) %>%
                arrange(ticker, DP.Method, MA.Type, Period, bought.on) %>%
                mutate(HROR = lag(ROR)
                       , HROI = lag(ROI)
                       , HCAP = lag(capacity)
                       , HDUR = lag(invest.period)
                ) %>%
                filter(HROR >= min.ROR, HROI >= min.ROI,
                       HCAP >= min.capacity, HDUR >= min.invest.period) %>%
                ungroup() %>%
                select(-c(DP.Method, MA.Type, Period, HROR, HROI, HCAP, HDUR)) %>%
                arrange(bought.on, sold.on)


          x4 <- foreach(maxtrades = seq(1, max.lot, 1)
                        , .combine = bind_rows, .packages = "dplyr"
                        , .multicombine = TRUE, .inorder = FALSE
                        , .errorhandling = 'remove') %dopar%
                  {

                    bank <- unique(c(all.models$bought.on, all.models$sold.on))
                    bank <- bank[bank > last.dev.date]
                    bank <- c(bank, max(bank) + 1)
                    bank <- data.frame(ds = bank, capital = 10000, trades = 0, missed = 0,
                                       capacity = 0, duration = 0, SR.02 = 0,
                                       stringsAsFactors = FALSE) %>% arrange(ds)

                    n <- nrow(bank)

                    for(i in 1:nrow(x3))
                    {
                      r.buy <- which(bank$ds == x3$bought.on[i])
                      r.sell <- which(bank$ds == x3$sold.on[i]) + 1

                      if(bank$capital[r.buy] > maxtrades*100) #i.e. minimum invest of 100
                      {
                        invest <- bank$capital[r.buy]/maxtrades
                        bank$capital[r.buy:n] <- bank$capital[r.buy:n] - invest
                        bank$capital[r.sell:n] <- bank$capital[r.sell:n] + invest*x3$ROI[i]

                        bank$trades[r.buy] <- bank$trades[r.buy] + 1
                        bank$capacity[r.buy] <- bank$capacity[r.buy] + x3$capacity[i]
                        bank$duration[r.buy] <- bank$duration[r.buy] + x3$invest.period[i]

                        bank$SR.02[r.buy] <- bank$SR.02[r.buy] + ifelse(x3$ROI[i] >= 1.02, 1, 0)

                        rm(invest)
                      } else
                      {
                        bank$missed[r.buy] <- bank$missed[r.buy] + 1
                      }

                      rm(r.buy, r.sell, i)
                    }

                    x4 <- bank %>%
                          summarise(factor = "Max Trades"
                                    , min.factor = maxtrades
                                    , Trades = sum(trades)
                                    , Missed = sum(missed)
                                    , capacity = sum(capacity)/Trades
                                    , Holding.Period = sum(duration)/Trades
                                    , SR.02 = sum(SR.02)/Trades
                                    , SR.factor = Trades/(Trades + Missed)
                                    , Total.ROI = last(capital)/10000
                                    , ROI = Total.ROI
                                    , ROR = Total.ROI^(1/(n()-1))
                                    , Annual.Returns = ROR^250) %>%
                          mutate(Type = xfactor, min.Type = min.value)

                    rm(maxtrades, bank, n)
                    return(x4)
                  }

          rm(min.ROR, min.ROI, min.capacity, min.invest.period)
          return(x4)
        }

  x3 <- x3 %>% arrange(min.Type, min.factor)
  rm(last.dev.date, max.lot, xfactor, xbreaks)
  Type.04.Plot(x3)
  rm(x3)
}

# -------------------------------------------------------------------------
# Type 6 Validation: 
# -------------------------------------------------------------------------
Val.Type.06 <- function(all.models, all.forecasts,
                             max.lot = 30, last.dev.date = as.Date("2019-01-31"),
                             Type = "BUY DATE" ) # "END OF PERIOD" or "BUY DATE"
{
  
  library(foreach)
  
  # all.models <- readRDS(paste0(data.folder, "Summary/20190615.All.Models.rds"))
  # all.forecasts <- readRDS(paste0(data.folder, "Summary/20190615.All.Forecasts.rds"))

  if(Type == "END OF PERIOD") # End of Validation Period Overfitting Index
  {
    x1 <- all.forecasts %>%
          filter(ds > last.dev.date) %>%
          group_by(ticker, ID) %>%
          summarise(Overfit.Index = sum(!is.na(buy.price))/n()) %>%
          inner_join(all.models, by = c("ticker", "ID"))
    
  } else if(Type == "BUY DATE") # Rolling Overfitting Index
  {
    x1 <- all.forecasts %>%
          filter(ds > last.dev.date) %>%
          group_by(ticker, ID) %>%
          arrange(ticker, ID, ds) %>%
          mutate(Overfit.Index = cumsum(!is.na(buy.price))/row_number()) %>%
          select(ticker, ID, ds, Overfit.Index) %>%
          rename(bought.on = ds) %>%
          inner_join(all.models, by = c("ticker", "ID", "bought.on"))
  } else 
  {
    return(cat("Invalid Type: Use Type as ROLLING or END OF PERIOD"))
  }   
  
  x2 <- foreach(max.value = seq(0, 1, 0.02)
                , .combine = bind_rows, .packages = "dplyr"
                , .multicombine = TRUE, .inorder = FALSE
                , .errorhandling = 'remove') %dopar%
  {
    x3 <- x1 %>%
          filter(bought.on > last.dev.date, Overfit.Index <= max.value) %>%
          group_by(ticker, bought.on) %>% 
          arrange(ticker, bought.on) %>%
          top_n(3, wt = -ID) %>%                      # Buy Upto 3 models a day
          ungroup() %>% 
          # arrange(bought.on, desc(sold.on) ) %>%    # Worst Case Scenario
          # arrange(bought.on, sold.on) %>%           # Best Case Scenario
          # arrange(bought.on) %>%                      # Random Scenario
          select(-c(DP.Method, MA.Type, Period))

    x4 <- data.frame()
    for(maxtrades in seq(2, max.lot, 2))
    {
      bank <- unique(c(all.models$bought.on, all.models$sold.on))
      bank <- bank[bank > last.dev.date]
      bank <- c(bank, max(bank) + 1)
      bank <- data.frame(ds = bank, capital = 10000, trades = 0, missed = 0,
                         capacity = 0, duration = 0, SR.02 = 0,
                         stringsAsFactors = FALSE) %>% arrange(ds)
      
      n <- nrow(bank)
      
      for(i in 1:nrow(x3))
      {
        r.buy <- which(bank$ds == x3$bought.on[i])
        r.sell <- which(bank$ds == x3$sold.on[i]) + 1
        
        if(bank$capital[r.buy] > 0)
        {
          invest <- min(bank$capital[r.buy], 10000/maxtrades)
          bank$capital[r.buy:n] <- bank$capital[r.buy:n] - invest
          bank$capital[r.sell:n] <- bank$capital[r.sell:n] + invest*x3$ROI[i]
          
          bank$trades[r.buy] <- bank$trades[r.buy] + 1
          bank$capacity[r.buy] <- bank$capacity[r.buy] + x3$capacity[i]
          bank$duration[r.buy] <- bank$duration[r.buy] + x3$invest.period[i]
          
          bank$SR.02[r.buy] <- bank$SR.02[r.buy] + ifelse(x3$ROI[i] >= 1.02, 1, 0)
          
          rm(invest)
        } else
        {
          bank$missed[r.buy] <- bank$missed[r.buy] + 1
        }
        
        rm(r.buy, r.sell, i)
      }
      
      x4 <- bind_rows(x4, 
                      bank %>% summarise(min.factor = maxtrades
                                         , min.Type = max.value
                                         , Trades = sum(trades)
                                         , Missed = sum(missed)
                                         , capacity = sum(capacity)/Trades
                                         , Holding.Period = sum(duration)/Trades
                                         , SR.02 = sum(SR.02)/Trades
                                         , SR.factor = Trades/(Trades + Missed)
                                         , ROI = last(capital)/10000
                                         , ROR = ROI^(1/(n()-1))
                                         , Annual.Returns = ROR^250
                                         , factor = "Max Trades"
                                         , Type = "Overfit Index"))
      
      rm(maxtrades, bank, n)
    }
    
    rm(x3, max.value)
    return(x4)
  }
  
  x2 <- x2 %>% arrange(min.Type, min.factor)
  Type.04.Plot(x2)
  rm(x1, x2, x4, max.lot, last.dev.date, Type)
  
}

# -------------------------------------------------------------------------
# Type 7 Validation: 
# -------------------------------------------------------------------------
Val.Type.07 <- function(all.models, all.forecasts,
                        max.lot = 30, last.dev.date = as.Date("2019-01-31"),
                        Type = "BUY DATE" ) # "END OF PERIOD" or "BUY DATE"
{
  
  # all.models <- readRDS(paste0(data.folder, "Summary/20190615.All.Models.rds"))
  # all.forecasts <- readRDS(paste0(data.folder, "Summary/20190615.All.Forecasts.rds"))
  
  if(Type == "END OF PERIOD") # End of Validation Period Overfitting Index
  {
    x1 <- all.forecasts %>%
          filter(ds > last.dev.date) %>%
          group_by(ticker, ID) %>%
          summarise(Overfit.Index = sum(!is.na(buy.price))/n()) %>%
          inner_join(all.models, by = c("ticker", "ID")) %>%
          filter(bought.on > last.dev.date)
      
  } else if(Type == "BUY DATE") # Rolling Overfitting Index
  {
    x1 <- all.forecasts %>%
      filter(ds > last.dev.date) %>%
      group_by(ticker, ID) %>%
      arrange(ticker, ID, ds) %>%
      mutate(Overfit.Index = cumsum(!is.na(buy.price))/row_number()) %>%
      select(ticker, ID, ds, Overfit.Index) %>%
      rename(bought.on = ds) %>%
      inner_join(all.models, by = c("ticker", "ID", "bought.on"))
  } else 
  {
    return(cat("Invalid Type: Use Type as ROLLING or END OF PERIOD"))
  }   
  
  x2 <- foreach(min.value = seq(0, 0.8, 0.04), .combine = bind_rows) %:%
        foreach(max.value = seq(min.value + 0.10, 1, 0.05), .combine = bind_rows
                , .packages = "dplyr", .errorhandling = 'remove'
                , .multicombine = TRUE, .inorder = FALSE) %dopar%
        {
          x3 <- x1 %>%
                filter(Overfit.Index >= min.value, Overfit.Index <= max.value) %>%
                group_by(ticker, bought.on) %>% 
                arrange(ticker, bought.on) %>%
                top_n(3, wt = -ID) %>%                      # Buy Upto 3 models a day
                ungroup() %>% 
                # arrange(bought.on, desc(sold.on) ) %>%    # Worst Case Scenario
                # arrange(bought.on, sold.on) %>%           # Best Case Scenario
                arrange(bought.on) %>%                      # Random Scenario
                select(-c(DP.Method, MA.Type, Period))
          
          if(nrow(x3) == 0){return(data.frame())}
          
          x4 <- data.frame()
          for(maxtrades in seq(2, max.lot, 2))
          {
            bank <- unique(c(all.models$bought.on, all.models$sold.on))
            bank <- bank[bank > last.dev.date]
            bank <- c(bank, max(bank) + 1)
            bank <- data.frame(ds = bank, capital = 10000, trades = 0, missed = 0,
                               capacity = 0, duration = 0, SR.02 = 0,
                               stringsAsFactors = FALSE) %>% arrange(ds)
              
            n <- nrow(bank)
              
            for(i in 1:nrow(x3))
            {
                r.buy <- which(bank$ds == x3$bought.on[i])
                r.sell <- which(bank$ds == x3$sold.on[i]) + 1
                
                if(bank$capital[r.buy] > 0)
                {
                  invest <- min(bank$capital[r.buy], 10000/maxtrades)
                  bank$capital[r.buy:n] <- bank$capital[r.buy:n] - invest
                  bank$capital[r.sell:n] <- bank$capital[r.sell:n] + invest*x3$ROI[i]
                  
                  bank$trades[r.buy] <- bank$trades[r.buy] + 1
                  bank$capacity[r.buy] <- bank$capacity[r.buy] + x3$capacity[i]
                  bank$duration[r.buy] <- bank$duration[r.buy] + x3$invest.period[i]
                  
                  bank$SR.02[r.buy] <- bank$SR.02[r.buy] + ifelse(x3$ROI[i] >= 1.02, 1, 0)
                  
                  rm(invest)
                } else
                {
                  bank$missed[r.buy] <- bank$missed[r.buy] + 1
                }
                
                rm(r.buy, r.sell, i)
              }
              
            x4 <- bind_rows(x4, bank %>% 
                                summarise(min.overfit = min.value
                                          , max.overfit = max.value
                                          , maxtrades
                                          , Trades = sum(trades)
                                          , Missed = sum(missed)
                                          , Miss.Rate = Missed/(Trades + Missed)
                                          , capacity = sum(capacity)/Trades
                                          , Holding.Period = sum(duration)/Trades
                                          , SR.02 = sum(SR.02)/Trades
                                          , ROI = last(capital)/10000
                                          , ROR = ROI^(1/(n()-1))
                                          , Annual.Returns = ROR^250)
                            )
          
            rm(bank, n, maxtrades)
          }

          rm(x3, min.value, max.value)
          return(x4)
        }
          

  x2 <- x2 %>% arrange(desc(Annual.Returns))
  rm(x1, max.lot, last.dev.date, Type)
  return(x2)
}

# -------------------------------------------------------------------------
# Type 8 Validation: 
# Best, Random, Worst case projections for a combination of 
# Last/Past Performance, Overfit Index Range, max Trades
# -------------------------------------------------------------------------
Val.Type.08 <- function(all.models, all.forecasts, last.dev.date)
{
  AF.Scenario.Planning <- function(df, Notes = "")
  {
    op <- data.frame()

    for(Scenario in c("Worst ROR", "Best ROR", "Random ROR"))
    {
      if(Scenario == "Worst ROR") {dfx <- df %>% arrange(bought.on, ROR)}
      if(Scenario == "Best ROR") {dfx <- df %>% arrange(bought.on, desc(ROR))}
      if(Scenario == "Random ROR") {dfx <- df %>% arrange(bought.on)}

      for(maxtrades in c(5, 8, 10, 15, 20, 25))
      {
        bank <- seq(from = min(dfx$bought.on), to = max(dfx$sold.on), by = 1)
        bank <- as.Date(setdiff(bank, as.Date(timeDate::holidayNYSE())), origin = "1970-01-01")
        bank <- bank[!(weekdays(bank) %in% c("Sunday", "Saturday"))]
        bank <- c(bank, max(bank) + 1)
        bank <- data.frame(ds = bank, capital = 10000, trades = 0, missed = 0,
                           capacity = 0, duration = 0, SR.02 = 0, stringsAsFactors = FALSE) %>%
                arrange(ds)

        n <- nrow(bank)

        for(i in 1:nrow(dfx))
        {
          r.buy <- which(bank$ds == dfx$bought.on[i])
          r.sell <- which(bank$ds == dfx$sold.on[i]) + 1L

          if(bank$capital[r.buy] > 0)
          {
            invest <- min(bank$capital[r.buy], 10000/maxtrades)
            bank$capital[r.buy:n] <- bank$capital[r.buy:n] - invest
            bank$capital[r.sell:n] <- bank$capital[r.sell:n] + invest*dfx$ROI[i]

            bank$trades[r.buy] <- bank$trades[r.buy] + 1
            bank$capacity[r.buy] <- bank$capacity[r.buy] + dfx$capacity[i]
            bank$duration[r.buy] <- bank$duration[r.buy] + dfx$invest.period[i]

            bank$SR.02[r.buy] <- bank$SR.02[r.buy] + ifelse(dfx$ROI[i] >= 1.02, 1, 0)

            rm(invest)
          } else
          {
            bank$missed[r.buy] <- bank$missed[r.buy] + 1
          }

          rm(r.buy, r.sell, i)
        }

        s <- bank %>% summarise(Scenario
                                , maxtrades
                                , Trades = sum(trades)
                                , Missed = sum(missed)
                                , Miss.Rate = Missed/(Trades + Missed)
                                , capacity = sum(capacity)/Trades
                                , Holding.Period = sum(duration)/Trades
                                , SR.02 = sum(SR.02)/Trades
                                , ROI = last(capital)/10000
                                , ROR = ROI^(1/(n()-1))
                                , Annual.Returns = ROR^250)

        op <- bind_rows(op, s)

        rm(bank, n, maxtrades, s)
      }

      rm(dfx, Scenario)
    }

    op <- op %>% arrange(maxtrades, Scenario) %>% mutate(Selection.Method = Notes)
    return(op)
  }

  x1 <- all.forecasts %>%
        filter(ds > last.dev.date) %>%
        group_by(ticker, ID) %>%
        summarise(Overfit.Index = sum(!is.na(buy.price))/n()) %>%
        inner_join(all.models, by = c("ticker", "ID")) %>%
        filter(bought.on > last.dev.date) %>%
        ungroup()

  x4 <- foreach(t.ROI = seq(0.8, 1.1, 0.05), .combine = bind_rows) %:%
        foreach(min.value = seq(0, 0.5, 0.1), .combine = bind_rows) %:%
        foreach(max.value = seq(min.value + 0.1, 1, 0.1), .combine = bind_rows
                , .packages = "dplyr", .errorhandling = 'remove'
                , .multicombine = TRUE, .inorder = FALSE) %dopar%
        {
          # t.ROI = 0.9
          # min.value = 0
          # max.value = 0.5

          x2 <- x1 %>%
                filter(Overfit.Index >= min.value, Overfit.Index <= max.value) %>%
                group_by(ticker, ID) %>% arrange(ticker, ID, bought.on) %>%
                mutate(S = ifelse(ROI >= t.ROI, 1, 0), Cum.S = cumsum(S),  SR = Cum.S/row_number(),
                       S = lag(S), Cum.S = lag(Cum.S), SR = lag(SR) ) %>%
                group_by(ticker, bought.on) %>% arrange(ticker, bought.on) %>%
                top_n(3, wt = -ID) %>%                      # Buy Upto 3 models a day
                ungroup() %>%
                select(-c(DP.Method, MA.Type, Period))

          x3 <- bind_rows(AF.Scenario.Planning(df = x2, Notes = "No Condition"),
                          AF.Scenario.Planning(df = x2 %>% filter(S >= 1), Notes = "Last was Success"),
                          AF.Scenario.Planning(df = x2 %>% filter(Cum.S >= 1), Notes = "1+ Success"),
                          AF.Scenario.Planning(df = x2 %>% filter(SR >= 0.5), Notes = "50% Success Rate")
                          )

          x3 <- x3 %>%
                mutate(min.Index = min.value, max.Index = max.value, t.ROI) %>%
                select(Selection.Method, Scenario, t.ROI, min.Index, max.Index, maxtrades,
                       Annual.Returns, ROR, ROI, SR.02, Trades, Missed, Miss.Rate, capacity, Holding.Period)

          rm(x2, t.ROI, min.value, max.value)
          return(x3)
        }

  rm(all.forecasts, all.models, last.dev.date, x1, AF.Scenario.Planning)
  return(x4)
}

# -------------------------------------------------------------------------
