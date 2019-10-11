library(dplyr)
library(foreach)
library(plotly)

all.fcasts <- readRDS(paste0(data.folder, "Summary/20190611.All.Forecasts.rds"))
last.dev.date <- as.Date("2019-01-31")

# -------------------------------------------------------------------------
# Associated Functions
# -------------------------------------------------------------------------
First.of.Sequence <- function(grouped.df)
{
  g <- group_vars(grouped.df)
  grouped.df <- grouped.df %>%
                arrange(!!!syms(c(g, "ds.N"))) %>%
                mutate(Seq = case_when(row_number() == 1 ~ 1,
                                       lag(ds.N) >= ds.N - 5 ~ 0,
                                       TRUE ~ 1 )
                       , Seq = cumsum(Seq)
                ) %>%
                group_by(!!!syms(c(g, "Seq"))) %>%
                filter(ds.N == min(ds.N)) %>%
                ungroup()

  return(grouped.df)
}

roll.max <- function(x, return.position = FALSE)
{
  p <- suppressWarnings(max(x, na.rm = TRUE))
  p <- ifelse(is.infinite(p), NA, p)
  if(return.position == TRUE) {return(match(p, x))}
  return(p)
}

roll.min <- function(x, return.position = FALSE)
{
  p <- suppressWarnings(min(x, na.rm = TRUE))
  p <- ifelse(is.infinite(p), NA, p)

  if(return.position == TRUE) {return(match(p, x))}
  return(p)
}

# -------------------------------------------------------------------------
# Basic Evaluation
# -------------------------------------------------------------------------
all.models <- readRDS(paste0(data.folder, "Summary/20190611.All.Models.rds"))
valid.models <- all.models %>% filter(ROI.Prod >= 1.15, N.Prod > 1) %>%
                mutate(WC.N = N.Prod - 1,
                       WC.02 = N.Prod*SR.02.Prod - 1,
                       WC.10 = N.Prod*SR.10.Prod - 1)

sum(valid.models$WC.N)
sum(valid.models$WC.02)/sum(valid.models$WC.N)
sum(valid.models$WC.10)/sum(valid.models$WC.N)
prod(valid.models$ROI.Prod)^(1/nrow(valid.models))

rm(valid.models, all.models)
# #########################################################################
# 
# Deep Evaluation: Use model if out of sample ROI is good
# 
# #########################################################################
# Option 1: Buy @ open when 90P & Sell @ EOD5
# Historical ROI from the first day of a sequence
# No need to remove 100 points, as it's already removed
# ----------------------
# Results: FAILED. No scenario gives any profit
# -------------------------------------------------------------------------
x1 <- all.fcasts %>% #filter(ticker == "AMRN") %>%
      group_by(ticker, ID) %>% arrange(ticker, ID, ds.N) %>%
      mutate(Key = paste(DP.Method, MA.Type, Period)
             , ROI.c = lead(adjusted, 5-1)/lag(adjusted) ) %>%
      filter(ds > last.dev.date) %>%
      select(-c(DP.Method, MA.Type, Period, volume, open, high, low, close, adjusted, Score)) 

x2 <- x1 %>% filter(Ptile >= 0.9) %>% na.omit() %>%
      group_by(ticker, ID) %>% First.of.Sequence() %>%
      group_by(ticker, ID) %>%
      mutate(H.ROI = cummean(ROI.c)
             , ds.N = ds.N + 5 # When this information is available
             ) %>%
      select(ID, ticker, ds.N, H.ROI)

x3 <- left_join(x1, x2, by = c("ID", "ticker", "ds.N")) %>%
      mutate(H.ROI = zoo::na.locf(H.ROI, na.rm = FALSE)) %>%
      filter(Ptile >= 0.9) %>% 
      group_by(ticker, ID) %>% First.of.Sequence() %>%
      na.omit()

x4 <- foreach(ROI.min = seq(0.9, 1.5, 0.01), .combine = bind_rows) %do%
      {
        x4 <- x3 %>% filter(H.ROI >= ROI.min, 
                            # H.ROI < ROI.min + 0.01,
                            ds > last.dev.date) %>%
              select(-c(Ptile, ID, Key, Seq, H.ROI)) %>% distinct() %>%
              group_by(ticker) %>% First.of.Sequence() %>%
              summarise(ROI.min
                        # , ROI.max = ROI.min + 0.01
                        , N = n()
                        , Tickers = length(unique(ticker))
                        , SR.02 = sum(ROI.c >= 1.02)/N
                        , SR.10 = sum(ROI.c >= 1.10)/N
                        , ROI = prod(ROI.c)^(1/N) )
        
        return(x4)
      }

plot_ly(data = x4, x = ~ROI.min, 
        y = ~ROI, name = "ROI", type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~SR.02, name = 'SR @ 2%', yaxis = "y2") %>%
  add_trace(y = ~SR.10, name = 'SR @ 10%', yaxis = "y2") %>%
  layout(yaxis2 = list(tickfont = list(color = "red")
                       , overlaying = "y"
                       , side = "right" )
         , legend = list(orientation = 'h')
         , hovermode = 'compare'
         )


rm(x1, x2, x3, x4, ROI.min)
gc()      

# -------------------------------------------------------------------------
# Option 2: Medium Haul Holding
# Buy @ enters 90 percentile; Sold @ 5 Days after exiting 90 percentile
# No need to remove 100 points, as it's already removed
# 
# Results: Good
# 4X-5X returns p.a. with cumulative ROR (Re-run)
# 8X-9X returns p.a. with last ROR (Re-run)
# -------------------------------------------------------------------------
x1 <- all.fcasts %>% #filter(ticker == "MDB") %>%
      group_by(ticker, ID) %>% arrange(ticker, ID, ds.N) %>%
      mutate(Key = paste(DP.Method, MA.Type, Period)
             , X = case_when(Ptile >= 0.9 ~ 1)
             , Y = zoo::rollapplyr(X, 5, fill = NA, align = "right", FUN = roll.max, partial = TRUE)
             , X = case_when(X == 1 & Y == 1 ~ 1, 
                             is.na(X) & Y == 1 ~ 0)
             , Seq = case_when(X >= 0 & is.na(lag(X)) ~ 1,
                               TRUE ~ 0 )
             , Seq = cumsum(Seq)*pmax(X, 1)
      ) %>%
      filter(Seq > 0) %>%
      group_by(ticker, ID, Key, Seq) %>%
      summarise(Open.Day = min(ds),
                Close.Day = max(ds),
                Type = ifelse(Open.Day <= last.dev.date, "Dev", "Prod"),
                Days = n(),
                seq.open = first(open), 
                seq.close = last(close),
                ROI = seq.close/seq.open,
                ROR = ROI^(1/Days) )


x2 <- foreach(min.ROR = seq(1, 1.05, 0.001), .combine = bind_rows) %do%
      {  
        x2 <- x1 %>% na.omit() %>%
              filter(Type == "Prod") %>% 
              group_by(ticker, ID) %>% arrange(ticker, ID, Seq) %>%
              mutate(#H.ROR = lag(cummean(ROR)) 
                     H.ROR = lag(ROR) 
                     ) %>%
              filter(H.ROR >= min.ROR) %>%
              ungroup() %>%
              summarise(min.ROR
                        , N = n()
                        , Tickers = length(unique(ticker))
                        , SR.02 = sum(ROI >= 1.02)/N
                        , SR.10 = sum(ROI >= 1.10)/N
                        , ROI = mean(ROI)
                        , ROR = prod(ROR)^(1/N)
                        , Days = mean(Days, na.rm = TRUE)
                        , Annual.Returns = (ROR)^250 )        
        
        rm(min.ROR)
        return(x2)
      }


plot_ly(data = x2, x = ~min.ROR, 
        y = ~Annual.Returns, name = "Annual.Returns", type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~SR.02, name = 'SR @ 2%', yaxis = "y2") %>%
  add_trace(y = ~SR.10, name = 'SR @ 10%', yaxis = "y2") %>%
  layout(yaxis2 = list(tickfont = list(color = "red")
                       , overlaying = "y"
                       , side = "right" )
         , legend = list(orientation = 'h')
         , hovermode = 'compare'
  )

rm(x1, x2)
gc()

# -------------------------------------------------------------------------
# Option 3: Medium Haul Holding
# Buy @ enters (X, Y]  Percentile Range; Sold 5 Days after drops out of range: Find X, Y
# No need to remove 100 points, as it's already removed
# 
# Results: Very Good
# last ROR better than cummean/cummin
# 10% threshold ROI better than 5%

# 6X-7X returns p.a. W/ 5% threshold ROI + last ROR + min ROR 1.028 (N = 42)
# 9X-12X returns p.a. W/ 10% threshold ROI + last ROR + min ROR 1.03 (N = 26)
# -------------------------------------------------------------------------
interval.bp = 5
thresh.ROI = 1.05

x1 <- all.fcasts %>% #filter(ticker == "MDB") %>%
      filter(ds <= last.dev.date) %>%
      group_by(ticker, ID) %>% arrange(ticker, ID, ds.N) %>%
      mutate(  Ptile = (interval.bp/100)*ceiling((100/interval.bp)*Ptile)
             , ROI = lead(adjusted, 5-1)/lag(adjusted) ) %>%
      na.omit() %>%
      group_by(ticker, ID, Ptile) %>%
      summarise(N = n(), ROI = mean(ROI)) %>%
      arrange(ticker, ID, desc(Ptile)) %>%
      mutate(ROI = cummin(ROI)) %>%
      filter(ROI >= thresh.ROI) %>%
      summarise(max.Ptile = max(Ptile), min.Ptile = min(Ptile) - interval.bp/100)

x2 <- all.fcasts %>% #filter(ticker == "MDB") %>%
      inner_join(x1, by = c("ticker", "ID")) %>%
      group_by(ticker, ID) %>% arrange(ticker, ID, ds.N) %>%
      mutate(Key = paste(DP.Method, MA.Type, Period)
             , X = case_when(Ptile > min.Ptile & Ptile <= max.Ptile ~ 1)
             , Y = zoo::rollapplyr(X, 5, fill = NA, align = "right", FUN = roll.max, partial = TRUE)
             , X = case_when(X == 1 & Y == 1 ~ 1, 
                             is.na(X) & Y == 1 ~ 0)
             , Seq = case_when(X >= 0 & is.na(lag(X)) ~ 1,
                               TRUE ~ 0 )
             , Seq = cumsum(Seq)*pmax(X, 1)
      ) %>%
      filter(Seq > 0) %>%
      group_by(ticker, ID, Key, Seq, min.Ptile, max.Ptile) %>%
      summarise(Open.Day = min(ds),
                Close.Day = max(ds),
                Type = ifelse(Open.Day <= last.dev.date, "Dev", "Prod"),
                Days = n(),
                seq.open = first(open), 
                seq.close = last(close),
                ROI = seq.close/seq.open,
                ROR = ROI^(1/Days) )


x3 <- foreach(min.ROR = seq(1, 1.05, 0.001), .combine = bind_rows) %do%
      {  
        x3 <- x2 %>% na.omit() %>%
              filter(Type == "Prod") %>% 
              group_by(ticker, ID) %>% arrange(ticker, ID, Seq) %>%
              mutate( H.ROR = lag(ROR)
                     # H.ROR = lag(cummean(ROR)) 
                     # H.ROR = lag(cummin(ROR))
              ) %>%
              filter(H.ROR >= min.ROR) %>%
              ungroup() %>%
              summarise(min.ROR
                        , N = n()
                        , Tickers = length(unique(ticker))
                        , SR.02 = sum(ROI >= 1.02)/N
                        , SR.10 = sum(ROI >= 1.10)/N
                        , ROI = mean(ROI)
                        , ROR = prod(ROR)^(1/N)
                        , Days = mean(Days, na.rm = TRUE)
                        , Annual.Returns = (ROR)^250 )        
        rm(min.ROR)
        return(x3)
      }

plot_ly(data = x3, x = ~min.ROR, 
        y = ~Annual.Returns, name = "Annual.Returns", type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~SR.02, name = 'SR @ 2%', yaxis = "y2") %>%
  add_trace(y = ~SR.10, name = 'SR @ 10%', yaxis = "y2") %>%
  layout(yaxis2 = list(tickfont = list(color = "red")
                       , overlaying = "y"
                       , side = "right" )
         , legend = list(orientation = 'h')
         , hovermode = 'compare'
  )

rm(x1, x2, x3, interval.bp, thresh.ROI)
gc()

# -------------------------------------------------------------------------
# Option 4: Medium Haul Holding
# Buy @ enters X Score; Sold 5 Days after drops below X: Find X from historical
# 
# Results: Very Good. Shows more promise than Option 3
# Better results at 10% threshold
# 20X returns  W/ 10% threshold ROI + 20 buckets + min.ROR 1.03+
# 12X+ returns W/ 10% threshold ROI + 10 buckets + min ROR 1.024 (N = 34)
# This is a fairly stable model
# -------------------------------------------------------------------------
thresh.ROI = 1.1
buckets = 10

x1 <- all.fcasts %>% #filter(ticker == "MDB", ID  == 2) %>%
      filter(ds <= last.dev.date) %>%
      group_by(ticker, ID) %>% arrange(ticker, ID, ds.N) %>%
      mutate(Bucket = as.character(cut(Score, buckets, dig.lab = 3))
             , Score.L = gsub('\\(|\\)|\\[|\\]|\\,.*', '', Bucket)
             # , Score.H = gsub('\\(|\\)|\\[|\\]|.*,', '', Bucket)
             , ROI = lead(adjusted, 5-1)/lag(adjusted)
             ) %>%
      na.omit() %>%
      group_by(ticker, ID, Score.L) %>%
      summarise(N = n(), ROI = mean(ROI)) %>%
      group_by(ticker, ID) %>%
      arrange(ticker, ID, desc(Score.L)) %>%
      mutate(ROI = cummin(ROI)) %>%
      filter(ROI >= thresh.ROI) %>%
      summarise(Score.L = min(Score.L))


x2 <- all.fcasts %>% #filter(ticker == "MDB") %>%
      inner_join(x1, by = c("ticker", "ID")) %>%
      group_by(ticker, ID) %>% arrange(ticker, ID, ds.N) %>%
      mutate(Key = paste(DP.Method, MA.Type, Period)
             , X = case_when(Score > Score.L ~ 1)
             , Y = zoo::rollapplyr(X, 5, fill = NA, align = "right", FUN = roll.max, partial = TRUE)
             , X = case_when(X == 1 & Y == 1 ~ 1, 
                             is.na(X) & Y == 1 ~ 0)
             , Seq = case_when(X >= 0 & is.na(lag(X)) ~ 1,
                               TRUE ~ 0 )
             , Seq = cumsum(Seq)*pmax(X, 1)
      ) %>%
      filter(Seq > 0) %>%
      group_by(ticker, ID, Key, Seq, Score.L) %>%
      summarise(Open.Day = min(ds),
                Close.Day = max(ds),
                Type = ifelse(Open.Day <= last.dev.date, "Dev", "Prod"),
                Days = n(),
                seq.open = first(open), 
                seq.close = last(close),
                ROI = seq.close/seq.open,
                ROR = ROI^(1/Days)
                )


x3 <- foreach(min.ROR = seq(1, 1.05, 0.001), .combine = bind_rows) %do%
      {  
        x3 <- x2 %>% na.omit() %>%
              filter(Type == "Prod") %>% 
              group_by(ticker, ID) %>% arrange(ticker, ID, Seq) %>%
              mutate( H.ROR = lag(ROR)
                # H.ROR = lag(cummean(ROR)) 
                 # H.ROR = lag(cummin(ROR))
              ) %>%
              filter(H.ROR >= min.ROR) %>%
              ungroup() %>%
              summarise(min.ROR
                        , N = n()
                        , Tickers = length(unique(ticker))
                        , SR.02 = sum(ROI >= 1.02)/N
                        , SR.10 = sum(ROI >= 1.10)/N
                        , ROI = mean(ROI)
                        , ROR = prod(ROR)^(1/N)
                        , Days = mean(Days, na.rm = TRUE)
                        , Annual.Returns = (ROR)^250 )
        
        rm(min.ROR)
        return(x3)
      }

plot_ly(data = x3, x = ~min.ROR, 
        y = ~Annual.Returns, name = "Annual.Returns", type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~SR.02, name = 'SR @ 2%', yaxis = "y2") %>%
  add_trace(y = ~SR.10, name = 'SR @ 10%', yaxis = "y2") %>%
  layout(yaxis2 = list(tickfont = list(color = "red")
                       , overlaying = "y"
                       , side = "right" )
         , legend = list(orientation = 'h')
         , hovermode = 'compare')

rm(x1, x2, x3, buckets, thresh.ROI)
gc()

# -------------------------------------------------------------------------
# Option 5: Medium Haul Holding
# Buy @ desired price PB; Sold @ PS / (if not sold) @ close of D5 : Find X, PB, PS
# PB & PS are defined by scores
# 
# Optimizing Factors: 
# Buckets: 10 buckets based on min-max or percentile: percentile gives much better results
# Min Opportunity: 20%
# Function for Opp (Percentiles): Selected 25p for ROI.l and 75p for ROI.h
# Function for selecting trades (last ROR/cummin/cummean): last ROR is the best indicator
# Trades a week: 5, 10, etc. (Not requried for higher Min. Opp)
# 
# Rules: 
# Buy@open, if its open < target; 
# Do Not Sell, if its target < last.close; 
# sell on D5 of last signal
# If a day has both buy/sell signal, only buy (because we dont the if low happened before high or vice versa)
# In any particular day select the model with maximum ROR
# Summarize at weekly level and then roll up annually

# Results
# low/high of 25/75 percentiles with 20% threshold gives best results ROR between [1.042, 1.07)
# -------------------------------------------------------------------------
source("F.Portfolio.Simulation.R")

thresh.Opp = 0.2
buckets = 10

x1 <- all.fcasts %>% #filter(ticker == "IO", ID == 19) %>%
      filter(ds <= last.dev.date, !is.na(Score)) %>%
      group_by(ticker, ID) %>% arrange(ticker, ID, ds.N) %>%
      mutate(last.close = lag(close)
             , Bucket = as.character(cut(Score,
                                         breaks = quantile(Score, probs = seq(0, 1, by = 1/buckets)),
                                         include.lowest = TRUE,
                                         dig.lab = 3))
             , Range = as.numeric(gsub('\\(|\\)|\\[|\\]|\\,.*', '', Bucket)) - 0.001
             , ROI.l = -zoo::rollmax(-low, 5, fill = NA, align = "left")/last.close
             , ROI.h = zoo::rollmax(high, 5, fill = NA, align = "left")/last.close
      ) %>%
      group_by(ticker, ID, Range) %>%
      summarise(  H.ROI.l = quantile(ROI.l, probs = 0.25, na.rm = TRUE, names = FALSE)
                , H.ROI.h = quantile(ROI.h, probs = 0.75, na.rm = TRUE, names = FALSE)
                , H.Opp = H.ROI.h - H.ROI.l
                ) %>%
      arrange(ticker, ID, desc(Range)) %>%
      mutate(Min.Opp = cummin(H.Opp)) %>%
      filter(Min.Opp >= thresh.Opp) %>%
      select(-c(Min.Opp)) %>%
      ungroup() %>%
      bind_rows(select(., ticker, ID) %>% distinct() %>% mutate(Range = 0))


x2 <- all.fcasts %>% #filter(ticker == "WVE", ID == 2) %>%
      inner_join(x1, by = c("ticker", "ID")) %>%
      group_by(ticker, ID, ds.N) %>%
      filter(Score > Range) %>% filter(Range == max(Range)) %>%
      group_by(ticker, ID) %>% arrange(ticker, ID, ds.N) %>%
      mutate(  X = case_when(!is.na(H.Opp) ~ 1)
             , Y = zoo::rollapplyr(X, 5, fill = NA, align = "right", FUN = roll.max, partial = TRUE)

             , B.T = X*round(H.ROI.l*lag(close), 2)
             , B.T = Y*zoo::na.locf(B.T, na.rm = FALSE)
             , B.T = ifelse(B.T >= open, open, B.T) # Buy@open, if its < target 
             , S.T = X*round(H.ROI.h*lag(close), 2)
             , S.T = Y*zoo::na.locf(S.T, na.rm = FALSE)
             , S.T = ifelse(S.T <= lag(close), NA, S.T) # Do Not Sell, if its < last.close 
             
             , buy.volume = case_when(B.T >= low  ~ 1) # Units Bought
             , buy.price = B.T
             , sell.proportion = case_when(S.T <= high ~ 1) # Proportion of shares disposed
             , sell.price = S.T
             , sell.close = case_when(Y == 1 & is.na(lead(Y)) ~ close) # Worst Selling Price
             
             , Last.Trade = case_when(buy.volume > 0 ~ lag((1/1000000)*volume*(open + close)/2))
             , Last.Trade = zoo::na.locf(Last.Trade, na.rm = FALSE)
             
             , ROI = portfolio.sim(buy.volume, buy.price, 
                                   sell.proportion, sell.price, sell.close, returns = "ROI")
             , Duration = portfolio.sim(buy.volume, buy.price, 
                                   sell.proportion, sell.price, sell.close, returns = "Duration")
      ) %>%
      # select(-c(DP.Method, MA.Type, Period, Ptile, volume, adjusted, H.Opp, Range, Score,
      #           H.ROI.l, H.ROI.h)) %>%
      select(ds, ticker, ID, Last.Trade, ROI, Duration) %>%
      mutate(ROR = ROI^(1/Duration)) %>%
      na.omit()

x3 <- foreach(min.ROR = seq(0.95, 1.1, 0.002), .combine = bind_rows) %do%
      {  
        x3 <- x2 %>% #filter(ticker == "MLNT") %>%
              filter(ds > last.dev.date) %>% 
              group_by(ticker, ID) %>% arrange(ticker, ID) %>%
              mutate(wk = format(ds,"%Y-%V"),
                     H.ROR = lag(ROR)
              ) %>%
              group_by(ticker, ds) %>%
              select(-ID) %>% distinct() %>%
              filter(H.ROR == max(H.ROR, na.rm = TRUE)) %>% filter(H.ROR >= min.ROR) %>%
              group_by(ticker, wk) %>% 
              filter(ds == min(ds)) %>%
              group_by(wk) %>% 
              # top_n(w = H.ROR, n = 5) %>%
              summarise(N = n()
                        , Capacity = sum(Last.Trade)
                        , Holding.Period = mean(Duration) 
                        , SR.02 = mean(ROI >= 1.02)
                        , SR.ROR = mean(ROR >= min.ROR)
                        , ROI = mean(ROI)
                        ) %>%
              ungroup() %>%
              summarise(min.ROR
                        , Trades = sum(N)
                        , Capacity = median(Capacity)
                        , Holding.Period = weighted.mean(Holding.Period, w = N)
                        , SR.02 = weighted.mean(SR.02, w = N)
                        , SR.ROR = weighted.mean(SR.ROR, w = N)
                        , ROI = prod(ROI)^(1/n())
                        , Annual.Returns = ROI^(50)
                        )
        
        rm(min.ROR)
        return(x3)
      }

# -------------------------------------------------------------------------
# Charts for Option 5
# -------------------------------------------------------------------------
sec.y <- list(overlaying = "y", side = "right", tickformat = '.1%')
margins <- list(l = 60, r = 60, b = 25, t = 25)

plot_ly(data = x3, x = ~min.ROR, 
        y = ~Annual.Returns, name = "Annual.Returns", type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~ROI, name = 'Mean ROI', yaxis = "y2") %>%
  layout(title = "Financial Performance", yaxis = list(tickformat = '.1f'), yaxis2 = sec.y, 
         legend = list(orientation = 'h'), hovermode = 'compare', margin = margins)


plot_ly(data = x3, x = ~min.ROR, 
        y = ~Holding.Period, name = "Holding.Period", type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~SR.02, name = 'SR @ 2%', yaxis = "y2") %>%
  add_trace(y = ~SR.ROR, name = 'SR @ ROR', yaxis = "y2") %>%
  layout(title = "Operational Performance", yaxis = list(tickformat = '.1f'), yaxis2 = sec.y, 
         legend = list(orientation = 'h'), hovermode = 'compare', margin = margins)

plot_ly(data = x3, x = ~min.ROR, 
        y = ~Trades, name = "Trades Since Feb", type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Capacity, name = 'Weekly Capacity ($ MM)') %>%
  add_trace(y = ~ROI, name = 'Mean ROI', yaxis = "y2") %>%
  layout(title = "Capacity Management", yaxis = list(tickformat = '.0f'), yaxis2 = sec.y, 
         legend = list(orientation = 'h'), hovermode = 'compare', margin = margins)


rm(buckets, thresh.Opp, portfolio.sim, x1, x2, sec.y, margins)
gc()
# -------------------------------------------------------------------------

rm(list = ls())
