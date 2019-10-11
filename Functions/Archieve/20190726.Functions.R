# Swapped the formula of close and adjusted 7/May
# Use adjusted (i.e. adjusted) for ROI.c

rm(list = ls())
options(scipen = 3)
set.seed(1024)
data.folder <- "F:/Project S/MA Linear Modelling/"

library(dplyr)
library(doSNOW)
library(foreach)
library(plotly)
library(htmlwidgets)
library(BatchGetSymbols)
# future::plan(future::multisession, workers = 4) # for BatchGetSymbols in Parallel

closeAllConnections()
cl <- makeCluster(4, outfile="dopar_log.txt")
registerDoSNOW(cl)

# -------------------------------------------------------------------------
# Batch Clean Data: Updated on 15 June, 2019
# -------------------------------------------------------------------------
Get.Portfolio.Tickers <- function()
{
  in.hand <- readRDS(paste0(data.folder, "IB/IB.Activity.rds")) %>%
              mutate(in.hand = ifelse(Action == "BUY", volume, -volume)) %>%
              group_by(ticker) %>% summarise(in.hand = sum(in.hand)) %>%
              filter(in.hand > 0) %>%
              select(ticker) %>% unlist()
  
  return(in.hand)
}

Get.Data.Clean <- function(d1, min.trade = 100000, min.tdays = 250, bad.jumps = 0.1, in.portfolio = FALSE)
{
  library(dplyr)
  library(zoo)
  library(timeDate)
  
  # NY.Time <- as.numeric(strftime(format(Sys.time(), tz = "US/Eastern"), format = "%H.%M"))
  # if(NY.Time >= 9.3) {d1 <- d1 %>% filter(ref.date < Sys.Date())}
  # rm(NY.Time)
  
  if(in.portfolio == TRUE)
  {
    min.trade <- 0
    min.tdays <- 0 
    bad.jumps <- 0.001
  }
  rm(in.portfolio)
  
  source("./Functions/F.Trading.Days.R")
  
  d1 <- d1 %>% distinct()
  if(nrow(d1) < min.tdays) 
  {
    cat(ticker, ": Data Pull from Yahoo Failed\n")
    rm(min.trade, min.tdays, bad.jumps)
    rm(NextTradingDate, PrevTradingDate, TradingDates, envir = .GlobalEnv)
    return(data.frame())
  }

  # ~100 Days of Prod + 150 Days of Dev: 250 Days of Good Data
  # Definition of Good Data: No Extreme Jumps, Non-NA/Zero Volume, $250k+ Daily Trading
  # Consistent for 250 Days

  # Volume Check
  d2x <- d1 %>% group_by(ticker) %>%
          mutate(volume = na_if(volume, 0)) %>%
          summarise(N = n(),
                    last.na = suppressWarnings(max(which(is.na(volume)))),
                    last.na = ifelse(is.infinite(last.na), 0, last.na),
                    good.days = N - last.na
          ) %>%
          # filter(good.days >= min.tdays) %>%
          select(-c(N, good.days)) %>%
          inner_join(d1, by = "ticker") %>%
          group_by(ticker) %>%
          arrange(ticker, ref.date) %>%
          filter(row_number() > last.na) %>%
          rename(ds = ref.date, adjusted = price.adjusted,
                 open = price.open, high = price.high,  low = price.low, close = price.close) %>%
          select(c(ds, ticker, volume, open, high, low, close, adjusted)) %>%
          # mutate_all(zoo::na.locf) # Replace NA with preceeding values
          mutate_at(vars(-group_cols()), zoo::na.locf) # Effective from dplyr 0.8.3

  if(nrow(d2x) < min.tdays) 
  {
    cat(d1$ticker[1], ": Filtered @ Cleaning -", nrow(d2x), "days with Valid Volume\n")
    rm(d2x, min.trade, min.tdays, bad.jumps)
    rm(NextTradingDate, PrevTradingDate, TradingDates, envir = .GlobalEnv)
    return(data.frame())
  }

  # Remove data until with low value trades & sudden jumps
  d2y <- d2x %>%
          group_by(ticker) %>%
          arrange(ticker, ds) %>%
          mutate(value = adjusted*volume,
                 delta = lead(adjusted)/adjusted,
                 bad = case_when(delta < bad.jumps ~ TRUE,
                                 delta > (1/bad.jumps) ~ TRUE,
                                 value < min.trade ~ TRUE,
                                 TRUE ~ FALSE)) %>%
          summarise(N = n(),
                    last.bad = suppressWarnings(max(which(bad == TRUE))),
                    last.bad = ifelse(is.infinite(last.bad), 0, last.bad),
                    good.days = N - last.bad ) %>%
          select(-c(N, good.days))

  d1 <- inner_join(d2x, d2y, by = "ticker") %>%
        group_by(ticker) %>%
        arrange(ticker, ds) %>%
        filter(row_number() > last.bad) %>%
        select(-last.bad) %>%
        ungroup()
  
  if(nrow(d1) < min.tdays) 
  {
    cat("Filtered @ Cleaning [[", ticker, "]]:",
        paste0(nrow(d1), " days with $", round(min.trade/1000, 2), "k in trade and"),
        paste0("price swings within [", bad.jumps*100, "%, ", 100/bad.jumps, "%]\n"))

    rm(d2x, d2y, min.trade, min.tdays, bad.jumps)
    rm(NextTradingDate, PrevTradingDate, TradingDates, envir = .GlobalEnv)
    return(data.frame())
  }
  
  # -------------------------------------------------------------------------
  d1 <- bind_rows(d1,  data.frame(ds = NextTradingDate(Date = max(d1$ds)),
                                  ticker = unique(d1$ticker), stringsAsFactors = FALSE)) %>%
        group_by(ticker) %>% arrange(ticker, ds) %>%
        mutate(ds.N = row_number())

  rm(d2x, d2y, min.trade, min.tdays, bad.jumps)
  rm(NextTradingDate, PrevTradingDate, TradingDates, envir = .GlobalEnv)
  gc()

  return(d1)

}

# -------------------------------------------------------------------------
# Modeling and Model Selection
# -------------------------------------------------------------------------
Get.Models <- function(d1, ticker,
                       T.models = NULL, # Pass NULL for initialization,
                       Type = "Development",
                       target.ROI = 1.20,
                       in.days = 5,
                       last.dev.date = as.Date("2019-01-31"),
                       data.folder = "F:/Project S/MA Linear Modelling/")
{
  # -------------------------------------------------------------------------
  # Inputs: For Testing
  # -------------------------------------------------------------------------
  # library(foreach)
  # library(TTR)
  # library(dplyr)
  # source("./Functions/F.Data.Transformation.R")
  # source("./Functions/F.Trading.Simulation.R")
  # ticker <- "STNG"
  # 
  # Type = "Development"
  # T.models <- NULL
  # # all.d1 <- readRDS(paste0(data.folder, "IB/Clean.Prices.rds"))
  # # Type = "Production"
  # # T.models <- selected.models %>% filter(ticker == !!ticker)
  # # all.d1 <- readRDS(paste0(data.folder, "IB/Prod.Clean.Prices.rds"))
  # 
  # d1 <- all.d1 %>% filter(ticker == !!ticker)
  # data.folder = "F:/Project S/MA Linear Modelling/"
  # target.ROI = 1.2
  # in.days = 5
  # last.dev.date = as.Date("2019-01-31")

  # -------------------------------------------------------------------------
  # Raw Data Extraction
  # -------------------------------------------------------------------------
  # When high is compared with close, use close; else use adjusted
  d1x <- d1 %>%
          mutate(last.close = lag(close)
                 , ROI.h = zoo::rollmax(high, in.days, fill = NA, align = "left")/last.close
                 , ROI.l = -zoo::rollmax(-low, in.days, fill = NA, align = "left")/last.close
                 , ROI.c = lead(adjusted, in.days-1)/lag(adjusted)
                 , Bare = as.factor(if_else(ROI.c > 1, 1, 0))
          ) %>%
          select(-c(adjusted, last.close, ROI.c))
  
  # d2 <- readRDS(paste0(data.folder, "MA/", ticker, ".rds"))
  d2 <- Get.MA.Compressed(d1, periods = seq(10, 180, 10))
  
  if(is.null(T.models))
  {
    T.models <- expand.grid(Period = unique(d2$Period),
                            MA.Type = c("SMA", "EMA", "EMAW", "ZLEMA", "HMA"),
                            DP.Method = c("LN", "MALN"),
                            stringsAsFactors = FALSE)
  }
  
  # -------------------------------------------------------------------------
  # Trend Models
  # -------------------------------------------------------------------------
  # profvis::profvis({
    
  T.Dev.Models <- data.frame()
  T.forecasts <- data.frame()
  
  for(i in 1:nrow(T.models))
  {
    # i = 160
    MA.Type <- T.models$MA.Type[i]
    n <- T.models$Period[i]
    DP.Method <- T.models$DP.Method[i]
    d2x <- get(paste0("AF.", DP.Method))(d2, d1x, MA.Type, n)

    if(nrow(d2x) == 0)
    {
      rm(i, MA.Type, n, DP.Method, d2x)
      next()
    }

    # -------------------------------------------------------------
    tdata <- d2x %>% filter(ds <= last.dev.date) %>%
      select(-c("ds", "ds.N", "ticker", "ROI.h", "ROI.l", "volume", "open", "high", "low", "close"))


    m2 <- tryCatch(glm(Bare ~ ., family = "binomial", data = tdata) %>%
                     MASS::stepAIC(direction = "both", trace = FALSE)
                   , error = function(w) {return(NULL)}
                   , warning = function(w) {return(NULL)})

    # -------------------------------------------------------------
    # Checking Validity of Models
    # -------------------------------------------------------------
    if(is.null(m2) || grepl("LN.", paste(names(m2$coefficients), collapse = ", ")) == FALSE)
    {
      rm(i, MA.Type, n, DP.Method, d2x, tdata, m2)
      next()
    }

    # -------------------------------------------------------------
    # Finding Favourable Ranges: 25 to 75 percentile ranges
    # -------------------------------------------------------------
    all <- d2x %>% arrange(ds.N) %>%
            mutate(Score = predict(m2, newdata = ., type = "response")) %>%
            select(ds.N, Score) %>%
            inner_join(d1x, by = "ds.N") %>%
            select(-Bare)

    bins <- all$Score[all$ds <= last.dev.date] %>% na.omit()
    bins <- quantile(bins, seq(0, 1, 0.1), names = FALSE)
    bins <- tail(head(bins, -1), -1)
    bins <- bins - 0.00000001
    bins <- bins[bins>0]
    bins <- c(0, bins, 1)
    bins <- unique(bins)

    all <- all %>%
            mutate(R.low = as.numeric(as.character(cut(Score, breaks = bins, labels = head(bins, -1))))
                   , R.low = ifelse(is.na(R.low), 0 , R.low)
                   , R.high = as.numeric(as.character(cut(Score, breaks = bins, labels = tail(bins, -1))))
                   , R.high = ifelse(is.na(R.high), 0 , R.high)
                   )

    ranges <- all %>% filter(ds <= last.dev.date) %>% rename(R.sell = ROI.h) %>%
              group_by(ticker, R.low, R.high)

    x <- foreach(ptile = seq(0.05, 0.5, 0.05), .combine = bind_rows, .errorhandling = 'remove') %do%
          {
            x <- ranges %>%
                  mutate(ptile
                         , R.buy  = quantile(ROI.l, probs = ptile, na.rm = TRUE, names = FALSE)
                         , R.stop = min(ROI.l, na.rm = TRUE)
                         , R.ROI = R.sell/R.buy
                         , R.ROR = (R.ROI - 1)/(1 - R.stop/R.buy)
                         ) %>%
                  filter(ROI.l <= R.buy) %>%
                  filter(R.sell == min(R.sell)) %>% # Sell @ minimum
                  select(ticker, R.low, R.high, R.buy, R.sell, R.stop, R.ROI, R.ROR)

            rm(ptile)
            return(x)
          }

    ranges <- x %>% group_by(ticker, R.low, R.high) %>%
              filter(R.ROI == max(R.ROI)) %>%
              filter(R.ROI >= target.ROI, R.ROR >= 4) %>% # ROR for qualification, ROI for selection
              select(-c(R.ROI, R.ROR))
    rm(x)

    if(nrow(ranges) == 0)
    {
      rm(i, MA.Type, n, DP.Method, d2x, tdata, m2, all, bins, ranges)
      next()
    }

    # -------------------------------------------------------------
    # Simulating Trading in Development Cycle
    # -------------------------------------------------------------
    all <- left_join(all, ranges, by = c("ticker", "R.high", "R.low")) %>%
            ungroup() %>% arrange(ds.N) %>%
            mutate(  buy.window = case_when(R.buy > 0 ~ 1)
                     , sell.window = AF.sell.window(buy.window)
                     , buy.price = round(R.buy*lag(close), 2)
                     , sell.price = sell.window*zoo::na.locf(R.sell*lag(close), na.rm = FALSE)
                     # # Sell @ Open if less than last close
                     # , sell.price = ifelse(sell.price <= lag(close), open, sell.price)
                     # # Do Not Sell if less than last close
                     , sell.price = ifelse(sell.price <= lag(close), NA, sell.price)
                     , last.sell = case_when(sell.window == 1 & is.na(lead(sell.window)) ~ close)
                     , buy.volume = case_when(buy.price >= low  ~ 1) # Units Bought
                     , sell.proportion = case_when(sell.price <= high ~ 1) # Prop. Units Sold
                     ) %>%
            AF.simulate.trade(.) %>%
            mutate(ID = i, DP.Method, MA.Type, Period = n)

    sdev <-  all %>% rename(sold.on = ds) %>%
              select(ticker, bought.on, sold.on, capacity, ROI, invest.period, ROR,
                     ID, DP.Method, MA.Type, Period) %>%
              na.omit()

    q <- sdev %>% ungroup() %>%
          filter(bought.on <= last.dev.date) %>%
          summarise(N = n(), ROI = mean(ROI), ROR = mean(ROR))


    if(is.null(q$N) || q$N < 1) # Past Filters: || q$N <= 2 #|| q$N > 10 || q$ROI < 1.10 || q$ROR < 1.01
    {
      rm(i, MA.Type, n, DP.Method, d2x, tdata, m2, all, bins, ranges, sdev, q)
      next()
    }

    # -------------------------------------------------------------
    # Return / Save
    # -------------------------------------------------------------
    if(Type == "Development")
    {
      T.Dev.Models <- bind_rows(T.Dev.Models, sdev)
      T.forecasts <- bind_rows(T.forecasts, all %>% select(-ds.N))
    }

    if(Type == "Production") # Type 8 Validation Incorporated: Last Success
    {
      q <- sdev %>% ungroup() %>%
        filter(bought.on > last.dev.date) %>%
        arrange(bought.on) %>%
        summarise(N = n(), Last.ROI = ifelse(nrow(.) > 0, last(ROI), 0))

      if(q$N >= 1 && q$Last.ROI >= 0.85)
      {
        f.prod <- all %>%
          filter(ds > last.dev.date) %>%
          select(ds, ticker, Score, buy.price, sell.price, last.sell,
                 bought.on, buy.volume, ROI,
                 ID, DP.Method, MA.Type, Period) %>%
          arrange(ds) %>%
          mutate(Overfit.Index = cumsum(!is.na(buy.price))/row_number())

        # Not Sold Yet/To be Bought
        if( !is.na(last(f.prod$bought.on)) | !is.na(last(f.prod$buy.price)) )
        {
          sdev <- sdev %>% filter(bought.on > last.dev.date) %>%
            group_by(ticker, ID, DP.Method, MA.Type, Period) %>%
            summarise(Prod.N = n(),
                      Duration = paste(round(invest.period, 1), collapse = ", " ),
                      ROI = paste(paste0(round(100*ROI, 1), "%"), collapse = ", "),
                      ROR = paste(paste0(round(100*ROR, 1), "%"), collapse = ", "),
                      ds = last(f.prod$ds),
                      buy.price = last(f.prod$buy.price),
                      sell.price = last(f.prod$sell.price),
                      last.sell = last(f.prod$last.sell)
            )

          T.Dev.Models <- bind_rows(T.Dev.Models, sdev)
          T.forecasts <- bind_rows(T.forecasts, f.prod)
        }
        rm(f.prod)
      }
    }

    rm(i, MA.Type, n, DP.Method, d2x, tdata, m2, all, bins, ranges, sdev, q)

  }
  


  # summary(T.Dev.Models %>% filter(bought.on > last.dev.date) %>% select(ROI))
  # T.Dev.Models %>% filter(bought.on > last.dev.date) %>% arrange(bought.on)
  # })  # For Profvis
  # -------------------------------------------------------------------------
  # Summarization, Saving and Cleaning
  # -------------------------------------------------------------------------
  if(nrow(T.Dev.Models) > 0)
  {
    if(Type == "Production")
    {T.Dev.Models <- left_join(T.Dev.Models, T.models, by = c("ticker", "DP.Method", "MA.Type", "Period"))}
    
    saveRDS(T.Dev.Models, paste0(data.folder, "Model.Performance/", ticker, ".rds"))
    saveRDS(T.forecasts, paste0(data.folder, "Prod.Forecasts/", ticker, ".rds"))
  }

  rm(d1, d1x, d2, T.models, target.ROI, in.days, last.dev.date, ticker, T.forecasts, Type, T.Dev.Models)
  gc()
  
}

