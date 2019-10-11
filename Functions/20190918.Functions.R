options(scipen = 3)
set.seed(1024)
# rm(list = ls())
# gc()

# -------------------------------------------------------------------------------
Parms <- list(last.dev.date = as.Date("2019-01-31"))

# if(!Parms[["pkg.folder"]] %in% .libPaths()) {.libPaths( c(Parms[["pkg.folder"]], .libPaths()) )}
# # .libPaths()

# -------------------------------------------------------------------------------
library(dplyr)
library(doSNOW)
library(foreach)
library(zoo)

closeAllConnections()
cl <- makeCluster(4, type = "SOCK", outfile="dopar_log.txt")
registerDoSNOW(cl)

# -------------------------------------------------------------------------
# Associated Functions
# -------------------------------------------------------------------------
Get.Tickers <- function()
{
  x <- data.table::fread("ftp://ftp.nasdaqtrader.com/symboldirectory/nasdaqtraded.txt", 
                           sep = "|", header = T, fill = T, showProgress = F)
  
  names(x) <- gsub(" ", ".", names(x)) 
  
  # Definitions: http://www.nasdaqtrader.com/trader.aspx?id=symboldirdefs
  x <- x %>%
        filter(ETF == "N", Test.Issue == "N", Nasdaq.Traded == "Y", 
               NextShares == "N", Symbol == NASDAQ.Symbol,
               !(Financial.Status %in% c("D", "E", "G", "H", "K", "Q", "J")) 
               ) %>%
        rename(ticker = Symbol, Name = Security.Name) %>%
        mutate(Exchange = case_when(Listing.Exchange %in% c("Q", "G", "S") ~ "ISLAND",
                                    Listing.Exchange %in% c("A", "N", "P") ~ "NYSE",
                                    Listing.Exchange %in% c("Z") ~ "BATS",
                                    Listing.Exchange %in% c("V") ~ "IEX")
               ) %>%
        select(ticker, Exchange, Name) %>% ungroup() %>% distinct() %>%
        arrange(ticker) %>%
        mutate(tickerID = row_number() + 1000)
  
  return(x)
}

Get.MA.Compressed <- function(d1, periods = seq(10, 180, 10))
{
  library(TTR)
  library(foreach)
  library(dplyr)
  
  d1[is.na(d1)] <- 0
  
  suppressWarnings(
    z <- foreach(n = periods, .combine = bind_rows, .errorhandling = 'remove') %:%
      foreach(name = c("open", "low", "high", "close")
              , .combine = bind_rows, .errorhandling = 'remove') %do%
              {
                z <- d1 %>%
                  mutate(Period = n,
                         MA.Var = name,
                         SMA := lag(SMA(get(name), n))/lag(close),
                         EMA := lag(EMA(get(name), n))/lag(close),
                         EMAW := lag(EMA(get(name), n, wilder = TRUE))/lag(close),
                         ZLEMA := lag(ZLEMA(get(name), n))/lag(close),
                         HMA := lag(HMA(get(name), n))/lag(close)
                  ) %>%
                  select(ds, ticker, Period, MA.Var, SMA, EMA, EMAW, ZLEMA, HMA) %>%
                  na.omit()
                
                rm(n, name)
                return(z)
              }
  )
  
  return(z)
}

AF.LN <- function(d2, d1x, MA.Type, n)
{
  d2x <- d2 %>% filter(Period == !!n) %>%
    select(ds, ticker, MA.Var, (!!MA.Type)) %>%
    rename(MA = MA.Type)
  
  if(min(d2x$MA) <= 0 ){ return(data.frame())}
  
  d2x <- d2x %>%
    mutate(LN = log(MA)) %>%
    select(-MA) %>% data.frame() %>%
    reshape(idvar = c("ds", "ticker"), timevar = "MA.Var", direction = "wide") %>%
    left_join(d1x, by = c("ticker", "ds"))
  
  return(d2x)
  
}

AF.MALN <- function(d2, d1x, MA.Type, n)
{
  d2x <- d2 %>%
    filter(Period == !!n) %>%
    select(ds, ticker, MA.Var, (!!MA.Type)) %>%
    rename(MA = MA.Type)
  
  if(min(d2x$MA) <= 0 ){ return(data.frame())}
  
  d2x <- d2x %>%
    mutate(LN = log(MA)) %>%
    data.frame() %>%
    reshape(idvar = c("ds", "ticker"), timevar = "MA.Var", direction = "wide") %>%
    left_join(d1x, by = c("ticker", "ds"))
  
  return(d2x)
  
}
# -------------------------------------------------------------------------
# Batch Clean Data Functions
# -------------------------------------------------------------------------

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
# Baseline Model Scores Function
# -------------------------------------------------------------------------
Get.Model.Scores <- function(ticker, d1, last.dev.date, exdata,
                             T.models = NULL, in.days = 5L, Type = "Development")
{
  # -------------------------------------------------------------------------
  # Inputs: For Testing
  # -------------------------------------------------------------------------
  # library(foreach)
  # library(dplyr)
  # ticker = "MDB"
  # d1 <- all.d1 %>% filter(ticker == !!ticker)
  # in.days = 5
  # last.dev.date = Parms[["last.dev.date"]]
  # Type = "Development"
  # T.models <- NULL
  # T.models <- prod.models %>% filter(ticker == !!ticker) %>%
  #             select(DP.Method, MA.Type, Period, ID) %>% distinct()
  
  # -------------------------------------------------------------------------
  # Raw Data Extraction
  # -------------------------------------------------------------------------
  
  if(as.numeric(last.dev.date - min(d1$ds)) < 180) {return(NULL)}
  
  d1x <- d1 %>% mutate(last.close = lag(close)
                       , ROI.h = zoo::rollmax(high, in.days, fill = NA, align = "left")/last.close
                       , ROI.l = -zoo::rollmax(-low, in.days, fill = NA, align = "left")/last.close
                       , ROI.c = lead(adjusted, in.days-1)/lag(adjusted)
                       , Bare = as.factor(ifelse(ROI.c > 1, 1, 0))
                       ) %>%
          select(-c(adjusted, last.close, ROI.c))

  d2 <- Get.MA.Compressed(d1, periods = seq(10, 180, 10))
  
  if(nrow(d2) == 0){return(NULL)}

  if(is.null(T.models))
  {
    T.models <- expand.grid(Period = unique(d2$Period),
                            MA.Type = c("SMA", "EMA", "EMAW", "ZLEMA", "HMA"),
                            DP.Method = c("LN", "MALN"),
                            stringsAsFactors = FALSE) %>%
                mutate(ID = row_number())
  }

  # -------------------------------------------------------------------------
  # Trend Models
  # -------------------------------------------------------------------------
  # profvis::profvis({

  T.scores <- data.frame()

  for(i in T.models$ID)
  {
    # i = 10
    DP.Method <- T.models$DP.Method[T.models$ID == i]
    MA.Type <- T.models$MA.Type[T.models$ID == i]
    n <- T.models$Period[T.models$ID == i]
    d2x <- get(paste0("AF.", DP.Method))(d2, d1x, MA.Type, n)

    if(nrow(d2x) == 0)
    {
      rm(i, MA.Type, n, DP.Method, d2x)
      next()
    }
    
    d2x <- left_join(d2x, exdata, by = "ds")
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

    all <- d2x %>% arrange(ds.N) %>%
            mutate(ID = i, DP.Method, Period = n, MA.Type,
                   Score = predict(m2, newdata = ., type = "response")) %>%
            select(ticker, ds, ID, DP.Method, MA.Type, Period, Score)

    T.scores <- bind_rows(T.scores, all)

    rm(i, MA.Type, n, DP.Method, d2x, tdata, m2, all)
  }
  
  # })  # For Profvis
  # -------------------------------------------------------------------------
  # Summarization, Saving and Cleaning
  # -------------------------------------------------------------------------
  saveRDS(T.scores, paste0("./Data/Scores/", ticker, ".rds"))

  rm(d1, d1x, d2, T.models, in.days, Type, ticker, T.scores, last.dev.date)
  gc()

}
