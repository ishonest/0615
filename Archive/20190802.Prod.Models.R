# -------------------------------------------------------------------------
# Initialization
# -------------------------------------------------------------------------
source("./Functions/20190802.Functions.R")

Parms <- list(algoIds = c("20190802")
              , invest.max.model  = 1000   # Maximum investment in a model
              , invest.max.ticker = 3000   # Maximum investment in a ticker
              , max.capacity      = 0.01   # Max % of yesterday's volume any model can buy
              , data.folder       = "F:/Project S/MA Linear Modelling/"
              , last.dev.date     = as.Date("2019-01-31")
              )


prod.models <- readRDS(paste0(Parms$data.folder, "Summary/Production.Models.rds")) %>%
                filter(algoId %in% Parms[["algoIds"]])
hist.d1 <- readRDS(paste0(Parms$data.folder, "Development/Summary/Clean.Prices.rds")) %>% 
            filter(ticker %in% unique(prod.models$ticker))

# -------------------------------------------------------------------------
# Incremental Data Pull
# -------------------------------------------------------------------------
stocks <- unique(hist.d1$ticker)
first.date <- max(hist.d1$ds)
all.d1 <- data.frame()

while(length(stocks) > 0)
{
  d1 <- foreach(ticker = stocks, .combine = bind_rows
                 , .errorhandling = 'remove', .packages = c("BatchGetSymbols", "timeDate") ) %dopar%
                 {
                   source("./Functions/F.Trading.Days.R")
                   d1 <- get.clean.data(ticker, src = "yahoo", first.date, last.date = Sys.Date()) %>%
                          rename(ds = ref.date, open = price.open, high = price.high, low = price.low,
                                 close = price.close, adjusted = price.adjusted) %>%
                          arrange(ds) %>%
                          select(ds, ticker, volume, open, high, low, close, adjusted)
                   
                   d1 <- d1 %>% bind_rows(data.frame(ds = NextTradingDate(Date = max(d1$ds)),
                                                     ticker, stringsAsFactors = FALSE))
                   
                   rm(ticker, NextTradingDate, PrevTradingDate, TradingDates)
                   return(d1)
                 }
  
  if(nrow(d1) == 0) 
  {
    cat("\nData Pull/Clean failed for", length(stocks), "stocks\n", stocks, "...\n")
    rm(d1, stocks)
    break
  } else 
  {
    all.d1 <- hist.d1 %>% na.omit() %>% select(-ds.N) %>% filter(ticker %in% unique(d1$ticker)) %>%
              bind_rows(d1) %>%
              group_by(ticker) %>% arrange(ticker, ds) %>% mutate(ds.N = row_number()) %>%
              bind_rows(all.d1) %>% distinct()
    stocks <- setdiff(stocks,  unique(d1$ticker))
    rm(d1)
    
    if(length(stocks) == 0) 
    {
      cat("\nData Pull 100% Succcessful.\n\n")
      rm(stocks, first.date)
      break
    }
    
    cat("\nTrying Again for:", length(stocks), "stocks:", "...\n\n")
  }
}

all.d1 <- all.d1 %>% ungroup()
rm(hist.d1)

# -------------------------------------------------------------------------
# Rescoring with New Data
# -------------------------------------------------------------------------
do.call(file.remove,
        list(list.files(paste0(Parms$data.folder, c("Process.Tracker/", "Production/Scores/")), full.names = TRUE)))

stocks <- setdiff(unique(prod.models$ticker),
                  gsub(".rds", "", list.files(paste0(Parms$data.folder, "Process.Tracker/"))) )

foreach(ticker = stocks
        , .export = c("AF.LN", "AF.MALN"), .packages = c("dplyr", "foreach")
        , .multicombine = TRUE, .inorder = FALSE, .errorhandling = 'remove'
) %dopar%
{
  # ticker <- unique(prod.models$ticker)[1]
  d1 <- all.d1 %>% filter(ticker == !!ticker)
  T.models <- prod.models %>% filter(ticker == !!ticker) %>%
                select(DP.Method, MA.Type, Period, ID) %>% distinct()
    
  Get.Model.Scores(ticker, d1, T.models, Type = "Production")
  saveRDS(ticker, paste0( Parms$data.folder, "Process.Tracker/", ticker, ".rds"))
  
  rm(ticker, d1, T.models)
}

rm(list = lsf.str())

# -------------------------------------------------------------------------
# Get Targets: Stocks in active zone + price points 
# -------------------------------------------------------------------------
source("./Functions/20190801.F.Trading.Simulation.R")
do.call(file.remove,
        list(list.files(paste0(Parms$data.folder, c("Process.Tracker/", "Production/Simulation/"))
                        , full.names = TRUE)))

stocks <- setdiff(gsub(".rds", "", list.files(paste0(Parms$data.folder, "Production/Scores/"))), 
                  gsub(".rds", "", list.files(paste0(Parms$data.folder, "Process.Tracker/")))
                  )

targets <- foreach(ticker = stocks, .combine = bind_rows, .packages = c("dplyr", "foreach"),
                   .multicombine = TRUE, .inorder = FALSE, .errorhandling = 'remove'
                   ) %dopar%
{
  # ticker = "FTCH"
  d1 <- all.d1 %>% filter(ticker == !!ticker) %>% ungroup() %>%
        select(ds, volume, open, low, high, close) %>% arrange(ds) %>% 
        mutate(ROI.l = -zoo::rollmax(-low, 5, fill = NA, align = "left")/lag(close)
               , ROI.h = zoo::rollmax(high, 5, fill = NA, align = "left")/lag(close) )
  
  all <- readRDS(paste0(Parms$data.folder, "Production/Scores/", ticker, ".rds"))
  all <- left_join(all, d1, by = "ds") %>% mutate(Score = round(Score, 4))
  
  if(is.null(all) || nrow(all) == 0)
  {
    saveRDS(ticker, paste0(Parms$data.folder, "Process.Tracker/", ticker, ".rds"))
    rm(ticker, d1, all)
    next()
  }
  
  all <- all %>% group_by(ID) %>%
          filter(ds <= Parms$last.dev.date) %>%
          mutate(R = ntile(Score, 20)) %>%
          group_by(ID, R) %>%
          summarise(R.low = min(Score)) %>%
          mutate(R.low = ifelse(R == min(R), 0, R.low),
                 R.high = ifelse(R == max(R), 1, lead(R.low) - 0.0001)) %>% # works for round 4
          full_join(all, by = "ID") %>%
          filter(Score >= R.low, Score <= R.high) %>% select(-c(R.low, R.high)) %>%
          ungroup() %>% arrange(ticker, ID, ds) %>%
          full_join(data.frame(ticker, algoId = Parms$algoIds, stringsAsFactors = FALSE)
                    , by = "ticker") %>%
          left_join(prod.models, by = c("algoId" , "ticker", "ID", "DP.Method", "MA.Type", "Period", "R"))

  sim <- foreach(algo.ID = Parms$algoIds, .combine = bind_rows, .errorhandling = 'remove') %:%
          foreach(model.ID = unique(all$ID), .combine = bind_rows, .errorhandling = 'remove') %do%
  {
    stop.loss <- case_when(algo.ID == "20190802" ~ 0.95,
                           algo.ID == "20190808" ~ 0.98)
    
    sim <- all %>% filter(ID == model.ID, algoId == algo.ID) %>%
            group_by(ID) %>% arrange(ID, ds) %>%
            mutate(buy.window = case_when(R.buy > 0 ~ 1)) %>%
            AF.roll(df = ., var = "buy.window", width = 3) %>%
            mutate(sell.window = ifelse(!is.na(lag(buy.window)) | !is.na(buy.window), 1, NA)
                   , buy.price  = round(R.buy*lag(close), 2)
                   , buy.price  = buy.window*zoo::na.locf(buy.price, na.rm = FALSE)
                   , sell.price = round(R.sell*lag(close), 2)
                   , sell.price = sell.window*zoo::na.locf(sell.price, na.rm = FALSE)
                   , stop.price = round(R.stop*lag(close), 2)
                   , stop.price = ifelse(R.stop/R.buy > stop.loss, 
                                         round(stop.loss*buy.price, 2), stop.price)
                   , stop.price = sell.window*zoo::na.locf(stop.price, na.rm = FALSE)
                   , last.sell = case_when(sell.window == 1 & is.na(lead(sell.window)) ~ close)
            ) %>%
            AF.simulate.trade(., Type = "Production") 
    
    rm(stop.loss, model.ID, algo.ID)
    return(sim)
  }
  
  # Filters investment in more than 3 models
  # Assigns the investment value
  # Keeps record of the holding investment for sell
  can.buy <- sim %>% group_by(ticker, ds) %>%
              summarise(BOD.in.hand = sum(grepl("HOLD|SELL", action)) ) %>% ungroup() %>%
              filter(ds == Sys.Date()) %>% select(BOD.in.hand) %>%
              unlist() %>% as.numeric()
  can.buy <- floor(max(Parms$invest.max.ticker/Parms$invest.max.model - can.buy, 0))

  suppressWarnings(
  sim <- sim %>% filter(ds == Sys.Date()) %>%
        filter(!is.na(buy.price) || !is.na(sell.price) ) %>%
        group_by(action) %>%
        mutate(rank = rank(-buy.price, ties.method = "min")) %>%
        filter(!is.na(action) | rank <= can.buy) %>%
        ungroup() %>% select(ID) %>%
        inner_join(sim, by = "ID") %>%
        group_by(ID) %>% arrange(ID, ds) %>%
        mutate(invest = case_when(#ds == max(ds) & is.na(action) &
                                  !is.na(buy.price) & 
                                    (is.na(action) | !grepl("HOLD|SELL", action))
                                  ~ round(Parms$max.capacity*lag(volume*(open + close)/2), 0))
               , invest = pmin(invest, Parms$invest.max.model)
               )
  )
  
  if(is.null(sim) || nrow(sim) == 0)
  {
    saveRDS(ticker, paste0(Parms$data.folder, "Process.Tracker/", ticker, ".rds"))
    rm(ticker, d1, all, sim, can.buy)
    return(data.frame())
  } else
  {
    sim <- sim %>% ungroup() 
    today <- sim %>% group_by(ticker, ID) %>% filter(ds == max(ds)) %>%
              select(ticker, ds, buy.price, sell.price, stop.price, last.sell, action, invest, 
                     algoId, ID, DP.Method, MA.Type, Period)
    
    saveRDS(sim, paste0(Parms$data.folder, "Production/Simulation/", ticker, ".rds"))
    saveRDS(ticker, paste0(Parms$data.folder, "Process.Tracker/", ticker, ".rds"))
    rm(ticker, d1, all, sim, can.buy)
    return(today)
  }
  
}

overview <- readRDS(paste0(Parms$data.folder, "Development/Summary/Overview.rds")) %>%
            select(ticker, Exchange, tickerID) %>%
            distinct()

targets <- left_join(targets, overview, by = "ticker") %>%
            mutate(Exchange = ifelse(Exchange == "NASDAQ", "ISLAND", Exchange))

saveRDS(targets, paste0(Parms$data.folder, "Production/Trading/01.Targets.rds"))

# -------------------------------------------------------------------------
stopCluster(cl)
rm(list = ls())

