rm(list = ls())
gc()
options(scipen = 3)
set.seed(1024)
data.folder <- "F:/Project S/MA Linear Modelling/"

library(dplyr)
library(doSNOW)
library(foreach)

closeAllConnections()
cl <- makeCluster(4, outfile="dopar_log.txt")
registerDoSNOW(cl)

# -------------------------------------------------------------------------
source("./Functions/20190801.F.Trading.Simulation.R")
all.d1 <- readRDS(paste0(data.folder, "IB/Clean.Prices.rds")) 
last.dev.date <- as.Date("2019-01-31")
target.ROI <- 1.2

# do.call(file.remove,
#         list(list.files(paste0(data.folder, c("Model.Tracker/", "Prod.Forecasts/")), full.names = TRUE)))

# -------------------------------------------------------------------------
stocks <- setdiff(gsub(".rds", "", list.files(paste0(data.folder, "Model.Scores/"))), 
                  gsub(".rds", "", list.files(paste0(data.folder, "Model.Tracker/")))
                  )

foreach(ticker = stocks,
        .packages = c("dplyr", "foreach"),
        .multicombine = TRUE, .inorder = FALSE,
        .errorhandling = 'remove'
        ) %dopar%
        {
          
          d1 <- all.d1 %>% filter(ticker == !!ticker) %>% 
                select(ds, volume, open, low, high, close) %>% arrange(ds) %>%
                mutate(ROI.l = -zoo::rollmax(-low, 5, fill = NA, align = "left")/lag(close)
                       , ROI.h = zoo::rollmax(high, 5, fill = NA, align = "left")/lag(close) )
          
          all <- left_join(readRDS(paste0(data.folder, "Model.Scores/", ticker, ".rds")), d1, by = "ds") %>%
                  mutate(Score = round(Score, 4), ROI.l = round(ROI.l, 4), ROI.h = round(ROI.h, 4))
          
          if(nrow(all) == 0)
          {
            saveRDS(ticker, paste0(data.folder, "Model.Tracker/", ticker, ".rds"))
            rm(d1, all)
            next()
          }

          sim <- foreach(i = unique(all$ID), .combine = bind_rows, .errorhandling = 'remove') %do%
                  {
                    # i = 117
                    all.ID <- all %>% filter(ID == i) 
                    
                    bins <- all.ID %>% filter(ds <= last.dev.date) %>% select(Score) %>% unlist()
                    bins <- quantile(bins, seq(0, 1, 0.1), names = FALSE)
                    bins <- tail(head(bins, -1), -1)
                    bins <- bins - 0.00000001
                    bins <- bins[bins>0]
                    bins <- c(0, bins, 1)
                    bins <- unique(bins)
                    
                    ranges <- all.ID %>%
                              mutate(R.low = as.numeric(as.character(cut(Score, breaks = bins, labels = head(bins, -1))))
                                   , R.low = ifelse(is.na(R.low), 0 , R.low)
                                   , R.high = as.numeric(as.character(cut(Score, breaks = bins, labels = tail(bins, -1))))
                                   , R.high = ifelse(is.na(R.high), 0 , R.high)) 
          
                    rm(bins, all.ID)
          
                    picks <- foreach(ptile = seq(0.05, 0.5, 0.05), .combine = bind_rows, .errorhandling = 'remove') %do%
                              {
                                picks <- ranges %>% filter(ds <= last.dev.date) %>%
                                          group_by(ticker, ID, R.low, R.high) %>%
                                          rename(R.sell = ROI.h) %>%
                                          mutate(ptile
                                                 , R.stop = min(ROI.l, na.rm = TRUE)                                       
                                                 , R.buy = quantile(ROI.l, probs = ptile, na.rm = TRUE, names = FALSE)
                                                 , R.buy = round(R.buy, 4)
                                                 , R.ROI = round(R.sell/R.buy, 4)
                                                 , R.ROR = round((R.ROI - 1)/(1 - R.stop/R.buy), 2)
                                          ) %>%
                                          filter(ROI.l <= R.buy) %>%
                                          filter(R.sell == min(R.sell)) %>% # Sell @ minimum
                                          select(ticker, ID, R.low, R.high, R.buy, R.sell, R.stop, R.ROI, R.ROR) %>%
                                          ungroup()
                                
                                rm(ptile)
                                return(picks)
                              }
                    
                    picks <- picks %>% 
                              ungroup() %>% mutate(R.Rank = ecdf(R.ROI)(R.ROI)) %>% 
                              group_by(ticker, ID, R.low, R.high) %>% filter(R.ROI == max(R.ROI)) %>%
                              distinct() %>%
                              filter(R.ROI >= target.ROI, R.ROR >= (target.ROI - 1)/0.05)
                    
                    if(nrow(picks) == 0)
                    {
                      rm(ranges, picks, i)
                      next()
                    }
          
                    
                    sim <- left_join(ranges, picks, by = c("ticker", "ID", "R.low", "R.high")) %>%
                            group_by(ID) %>% arrange(ID, ds) %>%
                            mutate(buy.window = case_when(R.buy > 0 ~ 1)) %>%
                            AF.roll(df = ., var = "buy.window", width = 3) %>%
                            mutate(sell.window = ifelse(!is.na(lag(buy.window)) | !is.na(buy.window), 1, NA)
                                   , buy.price  = round(R.buy*lag(close), 2)
                                   , buy.price  = buy.window*zoo::na.locf(buy.price, na.rm = FALSE)
                                   , sell.price = round(R.sell*lag(close), 2)
                                   , sell.price = sell.window*zoo::na.locf(sell.price, na.rm = FALSE)
                                   # Placing a gap of 5% between buy and stop loss
                                   , stop.price = round(R.stop*lag(close), 2)
                                   , stop.price = ifelse(R.stop/R.buy > 0.95, round(0.95*buy.price, 2), stop.price)
                                   , stop.price = sell.window*zoo::na.locf(stop.price, na.rm = FALSE)
                                   , last.sell = case_when(sell.window == 1 & is.na(lead(sell.window)) ~ close)
                            ) %>%
                            AF.simulate.trade(.)
                    
                    rm(picks, ranges)
                    if(all(is.na(sim$ROI)))
                    {
                      rm(sim, i)
                      next()
                    }
                    
                    return(sim)
                  }
          
          if(nrow(sim) == 0)
          {
            saveRDS(ticker, paste0(data.folder, "Model.Tracker/", ticker, ".rds"))
            rm(d1, all, sim)
            next()
          }

          q <-  sim %>% filter(ds <= last.dev.date) %>%
                  select(ticker, ds, ROI, ID, DP.Method, MA.Type, Period) %>%
                  na.omit() %>% group_by(ticker, ID) %>%
                  summarise(N = n()) %>% filter(N > 1)
          
          if(nrow(q) == 0)
          {
            saveRDS(ticker, paste0(data.folder, "Model.Tracker/", ticker, ".rds"))
            rm(d1, all, sim, q)
            next()
          }

          sim <- semi_join(sim, q, by = c("ticker", "ID"))
          saveRDS(sim, paste0(data.folder, "Prod.Forecasts/", ticker, ".rds"))
          saveRDS(ticker, paste0(data.folder, "Model.Tracker/", ticker, ".rds"))
          rm(d1, all, sim, q)
        
        }

rm(AF.roll, AF.simulate.trade)
# -------------------------------------------------------------------------
stocks <- gsub(".rds", "", list.files(paste0(data.folder, "Prod.Forecasts/")))

hist.perf <- foreach(ticker = stocks, .errorhandling = 'remove', .combine = bind_rows) %dopar%
{
  # ticker = stocks[1]
  x <- readRDS(paste0(data.folder, "Prod.Forecasts/", ticker, ".rds")) %>%
        group_by(ID) %>% arrange(ID, ds) %>%
        mutate(Overfit.Index = sum(ds > last.dev.date & !is.na(buy.price))/sum(ds > last.dev.date)) %>%
        filter(!is.na(action)) %>%
        mutate(bought.on = case_when(action == "BUY" ~ ds)) %>% tidyr::fill(bought.on) %>%
        rename(sold.on = ds, sell.type = action) %>%
        select(ticker, bought.on, sold.on, sell.type, capacity, ROI, invest.period, 
               Overfit.Index, ID, DP.Method, MA.Type, Period) %>%
        na.omit() %>% ungroup()
  
  return(x)
}

saveRDS(hist.perf, paste0(data.folder, "Summary/20190801.Historical.Performance.rds"))
rm(stocks)
# -------------------------------------------------------------------------
AF.Scenario.Planning <- function(df, Notes = "")
{
  op <- data.frame()

  for(Scenario in c("Worst ROR", "Best ROR", "Random ROR"))
  {
    if(Scenario == "Worst ROR") {dfx <- df %>% arrange(bought.on, ROR)}
    if(Scenario == "Best ROR") {dfx <- df %>% arrange(bought.on, desc(ROR))}
    if(Scenario == "Random ROR") {dfx <- df %>% arrange(bought.on)}

    for(maxtrades in c(5, 10, 15, 20, 25))
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

# -------------------------------------------------------------------------
# Option A Validation: No Filteration at development period
# -------------------------------------------------------------------------

T8 <- foreach(t.ROI = seq(0.9, 1.1, 0.05), .combine = bind_rows) %:%
      foreach(min.value = seq(0, 0.5, 0.1), .combine = bind_rows) %:%
      foreach(max.value = seq(min.value + 0.1, 1, 0.1), .combine = bind_rows
              , .packages = "dplyr", .errorhandling = 'remove'
              , .multicombine = TRUE, .inorder = FALSE) %dopar%
              {
                # t.ROI = 1
                # min.value = 0.1
                # max.value = 0.5
                
                x1 <- hist.perf %>%
                      filter(bought.on > last.dev.date, 
                             Overfit.Index >= min.value, Overfit.Index <= max.value) %>%
                      group_by(ticker, ID) %>% arrange(ticker, ID, bought.on) %>%
                      mutate(ROR = ROI^(1/(invest.period - 1)), 
                             S = ifelse(ROI >= t.ROI, 1, 0), Cum.S = cumsum(S),  SR = Cum.S/row_number(),
                             S = lag(S), Cum.S = lag(Cum.S), SR = lag(SR) ) %>%
                      group_by(ticker, bought.on) %>% arrange(ticker, bought.on) %>%
                      top_n(3, wt = -ID) %>%                      # Buy Upto 3 models a day
                      ungroup() %>%
                      select(-c(DP.Method, MA.Type, Period)) %>%
                      arrange(ticker, ID, bought.on)
                
                
                x2 <- bind_rows(AF.Scenario.Planning(df = x1, Notes = "No Condition"),
                                AF.Scenario.Planning(df = x1 %>% filter(S >= 1), Notes = "Last was Success"),
                                AF.Scenario.Planning(df = x1 %>% filter(Cum.S >= 1), Notes = "1+ Success"),
                                AF.Scenario.Planning(df = x1 %>% filter(SR >= 0.5), Notes = "50% Success Rate")
                                )
                
                x2 <- x2 %>%
                      mutate(min.Index = min.value, max.Index = max.value, t.ROI) %>%
                      select(Selection.Method, Scenario, t.ROI, min.Index, max.Index, maxtrades,
                             Annual.Returns, ROR, ROI, SR.02, Trades, Missed, Miss.Rate, capacity, 
                             Holding.Period)
                
                rm(x1, t.ROI, min.value, max.value)
                return(x2)
              }

T8.Summary <- reshape(T8
                      , idvar = c("Selection.Method", "t.ROI", "min.Index", "max.Index", "maxtrades")
                      , timevar = "Scenario"
                      , direction = "wide"
                      , sep = " ") %>%
              select("Selection.Method", "t.ROI", "min.Index", "max.Index", "maxtrades"
                     , "Annual.Returns Best ROR", "Annual.Returns Random ROR", "Annual.Returns Worst ROR"
                     , "Miss.Rate Best ROR", "Miss.Rate Random ROR", "Miss.Rate Worst ROR"
                     , "SR.02 Best ROR", "SR.02 Random ROR", "SR.02 Worst ROR"
                     , "Trades Best ROR", "Trades Random ROR", "Trades Worst ROR"
                     , "capacity Best ROR", "capacity Random ROR", "capacity Worst ROR"
                     , "Holding.Period Best ROR", "Holding.Period Random ROR", "Holding.Period Worst ROR"
              )

openxlsx::write.xlsx(list("Summary" = T8.Summary, "Results" = T8)
                     , file = "./Reports/20190801 - Option A - Type 8.xlsx")
# -------------------------------------------------------------------------

stopCluster(cl)
rm(list = ls())
gc()
