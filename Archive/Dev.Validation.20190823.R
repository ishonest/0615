# install.packages(c("doSNOW", "foreach", "IBrokers", "BatchGetSymbols", "TTR", 
#                    "timeDate", "dplyr", "zoo", "lubridate", "tidyr", "data.table", 
#                    "MASS", "profvis", "plotly", "htmlwidgets", "xlsx", "openxlsx"))

rm(list = ls())
gc()
options(scipen = 4)
set.seed(1024)
data.folder <- "F:/Project S/MA Linear Modelling/"

library(dplyr)
library(doSNOW)
library(foreach)
library(plotly)

closeAllConnections()
cl <- makeCluster(4, outfile="dopar_log.txt")
registerDoSNOW(cl)

source("./Functions/20190823.Trading.Simulation.R")
all.d1 <- readRDS(paste0(data.folder, "Development/Summary/Clean.Prices.rds")) 
last.dev.date <- as.Date("2019-01-31")
target.ROI <- 1.2

# -------------------------------------------------------------------------
# Binning in Ranges: 20 Bins
# Models & Buckets with 90% success rate
# Buy @ Nth percentile of low; Sell at minimum of high; Stop Loss at minimum of low
# Optimise Percentile Pick provided ROI >= 20%, ROR >= 4, Stop Loss Range: [1%, 5%]
# Picking the most risky price to pick more stocks (Anti-optimization)
# Qualifying Criterion: At least 1 trade in development
# -------------------------------------------------------------------------
do.call(file.remove,
        list(list.files(paste0(data.folder, c("Process.Tracker/", "Development/Simulation/"))
                        , full.names = TRUE)))

stocks <- setdiff(gsub(".rds", "", list.files(paste0(data.folder, "Development/Scores/"))), 
                  gsub(".rds", "", list.files(paste0(data.folder, "Process.Tracker/")))
                  )

foreach(ticker = stocks,
        .packages = c("dplyr", "foreach"),
        .multicombine = TRUE, .inorder = FALSE,
        .errorhandling = 'remove'
        ) %dopar%
        {
          # ticker = "LOVE"
          d1 <- all.d1 %>% filter(ticker == !!ticker) %>% 
                select(ds, volume, open, low, high, close) %>% arrange(ds) %>%
                mutate(ROI.l = -zoo::rollmax(-low, 5, fill = NA, align = "left")/lag(close)
                       , ROI.h = zoo::rollmax(high, 5, fill = NA, align = "left")/lag(close) )
          
          all <- readRDS(paste0(data.folder, "Development/Scores/", ticker, ".rds"))
          all <- left_join(all, d1, by = "ds") %>%
                  mutate(Score = round(Score, 4), ROI.l = round(ROI.l, 4), ROI.h = round(ROI.h, 4))
          
          if(is.null(all) || nrow(all) == 0)
          {
            saveRDS(ticker, paste0(data.folder, "Process.Tracker/", ticker, ".rds"))
            rm(ticker, d1, all)
            next()
          }
          
          # Binning in Ranges: 20 Bins
          all <- all %>% group_by(ID) %>%
                  filter(ds <= last.dev.date) %>%
                  mutate(R = ntile(Score, 20)) %>%
                  group_by(ID, R) %>%
                  summarise(R.low = min(Score)) %>%
                  mutate(R.low = ifelse(R == min(R), 0, R.low),
                         R.high = ifelse(R == max(R), 1, lead(R.low) - 0.0001)) %>% # works for round 4
                  full_join(all, by = "ID") %>%
                  filter(Score >= R.low, Score <= R.high) %>%
                  ungroup() %>% arrange(ticker, ID, ds)
          
          # Models & Buckets with 90% success rate
          # Buckets for LONG & SHORT need to be mutually exclusive
          val.models <- all %>% 
                        group_by(ID) %>% arrange(ID, ds) %>%
                        mutate(Type = ifelse(ds <= last.dev.date, "Dev", "Prod"),
                               Rise.5D = case_when(lead(close, 4) > lag(close) ~ 1),
                               Fall.5D = case_when(lead(close, 4) < lag(close) ~ 1)
                               ) %>%
                        group_by(ticker, ID, R, R.low, R.high, Type) %>%
                        summarise(N = n()
                                  , P.Rise.5D = sum(Rise.5D, na.rm = TRUE)/N
                                  , P.Fall.5D = sum(Fall.5D, na.rm = TRUE)/N
                                  ) %>%
                        data.table::setDT() %>%
                        data.table::dcast(ticker + ID + R + R.low + R.high ~ Type, 
                                        value.var = c("N", "P.Rise.5D", "P.Fall.5D"), sep = "." ) %>%
                        mutate(Type = case_when(P.Rise.5D.Dev >= 0.9 & P.Rise.5D.Prod >= 0.9 ~ "LONG",
                                                P.Fall.5D.Dev >= 0.9 & P.Fall.5D.Prod >= 0.9 ~ "SHRT")
                               ) %>%
                        group_by(ID) %>%
                        filter(!is.na(Type)) %>%
                        arrange(ticker, ID, R) %>%
                        select(ticker, ID, R, Type)
          
          if(is.null(val.models) || nrow(val.models) == 0)
          {
            saveRDS(ticker, paste0(data.folder, "Process.Tracker/", ticker, ".rds"))
            rm(ticker, d1, all, val.models)
            next()
          }

          sim <- foreach(i = unique(val.models$ID), .combine = bind_rows, .errorhandling = 'remove') %do%
                  {
                    # i = unique(val.models$ID)[2]
                    all.ID <- all %>% filter(ID == i) 
                    
                    picks <- foreach(ptile = seq(0.05, 0.5, 0.05), .combine = bind_rows, .errorhandling = 'remove') %do%
                              {
                                picks <- all.ID %>% filter(ds <= last.dev.date) %>%
                                          inner_join(val.models, by = c("ID", "R", "ticker")) %>%
                                          group_by(ticker, ID, R, R.low, R.high) %>%
                                          mutate(ptile
                                                 , R.buy = ifelse(Type == "LONG", 
                                                                  quantile(ROI.l, probs = ptile, 
                                                                           na.rm = TRUE, names = FALSE),
                                                                  quantile(ROI.h, probs = 1 - ptile, 
                                                                           na.rm = TRUE, names = FALSE))
                                                 , R.buy = round(R.buy, 4)
                                                 , R.sell = ifelse(Type == "LONG", ROI.h, ROI.l)
                                                 , R.stop = ifelse(Type == "LONG", 
                                                                   1 - min(ROI.l, na.rm = TRUE)/R.buy,
                                                                   max(ROI.h, na.rm = TRUE)/R.buy - 1)
                                                 , R.stop = round(R.stop, 4)

                                                 , R.ROI = ifelse(Type == "LONG", R.sell/R.buy, 2 - R.sell/R.buy)
                                                 , R.ROI = round(R.ROI, 4)
                                                 , R.ROR = (R.ROI - 1)/R.stop
                                          ) %>%
                                          filter((Type == "LONG" & ROI.l <= R.buy) |
                                                 (Type == "SHRT" & ROI.h >= R.buy) ) %>%
                                          filter((Type == "LONG" & R.sell == min(R.sell)) |
                                                 (Type == "SHRT" & R.sell == max(R.sell))) %>%
                                          select(ticker, ID, R, Type, ptile, R.low, R.high, 
                                                 R.buy, R.sell, R.stop, R.ROI, R.ROR) %>%
                                          ungroup()
                                
                                rm(ptile)
                                return(picks)
                              }
                    
                    picks <- picks %>% na.omit() %>%
                              group_by(ticker, ID, R, R.low, R.high, Type) %>% 
                              filter(R.ROI >= target.ROI, R.ROR >= 4, between(R.stop, 0.01, 0.05)) %>%
                              # >>> Anti-Optimization: Picking the most risky price to pick more stocks
                              filter(R.ROR == min(R.ROR)) %>% # Anti-optimization
                              select(-ptile) %>% distinct()
                    
                    if(nrow(picks) == 0)
                    {
                      rm(all.ID, picks, i)
                      next()
                    }
          
                    sim <- left_join(all.ID, picks, by = c("ticker", "ID", "R", "R.low", "R.high")) %>%
                            group_by(ID) %>% arrange(ID, ds) %>%
                            mutate(buy.window = case_when(R.buy > 0 ~ 1)) %>%
                            AF.roll(df = ., var = "buy.window", width = 3) %>%
                            mutate(  sell.window = ifelse(!is.na(lag(buy.window)) | !is.na(buy.window), 1, NA)
                                   , signal = AF.Signal.Strength(window = sell.window)
                                   , Type = zoo::na.locf(Type, na.rm = FALSE)
                                   , Type = case_when(sell.window == 1 ~ Type)

                                   , buy.price  = round(R.buy*lag(close), 2)
                                   , buy.price  = buy.window*zoo::na.locf(buy.price, na.rm = FALSE)
                                   
                                   , sell.price = round(R.sell*lag(close), 2)
                                   , sell.price = sell.window*zoo::na.locf(sell.price, na.rm = FALSE)

                                   # Prorated Stop Loss:  95% - 99% in Day 1-5
                                   , stop.price = ifelse(Type == "LONG", 
                                                         buy.price*(1 - R.stop), 
                                                         buy.price*(1 + R.stop))
                                   , stop.price = sell.window*zoo::na.locf(stop.price, na.rm = FALSE)
                                   , stop.price = case_when(Type == "LONG" & signal <= 5 ~ (0.94 + signal*0.01)*stop.price,
                                                            Type == "SHRT" & signal <= 5 ~ (1.06 - signal*0.01)*stop.price,
                                                            signal >  5 ~ stop.price)
                                   , stop.price = round(stop.price, 2)
                                   
                                   , last.sell = case_when(sell.window == 1 & is.na(buy.window) & !is.na(close) ~ close,
                                                           sell.window == 1 & is.na(buy.window) & is.na(close) ~ 0
                                   )
                            ) %>%
                            AF.simulate.hedge(.)
                    
                    rm(all.ID, picks, i)
                    if(all(is.na(sim$ROI)))
                    {
                      rm(sim)
                      next()
                    }
                    
                    return(sim)
                  }
          
          # x <- sim %>% filter(ds > last.dev.date, !is.na(action)) %>% arrange(ID, ds)
          # summary(x$ROI)
          
          if(is.null(sim) || nrow(sim) == 0)
          {
            saveRDS(ticker, paste0(data.folder, "Process.Tracker/", ticker, ".rds"))
            rm(ticker, d1, all, val.models, sim)
            next()
          }

          q <-  sim %>% filter(ds <= last.dev.date) %>%
                  select(ticker, ds, ROI, ID, DP.Method, MA.Type, Period) %>%
                  na.omit() %>% group_by(ticker, ID) %>%
                  summarise(N = n()) %>% filter(N >= 1)
          
          if(nrow(q) == 0)
          {
            saveRDS(ticker, paste0(data.folder, "Process.Tracker/", ticker, ".rds"))
            rm(ticker, d1, all, val.models, sim, q)
            next()
          }

          sim <- semi_join(sim, q, by = c("ticker", "ID"))
          saveRDS(sim, paste0(data.folder, "Development/Simulation/", ticker, ".rds"))
          saveRDS(ticker, paste0(data.folder, "Process.Tracker/", ticker, ".rds"))
          rm(ticker, d1, all, val.models, sim, q)
        }

rm(list = lsf.str())
rm(all.d1)

# -------------------------------------------------------------------------
# Summarized View of All Trades
# -------------------------------------------------------------------------
stocks <- gsub(".rds", "", list.files(paste0(data.folder, "Development/Simulation/")))

hist.perf <- foreach(ticker = stocks, .combine = bind_rows
                     , .packages = "dplyr", .errorhandling = 'remove') %dopar%
{
  # ticker = stocks[1]
  x <- readRDS(paste0(data.folder, "Development/Simulation/", ticker, ".rds")) %>%
        filter(!is.na(action)) %>%
        mutate(ROR = ROI^(1/(invest.period - 1)),
               bought.on = case_when(action == "BUY" ~ ds)) %>% 
        tidyr::fill(bought.on) %>%
        rename(sold.on = ds, sell.type = action) %>%
        select(ticker, Type, bought.on, sold.on, invest.period, capacity, ROI, ROR, 
               sell.type, ID, DP.Method, MA.Type, Period) %>%
        na.omit() %>% ungroup()
  
  rm(ticker)
  return(x)
}

summary(hist.perf %>% filter(bought.on > last.dev.date))

saveRDS(hist.perf, paste0(data.folder, "Summary/20190823.Historical.Performance.rds"))
rm(stocks)

# -------------------------------------------------------------------------
# Type 9 Validation
# -------------------------------------------------------------------------
source("./Functions/F.Validation.R")
T9.Summary <- Val.Type.09(hist.perf)
T9.Compare.Index(hist.perf, all.d1, last.dev.date)

openxlsx::write.xlsx(list("T9.Summary" = T9.Summary), file = "./Reports/20190823 Type 9.xlsx")


rm(list = lsf.str())

# -------------------------------------------------------------------------
# Production Models
# -------------------------------------------------------------------------
stocks <- gsub(".rds", "", list.files(paste0(data.folder, "Development/Simulation/")))

prod.models <- foreach(ticker = stocks, .combine = bind_rows
                     , .packages = "dplyr", .errorhandling = 'remove') %dopar%
                     {
                       # ticker = stocks[1]
                       x <- readRDS(paste0(data.folder, "Development/Simulation/", ticker, ".rds")) %>%
                              filter(!is.na(R.buy)) %>%
                              mutate(algoId = "20190823") %>%
                              select(algoId, ticker, DP.Method, MA.Type, Period, ID, Type,
                                     R, R.low, R.high, R.buy, R.sell, R.stop) %>%
                              distinct() %>%
                              arrange(ID, R)
                       
                       rm(ticker)
                       return(x)
                     }

saveRDS(prod.models, paste0(data.folder, "Summary/20190823.Production.Models.rds"))

# -------------------------------------------------------------------------
do.call(file.remove,
        list(list.files(paste0(data.folder, c("Process.Tracker/", "Development/Simulation/"))
                        , full.names = TRUE)))

stopCluster(cl)
rm(list = ls())
gc()
