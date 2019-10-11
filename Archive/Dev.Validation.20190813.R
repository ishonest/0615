# Optimise ROI and Stop Loss

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

source("./Functions/20190801.F.Trading.Simulation.R")
all.d1 <- readRDS(paste0(data.folder, "Development/Summary/Clean.Prices.rds")) 
last.dev.date <- as.Date("2019-01-31")

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
          # ticker = "EOLS"
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
          val.models <- all %>% 
                        group_by(ID) %>% arrange(ID, ds) %>%
                        mutate(Type = ifelse(ds <= last.dev.date, "Dev", "Prod"),
                               Rise.5D = case_when(lead(close, 4) > lag(close) ~ 1) ) %>%
                        group_by(ticker, ID, R, R.low, R.high, Type) %>%
                        summarise(P.Rise.5D = sum(Rise.5D, na.rm = TRUE)/n(), N = n() ) %>%
                        data.table::setDT() %>%
                        data.table::dcast(ticker + ID + R + R.low + R.high ~ Type, 
                                        value.var = c("P.Rise.5D", "N"), sep = "." ) %>%
                        group_by(ID) %>%
                        arrange(ticker, ID, -R) %>%
                        filter(P.Rise.5D.Dev >= 0.9, P.Rise.5D.Prod >= 0.9) %>%
                        select(ticker, ID, R)
          
          if(is.null(val.models) || nrow(val.models) == 0)
          {
            saveRDS(ticker, paste0(data.folder, "Process.Tracker/", ticker, ".rds"))
            rm(ticker, d1, all, val.models)
            next()
          }

          # # Stop Loss Range between 1% & 5%
          # # Minimum Reward of 10%
          sim <- foreach(i = unique(val.models$ID), .combine = bind_rows
                         , .errorhandling = 'remove') %do%
                  {
                    # i = unique(val.models$ID)[3]
                    all.ID <- all %>% filter(ID == i) 

                    # # Buy/Sell/Stop Loss points from Development Sample
                    # # Sell @ Lowest Possible; Buy @ Highest w/ Sharpe > 3
                    picks <- all.ID %>% 
                              filter(ds <= last.dev.date) %>%
                              semi_join(val.models, by = c("ID", "R", "ticker")) %>%
                              group_by(ticker, ID, R, R.low, R.high) %>%
                              mutate(R.buy = ROI.l
                                     , R.sell = min(ROI.h)
                                     , R.stop = min(ROI.l, na.rm = TRUE) - 0.0001
                                     , R.Risk = 1 - round(R.stop/R.buy, 4)
                                     , R.ROI = round(R.sell/R.buy, 4)
                                     , R.ROR = (R.ROI - 1)/sd(R.ROI)
                              ) %>%
                              filter(R.ROR >= 3, R.ROI >= 1.1, R.Risk >= 0.01, R.Risk <= 0.05) %>%
                              select(ticker, ID, R, R.low, R.high , R.buy, R.sell, R.stop
                                     , R.ROI, R.Risk, R.ROR) %>%
                              ungroup() %>% arrange(R, R.buy) %>%
                              mutate(Pick.ID = row_number())
                    
                    if(is.null(picks) || nrow(picks) == 0)
                    {
                      rm(i, all.ID, picks)
                      next()
                    }

                    # Simulation on Full Sample
                    q <- foreach(p = picks$Pick.ID, .combine = bind_rows, .errorhandling = 'remove') %do%
                    {
                      # p = picks$Pick.ID[1]
                      sim <- left_join(all.ID %>% filter(ds <= last.dev.date)
                                       , picks %>% filter(Pick.ID == p)
                                       , by = c("ticker", "ID", "R", "R.low", "R.high")) %>%
                              group_by(ID) %>% arrange(ID, ds) %>%
                              mutate(buy.window = case_when(R.buy > 0 ~ 1)) %>%
                              AF.roll(df = ., var = "buy.window", width = 3) %>%
                              mutate(sell.window = ifelse(!is.na(lag(buy.window)) | !is.na(buy.window), 
                                                          1, NA)
                                     , buy.price  = round(R.buy*lag(close), 2)
                                     , buy.price  = buy.window*zoo::na.locf(buy.price, na.rm = FALSE)
                                     , sell.price = round(R.sell*lag(close), 2)
                                     , sell.price = sell.window*zoo::na.locf(sell.price, na.rm = FALSE)
                                     , stop.price = round(R.stop*lag(close), 2)
                                     , stop.price = sell.window*zoo::na.locf(stop.price, na.rm = FALSE)
                                     , last.sell = case_when(sell.window == 1 & is.na(lead(sell.window)) 
                                                             ~ close)
                              ) %>%
                              AF.simulate.trade(.) %>%
                              ungroup()
                      
                      q <- sim %>% filter(!is.na(ROI)) %>%
                            summarise(Pick.ID = p,
                                      Dev.N = n(),
                                      Dev.ROI = prod(ROI),
                                      Dev.Period = mean(invest.period)
                                      )
                      
                      rm(p, sim)
                      return(q)
                    }
                    
                    picks <- inner_join(picks, q, by = "Pick.ID") %>%
                              group_by(ticker, ID, R) %>%
                              filter(Dev.ROI == max(Dev.ROI)) %>%
                              filter(R.Risk == min(R.Risk)) %>%
                              select(-c(Pick.ID, Dev.N, Dev.ROI, Dev.Period))
                    
                    if(is.null(picks) || nrow(picks) == 0)
                    {
                      rm(i, all.ID, picks, q)
                      next()
                    }

                    sim <- left_join(all.ID, picks, by = c("ticker", "ID", "R", "R.low", "R.high")) %>%
                            group_by(ID) %>% arrange(ID, ds) %>%
                            mutate(buy.window = case_when(R.buy > 0 ~ 1)) %>%
                            AF.roll(df = ., var = "buy.window", width = 3) %>%
                            mutate(sell.window = ifelse(!is.na(lag(buy.window)) | !is.na(buy.window), 
                                                        1, NA)
                                   , buy.price  = round(R.buy*lag(close), 2)
                                   , buy.price  = buy.window*zoo::na.locf(buy.price, na.rm = FALSE)
                                   , sell.price = round(R.sell*lag(close), 2)
                                   , sell.price = sell.window*zoo::na.locf(sell.price, na.rm = FALSE)
                                   , stop.price = round(R.stop*lag(close), 2)
                                   , stop.price = sell.window*zoo::na.locf(stop.price, na.rm = FALSE)
                                   , last.sell = case_when(sell.window == 1 & is.na(lead(sell.window)) 
                                                           ~ close)
                            ) %>%
                            AF.simulate.trade(.) %>%
                            ungroup()
                    
                    rm(i, all.ID, picks, q)
                    return(sim)
                  }

          # # Evaluation
          # x <- sim %>% filter(ds <= last.dev.date, !is.na(ROI)) %>% arrange(ID, ds) %>%
          #       select(ticker, ID, ds, action, capacity, ROI, invest.period)
          # summary(x$ROI)
          # 
          # y <- sim %>% filter(ds > last.dev.date, !is.na(ROI)) %>% arrange(ID, ds) %>%
          #   select(ticker, ID, ds, action, capacity, ROI, invest.period)
          # summary(y$ROI)
          # 
          # z <- sim %>% select(ticker, ID, R, R.buy, R.sell, R.stop, R.ROI, R.Risk, R.ROR) %>%
          #       distinct() %>% na.omit() %>%
          #       arrange(ticker, ID, R)
          # 
          # table(sim$action)
          # rm(x, y, z)
          
          saveRDS(ticker, paste0(data.folder, "Process.Tracker/", ticker, ".rds"))
          
          if(is.null(sim) || nrow(sim) == 0)
          {
            rm(ticker, d1, all, val.models, sim)
            next()
          } else
          {
            saveRDS(sim, paste0(data.folder, "Development/Simulation/", ticker, ".rds"))
            rm(ticker, d1, all, val.models, sim)
          }

        }

rm(AF.roll, AF.simulate.trade, all.d1)
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
        select(ticker, bought.on, sold.on, invest.period, capacity, ROI, ROR, 
               sell.type, ID, DP.Method, MA.Type, Period) %>%
        na.omit() %>% ungroup()
  
  rm(ticker)
  return(x)
}

summary(hist.perf %>% filter(bought.on > last.dev.date))

saveRDS(hist.perf, paste0(data.folder, "Summary/20190813.Historical.Performance.rds"))
rm(stocks)

# -------------------------------------------------------------------------
# Type 9 Validation
# -------------------------------------------------------------------------
source("./Functions/F.Validation.R")
T9.Summary <- Val.Type.09(hist.perf)

openxlsx::write.xlsx(list("T9.Summary" = T9.Summary), file = "./Reports/20190813 Type 9.xlsx")

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
                              mutate(algoId = "20190813") %>%
                              select(algoId, ticker, DP.Method, MA.Type, Period, ID, 
                                     R, R.low, R.high, R.buy, R.sell, R.stop) %>%
                              distinct() %>%
                              arrange(ID, desc(R))
                       
                       rm(ticker)
                       return(x)
                     }

saveRDS(prod.models, paste0(data.folder, "Summary/20190813.Production.Models.rds"))
prod.models <- readRDS(paste0(data.folder, "Summary/20190813.Production.Models.rds"))

if(file.exists(paste0(data.folder, "Summary/Production.Models.rds")))
{
  prod.models <- bind_rows(prod.models,
                           readRDS(paste0(data.folder, "Summary/Production.Models.rds"))
                           ) %>%
                  arrange(ticker, ID, R, algoId)
}

saveRDS(prod.models, paste0(data.folder, "Summary/Production.Models.rds"))
rm(prod.models)

# -------------------------------------------------------------------------
stopCluster(cl)
rm(list = ls())
gc()
