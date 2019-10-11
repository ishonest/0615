# 4 Months Validation is better than 6 Months Validation
# Vix models are inferior than non-VIX models

# rm(list = ls())
# gc()

Parms <- list(algoIds = c("20190919")
              , target.ROI = 1.2
              , invest.max.model  = 1000   # Maximum investment in a model
              , invest.max.ticker = 3000   # Maximum investment in a ticker
              , max.capacity      = 0.01   # Max % of yesterday's volume any model can buy
              , scores.folder     = "F:/Project S/MA Linear Modelling/Development/Scores/wo VIX/"
              , code.folder       = "E:/Project S/v0.6/0615/"
              , summary.folder    = "F:/Project S/MA Linear Modelling/Summary/"
              , last.dev.date     = as.Date("2019-01-31"))

# -------------------------------------------------------------------------
# Core Functions
# -------------------------------------------------------------------------
setwd(Parms[["code.folder"]])
source("./Functions/20190918.Trading.Simulation.R")
all.d1 <- readRDS("./Data/Summary/Clean.Prices.rds")
do.call(file.remove, list(list.files(c("./Data/Process.Tracker/", "./Data/Simulation/"), full.names = TRUE)))
stocks <- setdiff(gsub(".rds", "", list.files(Parms[["scores.folder"]])), 
                  gsub(".rds", "", list.files("./Data/Process.Tracker/")))

sum <- foreach(ticker = stocks
               , .combine = bind_rows , .packages = c("dplyr", "foreach", "zoo")
               , .errorhandling = 'remove'
               ) %dopar%
              {
                sum <- Dev.Simulation(ticker)
                saveRDS(ticker, paste0("./Data/Process.Tracker/", ticker, ".rds"))
                rm(ticker)
                if(!is.null(sum)) {return(sum)}
              }

sum <- sum %>%
        select(ticker, ID, R, Type, 
               Trades.Dev, Trades.Val, Trades.XXX, ROI.Dev, ROI.Val, ROI.XXX,
               R.ROI, R.ROR, R.buy, R.sell, R.stop, R.low, R.high)

rm(stocks)
rm(list = lsf.str())
saveRDS(sum, paste0(Parms[["summary.folder"]], "20190918.Performance.Summary.rds"))

sum %>% filter(ROI.Dev >= 1.1) %>% group_by(Type) %>% summarise(N = sum(Trades.XXX, na.rm = TRUE), Mean = mean(ROI.XXX, na.rm = TRUE), Median = median(ROI.XXX, na.rm = TRUE))
# 120 Days Val
# Type      N   Mean    Median
# 1 LONG   4367 0.998   0.993
# 2 SHRT   1633 1.01    1.01 

# -------------------------------------------------------------------------
# Summarized View of All Trades
# -------------------------------------------------------------------------
stocks <- gsub(".rds", "", list.files("./Data/Simulation/"))

hist.perf <- foreach(ticker = stocks, .combine = bind_rows
                     , .packages = "dplyr", .errorhandling = 'remove') %dopar%
{
    # ticker = stocks[1]
    x <- readRDS(paste0("./Data/Simulation/", ticker, ".rds")) %>%
          filter(!is.na(action)) %>%
          mutate(ROR = ROI^(1/invest.period),
                 bought.on = case_when(action == "BUY" ~ ds) ) %>% 
          tidyr::fill(bought.on) %>% select(-R) %>%
          rename(sold.on = ds, sell.type = action, R = buy.bin) %>% 
          select(ticker, Type, bought.on, sold.on, invest.period, capacity, ROI, ROR, 
                 sell.type, ID, R, DP.Method, MA.Type, Period) %>%
          na.omit() %>% ungroup() 
    
    rm(ticker)
    return(x)
  }

saveRDS(hist.perf, paste0(Parms[["summary.folder"]], "20190918.Performance.Detail.rds"))
rm(stocks)

# -------------------------------------------------------------------------
# Reverse-Engineering
# -------------------------------------------------------------------------

# sum1 <- sum %>% filter(between(ROI.Dev - ROI.Val, -0.05, 0.05))

x <- foreach(i = seq(0.8, 2, 0.01), .combine = bind_rows) %do%
  {
    x <- sum %>% 
      mutate(ROI.Dev = ifelse(is.na(ROI.Dev), 0.8, ROI.Dev),
             ROI.Val = ifelse(is.na(ROI.Val), 0.8, ROI.Val),
             Value = ROI.Dev) %>%
      # filter(ROI.Dev >= 1.2) %>%
      filter(Value >= i) %>% 
      group_by(Type) %>%
      summarise(Factor = i, 
                Mean = mean(ROI.XXX, na.rm = TRUE), 
                W.Mean = weighted.mean(ROI.XXX, w = Trades.XXX, na.rm = TRUE),
                Trades = sum(Trades.XXX, na.rm = TRUE)) %>%
      filter(Trades >= 30)
    rm(i)
    return(x)
  }

x %>% group_by(Type) %>%
  plot_ly(x = ~Factor, y = ~W.Mean, type = 'scatter', mode = 'lines', color = ~Type) %>%
  layout(hovermode = 'compare', legend = list(x = 1, y = 0.5))

rm(x)

# -------------------------------------------------------------------------
library(ggplot2)

x <- sum %>% 
  mutate(ROI.Dev = ifelse(is.na(ROI.Dev), 0.900, 0.001*floor(1000*ROI.Dev)),
         ROI.Val = ifelse(is.na(ROI.Val), 0.900, 0.001*floor(1000*ROI.Val))
  ) %>%
  group_by(Type, ROI.Dev, ROI.Val) %>%
  summarise(ROI.XXX = weighted.mean(ROI.XXX, w = Trades.XXX, na.rm = TRUE),
            Trades = sum(Trades.XXX, na.rm = TRUE)) %>%
  filter(Trades >= 30) %>% ungroup()

ggplot(data = x %>% filter(Type == "SHRT") %>% select(ROI.Dev, ROI.Val, ROI.XXX), 
       aes(x = ROI.Dev, y = ROI.Val, fill= ROI.XXX)) + 
  geom_tile() +  hrbrthemes::theme_ipsum()


# -------------------------------------------------------------------------
sum.select <- sum %>% 
              filter(R.ROR >= 5, Type == "SHRT") %>%
              select(ticker, ID, R) %>% distinct() 

# -------------------------------------------------------------------------
# Type 9 Validation
# -------------------------------------------------------------------------
source("./Functions/F.Validation.R")
hist.perf <- readRDS(paste0(Parms[["summary.folder"]], "20190918.Performance.Detail.rds")) %>%
              inner_join(sum.select, by = c("ticker", "ID", "R"))

T9.Summary <- Val.Type.09(hist.perf, last.dev.date = Parms[["last.dev.date"]] + 120)
T9.Compare.Index(hist.perf, all.d1, last.dev.date = Parms[["last.dev.date"]] + 120)

openxlsx::write.xlsx(list("T9.Summary" = T9.Summary), file = "./Reports/20190918 Type 9.xlsx")
rm(list = lsf.str())

# -------------------------------------------------------------------------
# Production Models
# -------------------------------------------------------------------------
prod.models <- inner_join(sum, sum.select, by = c("ticker", "ID", "R")) %>%
                mutate(algoId = "20190918") %>%
                  select(algoId, ticker, ID, Type, R, R.low, R.high, R.buy, R.sell, R.stop)  

saveRDS(prod.models, paste0(Parms[["summary.folder"]], "20190918.Production.Models.rds"))

# -------------------------------------------------------------------------
do.call(file.remove, list(list.files(c("./Data/Process.Tracker/", "./Data/Simulation/"), 
                                     full.names = TRUE)))

stopCluster(cl)
rm(list = ls())
gc()
