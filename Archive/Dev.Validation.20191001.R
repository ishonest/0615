# 4 Months Validation is better than 6 Months Validation
# Vix models are inferior than non-VIX models

# rm(list = ls())
# gc()

Parms <- list(algoIds = c("20191001")
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
source("./Functions/20191001.Trading.Simulation.R")
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
        select(ticker, ID, 
               Trades.Dev, Trades.Val, Trades.XXX, 
               ROI.Dev, ROI.Val, ROI.XXX,
               Period.Dev, Period.Val, Period.XXX)

rm(stocks)
rm(list = lsf.str())
saveRDS(sum, paste0(Parms[["summary.folder"]], "20191001.Performance.Summary.rds"))

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

saveRDS(hist.perf, paste0(Parms[["summary.folder"]], "20191001.Performance.Detail.rds"))
rm(stocks)


# -------------------------------------------------------------------------
sum.select <- sum %>% 
              filter(ROI.Dev >= 1.1, 
                     ROI.Val >= ROI.Dev - 0.05, ROI.Val <= ROI.Dev + 0.05) %>%
              group_by(ticker) %>%
              summarise(Models = n(), ROI.XXX = weighted.mean(ROI.XXX, Trades.XXX, na.rm = TRUE)) %>%
              filter(ROI.XXX >= 1.05) %>% select(-ROI.XXX) %>%
              inner_join(sum %>% 
                          filter(ROI.Dev >= 1.1, 
                                 ROI.Val >= ROI.Dev - 0.05, ROI.Val <= ROI.Dev + 0.05)
                         , by = c("ticker")) %>%
              select(ticker, ID)

# summary(sum.select$ROI.XXX)
# weighted.mean(sum.select$ROI.XXX, sum.select$Trades.XXX, na.rm = TRUE)

# -------------------------------------------------------------------------
# Type 9 Validation
# -------------------------------------------------------------------------
source("./Functions/F.Validation.R")
hist.perf <- readRDS(paste0(Parms[["summary.folder"]], "20191001.Performance.Detail.rds")) %>%
              inner_join(sum.select, by = c("ticker", "ID"))

T9.Summary <- Val.Type.09(hist.perf, last.dev.date = Parms[["last.dev.date"]] + 120)
T9.Compare.Index(hist.perf, all.d1, last.dev.date = Parms[["last.dev.date"]] + 120)

openxlsx::write.xlsx(list("T9.Summary" = T9.Summary), file = "./Reports/20191001 Type 9.xlsx")
rm(list = lsf.str())

# -------------------------------------------------------------------------
# Production Models
# -------------------------------------------------------------------------
prod.models <- inner_join(sum, sum.select, by = c("ticker", "ID", "R")) %>%
                mutate(algoId = "20191001") %>%
                  select(algoId, ticker, ID, Type, R, R.low, R.high, R.buy, R.sell, R.stop)  

saveRDS(prod.models, paste0(Parms[["summary.folder"]], "20191001.Production.Models.rds"))

# -------------------------------------------------------------------------
do.call(file.remove, list(list.files(c("./Data/Process.Tracker/", "./Data/Simulation/"), 
                                     full.names = TRUE)))

stopCluster(cl)
rm(list = ls())
gc()
