# ---------------------------------------------------------------------------------
# RESULTS
# ---------------------------------------------------------------------------------
# Long: Dev.ROI >= 1.05; Val.ROI >= 1.01
# Shrt: Dev.ROI >= 1.05; Val.ROI >= 1.01
# BRW ROI: 1.5 - 8.2 - 14.9 @ 10 picks
# XXX ROI: 1.72X
# ---------------------------------------------------------------------------------
# Long: Dev.ROI >= 1.10; Val.ROI >= 1.05
# Shrt: Dev.ROI >= 1.10; Val.ROI >= 1.05
# BRW ROI: 2.7 - 4.4 - 5.5 picks
# XXX ROI: 1.92X
# ---------------------------------------------------------------------------------

Parms <- list(algoIds = c("20191007")
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
source("./Functions/20191007.Trading.Simulation.R")
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

rm(stocks)
rm(list = lsf.str())
# saveRDS(sum, paste0(Parms[["summary.folder"]], "20191007.Performance.Summary.rds"))
# sum <- readRDS(paste0(Parms[["summary.folder"]], "20191007.Performance.Summary.rds"))
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

saveRDS(hist.perf, paste0(Parms[["summary.folder"]], "20191007.Performance.Detail.rds"))
# hist.perf <- readRDS(paste0(Parms[["summary.folder"]], "20191007.Performance.Detail.rds"))
rm(stocks)

# -------------------------------------------------------------------------
x1 <- sum %>% 
      filter(ROI.Dev >= 1.05, ROI.Val >= 1.01) %>%
      group_by(ticker) %>% 
      summarise(ROI.XXX = prod(ROI.XXX^Trades.XXX, na.rm = TRUE)) %>%
      filter(ROI.XXX < 1)

x1 <- anti_join(sum, x1, by = "ticker")

sum.select <- x1 %>% 
              filter((Type == "SHRT" & ROI.Dev >= 1.05 & ROI.Val >= 1.01) 
                     | (Type == "LONG" & ROI.Dev >= 1.05 & ROI.Val >= 1.01)
                     ) %>%
              select(ticker, ID, R)
rm(x1)  
# -------------------------------------------------------------------------
# Type 9 Validation
# -------------------------------------------------------------------------
source("./Functions/F.Validation.R")
hist.perf <- readRDS(paste0(Parms[["summary.folder"]], "20191007.Performance.Detail.rds")) %>%
              inner_join(sum.select, by = c("ticker", "ID", "R"))

T9.Summary <- Val.Type.09(hist.perf, last.dev.date = Parms[["last.dev.date"]] + 120)
T9.Compare.Index(hist.perf, all.d1, last.dev.date = Parms[["last.dev.date"]] + 120)

openxlsx::write.xlsx(list("T9.Summary" = T9.Summary), file = "./Reports/20191007 Type 9.xlsx")
rm(list = lsf.str())

# -------------------------------------------------------------------------
# Production Models
# -------------------------------------------------------------------------
prod.models <- inner_join(sum, sum.select, by = c("ticker", "ID", "R")) %>%
                inner_join(hist.perf %>%
                             select(ticker, Type, ID, R, DP.Method, MA.Type, Period) %>%
                             distinct()
                           , by = c("ticker", "Type", "ID", "R") ) %>%
                mutate(algoId = "20191007") %>%
                  select(algoId, ticker, Type, R.low, R.high, R.buy, R.sell, R.stop, 
                         ID, R, DP.Method, MA.Type, Period)  

saveRDS(prod.models, paste0(Parms[["summary.folder"]], "20191007.Production.Models.rds"))

# -------------------------------------------------------------------------
do.call(file.remove, list(list.files(c("./Data/Process.Tracker/", "./Data/Simulation/"), 
                                     full.names = TRUE)))

stopCluster(cl)
rm(list = ls())
gc()
