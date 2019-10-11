source("./Functions/20190615.Functions.R")
selected.models <- readRDS(paste0(data.folder, "Summary/Production.Models.rds"))
portfolio <- Get.Portfolio.Tickers()

# -------------------------------------------------------------------------------
# Get Cleaned Price Data
# -------------------------------------------------------------------------------
stocks <- unique(selected.models$ticker)
all.d1 <- data.frame()

while(!is.null(stocks))
{
  d1 <- foreach(ticker = stocks
                , .combine = bind_rows
                , .packages = c("BatchGetSymbols", "dplyr", "zoo", "timeDate")
                , .errorhandling = 'remove') %do%
                {
                  d1 <- get.clean.data(ticker, src = "yahoo",
                                       first.date = as.Date("2015-01-01"),
                                       last.date = Sys.Date() + 1) %>%
                        Get.Data.Clean(min.trade = 1000000, min.tdays = 250, bad.jumps = 0.1,
                                       in.portfolio = ifelse(ticker %in% portfolio, TRUE, FALSE)
                                       )
                      
                  rm(ticker)
                  return(d1)
                }
  
  if(nrow(d1) == 0) 
  {
    cat("\nData Pull/Clean failed for", length(stocks), "stocks:", stocks, "...\n")
    rm(d1, stocks)
    break
  } else 
  {
    all.d1 <- bind_rows(all.d1, d1)
    stocks <- setdiff(stocks,  unique(d1$ticker))
    cat("\nTrying Again for:", length(stocks), "stocks:", stocks, "...\n\n")
    rm(d1)
  }
}

saveRDS(all.d1, paste0(data.folder, "IB/Prod.Clean.Data.rds"))
rm(Get.Data.Clean, Get.Portfolio.Tickers, portfolio)
gc()

# -------------------------------------------------------------------------
# Function Call
# -------------------------------------------------------------------------
do.call(file.remove,
        list(list.files(paste0(data.folder, c("BGS_Cache/", "Model.Performance/", "Prod.Forecasts/")),
            full.names = TRUE)))

clusterEvalQ(cl, source("./Functions/F.Data.Transformation.R"))
clusterEvalQ(cl, source("./Functions/F.Trading.Simulation.R"))

if(!exists("all.d1", envir = .GlobalEnv)) {all.d1 <- readRDS(paste0(data.folder, "IB/Prod.Clean.Data.rds"))}
stocks <- setdiff(unique(all.d1$ticker), gsub(".rds", "", list.files(paste0(data.folder, "BGS_Cache/"))))

foreach(ticker = stocks,
        .packages = c("dplyr", "TTR", "foreach"),
        .multicombine = TRUE, .inorder = FALSE,
        .errorhandling = 'remove'
        ) %dopar%
      {
        d1 <- all.d1 %>% filter(ticker == !!ticker)
        T.models <- selected.models %>% ungroup() %>% filter(ticker == !!ticker)
        
        Get.Models(d1, ticker, T.models, Type = "Production")
        saveRDS(d1, paste0(data.folder, "BGS_Cache/", ticker, ".rds"))
        rm(ticker, d1, T.models)
      }

rm(stocks, all.d1, Get.Models)

# -------------------------------------------------------------------------
# Summarization
# -------------------------------------------------------------------------
prod.forecasts <- foreach(ticker = gsub(".rds", "", list.files(paste0(data.folder, "Prod.Forecasts/")))
                          , .combine = bind_rows, .errorhandling = 'remove') %do%
{return(readRDS(paste0(data.folder, "Prod.Forecasts/", ticker, ".rds")))}

prod.summary <- foreach(ticker = gsub(".rds", "", list.files(paste0(data.folder, "Model.Performance/")))
                        , .combine = bind_rows, .errorhandling = 'remove') %do%
{return(readRDS(paste0(data.folder, "Model.Performance/", ticker, ".rds")))}

rm(ticker)

m.prod.summary <-  bind_rows(readRDS(paste0(data.folder, "IB/Master.Prod.Summary.rds")), prod.summary)

saveRDS(prod.forecasts, paste0(data.folder, "IB/Prod.Forecasts.rds"))
saveRDS(prod.summary, paste0(data.folder, "IB/Prod.Summary.rds"))
saveRDS(m.prod.summary, paste0(data.folder, "IB/Master.Prod.Summary.rds"))

# -------------------------------------------------------------------------
stopCluster(cl)
rm(list = ls())
gc()

