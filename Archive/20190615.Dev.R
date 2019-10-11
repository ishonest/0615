source("./Functions/20190615.Functions.R")

# -------------------------------------------------------------------------------
# Get Cleaned Price Data
# -------------------------------------------------------------------------------
overview <- TTR::stockSymbols() %>%
            filter(!grepl("ETF$|Fund$|Trust$|Trust, Inc.$| Fund|^ProShares|^Proshares", Name)) %>%
            ungroup() %>% arrange(Symbol, desc(Exchange)) %>%
            group_by(Symbol) %>%
            top_n(1) %>%
            ungroup()

stocks <- unique(overview$Symbol)
all.d1 <- data.frame()

while(!is.null(stocks))
{
  d1 <- foreach(ticker = stocks, .combine = bind_rows
                , .errorhandling = 'remove', .packages = c("BatchGetSymbols") ) %dopar%
                {
                  d1 <- get.clean.data(ticker, src = "yahoo",
                                       first.date = as.Date("2015-01-01"),
                                       last.date = Sys.Date())
                  rm(ticker)
                  return(d1)
                }
  
  if(nrow(d1) == 0) 
  {
    cat("\nData Pull/Clean failed for", length(stocks), "stocks\n", stocks, "...\n")
    rm(d1, stocks)
    break
  } else 
  {
    stocks <- setdiff(stocks,  unique(d1$ticker))
    d1 <- d1 %>% group_by(ticker) %>% arrange(ticker, ref.date) %>% 
          Get.Data.Clean()
    all.d1 <- bind_rows(all.d1, d1) %>% distinct()
    cat("\nTrying Again for:", length(stocks), "stocks:", "...\n\n")
    rm(d1)
  }
}

saveRDS(all.d1, paste0(data.folder, "IB/Clean.Prices.rds"))
rm(Get.Data.Clean, Get.Portfolio.Tickers, overview)
gc()
# -------------------------------------------------------------------------
# Function Call: Development
# -------------------------------------------------------------------------
do.call(file.remove,
        list(list.files(paste0(data.folder, c("BGS_Cache/", "Model.Performance/", "Prod.Forecasts/")),
            full.names = TRUE)))

if(!exists("all.d1", envir = .GlobalEnv)) {all.d1 <- readRDS(paste0(data.folder, "IB/Clean.Prices.rds"))}
stocks <- setdiff(unique(all.d1$ticker), gsub(".rds", "", list.files(paste0(data.folder, "BGS_Cache/"))))

clusterEvalQ(cl, source("./Functions/F.Data.Transformation.R"))
clusterEvalQ(cl, source("./Functions/F.Trading.Simulation.R"))

foreach(ticker = stocks,
        .packages = c("dplyr", "TTR", "foreach"),
        .multicombine = TRUE, .inorder = FALSE, 
        .errorhandling = 'remove'
        ) %dopar%
      {
        # ticker <- "AMD"
        d1 <- all.d1 %>% filter(ticker == !!ticker)
        Get.Models(d1, ticker, Type = "Development")
        saveRDS(d1, paste0(data.folder, "BGS_Cache/", ticker, ".rds"))
        rm(d1, ticker)
      }

rm(stocks, all.d1, Get.Models)

# -------------------------------------------------------------------------
# Model Performances
# -------------------------------------------------------------------------
stocks <- gsub(".rds", "", list.files(paste0(data.folder, "Model.Performance/")))
all.models <- foreach(ticker = stocks, .combine = bind_rows, .errorhandling = 'remove') %do%
{return(readRDS(paste0(data.folder, "Model.Performance/", ticker, ".rds")))}

stocks <- gsub(".rds", "", list.files(paste0(data.folder, "Prod.Forecasts/")))
all.forecasts <- foreach(ticker = stocks, .combine = bind_rows, .errorhandling = 'remove') %dopar%
{return(readRDS(paste0(data.folder, "Prod.Forecasts/", ticker, ".rds")))}
rm(stocks, ticker)

saveRDS(all.models, file = paste0(data.folder, "Summary/20190725.All.Models.rds"))
saveRDS(all.forecasts, file = paste0(data.folder, "Summary/20190725.All.Forecasts.rds"))

summary(all.models)

stopCluster(cl)
rm(list = ls())
gc()

