source("./Functions/20190918.Functions.R")

# -------------------------------------------------------------------------------
# Get Cleaned Price Data
# -------------------------------------------------------------------------------
overview <- Get.Tickers()
saveRDS(overview, "./Data/Summary/Overview.rds")

stocks <- unique(overview$ticker)
all.d1 <- data.frame()

if(!is.null(stocks))
{
  d1 <- foreach(ticker = stocks, .combine = bind_rows
                , .errorhandling = 'remove', .packages = c("BatchGetSymbols") 
                ) %dopar%
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
    d1 <- d1 %>% group_by(ticker) %>% arrange(ticker, ref.date) %>% Get.Data.Clean()
    all.d1 <- bind_rows(all.d1, d1) %>% distinct()
    cat("\nSuccessful Data Pull for:", length(unique(d1$ticker)), "stocks:", "...\n\n")
    rm(d1)
  }
}

saveRDS(all.d1, "./Data/Summary/Clean.Prices.rds")
rm(Get.Tickers, Get.Data.Clean, overview)
gc()
# -------------------------------------------------------------------------------
# External / Macro Data
# -------------------------------------------------------------------------------

exdata <- BatchGetSymbols::get.clean.data(ticker = "^VIX", src = "yahoo",
                                          first.date = as.Date("2015-01-01")-1,
                                          last.date = Sys.Date()) %>%
          arrange(ref.date) %>%
          mutate(VIX = lag(price.close), ds = ref.date) %>%
          select(ds, VIX) %>% na.omit()

# -------------------------------------------------------------------------
# Function Calling
# -------------------------------------------------------------------------
if(!exists("all.d1", envir = .GlobalEnv)) {all.d1 <- readRDS("./Data/Summary/Clean.Prices.rds")}

do.call(file.remove, list(list.files(
        paste0("./Data/", c("Process.Tracker/", "Scores/", "Specs/")), 
        full.names = TRUE)))

stocks <- setdiff(unique(all.d1$ticker)
                  , gsub(".rds", "", list.files("./Data/Process.Tracker/")) )

foreach(ticker = stocks
        , .export = c("AF.LN", "AF.MALN")
        , .packages = c("dplyr", "foreach", "zoo")
        , .multicombine = TRUE, .inorder = FALSE, .errorhandling = 'remove'
        ) %dopar%
        {
          # ticker <- "STNG"
          d1 <- all.d1 %>% filter(ticker == !!ticker)
          Get.Model.Scores(ticker, d1, exdata, last.dev.date = Parms[["last.dev.date"]])
          saveRDS(ticker, paste0("./Data/Process.Tracker/", ticker, ".rds"))
          rm(d1, ticker)
        }

# -------------------------------------------------------------------------
stopCluster(cl)
rm(cl, stocks)
gc()
