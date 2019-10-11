# Last Run on 7 May, 2019

# Minimum $100 MM Market Cap
# Minimum $0.5 MM Trade in P3M

library(BatchGetSymbols)
library(foreach)
library(doSNOW)
closeAllConnections()
cl <- makeCluster(3, outfile="dopar_log.txt")
registerDoSNOW(cl)

stocks <- TTR::stockSymbols() %>%
          mutate(Market.Cap.M = as.numeric(
                                sub("\\$(\\d+(\\.\\d+)?)[A-Z]?", "\\1", MarketCap)) *
                                  case_when(gsub("[^A-Z]", "", MarketCap) == "M" ~ 1e6,
                                            gsub("[^A-Z]", "", MarketCap) == "B" ~ 1e9, 
                                            TRUE ~ 1.0),
                 Market.Cap.M = Market.Cap.M/1000000
          ) %>%
          rename(ticker = Symbol) %>%
          select(Exchange, ticker, Name, LastSale, Market.Cap.M, IPOyear, Sector, Industry) %>% 
          filter(!endsWith(Name, "ETF"),
                 !endsWith(Name, "Fund"),
                 !endsWith(Name, "Trust"),
                 !endsWith(Name, "Trust, Inc."),
                 !grepl(" Fund", Name),
                 !startsWith(Name, "ProShares"),
                 !startsWith(Name, "Proshares"),
                 !is.na(Market.Cap.M),
                 Market.Cap.M >= 100)

df <- foreach(ticker = unique(stocks$ticker), 
              .combine = bind_rows, .packages = "BatchGetSymbols", 
              .errorhandling = 'remove') %dopar%
      {
        x <- get.clean.data(ticker, src = "yahoo",
                            first.date = Sys.Date() - 90, last.date = Sys.Date() - 1) %>%
              group_by(ticker) %>%
              summarise(min.trade = min(price.close*volume, na.rm = TRUE),
                        med.trade = median(price.close*volume, na.rm = TRUE),
                        max.trade = max(price.close*volume, na.rm = TRUE),
                        zero.vol = sum(volume == 0)/n()
                        )
        rm(ticker)
        return(x)
      }


# x <- df %>% filter(min.trade >= 500000)
# y <- get.clean.data(ticker = "PHAS", src = "yahoo",
#                     first.date = Sys.Date() - 90, last.date = Sys.Date() - 1)


stocks <- inner_join(stocks, df, by = "ticker") %>%
          mutate(Min.Trade.M = min.trade/1000000) %>%
          filter(Min.Trade.M >= 0.5) %>%
          select(ticker, LastSale, Market.Cap.M, Min.Trade.M, Name, IPOyear, Sector, Industry) %>%
          unique()

saveRDS(stocks, "E:/Project S/v0.5/0415/Valid.Tickers.rds")
stopCluster(cl)
rm(list = ls())