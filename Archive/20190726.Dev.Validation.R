# -------------------------------------------------------------------------
# Validating Decision Making
# -------------------------------------------------------------------------
options(scipen = 3)
set.seed(1024)
data.folder <- "F:/Project S/MA Linear Modelling/"

library(dplyr)
library(doSNOW)
library(foreach)
library(plotly)

closeAllConnections()
cl <- makeCluster(4, outfile="dopar_log.txt")
registerDoSNOW(cl)

source("./Functions/F.Validation.R")
all.models <- readRDS(paste0(data.folder, "Summary/20190726.All.Models.rds"))
all.forecasts <- readRDS(paste0(data.folder, "Summary/20190726.All.Forecasts.rds"))
last.dev.date <- as.Date("2019-01-31")

# -------------------------------------------------------------------------
# Type 1-7 Validation in 20190615.Dev.Validation.R
# -------------------------------------------------------------------------
# Type 8 Validation
# -------------------------------------------------------------------------

T8.Results <- Val.Type.08(all.models, all.forecasts, last.dev.date)
T8.Summary <- reshape(T8.Results
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

openxlsx::write.xlsx(list("Summary" = T8.Summary, "Results" = T8.Results)
                     , file = "./Reports/20190726 Type 8 Sweetspots.xlsx")


# -------------------------------------------------------------------------
# Option 2
# -------------------------------------------------------------------------
x <- all.models %>%
  mutate(Type = ifelse(bought.on <= last.dev.date, "Dev", "Prod")) %>%
  group_by(ticker, DP.Method, MA.Type, Period, Type) %>%
  summarise(Trades = n(), ROI = mean(ROI), ROR = mean(ROR), Duration = mean(invest.period))

x <- data.table::dcast(data.table::setDT(x)
                       , ticker + DP.Method + MA.Type + Period ~ Type
                       , value.var = c("ROI", "ROR", "Trades", "Duration")
                       , sep = ".") %>%
  na.omit()


# 1+ Dev Trades have higher ROI/ROR
# Reports in /Reports/20190726 Type 8 Sweetspots - Option 2.xlsx

x %>% group_by(Trades.Dev) %>% 
  summarise(ROI = mean(ROI.Prod), ROR = mean(ROR.Prod), Trades = sum(Trades.Prod))
# 1+ Dev Trades have higher ROI/ROR

y <- x %>% filter(Trades.Dev > 1) %>% select(ticker, DP.Method, MA.Type, Period) %>%
  inner_join(all.models, by = c("ticker", "DP.Method", "MA.Type", "Period"))

T8.Results <- Val.Type.08(all.models = y, all.forecasts, last.dev.date)
T8.Summary <- reshape(T8.Results
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


openxlsx::write.xlsx(list("Summary" = T8.Summary, "Results" = T8.Results)
                     , file = "./Reports/20190726 Type 8 Sweetspots - Option 2.xlsx")

rm(y, T8.Results, T8.Summary)
# -------------------------------------------------------------------------

stopCluster(cl)
rm(list = ls())
gc()
