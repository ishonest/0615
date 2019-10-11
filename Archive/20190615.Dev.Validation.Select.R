# -------------------------------------------------------------------------
# Selected Models Based on Validation
# -------------------------------------------------------------------------

options(scipen = 3)
set.seed(1024)
data.folder <- "F:/Project S/MA Linear Modelling/"

library(dplyr)

all.forecasts <- readRDS(paste0(data.folder, "Summary/20190725.All.Forecasts.rds"))

# -------------------------------------------------------------------------
# selected.models <- all.forecasts %>%
#                     filter(ds > as.Date("2019-01-31")) %>%
#                     group_by(ticker, DP.Method, MA.Type, Period) %>%
#                     summarise(Buy.Days = sum(!is.na(buy.price))/n()) %>%
#                     filter(Buy.Days > 0.08, Buy.Days <= 0.35) %>%
#                     distinct()
# 
# saveRDS(selected.models, file = paste0(data.folder, "Summary/20190615.Selected.Models.rds"))

# -------------------------------------------------------------------------
# selected.models <- all.forecasts %>%
#   filter(ds > as.Date("2019-01-31")) %>%
#   group_by(ticker, DP.Method, MA.Type, Period) %>%
#   summarise(Overfit.Index = sum(!is.na(buy.price))/n()) %>%
#   filter(Overfit.Index >= 0.2, Overfit.Index<= 0.3)
# 
# saveRDS(selected.models, file = paste0(data.folder, "Summary/20190716.Selected.Models.rds"))

# -------------------------------------------------------------------------
saveRDS(selected.models, file = paste0(data.folder, "Summary/Production.Models.rds"))

# -------------------------------------------------------------------------
rm(list = ls())
gc()
