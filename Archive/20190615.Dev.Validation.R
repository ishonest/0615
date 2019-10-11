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
all.models <- readRDS(paste0(data.folder, "Summary/20190725.All.Models.rds"))
all.forecasts <- readRDS(paste0(data.folder, "Summary/20190725.All.Forecasts.rds"))
last.dev.date <- as.Date("2019-01-31")

# -------------------------------------------------------------------------
# Type 1: Invest on every model equally (meeting the criterion)
# -------------------------------------------------------------------------
# Val.Type.01(all.models, xfactor = "ROR", xbreaks = seq(0.85, 1.1, 0.005))
# Val.Type.01(all.models, xfactor = "ROI", xbreaks = seq(0.8, 1.5, 0.01))
# Val.Type.01(all.models, xfactor = "invest.period", xbreaks = seq(2, 40, 1))
# Val.Type.01(all.models, xfactor = "capacity", xbreaks = c(seq(1, 4.75, 0.25), seq(5, 100, 1)))

# -------------------------------------------------------------------------
# Type 2: Invest on the first trades from a ticker of week equally (meeting the criterion)
# -------------------------------------------------------------------------
# Val.Type.02(all.models, xfactor = "ROR", xbreaks = seq(0.85, 1.1, 0.005))
# Val.Type.02(all.models, xfactor = "ROI", xbreaks = seq(0.8, 1.5, 0.01))
# Val.Type.02(all.models, xfactor = "invest.period", xbreaks = seq(2, 40, 1))
# Val.Type.02(all.models, xfactor = "capacity", xbreaks = c(seq(1, 4.75, 0.25), seq(5, 100, 1)))

# -------------------------------------------------------------------------
# Type 3: Invest in all trades in a week until 10 trades (meeting the criterion)
# -------------------------------------------------------------------------
# Val.Type.03(all.models, maxtrades = 10, xfactor = "ROR", xbreaks = seq(0.85, 1.1, 0.005))
# Val.Type.03(all.models, maxtrades = 10, xfactor = "ROI", xbreaks = seq(0.8, 1.5, 0.01))
# Val.Type.03(all.models, maxtrades = 10, xfactor = "invest.period", xbreaks = seq(2, 40, 1))
# Val.Type.03(all.models, maxtrades = 10, xfactor = "capacity", xbreaks = c(seq(1, 4.75, 0.25), seq(5, 100, 1)))

# -------------------------------------------------------------------------
# Type 4 Validation: Invest with a fixed amount in bulks of N
# i.e. If initial investment = $10k and N = 5; Invest in lots of $2k
# -------------------------------------------------------------------------
# Val.Type.04(all.models, max.lot = 20, xfactor = "ROR", xbreaks = seq(0.85, 1.1, 0.005))
# Val.Type.04(all.models, max.lot = 20, xfactor = "ROI", xbreaks = seq(0.8, 1.5, 0.01))
# Val.Type.04(all.models, max.lot = 20, xfactor = "invest.period", xbreaks = seq(2, 40, 1))
# Val.Type.04(all.models, max.lot = 20, xfactor = "capacity", xbreaks = c(seq(1, 4.75, 0.25), seq(5, 100, 1)))

# -------------------------------------------------------------------------
# Type 5 Validation: Invest with a on.hand / N (min 100). Check ROI (since Feb);
# -------------------------------------------------------------------------
# Val.Type.05(all.models, max.lot = 20, xfactor = "ROR", xbreaks = seq(0.85, 1.1, 0.005))
# Val.Type.05(all.models, max.lot = 20, xfactor = "ROI", xbreaks = seq(0.8, 1.5, 0.01))
# Val.Type.05(all.models, max.lot = 20, xfactor = "invest.period", xbreaks = seq(2, 40, 1))
# Val.Type.05(all.models, max.lot = 20, xfactor = "capacity", xbreaks = c(seq(1, 4.75, 0.25), seq(5, 100, 1)))

# -------------------------------------------------------------------------
# Type 6 Validation: Optimize Overfitting Index for Type 4 investment
# -------------------------------------------------------------------------
# Val.Type.06(all.models, all.forecasts, Type = "BUY DATE")
# Val.Type.06(all.models, all.forecasts, Type = "END OF PERIOD")

# -------------------------------------------------------------------------
# Type 7 Validation
# -------------------------------------------------------------------------

# T7.Buy.Date <- Val.Type.07(all.models, all.forecasts, Type = "BUY DATE")
# T7.End.Period <- Val.Type.07(all.models, all.forecasts, Type = "END OF PERIOD")
# 
# openxlsx::write.xlsx(list("Buy Date" = T7.Buy.Date, "End of Period" = T7.End.Period), 
#                      file = "./Reports/Type 7 Sweetspots.xlsx")

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
stopCluster(cl)
rm(list = ls())
gc()
