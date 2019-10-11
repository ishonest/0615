source("./Functions/F.Trading.Simulation.R")

# -------------------------------------------------------------------------
# Logic
# -------------------------------------------------------------------------
# 10 Bins
# Minimum stop loss of 4%
# multiple bins / model selected
# LONG / SHORT defined by the difference in ROR
# -------------------------------------------------------------------------

Dev.Simulation <- function(ticker, validation.period = 120)
{
  # ticker = "AXGT"
  # validation.period = 120
  d1 <- all.d1 %>% filter(ticker == !!ticker) %>%
        select(ds, volume, open, low, high, close) %>% arrange(ds) %>%
        mutate(ROI.l = round(-zoo::rollmax(-low, 5, fill = NA, align = "left")/lag(close), 4)
               , ROI.h = round(zoo::rollmax(high, 5, fill = NA, align = "left")/lag(close), 4)
               , ROI.c = round(lead(close, 5-1)/lag(close), 4)
               , ROI.d = close/lag(close)
               )

  all <- readRDS(paste0(Parms[["scores.folder"]], ticker, ".rds")) %>%
        left_join(d1, by = "ds") %>% mutate(Score = round(Score, 4))

  if(is.null(all) || nrow(all) == 0)
  {
    rm(ticker, d1, all, validation.period)
    return(NULL)
  }

  # Binning in Ranges: 10 Bins
  bins <- 10
  all <- all %>% group_by(ID) %>%
          filter(ds <= Parms[["last.dev.date"]]) %>%
          mutate(R = ntile(Score, bins)) %>%
          group_by(ID, R) %>%
          summarise(R.low = min(Score)) %>%
          mutate(R.low = ifelse(R == min(R), 0, R.low),
                 R.high = ifelse(R == max(R), 1, lead(R.low) - 0.0001)) %>% # works for round 4
          full_join(all, by = "ID") %>%
          filter(Score >= R.low, Score <= R.high) %>%
          ungroup() %>% arrange(ticker, ID, ds)
  
  val.days <- nrow(d1 %>% filter(ds > Parms[["last.dev.date"]], 
                                 ds <= Parms[["last.dev.date"]] + validation.period))/bins
  
  val.models <- all %>%
                group_by(ID) %>% arrange(ID, ds) %>%
                mutate(ds.range = ifelse(ds <= Parms[["last.dev.date"]], "Dev",
                                         ifelse(ds <= Parms[["last.dev.date"]] + validation.period, "Val", 
                                                "XXX")) ) %>%
                group_by(ticker, ID, R, R.low, R.high, ds.range) %>%
                summarise(N = sum(!is.na(ROI.d))
                          , M = prod(ROI.d, na.rm = TRUE)^(1/N)
                          ) %>% 
                data.table::setDT() %>%
                data.table::dcast(ticker + ID + R + R.low + R.high ~ ds.range,
                                  value.var = c("N", "M"), sep = "." ) %>%
                filter(between(N.Val, 0.8*val.days, 2*val.days)
                       , M.Val >= M.Dev - 0.02
                       , M.Val <= M.Dev + 0.02
                       ) %>%
                group_by(ID) %>% arrange(ticker, ID, -R) %>%
                mutate(Type = case_when(M.Dev <= 0.98 & M.Val <= 0.98 ~ "SHRT",
                                        M.Dev >= 1.02 & M.Val >= 1.02 ~ "LONG")) %>%
                filter(!is.na(Type)) %>%
                ungroup() %>% arrange(ticker, ID, R)
  
  if(is.null(val.models) || nrow(val.models) == 0)
  {
    rm(ticker, d1, all, validation.period, val.models, bins, val.days)
    return(NULL)
  }
  
  picked <- all %>% filter(ds <= Parms[["last.dev.date"]]) %>%
            inner_join(val.models %>% select(ticker, ID, R, Type), by = c("ID", "R", "ticker")) %>%
            group_by(ticker, ID, R, Type, R.low, R.high) %>%
            mutate(b.key = ifelse(Type == "LONG", ROI.l, ROI.h),
                   s.key = ifelse(Type == "LONG", ROI.h, ROI.l)) %>%
            summarise(b.M = median(b.key)   , b.sd = sd(b.key)
                      , s.M = median(s.key) , s.sd = sd(s.key)
                      , Shapiro.SL = shapiro.test(b.key)$p.value
                      , Shapiro.S = shapiro.test(s.key)$p.value
                      ) %>%
            mutate(R.buy = b.M - b.sd*ifelse(Type == "LONG", 1, -1)
                   , R.sell = s.M + s.sd*ifelse(Type == "LONG", 1, -1)
                   , R.stop = 2*b.sd
                   , R.ROI = ifelse(Type == "LONG", R.sell/R.buy, 2 - R.sell/R.buy)
                   , R.ROR = (R.ROI - 1)/R.stop
                   ) %>%
          select(ticker, ID, R, Type, R.low, R.high, R.buy, R.sell, R.stop, R.ROI, R.ROR,
                 Shapiro.SL, Shapiro.S) %>%
          filter(R.stop <= 0.2) %>%
          ungroup()

  if(is.null(picked) || nrow(picked) == 0)
  {
    rm(ticker, d1, all, validation.period, val.models, picked, bins, val.days)
    return(NULL)
  }

  sim <- semi_join(all, picked, by = "ID") %>%
          left_join(picked, by = c("ticker", "ID", "R", "R.low", "R.high")) %>%
          Simulate.Trading(., Process = "Development")

  sum <- Simulated.Performance.Bin(sim, validation.period) %>%
          left_join(picked, by = c("ticker", "ID", "R", "Type")) %>%
          left_join(val.models %>% select(ticker, ID, R, Type, M.Dev, M.Val, M.XXX) 
                    , by = c("ticker", "ID", "R", "Type"))
  
  # summary(sum$ROI.XXX[sum$ROI.Dev >= 1.1 & sum$ROI.Val >= 1.1])
  # plot(sum$ROI.Dev, sum$ROI.XXX)

  saveRDS(sim, paste0("./Data/Simulation/", ticker, ".rds"))
  rm(ticker, d1, all, validation.period, val.models, picked, sim, bins, val.days)
  return(sum)
}
