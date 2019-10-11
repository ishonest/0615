source("./Functions/Archieve/F.Trading.Simulation.R")

# -------------------------------------------------------------------------
# Logic
# -------------------------------------------------------------------------
# Binning in Ranges: 20 Bins
# Models & Buckets with 90% success rate
# Buy @ Nth percentile of low; Sell at Nth percentile high; Stop Loss at 5%
# Optimise Percentile Pick provided ROI >= 20%, Stop Loss Range: [0%, 5%]
# Picking the most profitable price to pick more stocks
# Qualifying Criterion: At least 1 trade in development
# -------------------------------------------------------------------------

Dev.Simulation <- function(ticker, validation.period = 120)
{
  # ticker = "MDB" # "PVTL"
  d1 <- all.d1 %>% filter(ticker == !!ticker) %>%
    select(ds, volume, open, low, high, close) %>% arrange(ds) %>%
    mutate(ROI.l = round(-zoo::rollmax(-low, 5, fill = NA, align = "left")/lag(close), 4)
           , ROI.h = round(zoo::rollmax(high, 5, fill = NA, align = "left")/lag(close), 4)
           , ROI.c = round(lead(close, 5-1)/lag(close), 4)
           , Rise.5D = case_when(ROI.c >= 1 ~ 1, TRUE ~ 0))

  all <- readRDS(paste0(Parms[["scores.folder"]], ticker, ".rds")) %>%
          left_join(d1, by = "ds") %>% mutate(Score = round(Score, 4))

  if(is.null(all) || nrow(all) == 0)
  {
    rm(ticker, d1, all)
    return(NULL)
  }

  # Binning in Ranges: 10 Bins
  # 90% accuracy in Dev and Val

  all <- all %>% group_by(ID) %>%
          filter(ds <= Parms[["last.dev.date"]]) %>%
          mutate(R = ntile(Score, 10)) %>%
          group_by(ID, R) %>%
          summarise(R.low = min(Score)) %>%
          mutate(R.low = ifelse(R == min(R), 0, R.low),
                 R.high = ifelse(R == max(R), 1, lead(R.low) - 0.0001)) %>% # works for round 4
          full_join(all, by = "ID") %>%
          filter(Score >= R.low, Score <= R.high) %>%
          ungroup() %>% arrange(ticker, ID, ds)

  val.models <- all %>%
                group_by(ID) %>% arrange(ID, ds) %>%
                mutate(ds.range = ifelse(ds <= Parms[["last.dev.date"]], "Dev",
                                  ifelse(ds <= Parms[["last.dev.date"]] + validation.period, "Val", 
                                         "XXX"))
                       , Rise = case_when(lead(close, 4) > lag(close) ~ 1) ) %>%
                group_by(ticker, ID, R, R.low, R.high, ds.range) %>%
                summarise(Rise = sum(Rise, na.rm = TRUE)/n(), N = n() ) %>%
                data.table::setDT() %>%
                data.table::dcast(ticker + ID + R + R.low + R.high ~ ds.range,
                                  value.var = c("Rise", "N"), sep = "." ) %>%
                group_by(ID) %>%
                arrange(ticker, ID, -R) %>%
                mutate(Type = case_when(Rise.Dev >= 0.8 ~ "LONG",
                                        Rise.Dev <= 0.2 ~ "SHRT")) %>%
                filter(!is.na(Type), N.Dev > 2, N.Val > 2
                       , between(Rise.Dev - Rise.Val, -0.2, 0.5)
                       ) %>%
                ungroup() %>% arrange(ticker, ID, R)

  if(is.null(val.models) || nrow(val.models) == 0)
  {
    rm(ticker, d1, all, val.models)
    return(NULL)
  }

  picks <- foreach(ptile = seq(0, 0.5, 0.05), .combine = bind_rows, .errorhandling = 'remove') %do%
    {
      picks <- all %>% filter(ds <= Parms[["last.dev.date"]]) %>%
              inner_join(val.models %>% select(ticker, ID, R, Type)
                         , by = c("ID", "R", "ticker")) %>%
              group_by(ticker, ID, R, Type, R.low, R.high) %>%
              summarise(ptile
                        , R.buy = mean(ifelse(Type == "LONG",
                                              quantile(ROI.l, probs = ptile, na.rm = TRUE, names = FALSE),
                                              quantile(ROI.h, probs = 1 - ptile, na.rm = TRUE, names = FALSE)))
                        , R.sell = mean(ifelse(Type == "LONG",
                                               quantile(ROI.h, probs = ptile, na.rm = TRUE, names = FALSE),
                                               quantile(ROI.l, probs = 1 - ptile, na.rm = TRUE, names = FALSE)))
                        , R.stop.av = mean(ifelse(Type == "LONG", ROI.l, ROI.h), na.rm = TRUE)
                        , R.stop.sd = sd(ifelse(Type == "LONG", ROI.l, ROI.h))
              ) %>%
              mutate(R.stop = ifelse(Type == "LONG",
                                     1 - (R.stop.av - 2*R.stop.sd)/R.buy,
                                     -1 + (R.stop.av + 2*R.stop.sd)/R.buy)
                     # , R.stop = ifelse(R.stop < 0.03, 0.03, R.stop)
                     , R.ROI = ifelse(Type == "LONG", R.sell/R.buy, 2 - R.sell/R.buy)
                     , R.ROR = (R.ROI - 1)/R.stop
              ) %>%
              select(ticker, ID, R, Type, ptile, R.low, R.high, R.buy, R.sell, R.stop, R.ROI, R.ROR) %>%
              ungroup()

      rm(ptile)
      return(picks)
    }

  picked <- picks %>%
            filter(R.ROI >= Parms[["target.ROI"]]) %>% # New
            filter(R.stop >= 0.03) %>% # Changed from 0.03 to 0.02
            group_by(ticker, ID, R) %>% arrange(ticker, ID, R, ptile) %>%
            filter(R.ROR == max(R.ROR)) %>%
            filter(R.ROI == max(R.ROI)) %>%
            # filter(R.ROR >= 2) %>%
            ungroup() %>% select(-c(ptile)) %>% distinct()

  if(is.null(picked) || nrow(picked) == 0)
  {
    rm(ticker, d1, all, val.models, picks, picked)
    return(NULL)
  }

  sim <- semi_join(all, picked, by = "ID") %>%
          left_join(picked, by = c("ticker", "ID", "R", "R.low", "R.high")) %>%
          select(-c(Rise.5D, ROI.c, R.ROR)) %>%
          AF.simulate.20190918(., Process = "Development")

  sum <- AF.Simulation.Summary(sim) %>%
          left_join(picked, by = c("ticker", "ID", "R", "Type"))

  # summary(sum$ROI.XXX[sum$ROI.Dev >= 1.1 & sum$ROI.Val >= 1.1])
  
  saveRDS(sim, paste0("./Data/Simulation/", ticker, ".rds"))
  rm(ticker, d1, all, val.models, picks, picked, sim, validation.period)
  return(sum)
}
