# -------------------------------------------------------------------------
# Moving Average Calculations - Periods in gaps of 10
# -------------------------------------------------------------------------
Get.MA.Compressed <- function(d1, periods = seq(10, 180, 10))
{
  library(TTR)
  library(foreach)
  library(dplyr)
  
  d1[is.na(d1)] <- 0

  suppressWarnings(
  z <- foreach(n = periods, .combine = bind_rows, .errorhandling = 'remove') %:%
    foreach(name = c("open", "low", "high", "close")
            , .combine = bind_rows, .errorhandling = 'remove') %do%
    {
      z <- d1 %>%
            mutate(Period = n,
                   MA.Var = name,
                   SMA := lag(SMA(get(name), n))/lag(close),
                   EMA := lag(EMA(get(name), n))/lag(close),
                   EMAW := lag(EMA(get(name), n, wilder = TRUE))/lag(close),
                   ZLEMA := lag(ZLEMA(get(name), n))/lag(close),
                   HMA := lag(HMA(get(name), n))/lag(close)
            ) %>%
            select(ds, ticker, Period, MA.Var, SMA, EMA, EMAW, ZLEMA, HMA) %>%
            na.omit()

      rm(n, name)
      return(z)
    }
  )
  
  return(z)
}

# -------------------------------------------------------------------------
# Associated Functions
# -------------------------------------------------------------------------
AF.LN <- function(d2, d1x, MA.Type, n)
{
  d2x <- d2 %>% filter(Period == !!n) %>%
           select(ds, ticker, MA.Var, (!!MA.Type)) %>%
           rename(MA = MA.Type) 
  
  if(min(d2x$MA) <= 0 ){ return(data.frame())} 
  
  d2x <- d2x %>%
           mutate(LN = log(MA)) %>%
           select(-MA) %>% data.frame() %>%
           reshape(idvar = c("ds", "ticker"), timevar = "MA.Var", direction = "wide") %>%
           left_join(d1x, by = c("ticker", "ds"))
  
  return(d2x)
  
}

AF.MALN <- function(d2, d1x, MA.Type, n)
{
  d2x <- d2 %>% 
          filter(Period == !!n) %>%
          select(ds, ticker, MA.Var, (!!MA.Type)) %>%
          rename(MA = MA.Type) 
  
  if(min(d2x$MA) <= 0 ){ return(data.frame())} 
  
  d2x <- d2x %>%
          mutate(LN = log(MA)) %>%
          data.frame() %>%
          reshape(idvar = c("ds", "ticker"), timevar = "MA.Var", direction = "wide") %>%
          left_join(d1x, by = c("ticker", "ds"))
  
  return(d2x)
  
}
# -------------------------------------------------------------------------
