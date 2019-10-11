# -------------------------------------------------------------------------
# Type 9 Validation: 
# Best, Random, Worst case projections across max Trades
# Simplifying Type 8
# -------------------------------------------------------------------------
Val.Type.09 <- function(hist.perf, last.dev.date)
{
  AF.Scenario.Planning <- function(df, Notes = "")
  {
    op <- data.frame()
    
    for(Scenario in c("Worst ROR", "Best.ROR", "Random ROR"))
    {
      if(Scenario == "Worst ROR") {dfx <- df %>% arrange(bought.on, ROR)}
      if(Scenario == "Best.ROR") {dfx <- df %>% arrange(bought.on, desc(ROR))}
      if(Scenario == "Random ROR") {dfx <- df %>% arrange(bought.on)}
      
      for(maxtrades in seq(5, 50, 1))
      {
        bank <- seq(from = min(dfx$bought.on), to = max(dfx$sold.on), by = 1)
        bank <- as.Date(setdiff(bank, as.Date(timeDate::holidayNYSE())), origin = "1970-01-01")
        bank <- bank[!(weekdays(bank) %in% c("Sunday", "Saturday"))]
        bank <- c(bank, max(bank) + 1)
        bank <- data.frame(ds = bank, capital = 10000, trades = 0, missed = 0,
                           capacity = 0, duration = 0, SR.02 = 0, 
                           stringsAsFactors = FALSE) %>%
                arrange(ds)
        
        n <- nrow(bank)
        
        for(i in 1:nrow(dfx))
        {
          r.buy <- which(bank$ds == dfx$bought.on[i])
          r.sell <- which(bank$ds == dfx$sold.on[i]) + 1L
          
          if(bank$capital[r.buy] > 0)
          {
            invest <- min(bank$capital[r.buy], 10000/maxtrades)
            bank$capital[r.buy:n] <- bank$capital[r.buy:n] - invest
            bank$capital[r.sell:n] <- bank$capital[r.sell:n] + invest*dfx$ROI[i]
            
            bank$trades[r.buy] <- bank$trades[r.buy] + 1
            bank$capacity[r.buy] <- bank$capacity[r.buy] + dfx$capacity[i]
            bank$duration[r.buy] <- bank$duration[r.buy] + dfx$invest.period[i]
            
            bank$SR.02[r.buy] <- bank$SR.02[r.buy] + ifelse(dfx$ROI[i] >= 1.02, 1, 0)
            
            rm(invest)
          } else
          {
            bank$missed[r.buy] <- bank$missed[r.buy] + 1
          }
          
          rm(r.buy, r.sell, i)
        }
        
        s <- bank %>% summarise(Scenario
                                , maxtrades
                                , Trades = sum(trades)
                                , Missed = sum(missed)
                                , Miss.Rate = Missed/(Trades + Missed)
                                , capacity.MM = mean(capacity[capacity > 0])/1000000
                                , Holding.Period = sum(duration)/Trades
                                , SR.02 = sum(SR.02)/Trades
                                , ROI = last(capital)/10000
                                , ROR = ROI^(1/(n()-1))
                                , Annual.Returns = ROR^250)
        
        op <- bind_rows(op, s)
        
        rm(bank, n, maxtrades, s)
      }
      
      rm(dfx, Scenario)
    }
    
    op <- op %>% arrange(maxtrades, Scenario) %>% mutate(Selection.Method = Notes)
    return(op)
  }
  
  x1 <- hist.perf %>%
    filter(bought.on > last.dev.date) %>%
    group_by(ticker, bought.on) %>% arrange(ticker, bought.on) %>%
    top_n(3, wt = -ID) %>%                # Buy Upto 3 models a day
    ungroup() %>%
    select(-c(DP.Method, MA.Type, Period)) %>%
    arrange(ticker, ID, bought.on)
  
  x2 <- AF.Scenario.Planning(df = x1, Notes = "No Condition")
  
  x3 <- x2 %>%
    mutate( Scenario = gsub(" ", ".", Scenario)) %>%
    select(-Selection.Method) %>%
    reshape(idvar = c("maxtrades"), timevar = "Scenario", direction = "wide") %>%
    select("maxtrades", "Annual.Returns.Best.ROR", "Annual.Returns.Random.ROR"
           , "Annual.Returns.Worst.ROR"
           , "Miss.Rate.Best.ROR", "Miss.Rate.Random.ROR", "Miss.Rate.Worst.ROR"
           , "SR.02.Best.ROR", "SR.02.Random.ROR", "SR.02.Worst.ROR"
           , "Trades.Best.ROR", "Trades.Random.ROR", "Trades.Worst.ROR"
           , "capacity.MM.Best.ROR", "capacity.MM.Random.ROR", "capacity.MM.Worst.ROR"
           , "Holding.Period.Best.ROR", "Holding.Period.Random.ROR"
           , "Holding.Period.Worst.ROR" )
  
  # Annual Returns
  p <- plot_ly(data = x3, x = ~maxtrades, type = 'scatter', mode = 'lines'
               , y = ~Annual.Returns.Best.ROR, name = "Best") %>%
    add_trace(y = ~Annual.Returns.Random.ROR, name = "Random") %>%
    add_trace(y = ~Annual.Returns.Worst.ROR, name = "Worst") %>%
    layout(xaxis = list(title = "# Trades"),
           yaxis = list(title = "Annual Returns", tickformat = '.1f'),
           title = "Scenario Planning: Annual Returns",
           showlegend = FALSE, hovermode = 'compare')
  print(p)
  
  # Miss Rate
  p <- plot_ly(data = x3, x = ~maxtrades, type = 'scatter', mode = 'lines'
               , y = ~Miss.Rate.Best.ROR, name = "Best") %>%
    add_trace(y = ~Miss.Rate.Random.ROR, name = "Random") %>%
    add_trace(y = ~Miss.Rate.Worst.ROR, name = "Worst") %>%
    layout(xaxis = list(title = "# Trades"),
           yaxis = list(title = "Miss Rate", tickformat = '%'),
           title = "Scenario Planning: Miss Rate",
           showlegend = FALSE, hovermode = 'compare')
  print(p)
  
  # Strike Rate
  p <- plot_ly(data = x3, x = ~maxtrades, type = 'scatter', mode = 'lines'
               , y = ~SR.02.Best.ROR, name = "Best") %>%
    add_trace(y = ~SR.02.Random.ROR, name = "Random") %>%
    add_trace(y = ~SR.02.Worst.ROR, name = "Worst") %>%
    layout(xaxis = list(title = "# Trades"),
           yaxis = list(title = "Strike Rate @ 2%", tickformat = '.1%'),
           title = "Scenario Planning: Strike Rate",
           showlegend = FALSE, hovermode = 'compare')
  print(p)
  
  # Capacity
  p <- plot_ly(data = x3, x = ~maxtrades, type = 'scatter', mode = 'lines'
               , y = ~capacity.MM.Best.ROR, name = "Best") %>%
    add_trace(y = ~capacity.MM.Random.ROR, name = "Random") %>%
    add_trace(y = ~capacity.MM.Worst.ROR, name = "Worst") %>%
    layout(xaxis = list(title = "# Trades"),
           yaxis = list(title = "Capacity ($MM)", tickformat = '$.0f'),
           title = "Scenario Planning: Capacity @ 100%",
           showlegend = FALSE, hovermode = 'compare')
  print(p)
  
  # Holding Period
  p <- plot_ly(data = x3, x = ~maxtrades, type = 'scatter', mode = 'lines'
               , y = ~Holding.Period.Best.ROR, name = "Best") %>%
    add_trace(y = ~Holding.Period.Random.ROR, name = "Random") %>%
    add_trace(y = ~Holding.Period.Worst.ROR, name = "Worst") %>%
    layout(xaxis = list(title = "# Trades"),
           yaxis = list(title = "Holding Period (Days)", tickformat = '.2f'),
           title = "Scenario Planning: Holding Period",
           showlegend = FALSE, hovermode = 'compare')
  print(p)
  
  
  rm(x1, x2, p, AF.Scenario.Planning)
  
  names(x3) <- gsub("[[:punct:]]", " ", names(x3))
  return(x3)
}

# -------------------------------------------------------------------------
T9.Compare.Index <- function(hist.perf, all.d1, last.dev.date)
{
  
  dfx <- hist.perf %>% filter(bought.on > last.dev.date) %>% arrange(bought.on)
  if(!("Type" %in% names(dfx))) {dfx$Type <- "LONG"}
  
  bank <- foreach(i = 1:nrow(dfx), .combine = bind_rows
                  , .packages = "dplyr"
                  , .multicombine = TRUE, .errorhandling = 'remove') %dopar%
    {
      bank <- all.d1 %>%
        filter(ticker == dfx$ticker[i], ds >= dfx$bought.on[i], ds <= dfx$sold.on[i]) %>%
        select(ds, ticker, close) %>%
        mutate(ROI = case_when(row_number() == 1 ~ 1,
                               dfx$Type[i] == "LONG" ~ close/lag(close),
                               dfx$Type[i] == "SHRT" ~ 2 - close/lag(close) ))
      rm(i)
      return(bank)
    }
  
  x <- bank %>% group_by(ds) %>%
      summarise(N = n(), Q.Daily = mean(ROI)) %>%
      mutate(Q.Index = cumprod(Q.Daily))
  
  y <- BatchGetSymbols::get.clean.data(ticker = "^IXIC", src = "yahoo",
                                       first.date = min(dfx$bought.on),
                                       last.date = max(dfx$sold.on)) %>%
        rename(ds = ref.date, N.Index = price.close) %>% arrange(ds) %>%
        mutate(N.Daily = N.Index/lag(N.Index),
               N.Daily = ifelse(is.na(N.Daily), 1, N.Daily)) %>%
        select(ds, N.Daily, N.Index)
  
  z <- left_join(x, y, by = "ds")
  
  p1 <- plot_ly(data = z, x = ~ds, type = 'scatter', mode = 'lines',
                y = ~Q.Index, name = 'QED') %>%
        add_trace(y = ~N.Index, name = 'Nasdaq', yaxis = "y2") %>%
        layout(title = "Comparision of Indices",
               xaxis = list(title = ""),
               yaxis2 = list(overlaying = "y", side = "right"),
               legend = list(orientation = 'h'), hovermode = 'compare',
               margin = list(l = 60, r = 60, b = 25, t = 25))
  
  print(p1)
  
  p2 <- plot_ly(data = z, x = ~ds, type = 'scatter', mode = 'lines',
                y = ~Q.Daily, name = 'QED') %>%
    add_trace(y = ~N.Daily, name = 'Nasdaq') %>%
    layout(title = "Comparision of Daily Performances",
           xaxis = list(title = ""),
           legend = list(orientation = 'h'), hovermode = 'compare',
           margin = list(l = 60, r = 60, b = 25, t = 25))
  
  print(p2)
  
  rm(x, y, z, p1, p2, dfx, bank)
  gc()
  
}
