# Swapped the formula of close and adjusted 7/May
# Use adjusted (i.e. adjusted) for ROI.c

rm(list = ls())
options(scipen = 3)
set.seed(1024)
data.folder <- "F:/Project S/MA Linear Modelling/"

library(dplyr)
library(doSNOW)
library(foreach)
library(plotly)
library(htmlwidgets)
library(BatchGetSymbols)

closeAllConnections()
cl <- makeCluster(3, outfile="dopar_log.txt")
registerDoSNOW(cl)

# -------------------------------------------------------------------------
# Batch Clean Data: Updated on 15 June, 2019
# -------------------------------------------------------------------------

Get.Data.Clean <- function(d1, min.trade = 250000, min.tdays = 250, bad.jumps = 0.1)
{

  source("./F.Data.Clean.R")
  
  d1 <- d1 %>% distinct()
  if(nrow(d1) < min.tdays) {return(data.frame())}

  # ~100 Days of Prod + 150 Days of Dev: 250 Days of Good Data
  # Definition of Good Data: No Extreme Jumps, Non-NA/Zero Volume, $250k+ Daily Trading
  # Consistent for 250 Days

  # Volume Check
  d2x <- d1 %>% group_by(ticker) %>%
          mutate(volume = na_if(volume, 0)) %>%
          summarise(N = n(),
                    last.na = suppressWarnings(max(which(is.na(volume)))),
                    last.na = ifelse(is.infinite(last.na), 0, last.na),
                    good.days = N - last.na
          ) %>%
          filter(good.days >= min.tdays) %>%
          select(-c(N, good.days)) %>%
          inner_join(d1, by = "ticker") %>%
          group_by(ticker) %>%
          arrange(ticker, ref.date) %>%
          filter(row_number() > last.na) %>%
          rename(ds = ref.date, adjusted = price.adjusted,
                 open = price.open, high = price.high,  low = price.low, close = price.close) %>%
          select(c(ds, ticker, volume, open, high, low, close, adjusted)) %>%
          mutate_all(zoo::na.locf) # Replace NA with preceeding values

  if(nrow(d2x) < min.tdays) {return(data.frame())}

  # Remove data until with low value trades & sudden jumps
  d2y <- d2x %>%
          group_by(ticker) %>%
          arrange(ticker, ds) %>%
          mutate(value = adjusted*volume,
                 delta = lead(adjusted)/adjusted,
                 bad = case_when(delta < bad.jumps ~ TRUE,
                                 delta > (1/bad.jumps) ~ TRUE,
                                 value < min.trade ~ TRUE,
                                 TRUE ~ FALSE)) %>%
          summarise(N = n(),
                    last.bad = suppressWarnings(max(which(bad == TRUE))),
                    last.bad = ifelse(is.infinite(last.bad), 0, last.bad),
                    good.days = N - last.bad ) %>%
          filter(good.days >= min.tdays) %>%
          select(-c(N, good.days))

  d1 <- inner_join(d2x, d2y, by = "ticker") %>%
        group_by(ticker) %>%
        arrange(ticker, ds) %>%
        filter(row_number() > last.bad) %>%
        select(-last.bad) %>%
        ungroup()

  # -------------------------------------------------------------------------
  d1 <- bind_rows(d1,  data.frame(ds = NextTradingDate(Date = max(d1$ds)),
                                  ticker = unique(d1$ticker), stringsAsFactors = FALSE)) %>%
        group_by(ticker) %>% arrange(ticker, ds) %>%
        mutate(ds.N = row_number())

  rm(d2x, d2y)
  gc()

  return(d1)

}

# -------------------------------------------------------------------------
# Modeling and Model Selection
# -------------------------------------------------------------------------
Get.Models <- function(d1, ticker,
                       T.models = NULL, # Pass NULL for initialization,
                       Type = "Development",
                       target.ROI = 1.10,
                       in.days = 5,
                       last.dev.date = as.Date("2019-01-31"),
                       data.folder = "F:/Project S/MA Linear Modelling/")
{
  source("./F.Data.Transformation.R")
  
  # -------------------------------------------------------------------------
  # Inputs: For Testing
  # -------------------------------------------------------------------------
  library(foreach)
  library(TTR)
  library(dplyr)
  ticker <- "AMD"
  # all.d1 <- readRDS(paste0(data.folder, "IB/Clean.Prices.rds"))
  d1 <- all.d1 %>% filter(ticker == !!ticker)
  Type = "Development"
  data.folder = "F:/Project S/MA Linear Modelling/"
  T.models <- NULL
  # T.models <- readRDS(paste0(data.folder, "Summary/20190615.All.Models.rds")) %>% filter(ticker == !!ticker)
  target.ROI = 1.10
  in.days = 5
  last.dev.date = as.Date("2019-01-31")
  
  # -------------------------------------------------------------------------
  # Raw Data Extraction
  # -------------------------------------------------------------------------
  
  # When high is compared with close, use close; else use adjusted
  d1x <- d1 %>%
          mutate(last.close = lag(close)
                 , P1W = lag(adjusted)/lag(adjusted, 5 + 1)
                 , P1Y = lag(adjusted)/lag(adjusted, 252 + 1)
                 , ROI.h = zoo::rollmax(high, in.days, fill = NA, align = "left")/last.close
                 , ROI.l = -zoo::rollmax(-low, in.days, fill = NA, align = "left")/last.close
                 , ROI.c = lead(adjusted, in.days-1)/lag(adjusted)
                 , Bare = as.factor(if_else(lead(adjusted, in.days-1)/lag(adjusted) > 1, 1, 0))
          ) %>%
          select(-c(volume, open, low, high, adjusted))
  
  # d2 <- readRDS(paste0(data.folder, "MA/", ticker, ".rds"))
  d2 <- Get.MA.Compressed(d1, periods = seq(10, 250, 10))
  
  if(is.null(T.models))
  {
    T.models <- expand.grid(Period = unique(d2$Period),
                            MA.Type = c("SMA", "EMA", "EMAW", "ZLEMA", "HMA"),
                            DP.Method = c("LN", "MALN"),
                            stringsAsFactors = FALSE)
  }
  
  # -------------------------------------------------------------------------
  # Trend Models
  # -------------------------------------------------------------------------
  # profvis::profvis({
    
  T.forecasts <- data.frame()
  T.Dev.Models <- data.frame()
  
  for(i in 1:nrow(T.models))
  {
    # i = 8
    MA.Type <- T.models$MA.Type[i]
    n <- T.models$Period[i]
    DP.Method <- T.models$DP.Method[i]
    d2x <- get(paste0("AF.", DP.Method))(d2, d1x, MA.Type, n)
    
    # -------------------------------------------------------------
    tdata <- d2x %>% filter(ds <= last.dev.date) %>%
              select(-c("ds", "ds.N", "ticker", "P1W", "P1Y", 
                        "last.close", "close", "ROI.h", "ROI.l", "ROI.c"))
    
    m2 <- tryCatch(glm(Bare ~ ., family = "binomial", data = tdata) %>%
                     MASS::stepAIC(direction = "both", trace = FALSE),
                   error = function(w) {return(NULL)},
                   warning = function(w) {return(NULL)})
    
    # -------------------------------------------------------------
    # Checking Validity of Models
    # -------------------------------------------------------------
    if(is.null(m2) || grepl("LN.", paste(names(m2$coefficients), collapse = ", ")) == FALSE)
    {
      rm(i, MA.Type, n, DP.Method, d2x, tdata, m2)
      next()
    }
    
    # -------------------------------------------------------------
    # Check if model scores are performing in Validation Time Frame
    # -------------------------------------------------------------
    all <- d2x %>%
            arrange(ds.N) %>%
            mutate(Score = predict(m2, newdata = ., type = "response")
                   , Ptile = sapply(seq_along(ds.N),
                                    function(i) {sum(Score[1:i] <= Score[i])/i})
                   ) %>%
            select(ds, ticker, last.close, P1W, P1Y, ROI.h, ROI.l, ROI.c, Score, Ptile) %>%
            ungroup()
    
    # -------------------------------------------------------------
    # Simulating Scenarios: Buy@Lower Levels / Sell@5Day Closing
    # -------------------------------------------------------------
    s <- tail(all, -100) %>% 
          filter(!is.na(ROI.h), Ptile >= 0.9) %>% 
          mutate(Type = ifelse(ds <= last.dev.date, "Dev", "Prod")
                 , Drop = P1W*ROI.l, Drop = lag(cummean(Drop))
                 , Buy = Drop/P1W, Buy = case_when(Buy > 1 ~ 1, Buy > ROI.l ~ Buy)
                 # , Rise = P1Y*ROI.h, Rise = lag(cummean(Rise))
                 # , Sell = Rise/P1Y
                 # , Sell = ifelse(Sell < 1.1*Buy, NA, ifelse(Sell <= ROI.h, Sell, ROI.c))
                 # , ROI = Sell/Buy
                 , ROI = ROI.c/Buy
                 ) %>%
          filter(!is.na(ROI)) %>%
          group_by(ticker, Type) %>%
          summarise(N = n(), 
                    SR.02 = sum(ROI >= 1.02)/N,
                    SR.10 = sum(ROI >= 1.10)/N,
                    ROI = prod(ROI)^(1/N) ) %>%
          data.frame() %>%
          reshape(idvar = "ticker", timevar = "Type", direction = "wide") %>%
          mutate(DP.Method, MA.Type, Period = n, ID = i)
            
    if(is.null(s$N.Dev) || s$N.Dev <= 2 || s$ROI.Dev < target.ROI )
    {
      rm(i, MA.Type, n, DP.Method, d2x, tdata, m2, all, s)
      next()
    }
    
    # -------------------------------------------------------------
    # Return / Save
    # -------------------------------------------------------------
    T.Dev.Models <- bind_rows(T.Dev.Models, s)
    T.forecasts <- tail(all, -100) %>% 
                    select(ds, ticker, Score, Ptile) %>%
                    mutate(DP.Method, MA.Type, Period = n, ID = i) %>%
                    bind_rows(T.forecasts)

    rm(i, MA.Type, n, DP.Method, d2x, tdata, m2, all, s)

  }
  
  # })  # For Profvis
  # -------------------------------------------------------------------------
  # Summarization, Saving and Cleaning
  # -------------------------------------------------------------------------
  if(nrow(T.Dev.Models) > 0) 
  {
    
    targets <- T.forecasts %>% 
                group_by(ticker, DP.Method, MA.Type, Period) %>%
                filter(ds == max(ds), Ptile >= 0.9) %>%
                select(ticker, DP.Method, MA.Type, Period) %>%
                inner_join(T.Dev.Models, by = c("ticker", "DP.Method", "MA.Type", "Period"))
    
    T.forecasts <- left_join(T.forecasts, d1, by = c("ticker", "ds")) %>%
                    select(ds, ticker, volume, open, high, low, close, adjusted,
                           Score, Ptile, DP.Method, MA.Type, Period, ID, ds.N)

    saveRDS(T.forecasts, paste0(data.folder, "Prod.Forecasts/", ticker, ".rds"))
    saveRDS(T.Dev.Models, paste0(data.folder, "Model.Performance/", ticker, ".rds"))
    
  }
  
  rm(d1, d1x, d2, T.models, T.forecasts, T.Dev.Models, target.ROI, in.days, last.dev.date, ticker)
  gc()
  
  if(Type == "Production") {return(targets)}

}

# -------------------------------------------------------------------------
# Dashboard
# -------------------------------------------------------------------------
Get.All.Charts <- function(targets, prod.forecasts, row.number = NULL,
                           data.folder = "F:/Project S/MA Linear Modelling/")
{
  # targets = T.performance
  # prod.forecasts = T.forecasts
  # row.number = 1
  
  if(!is.null(row.number)) {d <- targets[row.number,]} else {d <- targets}
  
  for(i in 1:nrow(d))
  {
    # i = 1
    df <- prod.forecasts %>% filter(ticker == d$ticker[i], ID == d$ID[i])
    
    t <- paste0(d$ticker[i], "  >>  ",
                d$DP.Method[i], " ", d$MA.Type[i], " ", d$Period[i], "  >>  ",
                "Occurence: ", d$Past.N[i], "  >>  ",
                "Target Price: $", d$Target.Price[i], "  >>  ",
                "Stop Loss (D|P): ", 100*d$Stop.Loss.Dev[i], "% | ", 
                if_else(is.na(d$Stop.Loss.Prod[i]), "-" , paste0(100*d$Stop.Loss.Prod[i], "%")))
    
    # subt <- paste0("Target In: ", d$Past.Target.In[i], "  >>  ",
    #                "ROI: ", d$Past.ROI[i])
    
    if(max(df$last.close, na.rm = TRUE) > 20)
    {
      min.y <- 10*floor(min(df$last.close/10, na.rm = TRUE))
      max.y <- 10*ceiling(max(df$last.close/10, na.rm = TRUE))
    } else
    {
      min.y <- floor(min(df$last.close, na.rm = TRUE))
      max.y <- ceiling(max(df$last.close, na.rm = TRUE))
    }
    
    p <- plot_ly(data = df, x = ~ds) %>%
      add_trace(y = ~last.close, name = "Last Close",
                type = 'scatter', mode = 'lines',
                line = list(color = '#a6a6a6', width = 0.5),
                fill = 'tozeroy', fillcolor='#f9f7f7'
      ) %>%
      add_trace(y = ~ifelse(Ptile == 1, last.close, NA), name = 'Peaked',  
                type = 'scatter', mode = 'markers',
                marker = list(size = 5, color = '#fcfc00',
                              line = list(color = '#fc5300', width = 1.5)),
                hoverlabel = list(bgcolor = '#fc5300')
      ) %>%
      add_trace(y = ~Ptile, name = 'Percentile', yaxis = "y2",
                type = 'scatter', mode = 'lines',
                line = list(color = '#027007', width = 1.5)
      ) %>%
      add_trace(y = ~Score, name = 'Score', yaxis = "y2",
                type = 'scatter', mode = 'lines',
                line = list(color = '#004080', dash='dot', width = 1)
      ) %>%
      add_annotations(text = t,
                      xref = 'paper', yref = 'paper', x = 0, y = 1.10, 
                      showarrow = F, font = list(size = 13, color = '#004080')
      ) %>%
      add_annotations(text = paste0("ROI: ", d$Past.ROI[i]),
                      xref = 'paper', yref = 'paper', x = 0, y = 1.05, 
                      font = list(size = 12), showarrow = F 
      ) %>%
      add_annotations(text = paste0("Target In: ", d$Past.Target.In[i]),
                      xref = 'paper', yref = 'paper', x = 0, y = 1.00, 
                      font = list(size = 12), showarrow = F 
      ) %>%
      layout( font = list(size = 12), showlegend = FALSE, hovermode = 'compare',
              margin = list(l = 70, r = 70, b = 50, t = 50, pad = 10),
              xaxis = list(title = NA, showgrid = FALSE),
              yaxis = list(title = "Last Close ($)", color = '#a6a6a6', 
                           range = c(min.y, max.y), showgrid = FALSE),
              yaxis2 = list(title = "QED Score", color = '#027007', tickformat = ".1%",
                            overlaying = "y", side = "right", showgrid = TRUE)
      )
    
    fname <- paste0(data.folder, "Prod.Plots/", d$ticker[i], ".", d$ID[i], ".html")
    
    saveWidget(as_widget(p), title = t,
               file = fname, libdir = paste0(data.folder, "Prod.Plots/libdir/"))
    
    browseURL(fname)
    rm(i, df, t, min.y, max.y, p, fname)
  }
  
  rm(d)
}



