# Settings: https://blog.quantinsti.com/ibpy-tutorial-implement-python-interactive-brokers-api/
# -------------------------------------------------------------------------
# Initialization
# -------------------------------------------------------------------------
options(scipen = 3, digits = 8, digits.secs = 0)
set.seed(1024)
library(IBrokers)
library(dplyr)
library(foreach)
library(BatchGetSymbols)
library(plotly)
library(htmlwidgets)

# -------------------------------------------------------------------------
# Start-Up Functions 
# -------------------------------------------------------------------------
IB.Connect <- function()
{
  library(IBrokers)
  # This loop makes sure that the client id is what supplied
  while( !exists("tws", envir = .GlobalEnv) || tws$clientId != IB.Parms[["clientId"]] )
  {
    if(exists("tws", envir = .GlobalEnv)) 
    {
      if(tws$clientId > 0) {close(tws)}
      rm(tws, envir = .GlobalEnv)
    }
    
    tws <- tryCatch({twsConnect(clientId = IB.Parms[["clientId"]])}
                    , warning = function(w) {list(clientId = -1)}
                    , error = function(e) {list(clientId = -2)}
    )
    assign("tws", tws, envir = .GlobalEnv)
    print(tws)
  }
  
  setServerLogLevel(tws)
}

IB.Account.Status <- function()
{
  cancelAccountUpdates(tws)
  d <- reqAccountUpdates(conn = tws, acctCode = IB.Parms[["acctCode"]], subscribe = TRUE)
  assign("IB.00.account",
         data.frame(t(sapply(d[[1]],c)), stringsAsFactors = FALSE),
         envir = .GlobalEnv)
  
  d2 <- d[[2]]
  IB.00.positions <- data.frame()
  
  if(length(d2) > 0)
  {
    for(i in 1:length(d2))
    {
      x <- bind_cols(data.frame(t(sapply(d2[[i]][1][[1]],c)), stringsAsFactors = FALSE),
                     data.frame(t(sapply(d2[[i]][2][[1]],c)), stringsAsFactors = FALSE) )
      
      IB.00.positions <- bind_rows(IB.00.positions, x)
      rm(x, i)
    }
    
    IB.00.positions <- IB.00.positions[c("accountName", "conId", "symbol", "position",
                                   "marketPrice", "marketValue", "averageCost",
                                   "unrealizedPNL", "realizedPNL",
                                   "local", "currency", "exch", "primary", "sectype",
                                   "expiry", "strike", "right",
                                   "multiplier", "combo_legs_desc", "comboleg",
                                   "include_expired", "secIdType", "secId")]
    
    cols.to.numeric <- c("strike", "right", "position", "marketPrice",
                         "marketValue", "averageCost", "unrealizedPNL")
    IB.00.positions[cols.to.numeric] <- as.numeric(as.matrix(IB.00.positions[cols.to.numeric]))
    assign("IB.00.positions", IB.00.positions, envir = .GlobalEnv)
    rm(cols.to.numeric)
  }
  
  Current.Position <- sum(IB.00.positions$marketValue, na.rm = TRUE)
  Investment <- sum(IB.00.positions$position*IB.00.positions$averageCost, na.rm = TRUE)
  Available.Funds <- IB.Parms[["invest.max"]] - Investment
  ROI.investment <- round(100*(Current.Position/Investment - 1), 2)
  ROI.portfolio <- IB.00.positions %>% 
                    summarise(Gain = sum(as.numeric(unrealizedPNL), as.numeric(realizedPNL))) %>%
                    mutate(Gain = round(100*Gain/IB.Parms[["invest.max"]], 2)) %>% as.numeric()

  assign("Available.Funds", Available.Funds, envir = .GlobalEnv)
  assign("IB.00.positions", IB.00.positions, envir = .GlobalEnv)
  
  cat(paste0("\n\n-------------------------------", 
             "\n: Invested   : $ ", formatC(Investment, format="f", big.mark=",", digits=0),
             "\n: Position   : $ ", formatC(Current.Position, format="f", big.mark=",", digits=0),
             "\n: ROI        :   ", paste0(ROI.investment, "%"),
             "\n-------------------------------",
             "\n: Fund ROI   :   ", paste0(ROI.portfolio, "%"),
             "\n: Available  : $ ", formatC(Available.Funds, format="f", big.mark=",", digits=0),
             "\n-------------------------------\n"  ))
  
  if(Available.Funds <= 1000) {cat(paste0("\nWarning!!! Available Funds running low."))}
  
  rm(d, d2, Current.Position, Investment, ROI.investment, ROI.portfolio)
  
}

IB.System.Status <- function()
{
  source("./Functions/F.Trading.Days.R")
  NY.Time <- round(as.numeric(strftime(format(Sys.time(), tz = "US/Eastern"), format = "%H.%M")), 2)
  Next.Day <- format(NextTradingDate(), '%A, %B %d, %Y')
  Trade.Days <- TradingDates()
  rm(NextTradingDate, PrevTradingDate, TradingDates, envir = .GlobalEnv)
  
  # -------------------------------------------------------------------------
  Time.Difference <- function(to, fr)
  {
    # to = 9.3
    # fr = NY.Time
    to <- strptime(sprintf("%.2f", to), format="%H.%M")
    fr <- strptime(sprintf("%.2f", fr), format="%H.%M") 
    
    d <- difftime(to, fr, units = "hours")
    d.hours <- floor(as.numeric(d))
    d.minutes <- round((as.numeric(d)%% 1 * 60))
    
    op <- if(d.hours == 0) 
    {
      if(d.minutes == 1) {paste("1 minute")} else {paste(d.minutes, "minutes")}
    } else if(d.hours == 1)
    {
      paste("1 hour and", 
            if(d.minutes == 1) {paste("1 minute")} else {paste(d.minutes, "minutes")}
      )
    } else
    {
      paste(d.hours, "hours and", 
            if(d.minutes == 1) {paste("1 minute")} else {paste(d.minutes, "minutes")}
      )
    }
    
    rm(to, fr, d, d.hours, d.minutes)
    return(op)
  }
  
  # -------------------------------------------------------------------------
  if(!(Sys.Date() %in% Trade.Days)) 
  {
    IB.Parms[["System.Live"]] <- FALSE
    cat(paste("\nNYSE & NASDAQ are closed today. Markets will reopen on", Next.Day, "... \n"))
    rm(NY.Time, Next.Day, Trade.Days)
  } else if(NY.Time < 9.3)
  {
    IB.Parms[["System.Live"]] <- FALSE
    cat("\nMarkets are closed now. Will reopen in", Time.Difference(to = 9.3, fr = NY.Time), "... \n")
    rm(NY.Time, Next.Day, Trade.Days, Time.Difference)
    
  } else if(NY.Time < IB.Parms[["Start.Trading.At"]])
  {
    IB.Parms[["System.Live"]] <- TRUE
    cat("\nMarket is Open. The System has been set to SELL ONLY for another", 
        Time.Difference(to = IB.Parms[["Start.Trading.At"]], fr = NY.Time), "... \n")
    rm(NY.Time, Next.Day, Trade.Days, Time.Difference)
    
  } else if(NY.Time >= IB.Parms[["Stop.Trading.At"]])
  {
    IB.Parms[["System.Live"]] <- FALSE
    cat(paste("\nMarkets are closed for the day. They will reopen on", Next.Day), "... \n")
    rm(NY.Time, Next.Day, Trade.Days, Time.Difference)
    
  } else if(IB.Parms[["Emergency"]] == TRUE)
  {
    IB.Parms[["System.Live"]] <- FALSE
    cat("\nSystem halted due to Emergency", "... \n")
    rm(NY.Time, Next.Day, Trade.Days, Time.Difference)
    
  } else 
  {
    IB.Parms[["System.Live"]] <- TRUE
    rm(NY.Time, Next.Day, Trade.Days, Time.Difference)
    
  }
  
  assign("IB.Parms", IB.Parms, envir = .GlobalEnv)
  
  
}

IB.Missed.Order.Mgmt <- function()
{
  source("./Functions/F.Trading.Days.R")
  missed <- readRDS(paste0(IB.Parms[["data.folder"]], "Production/Trading/06.Historical.Misses.rds")) %>%
    filter(ds == PrevTradingDate())
  
  rm(NextTradingDate, PrevTradingDate, TradingDates, envir = .GlobalEnv)
  
  missed.buy <- data.frame()
  for(i in 1:nrow(missed))
  {
    if(missed$action[i] == "BUY")
    {
      y <- readRDS(paste0(IB.Parms[["data.folder"]], "Production/Simulation/", missed$ticker[i], ".rds")) %>%
            semi_join(missed, by = c("ds", "ticker", "DP.Method", "MA.Type", "Period", "algoId")) %>%
            rename(missed = invest) %>%
            select(ticker, algoId, DP.Method, MA.Type, Period, missed) 
      
      missed.buy <- bind_rows(missed.buy, y)
      rm(y)
    }
    
    if(missed$action[i] == "SELL")
    {
      Contract <- twsEquity(symbol = missed$ticker[i], local = missed$ticker[i]
                            , primary = missed$Exchange[i], currency = "USD", exch = "SMART")
      
      Order <- twsOrder(orderId = reqIds(tws) 
                        , action = "SELL"
                        , clientId = tws$clientId
                        , account = IB.Parms[["acctCode"]]
                        , totalQuantity = missed$volume[i]
                        , orderType = "MKT"
                        , tif = "DAY")
      
      Update.Orderbook(Contract, Order, order.type = "Missed Order Placed")
      if(i %% 40 == 0) {Sys.sleep(1)} # Limitations of API: Can process only 50 orders/second
      IBrokers::placeOrder(twsconn = tws, Contract, Order)
      rm(Contract, Order)
    }
    
    rm(i)
  }
  
  if(nrow(missed.buy) > 0)
  {
    x <- IB.01.targets %>%
      left_join(missed.buy, by = c("algoId", "ticker", "DP.Method", "MA.Type", "Period")) %>%
      mutate(action = case_when(is.na(missed) ~ action),
             invest = case_when(is.na(missed) ~ invest,
                                TRUE ~ missed)
      ) %>%
      select(-missed)
    
    assign("IB.01.targets", x, envir = .GlobalEnv)
    rm(x)
  }
  
  if(sum(missed$action == "SELL", na.rm = TRUE) > 0)
  {
    cat("\nSystem is halted for 4 minutes to execute the missed orders ...\n")
    Sys.sleep(240)
  }
  
  rm(missed, missed.buy)
}

IB.StartDay <- function()
{
  assign("IB.01.targets"
         , readRDS(paste0(IB.Parms[["data.folder"]], "Production/Trading/01.Targets.rds"))
         , envir = .GlobalEnv)
  assign("IB.03.orders", data.frame(stringsAsFactors = FALSE), envir = .GlobalEnv)
  assign("IB.04.activity"
         , readRDS(paste0(IB.Parms[["data.folder"]], "Production/Trading/02.Historical.Activity.rds"))
         , envir = .GlobalEnv)
  IB.Parms[["Last.Order.Time"]] <- Sys.time()
  assign("IB.Parms", IB.Parms, envir = .GlobalEnv)
  
  if(!exists("tws", envir = .GlobalEnv) || !isConnected(tws)) 
  {IB.Connect()}                          # Create a connection
  
  IB.Account.Status()                     # Creates IB.00.account & IB.00.positions
  IB.System.Status()
  # if(isTRUE(IB.Parms[["System.Live"]])){Get.Actions()}  # Creates IB.02.actions
  
  IB.Missed.Order.Mgmt()
}

# -------------------------------------------------------------------------
# Cancelling Orders
# -------------------------------------------------------------------------
# Saves a copy of open orders in Global Environment and returns copy, if asked
Get.Open.Orders <- function(return.df = FALSE)
{
  Open.Orders <- function(tws)
  {
    .reqOpenOrders(tws)
    con <- tws[[1]]
    eW  <- eWrapper()
    socketSelect(list(con), FALSE, NULL)
    curMsg <- readBin(con, character(), 1L)
    processMsg(curMsg, con, eW)
  }
  
  open <- data.frame()
  i <- 0 # Counter to extract information between 2 OPEN_ORDER_END messages
  n <- 0 # Counter to control the max messages / secton
  while(i < 2)
  {
    x <- Open.Orders(tws)
    if(!is.null(x) && !is.list(x))
    {
      #  5: .twsIncomingMSG$OPEN_ORDER
      # 53: .twsIncomingMSG$OPEN_ORDER_END
      if(x[1] == 53) {i = i + 1} else 
        if(x[1] == 5) {open <- bind_rows(open, data.frame(t(x), stringsAsFactors = FALSE))}
    }
    
    rm(x)
    n <- n + 1
    if(n %% 40 == 0) {Sys.sleep(1)}
  }
  
  rm(i, n, Open.Orders)
  
  if(nrow(open) > 0)
  {
    open <- open %>% distinct() %>%
      rename(IB.Version = X2, orderId = X3, conId = X4, symbol = X5, sectype = X6, strike = X10,
             currency = X11, action = X13, totalQuantity = X14,
             orderType = X15, lmtPrice = X16, auxPrice = X17,
             tif = X18, outsideRTH = X19, account = X20, 
             orderRef = X23, parentId = X25
      ) %>%
      select(account, parentId, orderId, conId, symbol, sectype, strike, currency,
             action, totalQuantity, orderType, lmtPrice, auxPrice, tif, IB.Version) %>%
      mutate(orderId = as.integer(orderId)
             , parentId = as.integer(parentId)
             , totalQuantity = as.numeric(totalQuantity)
             , lmtPrice = as.numeric(lmtPrice)
             , auxPrice = as.numeric(auxPrice) )
  } else 
  {
    open <- data.frame(account = character()
                       , parentId = integer()
                       , orderId = integer()
                       , conId = character()
                       , symbol = character()
                       , sectype = character()
                       , strike = character()
                       , currency = character()
                       , action = character()
                       , totalQuantity = numeric()
                       , orderType = character()
                       , lmtPrice = numeric()
                       , auxPrice = numeric()
                       , tif = character()
                       , IB.Version = character()
                       , stringsAsFactors = FALSE)
    
    cat("\nThere are NO Open Orders ... ")
  }
  
  assign("IB.05.open.orders", open, envir = .GlobalEnv)
  
  if(return.df == TRUE) 
  {return(open)} else
  {rm(open)}
}

IB.Cancel.Orders <- function(orderType = c("LMT", "MKT"))
{
  IB.05.open.orders <- Get.Open.Orders(return.df = TRUE)
  
  if(exists("IB.05.open.orders", envir = .GlobalEnv) & nrow(IB.05.open.orders) > 0)
  {
    to.be.cancelled <- IB.05.open.orders %>% filter(orderType %in% !!orderType)
    
    if(nrow(to.be.cancelled) > 0)
    {
      IB.03.orders <- to.be.cancelled %>% select(-IB.Version) %>% distinct() %>% 
                      mutate(order.ts = Sys.time(), order.type = "Order Cancelled") %>%
                      bind_rows(IB.03.orders)
      
      assign("IB.03.orders",  IB.03.orders, envir = .GlobalEnv)
      
      for(i in 1:nrow(to.be.cancelled))
      {cancelOrder(twsconn = tws, orderId = to.be.cancelled$orderId[i])}
      rm(i)
    }
    
    rm(to.be.cancelled)
  }
  rm(orderType)
}

# -------------------------------------------------------------------------
# Post Order Activities
# -------------------------------------------------------------------------
Update.Activity.Log <- function()
{
  # if(!exists("IB.02.actions", envir = .GlobalEnv) || !IB.Parms[["Emergency"]] & nrow(IB.02.actions) == 0)
  # {return(cat("\nThere are NO Activity Updates ... "))}
  
  source("./Functions/F.Additional.IB.R")
  
  # ---------------------------------------------------- List of Executions
  good.run <- FALSE
  while(!good.run)
  {
    x <- tryCatch({req.Exec.df(tws)}
                  , warning = function(w) {0}
                  , error = function(e) {0} )
    if(is.data.frame(x) | is.null(x)) {good.run <- TRUE}
  } 
  rm(good.run)
  
  if(is.null(x)) 
  {
    rm(twsExecution, req.Exec.2, req.Exec.df, print.twsExecution, envir = .GlobalEnv)
    return(cat("\nThere were NO Recent Activity ... "))
  }
  
  suppressWarnings(
    x <- x %>%
      select(conId, orderId, symbol, side, time, shares, price, avgPrice) %>%
      rename(ticker = symbol, action = side) %>%
      mutate(order.ts = as.POSIXct(time, format="%Y%m%d %H:%M:%S",tz = Sys.timezone())) %>%
      # There is a 2 second clock diff of IB / System
      filter(order.ts > IB.Parms[["Last.Order.Time"]] - 2) %>% 
      mutate(conId = as.character(conId)
             , NY.time = as.numeric(strftime(format(order.ts, tz = "US/Eastern"), format = "%H.%M"))
             , action = case_when(action == "BOT" ~ "BUY",
                                  action == "SLD" ~ "SELL")
             ) %>%
      group_by(orderId, action, ticker) %>%
      summarise(order.ts = max(order.ts, na.rm = TRUE),
                shares = sum(shares),
                price = weighted.mean(avgPrice, W = shares)) %>%
      ungroup()
  )
  
  if(nrow(x) == 0) 
  {
    rm(x)
    rm(twsExecution, req.Exec.2, req.Exec.df, print.twsExecution, envir = .GlobalEnv)
    return(cat("\nThere were NO Recent Activity ... "))
  }
  
  ############################# In.Hand Portfolio / From Activity #############################
  y <- IB.04.activity %>% group_by(ticker, algoId, DP.Method, MA.Type, Period) %>%
        filter(order.ts == max(order.ts)) %>% 
        summarise(in.hand = sum(ifelse(action == "BUY", volume, -volume), na.rm = TRUE ))

  ############################# Updating IB Activity #############################
  if(IB.Parms[["Emergency"]])
  {
    z <- inner_join(y, x, by = "ticker") %>%
      group_by(ticker, action) %>%
      mutate(cumvol = cumsum(in.hand)
             , sold = case_when(cumvol <= shares ~ in.hand,
                                cumvol - shares < in.hand ~ cumvol - shares,
                                TRUE ~ 0)
             , in.hand = in.hand - sold
             , volume = sold
             , Situation = "Emergency"
      ) %>%
      select(ticker, algoId, DP.Method, MA.Type, Period, Situation, action, 
             orderId, order.ts, volume, price)
    
  } else
  {
    z <- IB.02.actions %>%
          mutate(IB.action = as.character(ifelse(grepl("SELL", action), "SELL", "BUY")) ) %>%
          left_join(x %>% rename(IB.action = action), by = c("ticker", "IB.action")) %>%
          filter(!is.na(orderId)) %>%
          mutate(Situation = "Normal") %>%
          select(ticker, algoId, DP.Method, MA.Type, Period, Situation, action, 
                 orderId, order.ts, volume, price)
  }
  
  ############################# For Manual Override #############################
  if(nrow(z) == 0 & nrow(x) > 0)
  {
    z <- left_join(x, y, by = "ticker") %>% 
          mutate(Situation = "Manual") %>% 
          rename(volume = shares) %>% 
          select(ticker, algoId, DP.Method, MA.Type, Period, Situation, 
                 action, orderId, order.ts, volume, price)  
  }

  z <- bind_rows(z, IB.04.activity) %>%
        ungroup() %>% arrange(desc(order.ts)) %>%
        distinct()
  
  assign("IB.04.activity", z, envir = .GlobalEnv)
  IB.Parms[["Last.Order.Time"]] <- Sys.time()
  assign("IB.Parms", IB.Parms, envir = .GlobalEnv)
  
  rm(x, y, z)
  rm(twsExecution, req.Exec.2, req.Exec.df, print.twsExecution, envir = .GlobalEnv)
}

Update.Targets <- function()
{
  if(IB.Parms[["Emergency"]]) {return(cat("\n\nSystem halted due to Emergency"))}

  x <- left_join(IB.01.targets,
                 IB.04.activity %>% distinct() %>% 
                   group_by(ticker, algoId, DP.Method, MA.Type, Period) %>%
                   filter(order.ts == max(order.ts)) %>%
                   mutate(ds = as.Date(order.ts)) %>%
                   select(ticker, algoId, DP.Method, MA.Type, Period, ds, action) %>%
                   rename(action.new = action),
                 by = c("ticker", "algoId", "DP.Method", "MA.Type", "Period", "ds")) 
  
  for(i in 1:nrow(x))
  {
    if(is.na(x$action.new[i]))
    {next()}
    
    if(grepl("BUY", x$action.new[i]))
    {
      x$action[i] <- "HOLD"
      x$invest[i] <- NA
    } 
    
    if(grepl("SELL", x$action.new[i]))
    {
      x$action[i] <- NA
    }
  }
  
  x <- x %>% select(-action.new)

  assign("IB.01.targets", x, envir = .GlobalEnv)
  rm(x, i)
}

# -------------------------------------------------------------------------
# Order Placement
# -------------------------------------------------------------------------
Get.Actions <- function()
{
  if(length(unique(IB.01.targets$ticker)) == 0) {return(cat("\nNo Targets Available \n"))}
  
  d1 <- foreach(ticker = unique(IB.01.targets$ticker), .packages = "BatchGetSymbols"
                , .combine = bind_rows, .errorhandling = 'remove'
                ) %do%
                {
                  d1 <- get.clean.data(ticker, src = "yahoo",
                                       first.date = Sys.Date(), last.date  = Sys.Date() + 1 ) %>%
                        rename(ds = ref.date, open = price.open, high = price.high, 
                               low = price.low, close = price.close)
                  
                  rm(ticker)
                  return(d1)
                }
  
  failed <- setdiff(unique(IB.01.targets$ticker), unique(d1$ticker))
  if(length(failed) > 0)
  {cat("\nData Pull Failed for", length(failed), "targets:", paste(failed, collapse = ", ") )}
  rm(failed)

  d1 <- d1 %>%
        group_by(ticker) %>% arrange(ticker, ds) %>%
        mutate(NY.Time = as.numeric(format(Sys.time(), tz = "US/Eastern", format = "%H.%M")) ) %>%
        filter(ds == Sys.Date()) %>%
        select(c(ticker, NY.Time, open, high, low, close)) %>%
        ungroup()

  if(nrow(d1) == 0)
  {
    rm(d1)
    return(cat("\nNo Data Found. Check Connection with Yahoo!!!"))
  }

  # -------------------------------------------------------------------------
  position <- IB.04.activity %>% 
              group_by(ticker, algoId, DP.Method, MA.Type, Period) %>%
              summarise(model.position = sum(ifelse(action == "BUY", volume, -volume))) %>%
              filter(model.position > 0) %>%
              left_join(IB.00.positions %>% rename(ticker = symbol) %>%
                          group_by(ticker) %>% summarise(ticker.position = sum(position))
                        , by = "ticker") %>%
              filter(ticker.position >= model.position)
    
  IB.02.actions <- inner_join(IB.01.targets, d1, by = "ticker") %>%
                    left_join(position %>% select(-ticker.position)
                              , by = c("ticker", "algoId", "DP.Method", "MA.Type", "Period")) %>%
                    mutate(action = case_when(buy.price >= close & close > stop.price & invest > 0 ~ "BUY",
                                              sell.price <= close & model.position > 0 ~ "SELL",
                                              close <= stop.price & model.position > 0 ~ "STOP SELL",
                                              between(NY.Time, IB.Parms[["Last.Sell.At"]], 16) &
                                                last.sell > 0 & model.position > 0 ~ "EOD SELL")
                           ) %>%
                    filter(!is.na(action)) %>%
                    mutate(  t.price = case_when(action %in% c("BUY", "SELL") ~ round(close, 2))
                           , m.price = case_when(action == "SELL" ~ as.numeric(sell.price),
                                                 action == "STOP SELL" ~ as.numeric(stop.price),
                                                 action == "BUY" ~ as.numeric(buy.price) )
                           , volume = case_when(grepl("SELL", action) ~ model.position,
                                                grepl("BUY", action) ~ floor(invest/t.price))
                           ) %>%
                    as.data.frame() %>%
                    select(ds, ticker, action, volume, t.price, m.price
                           , algoId, DP.Method, MA.Type, Period, tickerID, NY.Time, Exchange) %>%
                    filter(!is.na(action), volume > 0) %>%
                    # ---------------------------------------------------------------------
                    # Order within fund limits: Check if we need the cummin function
                    group_by(action) %>%
                    mutate(Cost = if_else(action == "BUY", -volume*t.price, volume*t.price)
                           , Available.Funds = Available.Funds + cumsum(Cost)
                           ) %>%
                    filter(Available.Funds > 0 | action != "BUY") %>% 
                    select(-c(Cost, Available.Funds)) %>%
                    ungroup()

  # -------------------------------------------------------------------------
  assign("IB.02.actions", IB.02.actions, envir = .GlobalEnv)
  rm(d1, position)
}

Update.Orderbook <- function(Contract, Order, order.type = "")
{
  IB.03.orders <- data.frame(t(do.call(rbind, Contract)), stringsAsFactors = FALSE) %>%
    bind_cols(data.frame(t(do.call(rbind, Order)), stringsAsFactors = FALSE)) %>%
    select(account, parentId, orderId, conId, symbol, sectype, strike,
           currency, action, totalQuantity, orderType, lmtPrice, auxPrice, tif) %>%
    mutate(parentId = as.integer(parentId)
           , orderId = as.integer(orderId)
           , totalQuantity = as.numeric(totalQuantity)
           , lmtPrice = as.numeric(lmtPrice)
           , auxPrice = as.numeric(auxPrice)
           , order.ts = Sys.time()
           , order.type) %>%
    bind_rows(IB.03.orders)
  
  assign("IB.03.orders",  IB.03.orders, envir = .GlobalEnv)
  rm(IB.03.orders, order.type)
}

IB.Place.Orders <- function()
{
  if(IB.Parms[["Emergency"]]) {return(cat("\nSystem halted due to Emergency"))}
  if(nrow(IB.02.actions) == 0) {return(cat("\nThere are NO Actions ... "))}
  
  IB.Parms[["Last.Order.Time"]] <- Sys.time()
  assign("IB.Parms", IB.Parms, envir = .GlobalEnv)
  
  x <- IB.02.actions %>%
        group_by(NY.Time, ticker, Exchange, tickerID, action, t.price) %>%
        summarise(volume = sum(volume, na.rm = TRUE))

  for(i in 1:nrow(x))
  {
    Contract <- twsEquity(symbol = x$ticker[i], local = x$ticker[i], primary = x$Exchange[i]
                          , currency = "USD", exch = "SMART")

    if(x$action[i] == "BUY")
    {
      
      Order <- twsOrder(orderId = reqIds(tws) 
                        , action = "BUY"
                        , clientId = tws$clientId
                        , account = IB.Parms[["acctCode"]]
                        , totalQuantity = x$volume[i]
                        , orderType = "LMT"               # Limit Order
                        # If limit price >  market price, IB buys at limit price
                        , lmtPrice = x$t.price[i]
                        , tif = "DAY"                     # Order till Day
                        )
      
    } else if(x$action[i] == "SELL")
    {
      Order <- twsOrder(orderId = reqIds(tws) 
                        , action = "SELL"
                        , clientId = tws$clientId
                        , account = IB.Parms[["acctCode"]]
                        , totalQuantity = x$volume[i]
                        , orderType = "LMT"               # Limit Order
                        , lmtPrice = x$t.price[i]
                        , tif = "DAY"                     # Order till Day
                        )

    } else if(x$action[i] %in% c("EOD SELL", "STOP SELL") )
    {
      Order <- twsOrder(orderId = reqIds(tws) 
                        , action = "SELL"
                        , clientId = tws$clientId
                        , account = IB.Parms[["acctCode"]]
                        , totalQuantity = x$volume[i]
                        , orderType = "MKT"               # Market Order
                        , tif = "DAY"                     # Order till Day
                        )
    }
    
    Update.Orderbook(Contract, Order, order.type = "Order Placed")

    if(i %% 40 == 0) {Sys.sleep(1)} # Limitations of API: Can process only 50 orders/second
    IBrokers::placeOrder(twsconn = tws, Contract, Order)
    rm(i, Contract, Order)
  }
  
  rm(x)
}

Get.Action.Plots <- function()
{
  
  if(nrow(IB.02.actions) == 0) return(cat("\n"))

  for(i in 1:nrow(IB.02.actions))
  {
    # i = 1
    x <- IB.02.actions[i, ]
    df <- readRDS(paste0(IB.Parms[["data.folder"]], "Production/Simulation/", x$ticker, ".rds")) %>%
          semi_join(x, by = c("algoId", "DP.Method", "MA.Type", "Period")) %>%
          mutate(  A.Score = ifelse(!is.na(buy.price), Score, NA)
                 , P.Score = ifelse(is.na(buy.price), Score, NA)
                 , T.Score = ifelse(!is.na(A.Score*lead(P.Score)) |
                                      !is.na(lag(A.Score)*P.Score) |
                                      !is.na(P.Score*lead(A.Score)) |
                                      !is.na(lag(P.Score)*A.Score)
                                    , Score, NA)
                 ) %>%
          filter(ds > IB.Parms[["last.dev.date"]])

    x <- IB.02.actions[i,] %>%
          mutate(buy.price = last(df$buy.price)
                 , sell.price = last(df$sell.price)
                 , stop.price = last(df$stop.price)
                 , Model.ID = paste(algoId, DP.Method, MA.Type, Period)) %>%
          select(-c(tickerID, algoId, DP.Method, MA.Type, Period, m.price)) %>%
          bind_cols(df %>% filter(!is.na(ROI)) %>% 
                      summarise(N = n()
                                , ROI = paste0(round(100*mean(ROI), 1), "%")
                                , invest.period = round(mean(invest.period), 2)) )
    
    if(max(df$close, na.rm = TRUE) > 20)
    {
      min.y <- 10*floor(min(df$close/10, na.rm = TRUE)/1.25) # 1.25 to accomodate buy/sell prices
      max.y <- 10*ceiling(max(df$close/10, na.rm = TRUE)*1.25)
    } else
    {
      min.y <- floor(min(df$close, na.rm = TRUE)/1.25)
      max.y <- ceiling(max(df$close, na.rm = TRUE)*1.25)
    }
    
    t1 <- paste0(x$ticker, " | ", x$Model.ID)
    t2 <- ifelse(is.na(x$t.price),
                 paste0(x$action, " ", x$volume, " Units @ Market Price"),
                 paste0(x$action, " ", x$volume, " Units @ $", round(x$t.price, 2))
                 )
    t3 <- paste0("Buy $", x$buy.price, " | Sell $", x$sell.price, " | Stop Loss $", x$stop.price)
    t4 <- ifelse(x$N <= 0, "No History"
                 , paste0("History: ", x$N, " times for ", x$invest.period, " Days | ROI ", x$ROI))
    
    p <- plot_ly(data = df, x = ~ds) %>%
      add_trace(y = ~close, name = "Close",
                type = 'scatter', mode = 'lines',
                line = list(color = '#a6a6a6', width = 0.5),
                fill = 'tozeroy', fillcolor='#f9f7f7' ) %>%
      add_trace(y = ~P.Score, name = 'Passive Score', yaxis = "y2",
                type = 'scatter', mode = 'lines',
                line = list(color = '#3f6ea6', dash='dot', width = 1.5) ) %>%
      add_trace(y = ~A.Score, name = 'Active Score', yaxis = "y2",
                type = 'scatter', mode = 'lines',
                line = list(color = '#3f6ea6', width = 1.5) ) %>%
      add_trace(y = ~T.Score, name = 'Transition Score', yaxis = "y2",
                type = 'scatter', mode = 'lines+markers',
                line = list(color = '#cc0000', dash='dot', width = 1) ) %>%
      add_trace(y = ~buy.price, name = 'Buy Price', type = 'scatter', mode = 'lines',
                line = list(color = '#444444', width = 1) ) %>%
      add_trace(y = ~sell.price, name = 'Sell Price', type = 'scatter', mode = 'lines',
                line = list(color = '#444444', width = 1) ) %>%
      
      add_annotations(text = t1, xref = 'paper', yref = 'paper', x = 0, y = 1.09,
                      font = list(size = 14, color = '#004080'), showarrow = F) %>%
      add_annotations(text = t2, xref = 'paper', yref = 'paper', x = 0, y = 1.05,
                      font = list(size = 12, color = '#004080'), showarrow = F) %>%
      add_annotations(text = paste0("New York Time: ", x$NY.Time),
                      xref = 'paper', yref = 'paper', x = 1, y = 1.09,
                      font = list(size = 12), showarrow = F) %>%
      add_annotations(text = t3, xref = 'paper', yref = 'paper', x = 1, y = 1.06,
                      font = list(size = 12, color = '#004080'), showarrow = F) %>%
      add_annotations(text = t4, xref = 'paper', yref = 'paper', x = 1, y = 1.03,
                      font = list(size = 12, color = '#004080'), showarrow = F) %>%
      
      layout( font = list(size = 12), showlegend = FALSE, hovermode = 'compare',
              margin = list(l = 70, r = 70, b = 50, t = 50, pad = 10),
              xaxis = list(title = NA, showgrid = FALSE),
              yaxis = list(title = "Close ($)", color = '#a6a6a6',
                           range = c(min.y, max.y), showgrid = FALSE),
              yaxis2 = list(title = "Score", color = '#3f6ea6', tickformat = ".1%",
                            overlaying = "y", side = "right", showgrid = TRUE) )

    fname <- paste0(x$ticker, " ", x$Model.ID, ".html")
    
    saveWidget(as_widget(p)
               , title = paste0(x$ticker, ": ", x$action, " ", x$volume, " Units @ $", x$t.price)
               , libdir = paste0(IB.Parms[["data.folder"]], "Production/Plots/libdir/")
               , file = paste0(IB.Parms[["data.folder"]], "Production/Plots/", fname)
               )
    
    browseURL(paste0(IB.Parms[["data.folder"]], "Production/Plots/", fname))
    
    rm(max.y, min.y, t1, t2, t3, t4, fname, p, i, df, x)
    
  }

}

# -------------------------------------------------------------------------
# Closure of Business Day: Create Logs
# -------------------------------------------------------------------------
IB.Missed.Orders <- function(today = Sys.Date())
{
  d1 <- foreach(ticker = unique(IB.01.targets$ticker),
                .combine = bind_rows, .errorhandling = 'remove') %do%
                {
                  d1 <- get.clean.data(ticker, src = "yahoo", first.date = today, last.date  = today + 1) %>%
                    rename(ds = ref.date, open = price.open, high = price.high, 
                           low = price.low, close = price.close) %>%
                    select(ticker, ds, open, high, low, close)
                  
                  rm(ticker)
                  return(d1)
                }
  
  x <- IB.01.targets %>%
        left_join(d1, by = c("ticker", "ds")) %>%
        left_join(IB.04.activity %>% 
                    group_by(ticker, algoId, DP.Method, MA.Type, Period) %>%
                    summarise(in.hand = sum(ifelse(action == "BUY", volume, -volume))),
                  by = c("ticker", "algoId", "DP.Method", "MA.Type", "Period")) %>%
        mutate(action = case_when(buy.price >= low & invest > 0 ~ "BUY",
                                  sell.price <= high & in.hand > 0 ~ "SELL",
                                  last.sell > 0 & in.hand > 0 ~ "EOD SELL" ),
                m.price = case_when(action == "BUY" ~ buy.price,
                                    action == "SELL" ~ sell.price,
                                    action == "EOD SELL" ~ last.sell),
                volume = case_when(action == "BUY" ~ floor(invest/buy.price),
                                   action %in% c("SELL", "EOD SELL") ~ in.hand)
        ) %>%
        filter(!is.na(action)) %>%
        select(ds, ticker, action, volume, m.price, algoId, DP.Method, MA.Type, Period, tickerID, Exchange)
  
  assign("IB.06.missed.orders", x, envir = .GlobalEnv)
  rm(today, d1, x)
  
}

IB.FinishDay <- function(Force.Close = FALSE)
{
  NY.Time <- as.numeric(strftime(format(Sys.time(), tz = "US/Eastern"), format = "%H.%M"))
  if(!(NY.Time > 16.00 | Force.Close))
  {return(cat("\nWarning!!! Market is Still Open. \nCannot Shutdown the System ... "))}
  rm(NY.Time)

  IB.Cancel.Orders()
  IB.Missed.Orders()
  
  h.missed <- readRDS(paste0(IB.Parms[["data.folder"]], "Production/Trading/06.Historical.Misses.rds")) %>%
              bind_rows(IB.06.missed.orders) %>% distinct()
  
  h.activity <- readRDS(paste0(IB.Parms[["data.folder"]], "Production/Trading/02.Historical.Activity.rds")) %>%
                bind_rows(IB.04.activity) %>% distinct() %>% arrange(desc(order.ts))

  h.orders <- readRDS(paste0(IB.Parms[["data.folder"]], "Production/Trading/03.Historical.Orders.rds")) %>%
              bind_rows(IB.03.orders) %>% distinct() %>% arrange(desc(order.ts))
  
  saveRDS(h.missed, paste0(IB.Parms[["data.folder"]], "Production/Trading/06.Historical.Misses.rds"))
  saveRDS(h.activity, paste0(IB.Parms[["data.folder"]], "Production/Trading/02.Historical.Activity.rds"))
  saveRDS(h.orders, paste0(IB.Parms[["data.folder"]], "Production/Trading/03.Historical.Orders.rds"))
  
  rm(h.missed, h.activity, h.orders)
  rm(list = setdiff(ls(envir = .GlobalEnv), c("tws")), envir = .GlobalEnv)
}

# -------------------------------------------------------------------------
# Extreme Scenario Functions
# -------------------------------------------------------------------------
# IB.Close.Positions <- function()
# {
#   IB.Cancel.Orders(orderType = c("LMT", "MKT"))
#   IB.Account.Status()
# 
#   x <- IB.00.positions %>%
#         filter(position > 0) %>%
#         rename(ticker = symbol, volume = position) %>%
#         select(accountName, conId, ticker, local, volume, primary, currency) %>%
#         mutate(primary = ifelse(primary == "NASDAQ", "ISLAND", primary))
# 
#     if(nrow(x) > 0)
#     {
#       IB.Parms[["Last.Order.Time"]] <- Sys.time()
#       assign("IB.Parms", IB.Parms, envir = .GlobalEnv)
#       
#       for(i in 1:nrow(x))
#       {
#         Contract <- twsEquity(symbol = x$ticker[i]
#                              , primary = x$primary[i]
#                              , currency = x$currency[i]
#                              , exch = "SMART")
#         
#         Order <- twsOrder(orderId = reqIds(tws)
#                          , action = "SELL"
#                          , clientId = tws$clientId
#                          , account = IB.Parms[["acctCode"]]
#                          , totalQuantity = x$volume[i]
#                          , orderType = "MKT"
#                          , tif = "DAY")
#         
#         Update.Orderbook(Contract, Order, order.type = "Order Cancelled")
#         
#         IBrokers::placeOrder(twsconn = tws, Contract, Order)
#         rm(Contract, Order, i)
#       }
#       
#     } else
#     {
#       rm(x)
#       return(cat("\nAccount", IB.Parms[["acctCode"]],  "holds no positions. Verify on Workstation."))
#     }
# 
#   rm(x)
# }
# 
# IB.Restart <- function()
# {
#   IB.03.orders = data.frame()
#   IB.04.activity <- data.frame(ticker = character()
#                                , algoId = character()
#                                , DP.Method = character()
#                                , MA.Type = character()
#                                , Period = numeric()
#                                , Situation = character()
#                                , algoId = character()
#                                , action = character()
#                                , orderId = integer()
#                                , order.ts = as.POSIXct(character(), tz = Sys.timezone())
#                                , volume = numeric()
#                                , price = numeric()
#                                , stringsAsFactors=FALSE)
#   
#   IB.06.missed.orders <- data.frame()
#   
#   saveRDS(IB.04.activity, paste0(IB.Parms[["data.folder"]], "Production/Trading/02.Historical.Activity.rds"))
#   saveRDS(IB.03.orders, paste0(IB.Parms[["data.folder"]], "Production/Trading/03.Historical.Orders.rds"))
#   saveRDS(IB.06.missed.orders, paste0(IB.Parms[["data.folder"]], "Production/Trading/06.Historical.Misses.rds"))
#   
#   rm(IB.04.activity, IB.03.orders, IB.06.missed.orders)
# }
# 
# Initiate.Emergency <- function()
# {
#   IB.Parms[["Emergency"]] <- TRUE
#   assign("IB.Parms", IB.Parms, envir = .GlobalEnv)
# 
#   repeat{
#           IB.Close.Positions()
#           cat("\nSystem will wait for 60 seconds to close all open positions ... \n")
#           Sys.sleep(60)
#           Update.Activity.Log()
#           Get.Open.Orders()
#           if(nrow(IB.05.open.orders) == 0) {break}
#         }
# 
#   View(IB.04.activity)
#   cat("\nThe Emergency Procedure was completed.\n")
#   IB.Parms[["Emergency"]] <- FALSE
#   assign("IB.Parms", IB.Parms, envir = .GlobalEnv)
# }


# -------------------------------------------------------------------------
