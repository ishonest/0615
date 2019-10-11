# Settings: https://blog.quantinsti.com/ibpy-tutorial-implement-python-interactive-brokers-api/
# -------------------------------------------------------------------------
IB.Restart <- function()
{
  IB.activity <- data.frame(ticker = character()
                            , DP.Method = character()
                            , MA.Type = character()
                            , Period = numeric()
                            , Situation = character()
                            , algoId = character()
                            , Action = character()
                            , orderId = integer()
                            , order.ts = as.POSIXct(character(), tz = Sys.timezone())
                            , volume = numeric()
                            , in.hand = numeric()
                            , price = numeric()
                            , stringsAsFactors=FALSE)
  
  IB.orderbook = data.frame()

  overview <- TTR::stockSymbols() %>%
              filter(!grepl("ETF$|Fund$|Trust$|Trust, Inc.$| Fund|^ProShares|^Proshares", Name)) %>%
              rename(ticker = Symbol) %>% ungroup() %>% arrange(ticker, desc(Exchange)) %>%
              group_by(ticker) %>% top_n(1) %>%
              ungroup() %>% mutate(tickerID = row_number() + 1000)

  saveRDS(IB.activity, paste0(data.folder, "IB/IB.Activity.rds"))
  saveRDS(IB.orderbook, paste0(data.folder, "IB/IB.Orderbook.rds"))
  saveRDS(overview, paste0(data.folder, "IB/Prod.Overview.rds"))

  if(exists("IB.activity", envir = .GlobalEnv)) {assign("IB.activity", IB.activity, envir = .GlobalEnv)}
  if(exists("IB.orderbook", envir = .GlobalEnv)) {assign("IB.orderbook", IB.orderbook, envir = .GlobalEnv)}
  if(exists("IB.index", envir = .GlobalEnv)) {assign("IB.index", overview, envir = .GlobalEnv)}

  if(!exists("tws", envir = .GlobalEnv) || !isConnected(tws)) {IB.Connect()}
  if(exists("IB.contracts", envir = .GlobalEnv)) {IB.Account.Status()}
  if(exists("targets", envir = .GlobalEnv)) {First.Targets()}
  if(exists("actions", envir = .GlobalEnv)) {rm(actions, envir = .GlobalEnv)}
  assign("Last.Order.Time", Sys.time(), envir = .GlobalEnv)
  
  rm(IB.activity, overview)

}
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
# -------------------------------------------------------------------------
IB.Account.Status <- function()
{
  cancelAccountUpdates(tws)
  d <- reqAccountUpdates(conn = tws, acctCode = IB.Parms[["acctCode"]], subscribe = TRUE)
  assign("IB.account",
         data.frame(t(sapply(d[[1]],c)), stringsAsFactors = FALSE),
         envir = .GlobalEnv)

  d2 <- d[[2]]
  IB.contracts <- data.frame()
  
  if(length(d2) > 0)
  {
    for(i in 1:length(d2))
    {
      x <- bind_cols(data.frame(t(sapply(d2[[i]][1][[1]],c)), stringsAsFactors = FALSE),
                     data.frame(t(sapply(d2[[i]][2][[1]],c)), stringsAsFactors = FALSE) )
  
      IB.contracts <- bind_rows(IB.contracts, x)
      rm(x, i)
    }
   
    IB.contracts <- IB.contracts[c("accountName", "conId", "symbol", "position",
                                   "marketPrice", "marketValue", "averageCost",
                                   "unrealizedPNL", "realizedPNL",
                                   "local", "currency", "exch", "primary", "sectype",
                                   "expiry", "strike", "right",
                                   "multiplier", "combo_legs_desc", "comboleg",
                                   "include_expired", "secIdType", "secId")]

    cols.to.numeric <- c("strike", "right", "position", "marketPrice",
                         "marketValue", "averageCost", "unrealizedPNL")
    IB.contracts[cols.to.numeric] <- as.numeric(as.matrix(IB.contracts[cols.to.numeric]))
    assign("IB.contracts", IB.contracts, envir = .GlobalEnv)
    rm(cols.to.numeric)
  }

  Current.Position <- as.numeric(IB.account["GrossPositionValue", "value"])
  # Check this factor in live
  Available.Funds <- pmin(as.numeric(IB.account["AvailableFunds", "value"]), 
                          IB.Parms[["invest.max"]] - Current.Position)
  
  assign("Available.Funds", Available.Funds, envir = .GlobalEnv)
  
  cat(paste0("\n\n-------------------------------", 
             "\n:  Available Funds  : $ ", formatC(Available.Funds, format="f", big.mark=",", digits=0),
             "\n:  Current Position : $ ", formatC(Current.Position, format="f", big.mark=",", digits=0),
             "\n-------------------------------\n"
             ))

  if(Current.Position >= IB.Parms[["invest.max"]])
  {
    cat(paste0("\nThe System has reached its maximum permissible position of $ ", 
               formatC(IB.Parms[["invest.max"]], format="f", big.mark=",", digits=0)))
  }
  
  rm(d, d2, Current.Position)

}
# -------------------------------------------------------------------------
First.Targets <- function()
{
  overview <- IB.index %>% select(ticker, Exchange)
  targets <- left_join(readRDS(paste0(data.folder, "IB/Prod.Summary.rds")), overview, by = "ticker") %>%
              mutate(Exchange = ifelse(Exchange == "NASDAQ", "ISLAND", Exchange))
  #In the API side, NASDAQ is always defined as ISLAND in the exchange field
  rm(overview)
  assign("targets", targets, envir = .GlobalEnv)
}
# -------------------------------------------------------------------------
AF.Buy.Restrictions <- function(Action, close, max.in.hand, model.in.hand, total.in.hand)
{
  if(!is.na(Action) & Action == "BUY")
  {
    b1 = ifelse(total.in.hand >= max.in.hand, 0, max.in.hand - total.in.hand) # Can Buy based on macro
    b1 = b1*close
    if(b1 == 0) {return(0)}
    
    max.invest = IB.Parms[["invest.max.M"]] - model.in.hand*close
    min.invest = IB.Parms[["invest.min.M"]]
    if(max.invest <= min.invest | b1 <= min.invest) {return(0)}
    
    invest = pmin(b1, max.invest)
    buy.vol = floor(invest/close)
    
    rm(b1, max.invest, min.invest, close, max.in.hand, model.in.hand, total.in.hand)
    return(buy.vol)
  } else {return(0)}
  
}
# -------------------------------------------------------------------------
Get.Actions <- function()
{
  if(length(unique(targets$ticker)) == 0) {return(cat("\nNo Targets Available \n"))}
  
  d1 <- foreach(ticker = unique(targets$ticker),
                .combine = bind_rows, .errorhandling = 'remove'
                ) %do%
                {
                  d1x <- get.clean.data(ticker, src = "yahoo",
                                        first.date = Sys.Date() - 7,
                                        last.date  = Sys.Date() + 1
                                        )
                  rm(ticker)
                  return(d1x)
                }
  rm(d1x)

  d1 <- d1 %>%
        group_by(ticker) %>% arrange(ticker, ref.date) %>%
        mutate(NY.Time = as.numeric(strftime(format(Sys.time(), tz = "US/Eastern"), format = "%H.%M"))
               , max.in.hand1 = IB.Parms[["max.vol.ratio"]]*lag(volume) # Based on last volume
               , max.in.hand2 = IB.Parms[["invest.max.T"]]/price.close  # Based on max invest
               , max.in.hand = floor(pmin(max.in.hand1, max.in.hand2))
               ) %>%
        select(-c(max.in.hand1, max.in.hand2)) %>%
        filter(ref.date == Sys.Date()) %>%
        rename(ds = ref.date, open = price.open, high = price.high, low = price.low, close = price.close) %>%
        select(c(ticker, NY.Time, open, high, low, close, max.in.hand))

  if(nrow(d1) == 0)
  {
    rm(d1)
    return(cat("\nNo Data Found. Check Connection with Yahoo!!!"))
  }

  # -------------------------------------------------------------------------
  model.in.hand <- IB.activity %>% 
                    mutate(volume = ifelse(Action == "BUY", volume, -volume)) %>%
                    group_by(ticker, DP.Method, MA.Type, Period) %>% 
                    summarise(model.in.hand = sum(volume))
  
  total.in.hand <- model.in.hand %>%
                    group_by(ticker) %>% 
                    summarise(total.in.hand = sum(model.in.hand))

  actions <- inner_join(targets, d1, by = "ticker") %>%
              left_join(model.in.hand, by = c("ticker", "DP.Method", "MA.Type", "Period")) %>%
              left_join(total.in.hand, by = c("ticker")) %>%
              rowwise() %>%
              mutate(model.in.hand = tidyr::replace_na(model.in.hand, 0)
                     , total.in.hand = tidyr::replace_na(total.in.hand, 0)
                     , Action = case_when(buy.price >= close
                                          & total.in.hand <= max.in.hand
                                          # & NY.Time >= IB.Parms[["Start.Trading.At"]] 
                                          ~ "BUY",
                                          sell.price <= close & model.in.hand > 0 
                                          ~ "SELL",
                                          last.sell > 0 & model.in.hand > 0 
                                          & between(NY.Time, IB.Parms[["Last.Sell.At"]], 16) 
                                          ~ "EOD SELL")
                     , buy.vol = AF.Buy.Restrictions(Action, close, max.in.hand, model.in.hand, total.in.hand)
                     , volume = case_when(Action == "SELL" ~ model.in.hand,
                                          Action == "EOD SELL" ~ model.in.hand,
                                          Action == "BUY" ~ buy.vol )
                     # , Action = ifelse(volume <= 0, NA, Action) # Sanity Check
                     , t.price = case_when(Action %in% c("BUY", "SELL") ~ 0.01*floor(100*close))
                     , m.price = case_when(Action == "SELL" ~ sell.price,
                                           Action == "EOD SELL" ~ sell.price,
                                           Action == "BUY" ~ buy.price )
                     ) %>%
              as.data.frame() %>%
              select(ds, NY.Time, ticker, Exchange, Action, volume, t.price, m.price, DP.Method, MA.Type, Period) %>%
              filter(!is.na(Action), volume > 0) %>%
              # ---------------------------------------------------------------------
              # Order within fund limits: Check if we need the cummin function
              mutate(Cost = if_else(Action == "BUY", -volume*t.price, volume*t.price)
                     , Available.Funds = Available.Funds + cumsum(Cost)
                     # , Available.Funds = cummin(Available.Funds)
                     ) %>%
              filter(Available.Funds > 0 | Action != "BUY") %>% 
              select(-c(Cost, Available.Funds)) %>%
              # ---------------------------------------------------------------------
              # Buy for a max 3 models
              group_by(ticker, Action) %>% mutate(N = row_number()) %>%
              filter(N <= IB.Parms[["invest.max.T"]]/IB.Parms[["invest.max.M"]]  |  Action != "BUY") %>%
              select(-N) %>% ungroup() %>%
              # ---------------------------------------------------------------------
              left_join(IB.index %>% select(ticker, tickerID), by = "ticker")
  
  
  # -------------------------------------------------------------------------
  assign("actions", actions, envir = .GlobalEnv)
  rm(d1, model.in.hand, total.in.hand)
}
# -------------------------------------------------------------------------
IB.StartDay <- function()
{
  options(scipen = 3, digits = 8)
  set.seed(1024)
  library(IBrokers)
  library(dplyr)
  library(foreach)
  library(BatchGetSymbols)

  assign("IB.index", readRDS(paste0(data.folder, "IB/Prod.Overview.rds")), envir = .GlobalEnv)
  assign("IB.activity", readRDS(paste0(data.folder, "IB/IB.Activity.rds")), envir = .GlobalEnv)
  assign("IB.orderbook", readRDS(paste0(data.folder, "IB/IB.Orderbook.rds")), envir = .GlobalEnv)
  assign("Last.Order.Time", Sys.time(), envir = .GlobalEnv)
  assign("Emergency", FALSE, envir = .GlobalEnv)
  
  if(!exists("tws", envir = .GlobalEnv) || !isConnected(tws)) 
  {IB.Connect()}                          # Create a connection
  IB.Account.Status()                     # Creates IB.account & IB.contracts
  First.Targets()                         # Creates targets
  IB.System.Status()
  # if(isTRUE(System.Live)){Get.Actions()}  # Creates actions
}
# -------------------------------------------------------------------------
IB.Place.Orders <- function()
{
  if(Emergency) {return(cat("\nSystem halted due to Emergency"))}
  if(nrow(actions) == 0) {return(cat("\nThere are NO Actions ... "))}
  
  assign("Last.Order.Time", Sys.time(), envir = .GlobalEnv)
  x <- actions %>%
        group_by(NY.Time, ticker, Exchange, tickerID, Action, t.price) %>%
        summarise(volume = sum(volume, na.rm = TRUE))

  for(i in 1:nrow(x))
  {
    Contract <- twsEquity(symbol = x$ticker[i], local = x$ticker[i], primary = x$Exchange[i]
                          , currency = "USD", exch = "SMART")

    if(x$Action[i] == "BUY")
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
      
    } else if(x$Action[i] == "SELL")
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

    } else if(x$Action[i] == "EOD SELL")
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

    # assign("IB.counter",  IB.counter + 1, envir = .GlobalEnv)
    if(i %% 40 == 0) {Sys.sleep(1)} # Limitations of API: Can process only 50 orders/second
    IBrokers::placeOrder(twsconn = tws, Contract, Order)
    rm(i, Contract, Order)
  }
  
  rm(x)
}
# -------------------------------------------------------------------------
Update.Activity.Log <- function()
{
  if(!exists("actions", envir = .GlobalEnv) || !Emergency & nrow(actions) == 0) 
    {return(cat("\nThere are NO Activity Updates ... "))}
  
  source("./Functions/F.Additional.IB.R")

  # ---------------------------------------------------- List of Executions
  good.run <- FALSE
  while(!good.run)
  {
    x <- tryCatch({req.Exec.df(tws)}
                  , warning = function(w) {0}
                  , error = function(e) {0}
                  )
    if(is.data.frame(x) | is.null(x)) {good.run <- TRUE}
  } 
  rm(good.run)
     
  if(is.null(x)) 
  {
    rm(twsExecution, req.Exec.2, req.Exec.df, print.twsExecution, envir = .GlobalEnv)
    return(cat("\nThere were NO Recent Activity ... "))
  }
  
  x <- x %>%
        select(conId, orderId, symbol, side, time, shares, price, avgPrice) %>%
        rename(ticker = symbol, Action = side) %>%
        mutate(order.ts = as.POSIXct(time, format="%Y%m%d %H:%M:%S",tz = Sys.timezone())) %>%
        filter(order.ts > Last.Order.Time - 2) %>% # There is a 2 second clock diff of IB / System
        mutate(conId = as.character(conId)
               , NY.time = as.numeric(strftime(format(order.ts, tz = "US/Eastern"), format = "%H.%M"))
               , Action = case_when(Action == "BOT" ~ "BUY",
                                    Action == "SLD" ~ "SELL",
                                    Action == "SLD" & NY.time >= IB.Parms[["Last.Sell.At"]] ~ "EOD SELL")
        ) %>%
        group_by(orderId, Action, ticker) %>%
        summarise(order.ts = max(order.ts, na.rm = TRUE),
                  shares = sum(shares),
                  price = weighted.mean(avgPrice, W = shares)) %>%
        ungroup()

  # ---------------------------------------------------- In.Hand Portfolio / From Activity
  y <- IB.activity %>% group_by(ticker, DP.Method, MA.Type, Period) %>%
        filter(order.ts == max(order.ts)) %>% 
        # filter(in.hand == min(in.hand)) %>% # Because EOD Sell can happen after Buy
        select(ticker, DP.Method, MA.Type, Period, in.hand)

  # ---------------------------------------------------- Updating IB Activity
  if(Emergency)
  {
      z <- inner_join(y, x, by = "ticker") %>%
            group_by(ticker, Action) %>%
            mutate(cumvol = cumsum(in.hand)
                   , sold = case_when(cumvol <= shares ~ in.hand,
                                      cumvol - shares < in.hand ~ cumvol - shares,
                                      TRUE ~ 0)
                   , in.hand = in.hand - sold
                   , volume = sold
                   , Situation = "Emergency"
                   , algoId = IB.Parms[["algoId"]]
            ) %>%
            select(ticker, DP.Method, MA.Type, Period, Situation, algoId, Action, 
                   orderId, order.ts, volume, in.hand, price)
    
  } else
  {
    z <- left_join(actions, x, by = c("ticker", "Action")) %>%
          na.omit() %>%
          group_by(ticker, Action) %>%
          mutate(cumvol = cumsum(volume),
                 shares = case_when(cumvol <= shares ~ volume,
                                    cumvol - shares < volume ~ cumvol - shares,
                                    TRUE ~ 0)
                 ) %>%
          left_join(y, by = c("ticker", "DP.Method", "MA.Type", "Period")) %>%
          mutate(in.hand = ifelse(is.na(in.hand), 0, in.hand)
                 , in.hand = in.hand + shares
                 , Situation = "Normal"
                 , algoId = IB.Parms[["algoId"]]
          ) %>%
          select(ticker, DP.Method, MA.Type, Period, Situation, algoId, Action, 
                 orderId, order.ts, volume, in.hand, price)
  }
  
  z <- bind_rows(z, IB.activity) %>%
        group_by(ticker, DP.Method, MA.Type, Period, orderId) %>%
        filter(in.hand == min(in.hand)) %>% 
        ungroup() %>% arrange(desc(order.ts)) %>%
        distinct()

  assign("IB.activity", z, envir = .GlobalEnv)
  assign("Last.Order.Time", Sys.time(), envir = .GlobalEnv)
  
  rm(x, y, z)
  rm(twsExecution, req.Exec.2, req.Exec.df, print.twsExecution, envir = .GlobalEnv)
}
# -------------------------------------------------------------------------
Update.Targets <- function()
{
  if(Emergency) {return(cat("\n\nSystem halted due to Emergency"))}
  
  x <- left_join(targets,
                  IB.activity %>% mutate(ds = as.Date(order.ts)) %>%
                   select(ticker, DP.Method, MA.Type, Period, ds, Action),
                  by = c("ticker", "DP.Method", "MA.Type", "Period", "ds")) %>%
        mutate(buy.price  = ifelse(!is.na(Action) & Action == "BUY", NA, buy.price),
               sell.price = ifelse(!is.na(Action) & Action == "SELL", NA, sell.price),
               last.sell  = ifelse(!is.na(Action) & Action == "EOD SELL", NA, last.sell),
              ) %>%
        select(-Action)

  assign("targets", x, envir = .GlobalEnv)
  rm(x)
}
# -------------------------------------------------------------------------
Update.Orderbook <- function(Contract, Order, order.type = "")
{
  IB.orderbook <- data.frame(t(do.call(rbind, Contract)), stringsAsFactors = FALSE) %>%
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
                  bind_rows(IB.orderbook)
  
  assign("IB.orderbook",  IB.orderbook, envir = .GlobalEnv)
  rm(IB.orderbook, order.type)
}
# -------------------------------------------------------------------------
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
  
  assign("IB.open.orders", open, envir = .GlobalEnv)
  
  if(return.df == TRUE) 
    {return(open)} else
    {rm(open)}
}
# -------------------------------------------------------------------------
IB.Cancel.Orders <- function()
{
  IB.open.orders <- Get.Open.Orders(return.df = TRUE)

  if(exists("IB.open.orders", envir = .GlobalEnv) & nrow(IB.open.orders) > 0)
  {
    IB.orderbook <- IB.open.orders %>% select(-IB.Version) %>% distinct() %>% 
                    mutate(order.ts = Sys.time(), order.type = "Order Cancelled") %>%
                    bind_rows(IB.orderbook)

    assign("IB.orderbook",  IB.orderbook, envir = .GlobalEnv)
    
    for(i in 1:nrow(IB.open.orders))
    {cancelOrder(twsconn = tws, orderId = IB.open.orders$orderId[i])}
    rm(i)
  }
}
# -------------------------------------------------------------------------
IB.Close.Positions <- function()
{
  IB.Cancel.Orders()
  IB.Account.Status()

  x <- IB.contracts %>%
        filter(position > 0) %>%
        rename(ticker = symbol, volume = position) %>%
        select(accountName, conId, ticker, local, volume, primary, currency) %>%
        mutate(primary = ifelse(primary == "NASDAQ", "ISLAND", primary))

    if(nrow(x) > 0)
    {
      assign("Last.Order.Time", Sys.time(), envir = .GlobalEnv)
      for(i in 1:nrow(x))
      {
        Contract <- twsEquity(symbol = x$ticker[i]
                             , primary = x$primary[i]
                             , currency = x$currency[i]
                             , exch = "SMART")
        
        Order <- twsOrder(orderId = reqIds(tws)
                         , action = "SELL"
                         , clientId = tws$clientId
                         , account = IB.Parms[["acctCode"]]
                         , totalQuantity = x$volume[i]
                         , orderType = "MKT"
                         , tif = "DAY")
        
        Update.Orderbook(Contract, Order, order.type = "Order Cancelled")
        
        IBrokers::placeOrder(twsconn = tws, Contract, Order)
        rm(Contract, Order, i)
      }
    } else
    {
      rm(x)
      return(cat("\nAccount", IB.Parms[["acctCode"]],  "holds no positions. Verify on Workstation."))
    }

  rm(x)
}
# -------------------------------------------------------------------------
IB.FinishDay <- function(Force.Close = FALSE)
{
  NY.Time <- as.numeric(strftime(format(Sys.time(), tz = "US/Eastern"), format = "%H.%M"))
  if(!(NY.Time > 16.00 | Force.Close)) 
  {return(cat("\nWarning!!! Market is Still Open. \nCannot Shutdown the System ... "))}
  rm(NY.Time)

  IB.Cancel.Orders()
  IB.activity <- bind_rows(readRDS(paste0(data.folder, "IB/IB.Activity.rds")), IB.activity) %>% distinct()
  saveRDS(IB.activity, paste0(data.folder, "IB/IB.Activity.rds"))
  saveRDS(IB.orderbook, paste0(data.folder, "IB/IB.Orderbook.rds"))
  
  rm(list = setdiff(ls(envir = .GlobalEnv), c("tws")), envir = .GlobalEnv)
  
  unlink(paste0(data.folder, "IB/Prod.Clean.Data.rds"))
  unlink(paste0(data.folder, "IB/Prod.Forecasts.rds"))
  unlink(paste0(data.folder, "IB/Prod.Summary.rds"))
  
}
# -------------------------------------------------------------------------
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
    assign("System.Live", FALSE, envir = .GlobalEnv)
    cat(paste("\nNYSE & NASDAQ are closed today. Markets will reopen on", Next.Day, "... \n"))
    rm(NY.Time, Next.Day, Trade.Days)
  } else if(NY.Time < 9.3)
  {
    assign("System.Live", FALSE, envir = .GlobalEnv)
    cat("\nMarkets are closed now. Will reopen in", Time.Difference(to = 9.3, fr = NY.Time), "... \n")
    rm(NY.Time, Next.Day, Trade.Days, Time.Difference)
    
  } else if(NY.Time < IB.Parms[["Start.Trading.At"]])
  {
    assign("System.Live", TRUE, envir = .GlobalEnv)
    cat("\nMarket is Open. The System has been set to SELL ONLY for another", 
        Time.Difference(to = IB.Parms[["Start.Trading.At"]], fr = NY.Time), "... \n")
    rm(NY.Time, Next.Day, Trade.Days, Time.Difference)
    
  } else if(NY.Time >= IB.Parms[["Stop.Trading.At"]])
  {
    assign("System.Live", FALSE, envir = .GlobalEnv)
    cat(paste("\nMarkets are closed for the day. They will reopen on", Next.Day), "... \n")
    rm(NY.Time, Next.Day, Trade.Days, Time.Difference)
    
  } else if(Emergency == TRUE)
  {
    assign("System.Live", FALSE, envir = .GlobalEnv)
    cat("\nSystem halted due to Emergency", "... \n")
    rm(NY.Time, Next.Day, Trade.Days, Time.Difference)
    
  } else 
  {
    assign("System.Live", TRUE, envir = .GlobalEnv)
    rm(NY.Time, Next.Day, Trade.Days, Time.Difference)
    
  }
  
}
# -------------------------------------------------------------------------
Initiate.Emergency <- function()
{
  assign("Emergency", TRUE, envir = .GlobalEnv)
  repeat{
          IB.Close.Positions()
          cat("\nSystem will wait for 60 seconds to close all open positions ... \n")
          Sys.sleep(60)
          Get.Open.Orders()
          if(nrow(IB.open.orders) == 0) {break}
        }
  
  Update.Activity.Log()
  View(IB.activity)
  cat("\nThe Emergency Procedure was completed.\n")
  assign("Emergency", FALSE, envir = .GlobalEnv)
}
# -------------------------------------------------------------------------
Get.Action.Plots <- function(plot.folder = "F:/Project S/MA Linear Modelling/Prod.Plots/")
{
  # plot.folder = "F:/Project S/MA Linear Modelling/Prod.Plots/"
  
  library(plotly)
  library(htmlwidgets)
  
  if(nrow(actions) == 0) return(cat("\n"))

  prod.summary <- readRDS(paste0(data.folder, "IB/Prod.Summary.rds"))
  prod.charts <- readRDS(paste0(data.folder, "IB/Prod.Forecasts.rds")) %>%
                  inner_join(readRDS(paste0(data.folder, "IB/Prod.Clean.Data.rds")) %>% 
                               select(ds, ticker, close)
                             , by = c("ds", "ticker")) %>%
                  semi_join(actions, by = c("ticker", "DP.Method", "MA.Type", "Period")) %>%
                  mutate('Model Type' = paste(DP.Method, MA.Type, Period)
                         , A.Score = ifelse(!is.na(buy.price), Score, NA)
                         , P.Score = ifelse(is.na(buy.price), Score, NA)
                         , T.Score = ifelse(!is.na(A.Score*lead(P.Score)) |
                                              !is.na(lag(A.Score)*P.Score) |
                                              !is.na(P.Score*lead(A.Score)) |
                                              !is.na(lag(P.Score)*A.Score)
                                            , Score, NA)
                         )%>%
                  select(ds, ticker, 'Model Type', Score, A.Score, P.Score, T.Score, 
                         close, buy.price, sell.price)


  for(i in 1:nrow(actions))
  {
    # i = 1
    a.summary <- actions[i,] %>%
                  inner_join(prod.summary, by = c("ticker", "DP.Method", "MA.Type", "Period")) %>%
                  mutate(Model.ID = paste(DP.Method, MA.Type, Period)) %>%
                  select(ticker, Action, Model.ID, NY.Time, Overfit.Index,
                         volume, t.price, buy.price, sell.price, last.sell,
                         Prod.N, Duration, ROI, ROR) %>%
                  rename('Model Type' = Model.ID
                         , 'NY Time' = NY.Time
                         , 'Overfit Index' = Overfit.Index
                         , 'Volume' = volume
                         , 'Current Price' = t.price
                         , 'Buy Price' = buy.price
                         , 'Sell Price' = sell.price
                         , 'EOD Sell Price' = last.sell
                         , 'Past Transactions' = Prod.N
                         )

    df <- semi_join(prod.charts, a.summary, by = c('ticker', 'Model Type'))

    if(max(df$close, na.rm = TRUE) > 20)
    {
      min.y <- 10*floor(min(df$close/10, na.rm = TRUE)/1.25) # 1.25 to accomodate buy/sell prices
      max.y <- 10*ceiling(max(df$close/10, na.rm = TRUE)*1.25)
    } else
    {
      min.y <- floor(min(df$close, na.rm = TRUE)/1.25)
      max.y <- ceiling(max(df$close, na.rm = TRUE)*1.25)
    }

    t1 <- paste0(a.summary$ticker, " | ",
                 a.summary$`Model Type`, " | ",
                 "Overfit Index: ", 100*round(a.summary$`Overfit Index`, 2), "%")
    t2 <- paste0(a.summary$Action, " ", a.summary$Volume, " Units @ $", a.summary$`Current Price`)
    t3 <- paste0("History: ", a.summary$`Past Transactions`,
                 " for ", a.summary$Duration, " Days | ",
                 "ROI: ", a.summary$ROI, " | ",
                 "ROR: ", a.summary$ROR)
    t4 <- paste0("Model Limits: ",
                 "Buy @ $", a.summary$`Buy Price`, " | ",
                 "Sell @ $", a.summary$`Sell Price`,
                 ifelse(is.na(a.summary$`EOD Sell Price`), "", " | Sell by EOD") )

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
                          font = list(size = 14, color = '#004080'), showarrow = F
                          ) %>%
          add_annotations(text = t2, xref = 'paper', yref = 'paper', x = 0, y = 1.04,
                          font = list(size = 14, color = '#004080'), showarrow = F
                          ) %>%
          add_annotations(text = t3, xref = 'paper', yref = 'paper', x = 1, y = 1.03,
                          font = list(size = 12), showarrow = F
                          ) %>%
          add_annotations(text = t4, xref = 'paper', yref = 'paper', x = 1, y = 1.06,
                          font = list(size = 12), showarrow = F
                          ) %>%
          add_annotations(text = paste0("New York Time: ", a.summary$`NY Time`),
                          xref = 'paper', yref = 'paper', x = 1, y = 1.09,
                          font = list(size = 12), showarrow = F
                          ) %>%
          layout( font = list(size = 12), showlegend = FALSE, hovermode = 'compare',
                  margin = list(l = 70, r = 70, b = 50, t = 50, pad = 10),
                  xaxis = list(title = NA, showgrid = FALSE),
                  yaxis = list(title = "Close ($)", color = '#a6a6a6',
                               range = c(min.y, max.y), showgrid = FALSE),
                  yaxis2 = list(title = "Score", color = '#3f6ea6', tickformat = ".1%",
                                overlaying = "y", side = "right", showgrid = TRUE) )


    fname <- paste0(a.summary$ticker, " ", a.summary$`Model Type`, ".html")

    saveWidget(as_widget(p), title = t2
               , libdir = paste0(plot.folder, "libdir/"), file = paste0(plot.folder, fname))
    browseURL(paste0(plot.folder, fname))

    rm(max.y, min.y, t1, t2, t3, t4, fname, p, i, a.summary, df)

  }

  rm(prod.summary, prod.charts, plot.folder)
}



# -------------------------------------------------------------------------
# To Be Completed Later
# Get.PNL <- function()
# {
#   IB.pnl <- x %>% filter(Action %in% c("SELL", "EOD SELL")) %>% rename(symbol = ticker) %>%
#     inner_join(IB.contracts %>% 
#                  select(accountName, conId, symbol, averageCost)
#                , by = c("conId", "symbol")) %>%
#     rename(volume = shares, sell.price = price, buy.price = averageCost) %>%
#     mutate(ROI = sell.price/buy.price, sell.ds = Sys.Date()) %>%
#     select(accountName, conId, symbol, volume, buy.price, sell.price, sell.ds) %>%
#     bind_rows(IB.pnl) %>%
#     distinct()
#   
#   assign("IB.pnl", IB.pnl, envir = .GlobalEnv)
#   
# }