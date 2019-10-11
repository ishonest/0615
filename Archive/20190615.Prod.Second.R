# Add an error catcher in Get.Portfolio.Tickers

source("./20190615.Prod.First.R", echo = TRUE)
system("C:/Jts/tws.exe", wait = FALSE)

source("./Functions/F.Trading.IB.R")

# -------------------------------------------------------------------------
# Global Parameter Calls
# -------------------------------------------------------------------------
IB.Parms <- list(algoId             = "20190716"  # Algo ID (Internal)
                 , acctCode         = "DU1534576" # IB Account Number
                 , clientId         = 100         # Must be a positive integer
                 
                 , invest.max       = 10000       # Maximum position
                 , invest.max.M     = 2000        # Maximum investment in a model
                 , invest.min.M     = 500        # Minimum investment in a model
                 , invest.max.T     = 6000        # Maximum investment in a ticker
                 
                 , max.vol.ratio    = 0.01        # Max Percentage of yesterday's volume any model can buy
                 , Start.Trading.At = 9.3        # NY Time to start buying
                 , Stop.Trading.At  = 16          # NY Time to stop trading
                 , Last.Sell.At     = 15.55       # NY Time to initiate EOD Sell
                 )

# -------------------------------------------------------------------------
IB.StartDay()             # Creates connection, account stats, first targets

while(lubridate::hour(format(Sys.time(), tz = "US/Eastern")) < IB.Parms[["Stop.Trading.At"]])
{
  View(targets)
  IB.System.Status()
  if(isTRUE(System.Live))
  {
    cat("\n----------------------------------------------------------\n")
    IB.Cancel.Orders()          # Lists open orders & Cancels them
    Sys.sleep(20)
    Update.Activity.Log()       # Updates Activity Log
    View(IB.activity)

    IB.Account.Status()         # For Available Funds Fuds
    Update.Targets()            # Updates targets Based on Last Activity: BP/SP becomes NA
    Get.Actions()               # Updates actions Based on Activity during the day
    View(actions)
    
    IB.Place.Orders()           # Place eligible orders
    Get.Action.Plots()
  }  
  
  cat("\nCycle Complete: Next cycle will commence at", format(Sys.time() + 240, "%X"), "...\n")
  Sys.sleep(240)
  
}

# -------------------------------------------------------------------------
IB.FinishDay(Force.Close = TRUE)
twsDisconnect(tws)
rm(list = ls())

# -------------------------------------------------------------------------
# Extreme Functions
# -------------------------------------------------------------------------
# IB.Restart()            # Will clean all logs/records/restarts system.
# Initiate.Emergency()    # Run until all orders are closed


# -------------------------------------------------------------------------
# Write a loop if last sell is not executed, then execute at opening
