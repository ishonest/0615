source("./20190802.Prod.Models.R", echo = TRUE)
# -------------------------------------------------------------------------
system("C:/Jts/tws.exe", wait = FALSE)
source("./Functions/20190802.Trading.IB.R")

# -------------------------------------------------------------------------
# Global Parameter Calls
# -------------------------------------------------------------------------
IB.Parms <- list(  last.dev.date    = as.Date("2019-01-31")
                 , data.folder      = "F:/Project S/MA Linear Modelling/"
                 
                 , clientId         = 100           # Must be a positive integer
                 , acctCode         = "DU1534576"   # IB Account Number
                   
                 , invest.max       = 50000         # Maximum position
                 , Start.Trading.At = 9.35          # NY Time to start buying
                 , Stop.Trading.At  = 16            # NY Time to stop trading
                 , Last.Sell.At     = 15.55         # NY Time to initiate EOD Sell
                   
                 , Emergency        = FALSE
                 , System.Live      = FALSE
                 , Last.Order.Time  = Sys.time()
                 )

# -------------------------------------------------------------------------
# Trading Process
# -------------------------------------------------------------------------
IB.StartDay()             # Creates connection, account stats, missed order management

while(lubridate::hour(format(Sys.time(), tz = "US/Eastern")) < IB.Parms[["Stop.Trading.At"]])
{
  View(IB.01.targets)
  IB.System.Status()
  if(isTRUE(IB.Parms[["System.Live"]]))
  {
    cat("\n----------------------------------------------------------\n")
    IB.Cancel.Orders()          # Lists open orders & Cancels them
    Sys.sleep(20)
    Update.Activity.Log()       # Updates Activity Log
    View(IB.04.activity)

    IB.Account.Status()         # For Available Funds Funds
    Update.Targets()            # Updates targets Based on Last Activity: BP/SP becomes NA
    Get.Actions()               # Updates actions Based on Activity during the day
    View(IB.02.actions)
    
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

