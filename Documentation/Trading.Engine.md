### TRADING ENGINE RULES

#### Active Hours
- Execute on a trading day only
- Buy/Sell between 930 hours and 1600 hours
- EOD Sell between 1555 hours and 1600 hours (Can be changed)
  
#### Timing
- Stop for 20 seconds after cancelling past orders 
- Stop for 4 minutes after placing new orders
    + Why 4 minutes: Last Sell is executed 5 minutes before closing
- [To be executed] If EOD Sell is not executed, then sell at BOD

#### Buying Restrictions
- Maximum Inventory 
    + Max investment in a ticker = invest.max.T (User Defined)
    + Max volume a ticker can be buy = 1% of Yesterday's Volume         << Change this to hold
    + Max inventory (Max.in.hand) = Minimum of the conditions above
  
- Can Buy
    + Minimum: Models must buy at least for invest.min.M (ensures avoidance of high fees)
    + Maximum: (Max In Hand - Actual in Hand) or 0 (if -ve)
        + Must be within Permissible Model investment [invest.min.M, invest.max.M]
- Multiple Buys 
    + By a Model: A model can only buy upto permissible limit (invest.max.M) [See Get.Action function]
    + By a Stock: If N models call for buy at the same time, invest.max.T/invest.max.M models will be executed.

#### Other Features
- Emergency Exit: Sell Every position & Do not cancel orders
- IB.activity table stores the local timezone
