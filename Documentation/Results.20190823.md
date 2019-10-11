### For Model IDs 20190823
### DEPENDENCIES 
- Platform    	: doSNOW, foreach, IBrokers
- Data Pull   	: BatchGetSymbols, TTR, timeDate
- Processing  	: dplyr, zoo, lubridate, tidyr, data.table
- Modelling   	: MASS
- Testing     	: profvis
- Charting    	: plotly, htmlwidgets (Optional), xlsx

***
### DATA PROCESSING
#### ANALYSIS UNIVERSE: LIST OF TICKERS
- Source      	: TTR package
- Exclutions  	: EFTs, Funds, Trusts, Proshares

#### RAW DATA 
- Source      	: Yahoo Finance (BatchGetSymbols)
- Type        	: OHLC, volume and adjusted price
- Window      	: Jan 01, 2015 onwards

#### DATA CLEANING
- Consistency: Trends for last 250 days (why 250? ~100 Days of Production + 150 Days of Development)
    + No Extreme Jumps, i.e. Day-to-Day change in closing prices between [0.1X, 10.0X]
    + Non-NA / Zero Volume
    + Minimum Daily Trading of $100k. Since, each model buys at most 1% of yesterday's trade, limiting to lower number gets more trading history of medium sized tickers and more small sized tickers

#### MODEL DATA 

##### Moving Averages
    Type          : SMA, EMA, EMAW, ZLEMA, HMA 
    Periods (N)   : {10, 20, 30, ... , 180} (why 180? models beyond 180 have high variance performance)
    Data          : lag(open, N)/last, lag(open, N)/last, lag(open, N)/last, lag(open, N)/last 
    Combinations  : 2 sets of models: LN (Only logs of MAs) & MALN (MAs + logs of MAs)
        Exclusions: If any variables have negative MAs

##### ROIs
    5.Day.Close (ROI.c) : (Adjusted Close in Day 5)/(Yesterday's Adjusted close) - Used for Modeling
    5.Day.High (ROI.h)  : (Highest in Next 5 Days)/(Yesterday's close) - Used for Model Selection
    5.Day.Low (ROI.l)   : -(Lowest in Next 5 Days)/(Yesterday's close) - Used for Model Selection
    
> Dependent Factor: if 5.Day.Close > 1 then 1 else 0
  
***
### MODELLING PROCESS
#### METHOD
- Binomial / Logistic Regression 
- With Intercept
- Stepwise (both sides) for variable selection
- Development Period: 01/01/2015 - 31/01/2019
- Validation Period : 01/02/2019 - now
  
#### MODEL SELECTION
    Valid Model               : Model is not NULL
    Valid Independent Factors : More than intercept as an independent variable
    Bin Model Selection       : Split the development scores in 20 bins [Non-zero scores]. Bins represent similar state of trading. Select bins with 90% success to close at high in development & validation samples, i.e., close @ end of day 5 is higher than close @ yesterday.
    Source Code               : Master.Dev.Models.R

#### TRADING PRICE SELECTION

##### Algo ID 20190823
- For long (Same as 20190821)
    + Buy @ Nth percentile of low
    + Sell @ minimum of high
    + Stop Loss @ minimum of low
- For short 
    + Buy @ Nth percentile of high
    + Sell @ maximum of low
    + Stop Loss @ maximum of high
- Optimise Percentile Pick provided ROI >= 20%, ROR >= 4, Stop Loss Range: [1%, 5%]
- Stop loss is a range between 1% & 5% [In algo 20190821, it was a ratio w.r.t last close]
- Prorated stop loss: 
    + Long : @ 95% - 99% of stop loss from day 1-5. 100% from day 6
    + Short: @ 105% - 101% of stop loss from day 1-5. 100% from day 6
- Pick the most risky price to pick more stocks (Anti-optimization)
- Selection Criterion: At least 1 trade in development


***
### VALIDATION 
- Type 9: Best/Random/Worst case projections for a different # Trades. Buy restrictions* are imposed
*Buying Restriction Imposed: Can buy for upto 3 models/ticker/day*

***
## Performance

### Last Best Model: Algo ID 20190821
- Least Risky BRW Range [34.5 - 28.3 - 17.8] for 10 trades; Miss 22%-31%; SR 86%; Hold ~5 Days
- Most Profitable BRW Range [99.6 - 64.2 - 10.4] for 5 trades; Miss 30%-60%; SR 87%; Hold ~4.9 Days
- Convergence of BRW (if any): 5.5X @ 40 trades
- Concerns: Trading capacity of ~$1 Million @ 1%

### Algo ID 20190823
- Least Risky BRW Range [95.3 - 72.5 - 30.0] for 9 trades; Miss 26%-47%; SR 97%; Hold ~5.7 Days
- Most Profitable BRW Range [246.4 - 149.0 - 22.9] for 5 trades; Miss 33%-66%; SR 96%; Hold ~5.9 Days
- Convergence of BRW (if any): 12X @ 35 trades
- Concerns: Trading capacity of ~$1 Million @ 1%

***
### Check Later 
- Available funds are typically 10X of cash deposits. Check this in production
- Order within fund limits: Check if we need the cummin function [See Get.Action function]
