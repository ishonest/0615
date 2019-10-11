### For Model IDs 20190802, 20190808, 20190813, 20190821, 20190822
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

##### Algo ID 20190802
- Buy @ Nth percentile of low
- Sell @ minimum of high
- Stop Loss @ minimum of low
- Optimise Percentile Pick provided ROI >= 20%, ROR >= 4, Stop Loss Range: [1%, 5%]
- Pick the most risky price to pick more stocks (Anti-optimization)
- Selection Criterion: At least 1 trade in development

##### Algo ID 20190803
- Algo 20190802, plus
- Pick the least risky price
- No selection criterion

##### Algo ID 20190804
- Algo 20190802, plus
- No selection criterion

##### Algo ID 20190808
- Algo 20190802, plus
- Optimise Percentile Pick provided ROI >= 10%

##### Algo ID 20190813
- Buy @ maximum of low, with a minimum Sharpe Ratio of 3
- Sell @ minimum of high
- Stop Loss @ minimum of low
- Provided a minimum ROI >= 20%, Stop Loss Range: [1%, 5%]
- Selection Criterion: Pick candidate with maximum Cummulative ROI (in Development)

##### Algo ID 20190821
- Algo 20190802, plus
- Prorated stop loss: @ 95% - 100% of stop loss from day 1-5 of trend

##### Algo ID 20190822
- Algo 20190802, plus
- Prorated stop loss: @ 95% - 100% of stop loss from day 1-5 of trend
- Prorated sell price: @ 90% - 100% of sell price from day 1-5 of trend

***
### VALIDATION 
- Type 9: Best/Random/Worst case projections for a different # Trades. Buy restrictions* are imposed
*Buying Restriction Imposed: Can buy for upto 3 models/ticker/day*

***
## Performance

### Algo ID 20190802
- Least Risky BRW Range [27.5	- 23.1 - 15.7] for 10 trades; Miss 20%-30%; SR 70%; Hold ~4.5 Days
- Most Profitable BRW Range [80.1	- 54.4 - 8.6] for 5 trades; Miss 30%-60%; SR 60%; Hold ~4.5 Days
- Convergence of BRW (if any): 5.5X @ 35 trades
- Concerns: Trading capacity of ~$1 Million @ 1%

### Algo ID 20190803
- Steady drop in performance with increasing # trades
- BRW Range [19.1	- 17.6 - 15.4] for 10 trades; Miss 15%; SR 65%; Hold ~4.1 Days
- Convergence of BRW (if any): 6X @ 25 trades
- Concerns: Trading capacity of ~$0.75 Million @ 1%

### Algo ID 20190804
- Similar results to 20190802
- Least Risky BRW Range [28.8 - 22.8 - 15.6] for 10 trades; Miss 25%-30%; SR 73%; Hold ~4.5 Days
- Most Profitable BRW Range [85.4 - 53.0 - 8.6] for 5 trades; Miss 30%-55%; SR 70%; Hold ~4.4 Days
- Convergence of BRW (if any): 5.5X @ 35 trades
- Concerns: Trading capacity of ~$1-1.5 Million @ 1%

### Algo ID 20190808
- Least Risky BRW Range [18.5	- 14.1 - 8.0] for 35 trades; Miss 22%-33%; SR 70%; Hold ~4.3 Days; Capacity of ~$50-70 Million @ 1%
- Convergence of BRW (if any): 8X @ 40 trades

### Algo ID 20190813
- Least Risky BRW Range [14.1 - 11.2 - 8.3] for 30 trades; Miss 20%-36%; SR 70%; Hold ~4 Days; Capacity of ~40 Million @ 1%
- Convergence of BRW (if any): 9X @ 35 trades

### Algo ID 20190821 [BEST MODEL]
- Better than 20190802. 15%+ jump in SR
- Least Risky BRW Range [34.5 - 28.3 - 17.8] for 10 trades; Miss 22%-31%; SR 86%; Hold ~5 Days
- Most Profitable BRW Range [99.6 - 64.2 - 10.4] for 5 trades; Miss 30%-60%; SR 87%; Hold ~4.9 Days
- Convergence of BRW (if any): 5.5X @ 40 trades
- Concerns: Trading capacity of ~$1 Million @ 1%

### Algo ID 20190822
- Similar to 20190821, but slightly inferior performance
- Least Risky BRW Range [31.3 - 25.5 - 15.4] for 10 trades; Miss 21%-30%; SR 82-87%; Hold ~4.75 Days

