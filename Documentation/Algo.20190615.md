### For Model IDs 20190615, 20190716, 20190724, 20190725
### DEPENDENCIES 
- Platform    	: doSNOW, foreach, IBrokers
- Data Pull   	: BatchGetSymbols, TTR, timeDate
- Processing  	: dplyr, zoo, lubridate, tidyr, data.table
- Modelling   	: MASS
- Testing     	: profvis
- Charting    	: plotly, htmlwidgets, xlsx

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
    + Minimum Daily Trading of $100k. Changed this from $1MM (20190724 onwards), because each model buys at most 1% of yesterday's trade). This enables a longer window data for hundreds of tickers.

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
  
###### Source Code
    Analysis Universe : ./20190615.Dev.R
    Data Pull         : ./20190615.Dev.R & ./20190615.Prod.R
    Data Cleaning     : ./Functions/20190615.Functions.R
    ROI Calculations  : ./Functions/20190615.Functions.R
    Moving Averages   : ./Functions/F.Data.Transformation.R
    Next Trading Date : ./Functions/F.Trading.Days.R

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
    Profitable Range Trading  : Split the development scores in 10 bins [Non-zero scores]. 
                                For each bin, calculate:
                                  Probable Drop (H.ROI.l) = 10th percentile of 5 Day Low (ROI.l)
                                  Probable Peak (H.ROI.h) = 90th percentile of 5 Day High (ROI.h)
                                  Opportunity = Probable Peak - Probable Drop
                                  Worst Case Opportunity = Rolling min. of Opportunity [Sort bins (desc)] 
                                Get the bins with minimum Worst Case Opportunity of 20%
                                
                                Simulate trading based on these ranges 
                                  Rules       : In Source Code File
                                  Source Code : ./Functions/F.Trading.Simulation.R )
                                  Simulation also provides buy/sell prices for production
                                
                                Conditions to select a model (based on simulation in development period)
                                  No. of Transactions : [3, 10] (why 10? Too many opportunities are inconsistent)
                                  Minimum Average ROI : 10%
                                  Minimum Average ROR : 1% (ROR = ROI/holding days)
  
                                Conditions based on simulation in validation period
                                  Minimum Transaction : 1 [Only for 20190615, because Type 6 simulation doesn't account this]
    
    Validation Factors        : Overfit Index. Explained in next section

***
### VALIDATION 

#### TYPES TESTED
- Type 1: Invest on every model equally
- Type 2: Invest on the first trades from a ticker of week equally
- Type 3: Invest in all trades in a week until 10 trades
- Type 4: Invest with a fixed amount in bulks, i.e. if bank = $10k and N = 5; Invest in lots of $2k
- Type 5: Invest with a on.hand / N (min 100). Check ROI (since Feb);
- Type 6: Optimize overfit index for Type 4 investment*
- Type 7: Type 6* with exact ranges and maxtrades (Realistic view of Type 6)
- Type 8: Best/Random/Worst case projections for a combination of Past Performance, Overfit Index, Trades (TBC). Buy restrictions* are imposed

*Buying Restriction Imposed: Can buy for upto 3 models/ticker/day*

> Type 8 Validation is used in production

***
### Check Later 
- Available funds are typically 10X of cash deposits. Check this in production
- Order within fund limits: Check if we need the cummin function [See Get.Action function]


