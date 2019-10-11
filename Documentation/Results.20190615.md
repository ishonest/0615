### Algo ID 20190615
#### Features
- The algorithm buys in holding period. [Incosistent w/ trading]
- The algorithm buys multiple times in an active sequence. [Incosistent w/ trading]
- The algorithm picks and drops at 25 percentile & 75 percentile respectively
- The simulation contained best possible returns. 

#### Type 8 Validation
- Annual BRW ROI Range of [17 | 11 | 7] for End of period Overfit Index [0.2, 0.3]; 5 trades; and "Last was Success" for 85% target ROI.

***
### Algo ID 20190716
#### Features
- Same as 20190615 plus,
- The simulation contains best, worst and random senarios

#### Type 8 Validation
- Annual Best-Random-Worst (BRW) ROI Range of [16.7 | 11.1 | 7.1] for Overfit Index [0.2, 0.3]; 5 trades; and "1+ Success" / "50% Success Rate" / "Last was Success" for 85% target ROI. 
- Annual BRW ROI Range of [16.3 | 10.5 | 6.8] Similar performance for 80% target ROI. [Similar performance]

***
### Algo ID 20190724
#### Features
- Same as 20190716 plus,
- No buying in holding period
- The algorithm picks and drops at 10 percentile & 90 percentile respectively (instead of 25, 75)

#### Type 8 Validation
- Holding Period is high, because buying @ 10 percentile and selling @ 90 percentile  automatically leads to higher holding period
- High strike rate (SR02), up to 95%
- The best results have few trades (20 - 30) between Feb % July
- Annual BRW ROI of [4.8 | 2.6 | 2.1] for Overfit Index of [0.2, 0.3] & 5 Trades. (No condition on last performance)

***
### Algo ID 20190725
#### Features
- Same as 20190724 plus,
- The algorithm picks and drops at 25 percentile & 75 percentile respectively (instead of 10, 90)

#### Type 8 Validation
- Several sweetspots (See report).
- Most Greedy: Annual BRW ROI Range of [9.2 | 3.3 | 1.2] for Overfit Index of [0.2, 0.4], 5 Trades and last ROI was 1+
- Most Practical: Annual BRW ROI of [5.2 | 4.1 | 2.9] for Overfit Index of [0.0, 0.1] & 20 Trades. (No condition on last performance)

***
### Algo ID 20190726
#### Features
- Same as 20190725 plus,
- The algorithm optimizes picks and drops (instead of 25, 75)

#### Type 8 Validation
- At least 1 Dev Trades: Annual BRW ROI of [5.2 | 4.1 | 2.9] for Overfit Index of [0.0, 0.1] & 20 Trades. (No condition on last performance)
- At least 2 Dev Trades: Annual BRW ROI of [4.8	| 4.5	| 1.9] for Overfit Index of [0.2, 1.0] & 5 Trades. (Last ROI was 110%+)
- Performance degrades minimum #last trades increased (checked 3+ trades)

***
### Algo ID 20190801
#### Features
- Same as 20190726 plus,
- Stop loss of 5%
- New Program on the back of 20190615.Dev.Model.Scores.R 
- No filteration of Dev Period (except for 1 incidence)

#### Type 8 Validation
- Terrible Results: Annual BRW ROI of [1.6	| 1.6	| 1.6] for Overfit Index of [0.5, 0.6] & 5 Trades. (Last ROI was 105%+)

***
