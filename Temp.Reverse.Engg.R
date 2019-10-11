x1 <- sum %>%
      filter(ROI.Dev >= 1.05, ROI.Val >= 1.01) %>%
      group_by(ticker) %>%
      summarise(ROI.XXX = prod(ROI.XXX^Trades.XXX, na.rm = TRUE)) %>%
      filter(ROI.XXX < 1)

x1 <- anti_join(sum, x1, by = "ticker")

x2 <- foreach(D = seq(0, 2, 0.01), .combine = bind_rows, .packages = "dplyr") %:%
      foreach(V = seq(0, 2, 0.01), .combine = bind_rows) %dopar%
      {
        x2 <- x1 %>%
          filter(ROI.Dev >= D, ROI.Val >= V) %>%
          group_by(Type) %>%
          summarise(D, V,
                    Mean = mean(ROI.XXX, na.rm = TRUE),
                    W.Mean = weighted.mean(ROI.XXX, w = Trades.XXX, na.rm = TRUE),
                    Trades = sum(Trades.XXX, na.rm = TRUE))
        rm(D, V)
        return(x2)
      }

# -------------------------------------------------------------------------
PerformanceAnalytics::chart.Correlation(
                        sum %>% ungroup() %>% 
                        select(-c(Type, ticker, ID, R, R.low, R.high, R.buy, R.sell, R.stop,
                                  Trades.XXX, M.XXX))
                        , histogram=TRUE, pch=19)
# -------------------------------------------------------------------------
library(rpart)
library(rpart.plot)
library(rattle)

x1 <- sum %>%
      group_by(ticker, Type) %>%
      summarise(Models = n(), ROI = weighted.mean(ROI.Val, Trades.Val, na.rm = TRUE)) %>%
      filter(ROI > 1) %>% select(-c(ROI, Models)) %>%
      inner_join(sum, by = c("ticker", "Type"))

x2 <- sum %>% filter(Trades.XXX > 0, !is.na(ROI.Dev), !is.na(ROI.Val)) %>%
      mutate(ROI.XXX = 100*ROI.XXX^Trades.XXX,
             M.All = M.Dev*M.Val,
             ROI.Dev =ifelse(is.na(ROI.Dev), 1, ROI.Dev),
             ROI.Val =ifelse(is.na(ROI.Val), 1, ROI.Val),
             ROI.All = ROI.Dev*ROI.Val,
             Trades.Dev = ifelse(is.na(Trades.Dev), 0, Trades.Dev),
             Trades.Val = ifelse(is.na(Trades.Val), 0, Trades.Val))

fit <- rpart(ROI.XXX ~ Trades.Dev + Trades.Val + ROI.Dev + ROI.Val + ROI.All+ 
                         M.Dev + M.Val + M.All + Shapiro.S + Shapiro.SL +
                         R.stop + R.ROR  + R.ROI
             ,
             data = x2 %>% filter(Type == "LONG"),
             method = "anova",
             control = rpart.control(minbucket = 28)
             )

fancyRpartPlot(fit)
# -------------------------------------------------------------------------

x <- sum %>% 
      filter(ROI.Dev >= 1.1)
  
summary(x$ROI.XXX)

# -------------------------------------------------------------------------

x <- sum %>% 
  mutate(ROI.Dev = ifelse(is.na(ROI.Dev), 0.80, 0.01*floor(100*ROI.Dev)),
         ROI.Val = ifelse(is.na(ROI.Val), 0.80, 0.01*floor(100*ROI.Val))
  ) %>%
  group_by(Type, ROI.Dev, ROI.Val) %>%
  summarise(ROI.XXX = weighted.mean(ROI.XXX, w = Trades.XXX, na.rm = TRUE),
            Trades = sum(Trades.XXX, na.rm = TRUE)) %>%
  filter(Trades >= 30) %>% ungroup()

ggplot(data = x %>% filter(Type == "SHRT") %>% select(ROI.Dev, ROI.Val, ROI.XXX), 
       aes(x = ROI.Dev, y = ROI.Val, fill= ROI.XXX)) + 
  geom_tile() +  
  scale_fill_gradientn(colours = c("white", "blue", "green"), values = c(0, 1.02, 1.05))

ggplot(data = x %>% filter(Type == "LONG") %>% select(ROI.Dev, ROI.Val, ROI.XXX), 
       aes(x = ROI.Dev, y = ROI.Val, fill= ROI.XXX)) + 
  geom_tile() +  
  scale_fill_gradientn(colours = c("white", "blue", "green"), values = c(0, 1.02, 1.05))
