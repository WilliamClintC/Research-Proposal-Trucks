# Install and load required packages
install.packages(c("eventstudies", "quantmod", "zoo"))
library(eventstudies)
library(quantmod)
library(zoo)

# 1. Get stock and market return data
symbols <- c("AAPL", "^GSPC")  # Example: Apple stock and S&P 500
getSymbols(symbols, from="2022-01-01", to="2023-01-01")

# 2. Calculate returns
returns <- merge(dailyReturn(AAPL), dailyReturn(GSPC))
colnames(returns) <- c("stock", "market")

# 3. Define event dates
events <- as.Date(c("2022-06-15"))  # Example event date

# 4. Run the event study
es_result <- eventstudy(returns, 
                        event.dates = events,
                        type = "marketModel",
                        window = c(-10, +10),  # Event window: 10 days before/after
                        est.win = c(-100, -11)) # Estimation window

# 5. Plot results and perform statistical tests
plot(es_result)
summary(es_result)