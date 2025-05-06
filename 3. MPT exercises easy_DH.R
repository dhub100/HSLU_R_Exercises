### Do it yourself! - MPT ###


# Loading required package
#install.packages("quantmod")
library(quantmod)
library(tidyverse)


## Slides S.61: Diversification effect

w1 <- .5
w2 <- .5
sd1 <- .06
sd2 <- .13
cor12 <- c(-1, 0, 1) # Different correlations: -1, 0, 1

# portfolio risk
p_risk <- sqrt(w1^2 * sd1^2 + w2^2 * sd2^2 + 2 * w1 * sd1 * w2 * sd2 * cor12)
p_risk


## Slides S.64: expected return of portfolio

w1 <- .5
w2 <- .5
er1 <- .03
sd1 <- .06
er2 <- .1
sd2 <- .13
cor12 <- -.5

# expected portfolio return
p_ret <- w1 * er1 + w2 * er2
p_ret

# portfolio risk
p_risk <- sqrt(w1^2 * sd1^2 + w2^2 * sd2^2 + 2 * w1 * sd1 * w2 * sd2 * cor12)
p_risk

# evaluation compared to share 1:
# portfolio is superior as it has clearly higher return and slightly lower risk

## Slides S.69: Sharpe Ratio

# The Sharpe ratio shows how much excess return a portfolio generates per unit of risk taken
risk_free_rate <- 0
sr1 <- (p_ret - risk_free_rate) / p_risk
sr1



### EXERCISE

# Downloading Prices via Yahoo Finance API
data <- NULL
tickers_index <- c("^GSPC", "AGG") # Tickers from Yahoo Finance for S&P 500 (^GSPC) and US Aggregate Bond Index (AGG)

for (Ticker in tickers_index){
  data <- cbind(data,
        getSymbols.yahoo(Ticker, from="2010-01-01", periodicity = "weekly", auto.assign=FALSE)[,6])
}

colnames(data) <- c("Stocks", "Bonds")
head(data)
tail(data)

# Calculating continuous returns
log_Returns <- diff(log(data))[-1,] # data is a xts object; data column treated differently (i.e. diff-log not applied on date)
head(log_Returns)

# Defining risk free rate
risk_free_rate <- 0


## a) Calculating the Sharpe-ratios for bonds and stocks

# I) Expected returns (see slide 57)
er_bonds <- mean(log_Returns$Bonds) # simple arithmetic mean as we have log returns
er_stocks <- mean(log_Returns$Stocks)

# II) Standard deviations (see slide 57)
sd_bonds <- sd(log_Returns$Bonds)
sd_stocks <- sd(log_Returns$Stocks)

# III) Sharpe-ratios for bonds and stocks
# Apply formula on slide 69, use the predefined objects er_bonds, er_stocks, sd_bonds, sd_stocks and risk_free_rate
sr_bonds <- (er_bonds - risk_free_rate)/sd_bonds
sr_stocks <- (er_stocks - risk_free_rate)/sd_stocks


## b) Plotting efficient frontier

# Creating 1000 portfolio weights and calculating the correlation between stocks and bonds returns
x_weights <- seq(from = 0, to = 1, length.out = 1000)
cor_bs <- cor(log_Returns$Stocks, log_Returns$Bonds)
cor_bs

# Creating a data.frame that contains the weights for the two asset and empty columns for the portfolio return, standard deviation and Sharpe-rations
pf <- data.frame(w_bonds = x_weights, w_stocks = 1 - x_weights, er_p=NA, sd_p=NA, sr_p=NA)
head(pf)

# Calculating the expected returns and standard deviations for the 1000 portfolios
for(i in 1:nrow(pf)){
  pf$er_p[i] <- pf$w_bonds[i] * er_bonds + pf$w_stocks[i] * er_stocks # Formula for calculating portfolio returns (see slide 58)
  pf$sd_p[i] <- sqrt(pf$w_bonds[i]^2 * sd_bonds^2 +        # Formula for calculating portfolio standard deviation (see slide 61)
                  pf$w_stocks[i]^2 * sd_stocks^2 + 
                    2 * pf$w_bonds[i] * sd_bonds * pf$w_stocks[i] * sd_stocks* cor_bs)
}
head(pf)

# Plotting the efficient frontier
plot(x=pf$sd_p, y=pf$er_p, xlab="SD Portfolio", ylab="Expected Return Portfolio") +
  grid()

ggplot(pf) +
  geom_point(aes(x = sd_p, y = er_p), shape = 1, color = "darkgrey", size = .5, alpha = .5) +
  labs(x = "SD/Risk of Portfolio",
       y = "ER/Return of Portfolio",
       title ="Efficient Frontier") +
  theme_minimal()


## c) Deriving the weightings of the market portfolio, i.e. the one that maximizes the Sharpe-ratio

# Calculating the Sharpe-ratio per portfolio (see slide 69)
pf$sr_p <- (pf$er_p - risk_free_rate) / pf$sd_p
head(pf)

# Identifying the index of the market portfolio, i.e. the row number of the Sharpe-ratio-maximizing portfolio
indx_mp <- which.max(pf$sr_p)

# Identifying the weightings of the market portfolio
weightings <- cbind(pf$w_stocks[indx_mp], pf$w_bonds[indx_mp])
colnames(weightings) <- c("Stocks", "Bonds")
pie(weightings, labels = paste(round(weightings*100), "% ", colnames(weightings),sep = ""), main = "Asset allocation of market portfolio")

# ggplot
df <- pivot_longer(as.data.frame(weightings), cols = c(Stocks, Bonds), names_to = 'Asset')
df
ggplot(df, aes(x = "", y = value, fill = Asset)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(value * 100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 6) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  labs(title = "Asset Allocation of Market Portfolio") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = "right",
        legend.title = element_text(face = "bold"))


## d) Extracting the Sharpe-ratio of the market portfolio

sr_mp <- pf$sr_p[indx_mp]
cbind(sr_stocks, sr_bonds, sr_mp)
plot(x=pf$sd_p, y=pf$er_p, xlab="SD Portfolio", ylab="Expected Return Portfolio") +
  grid()
abline(a=risk_free_rate, b=sr_mp, lty=2, col="red")


marker <- pf %>% 
  slice(indx_mp) %>% 
  as_tibble()
marker

# ggplot
ggplot(pf) +
  geom_point(aes(x = sd_p, y = er_p), color = "darkgrey") +
  labs(x = "SD/Risk of Portfolio",
       y = "ER/Return of Portfolio",
       title ="Efficient Frontier and Market Portfolio") +
  geom_abline(
    slope = sr_mp,
    intercept = risk_free_rate,
    color = "red",
    linetype = "dashed",  # Optional: "solid", "dotted", etc.
    linewidth = 1) +
  geom_point(data = marker, aes(x = sd_p, y = er_p),
    color = "black",
    size = 5,
    shape = 17  # Triangle (use 16 for circle)
  ) +
  theme_minimal()


###############################################################

### Alternative proceeding using the package fPortfolio
## Package loading, data import
# install.packages("fPortfolio")
# install.packages("quantmod")
library(fPortfolio)
library(quantmod)

# Downloading Prices via Yahoo Finance API
data <- NULL
tickers_index <- c("^GSPC", "AGG") # Tickers from Yahoo Finance for S&P 500 (^GSPC) and US Aggregate Bond Index (AGG)

for (Ticker in tickers_index){
  data <- cbind(data,
                getSymbols.yahoo(Ticker, from="2010-01-01", periodicity = "weekly", auto.assign=FALSE)[,6])
}
colnames(data) <- c("Stocks", "Bonds")

# Calculating log-returns and definition of risk-free rate
log_Returns <- diff(log(data))[-1,]
risk_free_rate <- 0

# Efficient Frontier
Spec<-portfolioSpec()                    # Creating variable for the subsequent portfolio optimization specification
setRiskFreeRate(Spec) <- risk_free_rate  # Defining risk-free rate
setTargetRisk(Spec) <- "Sigma"           # Defining target risk to be optimized, here Sigma = Standard deviation
setNFrontierPoints(Spec) <- 1000         # Defining number of efficient portfolios to be constructed

# Definition of constraints
constraint1 <- "maxW[] <= 1"   # Constraint that the maximum weighting of a single asset is equal or smaller than 100%
constraint2 <- "LongOnly"       # Constraint that investors can't take short positions
constraint3 <- "maxsumW[]=1"    # Constraint that the sum of all weightings needs to equal 100% 

# Calculating and plotting efficient frontier and tangency line
pf <- portfolioFrontier(as.timeSeries(log_Returns), spec = Spec, constraints = c(constraint1, constraint2, constraint3))
frontierPlot(pf, return = "mean", risk = "Sigma")
grid()
tangencyLines(pf, col="blue")

# Alternatively: Shortcut
tailoredFrontierPlot(pf, return="mean", risk="Sigma", sharpeRatio = FALSE, twoAssets = TRUE)

# Deriving tangency portfolio (TP) and corresponding weights of individual assets included
tp <- tangencyPortfolio(as.timeSeries(log_Returns), spec = Spec, constraints = c(constraint1, constraint2, constraint3))
weightsPie(tp)

# Sharpe-Ratio of tangency portfolio
SR_Tangency <- (getPortfolio(tp)$targetReturn[1] - risk_free_rate) / getPortfolio(tp)$targetRisk[1]
names(SR_Tangency) <- "Sharpe-Ratio TP"
SR_Tangency



###############################################################
### Alternative proceeding: In-sample optimization, out-of-sample performance measurement

## Package loading, data import
# install.packages("fPortfolio")
# install.packages("quantmod")
# install.packages("tidyverse")
# install.packages("lubridate")
library(fPortfolio)
library(quantmod)
library(tidyverse)
library(lubridate)

# Downloading Prices via Yahoo Finance API
data <- NULL
tickers_index <- c("^GSPC", "AGG") # Tickers from Yahoo Finance for S&P 500 (^GSPC) and US Aggregate Bond Index (AGG)

for (Ticker in tickers_index){
  data <- cbind(data,
                getSymbols.yahoo(Ticker, from="2010-01-01", periodicity = "weekly", auto.assign=FALSE)[,6])
}
colnames(data) <- c("Stocks", "Bonds")

# Calculating log-returns and definition of risk-free rate
log_Returns<-diff(log(data))[-1,]
risk_free_rate<-0

# Optimization set
data_train <- log_Returns %>% subset(year(index(.)) < 2022) # Training set (before 2022)
data_test <- log_Returns %>% subset(year(index(.)) >= 2022) # Test set (after 2022)

# Efficient Frontier
Spec<-portfolioSpec() # Creating variable for the following portfolio optimization specification
setRiskFreeRate(Spec)<-risk_free_rate # Defining risk-free rate
setTargetRisk(Spec)<-"Sigma" # Defining target risk to be optimized
setNFrontierPoints(Spec)<-1000  # Defining number of efficient portfolios to be constructed

# Definition of constraints
constraint1<-"maxW[]<=1" # Constraint that the maximum weighting of a single asset is equal or smaller than 100%
constraint2<-"LongOnly" # Constraint that investors can't take short positions
constraint3<-"maxsumW[]=1" # Constraint that the sum of all weightings needs to equal 100% 

# Estimation of tangency portfolio
tp<-tangencyPortfolio(as.timeSeries(data_train), spec=Spec, constraints=c(constraint1, constraint2, constraint3))
weights<-getWeights(tp)

# Calculation and plotting of out-of-sample performance (rebalancing every week)
data_test$ret_portfolio <- data_test%*%weights
plot(cumsum(data_test$ret_portfolio), main="Portfolio performance")
plot(cumsum(data_test$Stocks), main="Stocks performance")
plot(cumsum(data_test$Bonds), main="Bonds performance")

# Alternatively:
# install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
data_test <- log_Returns %>% subset(year(index(.)) >= 2022)
pf_months_rebalanced <- Return.portfolio(data_test, weights = weights, rebalance_on = "months", verbose=TRUE) # Weekly rebalancing
plot(cumsum(pf_months_rebalanced$returns), main="Portfolio performance, monthly rebalancing")

pf_not_rebalanced <- Return.portfolio(data_test, weights = weights, rebalance_on = NA) # No rebalancing
plot(cumsum(pf_not_rebalanced), main="Portfolio performance, no rebalancing")

# Plotting end-of-period weights before rebalancing
library(ggplot2)
library(reshape2)
library(xts)

# Extracting end-of-period weights before rebalancing
eop_weights <- pf_months_rebalanced$EOP.Weight

# Convert xts EOP weights to data frame
weights_df <- data.frame(Date = index(eop_weights), coredata(eop_weights))

# Reshape for ggplot
weights_long <- melt(weights_df, id.vars = "Date", variable.name = "Asset", value.name = "Weight")

# Plot stacked barplot
ggplot(weights_long, aes(x = Date, y = Weight, fill = Asset)) +
  geom_bar(stat = "identity") +
  labs(title = "End-of-Period Portfolio Weights",
       x = "Date",
       y = "Weight",
       fill = "Asset") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
