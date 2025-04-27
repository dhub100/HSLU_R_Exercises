library(tidyverse)
library(quantmod)


### Do it yourself! - Value-at-Risk ###


## Downloading levels of SMI ("^SSMI") via Yahoo Finance API

# Adapted approach with map and with a tibble besides the xts object

tickers_index <- c("^SSMI")  # your tickers

# Map over tickers, pull adjusted prices, rename columns

SMI_xts <- tickers_index %>%
  map(~ {
    getSymbols.yahoo(.x, from = "2000-01-01", periodicity = "monthly", auto.assign = FALSE)[,6] %>%
      `colnames<-`(.x)  # rename the column to ticker name
  }) %>%
  reduce(merge)  # merge all xts objects into one

# Convert to tibble to have it ready for ggplots etc.
SMI_tbl <- SMI_xts %>%
  as_tibble(rownames = "date") %>% 
  mutate(date = as.Date(date)) %>% 
  rename(SSMI = "^SSMI")

# View the result
# print(SMI_tbl)
# plot(SMI_xts)

SMI_tbl %>% ggplot() +
  geom_line(aes(x = date, y = SSMI)) +
  labs(
    title = "SMI",
    subtitle = "From 2000 onwards",
    x = "Date",
    y = "SMI"
    ) +
  theme_minimal()

# -----------------------------------------------


## Calculating continuous returns ("stetige Renditen") using the diff-log-transformation

# SMI_returns <- SMI_xts %>% 
#   log() %>%
#   diff() %>% 
#   na.omit() %>%
#   magrittr::set_names("SMI Returns")

# plot(SMI_returns)

# Diff logs in the tibble

SMI_tbl <- SMI_tbl %>%
  mutate(SSMI_log = log(SSMI)) %>% 
  mutate(Log_Return = c(NA, diff(SSMI_log))) %>% # 1 NA at beginning to fit tibble length
  filter(!is.na(Log_Return))

# head(SMI_tbl)

SMI_tbl %>% ggplot() +
  geom_line(aes(x = date, y = Log_Return)) +
  labs(
    title = "SMI Log returns",
    subtitle = "From 2000 onwards",
    x = "Date",
    y = "SMI Log Returns"
  ) +
  theme_minimal()


# -----------------------------------------------

# Historical Value-at-Risk


# Setting Parameters

inv_volume <- 1000       # Investment volume
hp <- 1                  # Holding period (in months)
alpha <- .05             # Confidence level of 95%

# Calculating historical Value-at-Risk
# This is clumsy and does not correctly interpolate as 'quantile' would.


# Returns_sorted <- sort(as.numeric(SMI_returns, decreasing=FALSE))
# position_quantil <- floor(length(Returns_sorted) * alpha)
# 
# alpha_quantil <- Returns_sorted[position_quantil]
# hvar <- alpha_quantil * inv_volume
# print(hvar)


# Much better version:
# Calculating hist VaR based on tibble

# Calculate quantile once and then use it to compute VaR and return_at_alpha

results <- SMI_tbl %>%
  summarize(return_at_alpha = quantile(Log_Return, probs = alpha, type = 1)) %>%
  mutate(VaR = return_at_alpha * inv_volume)

# Extract return_at_alpha and historical VaR
return_at_alpha <- results$return_at_alpha
hist_VaR <- results$VaR

# Print the results
print(return_at_alpha)
print(hist_VaR)



# -----------------------------------------------


# ECDF: Empirical Cumulative Distribution Function

# ecdf <- 1:length(Returns_sorted) / length(Returns_sorted)
# plot(x=Returns_sorted, y=ecdf, xlab="SMI Returns", ylab="ECDF", main = "ECDF of SMI Returns")
# abline(v=alpha_quantil, col="red")


# ggplot has a ecdf function implemented AND no need to sort the log return

SMI_tbl %>% ggplot(aes(x = Log_Return)) +
  geom_smooth(stat = "ecdf", se = FALSE, color = "darkgrey") +
  # geom_step(stat = "ecdf") +
  # geom_point(stat = "ecdf", color = "lightblue", shape = 1) +
  # stat_ecdf(geom = "smooth", se = FALSE, color = "darkgrey") +
  labs(
    title = "ECDF of SMI Log Returns",
    x = "SMI Log Returns",
    y = "ECDF"
  ) +
  geom_vline(xintercept = return_at_alpha, color = "red") +
  theme_minimal()


# -----------------------------------------------

# Calculating parametric Value-at-Risk


# vola <- sd(Returns)    # Standard deviation of SMI returns
# mean <- mean(Returns) # Average return of SMI
# parvar<-(mean*hp-qnorm(1-alpha)*vola*sqrt(hp))*inv_volume
# print(parvar)

# Plotting estimated VaRs

# hist(Returns*inv_volume, breaks=30, xlab = "Profit/Loss per Month (Returns*Investment Volume)", main = "")
# abline(v=hvar, col="red")
# abline(v=parvar, col="blue")
# legend("topleft", legend = c("Historical VaR", "Parametric VaR"), col=c("red","blue"), lty=1)
# 
# abs(hvar)-abs(parvar)


## More tidy solution:

inv_volume <- 1000        # Investment volume in CHF
hp <- 1                   # Holding period in months
alpha <- .05              # Confidence level of 95%

par_VaR <- SMI_tbl %>%
  summarize(
    volatility = sd(Log_Return),
    mean_return = mean(Log_Return)
  ) %>%
  mutate(
    par_VaR = (mean_return * hp - qnorm(1 - alpha) * volatility * sqrt(hp)) * inv_volume
  ) %>%
  pull(par_VaR)

# Print result
print(par_VaR)

# Print with ggplot:

SMI_tbl <- SMI_tbl %>%
  mutate(profit_loss = Log_Return * inv_volume)  # Add absolute profit_loss

# Plot using ggplot2
ggplot(SMI_tbl, aes(x = profit_loss)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = hist_VaR), col = "red", linetype = "solid", size = 1) + 
  geom_vline(aes(xintercept = par_VaR), col = "blue", linetype = "solid", size = 1) +
  labs(
    x = "Profit/Loss per Month (Log Return * Investment Volume)",
    y = "Frequency",
    title = "Profit/Loss Distribution with Historical and Parametric VaR"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_color_manual(values = c("red", "blue")) +
  guides(colour = guide_legend(title = "VaR Type")) +
  annotate("text", x = hist_VaR, y = 25, label = "Historical VaR", color = "red", hjust = -0.1) +
  annotate("text", x = par_VaR, y = 28, label = "Parametric VaR", color = "blue", hjust = -0.1)

print(hist_VaR)
print(par_VaR)
abs(hist_VaR)-abs(par_VaR)


#Interpretation (based on data until 23/04/2025): The historical (parametric) Value-at-Risk 
# indicates that there is a 5% chance of having losses that exceed CHF 67.48 (CHF 61.01) 
# over a monthly period. Since the absolute historical VaR is larger than the parametric, it can 
# be interpreted as a more conservative measure of risk, as it indicates that with a 5% probability, 
# we will lose CHF 67.48 or more when holding the investment for one month rather than just only 61.01.
