# Clear the environment and set the working directory ----
rm(list = ls())
setwd("~/Documents/5504 Spring 2025/Ch_17")

# 17-7 ----
# method a TTR package

library(TTR)

# Given gasoline sales data
gasoline_sales <- c(17, 21, 19, 23, 18, 16, 20, 18, 22, 20, 15, 22)

# Compute moving averages using SMA from TTR
three_week_ma <- SMA(gasoline_sales, n = 3)
four_week_ma <- SMA(gasoline_sales, n = 4)
five_week_ma <- SMA(gasoline_sales, n = 5)

# Function to compute MSE manually without removing NA values
compute_mse <- function(actual, predicted) {
  valid_indices <- !is.na(predicted)  # Only compare non-NA forecast values
  mean((actual[valid_indices] - predicted[valid_indices])^2)
}

# Calculate MSE manually
mse_three_week <- compute_mse(gasoline_sales, three_week_ma)
mse_four_week <- compute_mse(gasoline_sales, four_week_ma)
mse_five_week <- compute_mse(gasoline_sales, five_week_ma)

# Print results
cat("MSE for three-week moving average:", mse_three_week, "\n")
cat("MSE for four-week moving average:", mse_four_week, "\n")
cat("MSE for five-week moving average:", mse_five_week, "\n")

# Discrepancy between text MSE (10.22) and our MSE (5.0) is due to alignment and centering

# method b forecast package
library(forecast)

# Given data
sales <- c(17, 21, 19, 23, 18, 16, 20, 18, 22, 20, 15, 22)

# Compute moving averages
three_week_ma <- ma(sales, order = 3)
four_week_ma <- ma(sales, order = 4)
five_week_ma <- ma(sales, order = 5)

# Compute accuracy measures for four-week moving average
accuracy(three_week_ma, x = sales)
accuracy(four_week_ma, x = sales)
accuracy(five_week_ma, x = sales)

mse_3 <- accuracy(three_week_ma, sales)[1,2]^2
mse_4 <- accuracy(four_week_ma, sales)[1,2]^2
mse_5 <- accuracy(five_week_ma, sales)[1,2]^2

cat("MSE for three-week moving average:",mse_3)
cat("MSE for four-week moving average:",mse_4)
cat("MSE for five-week moving average:",mse_5)

# The discrepancy in MSE is from the different approaches used in centering and alignment
# In larger sample sizes the difference is negligible

# 17-9 ----
# Load the forecast package
library(forecast)

# Given data
sales <- c(17, 21, 19, 23, 18, 16, 20, 18, 22, 20, 15, 22)

# Apply exponential smoothing with alpha = 0.1
es_01 <- ses(sales, alpha = 0.1)

# Apply exponential smoothing with alpha = 0.2
es_02 <- ses(sales, alpha = 0.2)

# Calculate Mean Squared Error (MSE) for alpha = 0.1
accuracy(es_01)

# Calculate Mean Squared Error (MSE) for alpha = 0.2
accuracy(es_02)

# Model 1 appears to perform slightly better than model 2

# 17-11 ----
library(forecast)
library(tidyverse)

# Given data
shipments <- c(80, 82, 84, 83, 83, 84, 85, 84, 82, 83, 84, 83)

# Convert to time series object
shipments_ts <- ts(shipments)

# a. Construct a time series plot
autoplot(shipments_ts)
plot(shipments_ts, main = "Monthly Percentage of On-Time Shipments",
     xlab = "Month", ylab = "Percentage", col = "blue", lwd = 2)

# b. Compare the 3-month moving average and exponential smoothing with alpha = 0.2
# Three-month moving average
ma3 <- ma(shipments_ts, order = 3)

# Exponential smoothing with alpha = 0.2, 0.3
es2 <- ses(shipments_ts, alpha = 0.2)
es3 <- ses(shipments_ts, alpha = 0.3)

# Calculate MSE for both approaches
accuracy(ma3, x = shipments_ts) # accuracy against actual values (shipments_ts)
accuracy(es2)
accuracy(es3)

# c. Forecast for next month using exponential smoothing
forecast(es3, h = 1) #  one-step-ahead forecast

# 17-15 ----
library(forecast)

# Given data
futures_index <- c(7.35, 7.40, 7.55, 7.56, 7.60, 7.52, 7.52, 7.70, 7.62, 7.55)

# Convert to time series object
futures_index_ts <- ts(futures_index)

# a. Construct a time series plot
plot(futures_index_ts, main = "Commodity Futures Index",
     xlab = "Week", ylab = "Index Value", col = "blue", lwd = 2)
autoplot(futures_index_ts)

# b. Compute exponential smoothing forecasts for alpha = 0.2
es_02 <- ses(futures_index_ts, alpha = 0.2)

# c. Compute exponential smoothing forecasts for alpha = 0.3
es_03 <- ses(futures_index_ts, alpha = 0.3)

# d. Compare the MSE for both forecasts and forecast for week 11
# Calculate MSE for alpha = 0.2
accuracy(es_02)

# Calculate MSE for alpha = 0.3
accuracy(es_03)

# alpha = 0.3 has a lower MSE compared to alpha = 0.2  the model 

# Forecast for week 11 using exponential smoothing with alpha = 0.3
forecast(es_03, h = 1)

# 17-19 ----

library(forecast)
library(ggplot2)

# Given time series data
time_series_data <- data.frame(
  t = 1:7,
  Yt = c(120, 110, 100, 96, 94, 92, 88)
)

# a. Construct a time series plot
ggplot(time_series_data, aes(x = t, y = Yt)) +
  geom_line() +
  geom_point() +
  labs(title = "Time Series Plot", x = "Time (t)", y = "Yt")

# b. Develop the linear trend equation
linear_model <- lm(Yt ~ t, data = time_series_data)
summary(linear_model)

# c. Forecast for t = 8 using forecast package
forecast(linear_model, newdata = data.frame(t = 8))
predict(linear_model, newdata = data.frame(t = 8))

# Compute Mean Squared Error (MSE) using accuracy function (optional)
fitted_values <- fitted(linear_model)
accuracy(fitted_values, time_series_data$Yt)
accuracy(fitted_values, time_series_data$Yt)["Test set", "RMSE"]^2


# 17-25 ----

library(ggplot2)
library(forecast)

# Given Netflix subscriber data
netflix_data <- data.frame(
  Period = c(1, 2, 3, 4, 5, 6),
  Subscribers = c(33.27, 44.35, 57.39, 74.76, 93.80, 117.58)
)

# a. Construct a time-series plot
ggplot(netflix_data, aes(x = Period, y = Subscribers)) +
  geom_line() +
  geom_point() +
  labs(title = "Netflix Subscribers Over Time",
       x = "Period (Years 2012-2017)",
       y = "Subscribers (Millions)")

# b. Develop a linear trend equation
linear_model <- lm(Subscribers ~ Period, data = netflix_data)
summary(linear_model)
anova(linear_model)

# c. Develop a quadratic trend equation
quadratic_model <- lm(Subscribers ~ poly(Period, 2, raw = TRUE), data = netflix_data)
summary(quadratic_model)
anova(quadratic_model)

# Compute MSE for both models (alt method)
linear_fitted <- fitted(linear_model)
quadratic_fitted <- fitted(quadratic_model)

mse_linear <- mean((netflix_data$Subscribers - linear_fitted)^2)
mse_quadratic <- mean((netflix_data$Subscribers - quadratic_fitted)^2)

cat("MSE for Linear Model:", round(mse_linear, 2), "\n")
cat("MSE for Quadratic Model:", round(mse_quadratic, 2), "\n")


# Forecast for next period (t = 7) using both models
next_period <- data.frame(Period = 7)

forecast_linear <- predict(linear_model, newdata = next_period)
forecast_quadratic <- predict(quadratic_model, newdata = next_period)

cat("Forecast for t = 7 using Linear Model:", round(forecast_linear, 2), "million subscribers\n")
cat("Forecast for t = 7 using Quadratic Model:", round(forecast_quadratic, 2), "million subscribers\n")
# The quadratic model’s projection for subscriber growth is more aggressive for one period ahead OK

# 17-28 ----

# part a, Visualization
TS28 <- ts(c(71, 49, 58, 75, 68, 41, 60, 84, 62, 51, 53, 72),
         start=c(2020,1), 
         frequency = 4) # quarterly
print(TS28)
autoplot(TS28) # Horizontal Pattern

# Part b, Regression
Q28 <- data.frame(Quarter=c(1:4), TS28) # 1:4 repeats itself

# Convert Quarter to Factor
Q28$Quarter <- as.factor(Q28$Quarter)
levels(Q28$Quarter)

# Simple Linear Regression without dummy variables
Q28_slr_1 <- lm(TS28 ~ Quarter, data = Q28)
summary(Q28_slr_1)
predict(Q28_slr_1)[1:4]

# Simple Linear Regression with dummy variables
Q28_dummy <- model.matrix(~ Quarter-1, Q28) # -1: exclude the intercept
Q28 <- cbind(Q28_dummy, Q28)

# Simple Linear Regression
Q28_slr <- lm(TS28 ~ Quarter1 + Quarter2 + Quarter3, data = Q28)
summary(Q28_slr) 
predict(Q28_slr)
predict(Q28_slr)[1:4]

# 17-32 ----

# part a

# build data frame
Q32 <-  c(20,  100, 175, 13,
          37,  136, 245, 26,
          75,  155, 326, 48,
          92,  202, 384, 82,
          176, 282, 445, 181)

# convert it into a time series
TS32 <- ts(Q32, start=2019, frequency=4) # quarterly

# Data is in a time series format
print(TS32)

# time series plot
plot(TS32)

# alt method using autoplot from ggfortify
library(ggfortifyggfortify) # Load the ggfortify package

autoplot(TS32) + xlab("Year") + ylab("Revenue (1000)") # Upward Linear trend + Seasonal

# add a column for quarters 1:4
Q32 <- data.frame(Quarter= c(1:4), Revenue = Q32)

# convert this variable to a factor
Q32$Quarter <- as.factor(Q32$Quarter)
levels(Q32$Quarter)

# Simple Linear Regression without dummy variables
Q32_slr_1 <- lm(Revenue ~ Quarter, data = Q32)
summary(Q32_slr_1)
predict(Q32_slr_1)
predict(Q32_slr_1)[1:4]

# create dummy variable matrix
Q32_dummy <- model.matrix(~ Quarter -1, Q32) # -1: exclude the intercept
Q32 <- cbind(Q32_dummy, Q32)

# Simple Linear Regression
Q32_slr_2 <- lm(Revenue ~ Quarter1 + Quarter2 + Quarter3, data = Q32)
summary(Q32_slr_2)
predict(Q32_slr_2)
predict(Q32_slr_2)[1:4]

# part c
# adding Period
Q32$Period <- c(1:20)

# Simple Linear Regression with trend
Q32_slr_2 <- lm(Revenue ~ Quarter1 + Quarter2 + Quarter3 + Period, data = Q32)

# coefficients
summary(Q32_slr_2)

# forecasts
predict(Q32_slr_2, data.frame(Quarter1=1, Quarter2=0, Quarter3=0, Period=21))
predict(Q32_slr_2, data.frame(Quarter1=0, Quarter2=1, Quarter3=0, Period=22))
predict(Q32_slr_2, data.frame(Quarter1=0, Quarter2=0, Quarter3=1, Period=23))
predict(Q32_slr_2, data.frame(Quarter1=0, Quarter2=0, Quarter3=0, Period=24))

# 17-37 ----

# part a

TS37 <- ts(c(1690, 940, 2625, 2500,
             1800, 900, 2900, 2360,
             1850, 1100, 2930, 2615),
          start=2020, 
          frequency=4)

print(TS37)
autoplot(TS37) # Linear Trend with Seasonal Pattern

# part b (optional)
library(fpp2) # Forecasting: Principles and Practice

# Moving Average 4 quarters (not centered)
TS37_MA_4 <- ma(TS37, order = 4, centre = F) # 4-period MA, entre = F (Trailing MA)
print(TS37_MA_4) # The last entry is the forecast for period 13.

# Centered Moving Average 4 quarters
TS37_MA_4_C <- ma(TS37, order = 4, centre = T)
print(TS37_MA_4_C)

# part c
# Seasonal Indexes (Decomposition)

# Manual Calculation (optional)
## Step 1: TS values / centered MA = un-adjusted seasonal index for each period
## Step 2: Average un-adjusted seasonal indices for each period
## Step 3: Multiplicative model requires that sum of seasonal indices come to 4.00.
## Step 4: Adjustment: multiply indexes by the # of seasons divide by sum of the un-adjusted indexes.

# R gives you adjusted seasonal indexes.

TS37_Decomp <- decompose(TS37, type = "multiplicative" )
print(TS37_Decomp)
autoplot(TS37_Decomp)

# adjusted seasonal indexes.
print(TS37_Decomp$seasonal)

# part d
# Largest seasonal index is 3rd Quarter (July, Aug, Sep) back-to-school

# part e
# Calculating De-Seasonalized Time Series
TS37_des <- TS37/TS37_Decomp$seasonal
print(TS37_des)

# Part f
# Using the De-seasonalized Time Series to Identify Trend
# Simple Linear Regression
Q37 <- data.frame(Period=(1:12), TS37_des)
Q37_slr <- lm(TS37_des ~ Period, data = Q37)
summary(Q37_slr)

# Forecast Year 4
Q37_forecast <- predict(Q37_slr, list(Period=c(13:16)))
Q37_forecast

# Part g
# Adjusted linear trend forecasts by multiplying in the adjusted seasonal indexes
Adj_Q37_forecast <- Q37_forecast * TS37_Decomp$seasonal[1:4]
print(Adj_Q37_forecast)

# 17-43 ----

# part a
TS_43 <- ts(c(2750, 3100, 3250, 2800, 2900, 3050, 3300, 3100, 2950, 3000, 3200, 3150),
         start=c(2020,1,1),
         frequency = 52) # weekly

print(TS_43)
autoplot(TS_43) # Linear Trend?
autoplot(TS_43, ylim = c(0, 4000)) # No Linear Trend: Horizontal Pattern

# part b
# Exponential smoothing Alpha = 0.4
library(forecast)
ses(TS_43, alpha=0.4, h=1)

# 17-54 ----

TS54 <- ts(c(6,  15, 10, 4,
             10, 18, 15, 7,
             14, 26, 23, 12,
             19, 28, 25, 18,
             22, 34, 28, 21,
             24, 36, 30, 20,
             28, 40, 35, 27), start=2011, frequency=4)
print(TS54)
autoplot(TS54)

# part a
library(fpp2) # Forecasting: Principles and Practice Ver.2

# Centered Moving Average 4 quarters
TS54_CMA <- ma(TS54, order = 4, centre = T)
print(TS54_CMA)

# part b
# ts.plot plots several time series on a common plot.
ts.plot(TS54_CMA, TS54) 
# CMA values smooths data by removing seasonal effects and shows the trend.

# part c
# Seasonal Indexes (Decomposition)
Decomp_TS54 <- decompose(TS54, type = "multiplicative" )
autoplot(Decomp_TS54)
print(Decomp_TS54$seasonal)

# largest seasonal index in Q2 (Apr-Jun) (peak summer boating season)
# smallest seasonal index in Q4 (Oct- Dec) (Decreased boating in the fall and winter)

# Case Study (Forecasting Lost Sales  Carlson Department Store) ----

## Q1  ----
# An estimate of sales for Carlson Department Store had there been no hurricane.
library(tidyverse)
library(readxl)
carlson <- read_excel("carlsonsales.xlsx")
glimpse(carlson)
head(carlson)

# Building time series
# Start 9th month (September 2015)
carl_ts <- ts(carlson$Sales, start= c(2015, 9), frequency = 12)
print(carl_ts)
autoplot(carl_ts)

# Decomposition
carl_decomp <- decompose(carl_ts, type = "multiplicative" )
autoplot(carl_decomp)
print(carl_decomp)

# Calculating De-seasonalized Sales
carl_des_sales <- carl_ts/carl_decomp$seasonal
carlson <- cbind(carlson, carl_des_sales)

# Using the Deseasonalized Time Series to Identify Trend
# Simple Linear Regression
carl_slr <- lm(carl_des_sales ~ Period, data = carlson)
summary(carl_slr)

# Forecasting without irregularity or hurricane (4 month closed, Sep - Dec) 
carl_des_forecast <- predict(carl_slr, list(Period=c(49:52)))
carl_des_forecast

# Now we add the seasonal effect
carl_forecast <- carl_des_forecast * carl_decomp$seasonal[1:4]
print(carl_forecast)


## Q2  ----
# An estimate of countywide department store sales had there been no hurricane.
library(readxl)
county_df_full <- read_excel("countysales.xlsx")
glimpse(county_df_full)
head(county_df_full)
# Note: County Excel File has 52 month with Actual values of Sep - Dec of yr 5)

county_df <- county_df_full[c(1:48),] # we need a comparable data set (first 48 rows)

# Building time series
# Start 9th month (Sep) of 2015
county_ts <- ts(county_df$Sales, start= c(2015, 9), frequency = 12)
print(county_ts)
autoplot(county_ts)

# Decomposition
county_decomp <- decompose(county_ts, type = "multiplicative" )
autoplot(county_decomp)
print(county_decomp)

# Calculating De-seasonalized Sales
county_des_sales <- county_ts/county_decomp$seasonal
county_df <- cbind(county_df, county_des_sales)
county_df$Period <- c(1:48)

# Using  Deseasonalized Time Series to Identify Trend
# Simple Linear Regression
county_slr <- lm(county_des_sales ~ Period, data = county_df)
summary(county_slr)

# Forecasting based on the slr (4 month closed, Sep - Dec)
county_des_forecast <- predict(county_slr, list(Period=c(49:52)))
county_des_forecast

# Now we add the seasonal effect
county_forecast <- county_des_forecast * county_decomp$seasonal[1:4]
print(county_forecast)


## Q3  -----
# An estimate of lost sales for Carlson from September to December.

# Comparison: county forecast vs. county actual sales
# Actual Values (4 month closed, Sep - Dec)
county_actual <- county_df_full$Sales[c(49:52)]
print(county_actual)

# Let's put county forecast and county actual sales in a data frame
Lost_Sales <- data.frame(county_actual, county_forecast)

# Then let's calculate the ratio of these two values (lift factor)
Lost_Sales$Ratio_A_F <- Lost_Sales$county_actual/Lost_Sales$county_forecast

# For the four-month total, we calculate the average lift factor 
mean(Lost_Sales$Ratio_A_F)
# For the 4-month total, actual sales exceeded the forecast by around 30%.
# Explanation: people had to replace personal property damaged by the storm.

# Now, let's put the forecast for Carlson in this data frame
Lost_Sales$Carl_F <- carl_forecast

# Then we multiply the forecast by corresponding lift factor
Lost_Sales$Carl_LS <- Lost_Sales$Carl_F * Lost_Sales$Ratio_A_F

# By adding them together we find the total lost sales
sum(Lost_Sales$Carl_LS)
# Carlson suffered a business interruption claim of $15,864,850.


# In class Exercise Case 17-1 Forecasting Food and Beverage Sales ----
# Solution ----
# Load necessary libraries
library(readxl)   # For reading Excel files
library(fpp2)     # For time series forecasting and visualization
library(tidyverse) # For data manipulation

# Load the dataset
df <- read_excel("Vintage.xlsx")

# Convert data into a time series format (Year 1 is 2020)
sales_ts <- ts((df$Sales), start = c(2020,1), frequency = 12)

# Q1: Time series plot - Identify patterns
autoplot(sales_ts) + 
  ggtitle("Time Series Plot of Sales Data") + 
  xlab("Year") + ylab("Sales ($1000s)")

# The plot shows a linear trend (increasing sales over time) and seasonal variation.

# Q2: Seasonality Analysis - Extracting seasonal indices
decomp <- decompose(sales_ts, type = "multiplicative")  # Classical decomposition
autoplot(decomp)

# Extract seasonal component (first 12 months to get indices)
seasonal_indices <- decomp$seasonal[1:12] / mean(decomp$seasonal[1:12])
# Normalizes the indices so that they average around 1, sum around 12

seasonal_indices
# These seasonal indices align with typical retail and service industry trends
# High sales in December due to holiday shopping and festivities.
# Lower sales in summer (June–July, September) likely reflect consumer spending shifts.
# January remains relatively strong due to New Year’s sales and promotions.

# Q3: Deseasonalizing the data
df$deseasonalized_sales <- sales_ts / seasonal_indices
autoplot(ts(df$deseasonalized_sales, start = c(2020,1), frequency = 12)) + 
  ggtitle("Deseasonalized Sales Data")
# The deseasonalized ts shows a clear upward trend, confirming a consistent growth pattern.

# Q4: Forecast using Time Series Decomposition Method
model <- lm(deseasonalized_sales ~ Month, data = df) # Linear regression model on deseasonalized data
summary(model)

# Forecast trend for Year 4 (2023)
future_months <- c(37: 48)
future_trend <- predict(model, newdata = data.frame(Month = future_months))
future_trend

# Reintroduce seasonality
future_forecast_decomp <- future_trend * seasonal_indices
future_forecast_decomp

# Q5: Forecast using Dummy Variable Regression Approach (Alternative method)
# Create a repeating month factor from 1 to 12 (repeats for each year)
df$Month_Factor <- rep(1:12, length.out = nrow(df))

# Fit dummy variable regression model
dummy_model <- lm(Sales ~ Month_Factor + Month, data = df)
summary(dummy_model)

# Forecast using dummy regression
future_df <- data.frame(
  Month = 37:48,  # Extending time index
  Month_Factor = c(1:12)  # Repeating months 1-12
)
future_forecast_dummy <- predict(dummy_model, newdata = future_df)
future_forecast_dummy

# Create a comparison table for both forecast methods
forecast_table <- data.frame(
  Month = future_months,
  Month_Name = rep(month.abb),  # Assign month names
  Forecast_Decomposition = future_forecast_decomp,
  Forecast_Dummy_Regression = future_forecast_dummy
)

# Print the table
print(forecast_table)

# Discussion on discrepancy:
# The actual sales for January 2023 is $295,000.

# Actual sales for January Year 4 (Month 37)
actual_sales_jan_2023 <- 295  

# Compute forecast errors for both methods
forecast_error_decomp <- actual_sales_jan_2023 - future_forecast_decomp[1]
forecast_error_dummy <- actual_sales_jan_2023 - future_forecast_dummy[1]

# Print forecast errors
print(paste("Forecast Error (Decomposition Method) for January 2023:", round(forecast_error_decomp, 2)))
print(paste("Forecast Error (Dummy Regression Method) for January 2023:", round(forecast_error_dummy, 2)))

# The decomposition forecast error is less than the dummy regression approach
# The error for decomposition model is relatively small, meaning the models performed well.
# Possible sources of discrepancy:
# 1. External factors (economic conditions, competition)
# 2. Unexpected events (weather, promotions, new competitors)
# 3. Data limitations (outliers, changing business trends)

