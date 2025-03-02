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
