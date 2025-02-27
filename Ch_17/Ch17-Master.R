# Clear the environment and set the working directory ----
rm(list = ls())
setwd("~/Documents/5504 Spring 2025/Ch_17")

# Section 1 Patterns Visualization ----

library(fpp2) # Forecasting: principles and practice by Rob J Hyndman 

# Trend (Australian GDP)
autoplot(ausgdp) +
  ggtitle("Australian GDP (Trend pattern)")
  
# autoplot is a generic function to visualize various data object
# It is much simpler and easy to produce fairly complicate graphics.

# Trend and Seasonal (Monthly Australian electricity demand from 1980–1995)
?elec
elec

aelec <- window(elec, start=1980)
aelec

autoplot(aelec) +
     xlab("Year") +
     ylab("GWh") +
     ggtitle("Electricity Demand (Seasonal Trend Pattern)")

# Trend, Seasonality and Cyclicity (Quarterly Australian clay brick production)
autoplot(bricksq) +
  ggtitle("Australian clay brick production (") +
  xlab("Year") + ylab("million units") +
  ggtitle("Clay Brick Production (Trend, Seasonality and Cyclicity)")

# Cyclic pattern (famous Canadian lynx data)
autoplot(lynx) + xlab("Year") + ylab("Number of lynx trapped") +
  ggtitle("Annual Canadian Lynx Trappings (Cyclical)")
# ~10-year cyclic pattern driven by predator-prey dynamics

# Seasonality and Cyclical behavior (monthly sales of houses in the USA 1973-1995)
autoplot(hsales) +
  ggtitle("Sales of new one-family houses, USA") + xlab("Year") + ylab("Total sales") +
  ggtitle("Sales of houses in the USA (Seasonal and Cyclical)")

# Section 2 Moving Average ----

library(TTR) # Technical Trading Rules Package
library (tidyverse)

# Read Excel File 
library(readxl)
gas <- read_excel("gasoline.xlsx")
glimpse(gas)
head(gas)

# Simple Moving Average
gas$gas_sma <- SMA(gas$Sales, n=3)
print(gas$gas_sma)
#The last entry is the forecast for period 13.

# Visualization
gas %>% 
  ggplot(aes(Week, Sales)) + 
  ylim(0,25) +
  geom_point(size = 5, alpha =.2) + 
  geom_line(lwd = 2, color = "tomato") + #geom_line connects the dots
  geom_point(y = gas$gas_sma, size = 5, alpha =0.1) +
  geom_line(y = gas$gas_sma, lwd = 2, color = "cornflowerblue")

# Calculate accuracy measures for this model
library(forecast)
gas$gas_sma <- SMA(gas$Sales, n=3)
accuracy(gas$gas_sma, gas$Sales)

# Calculate accuracy measures for MA with different values of k
accuracy_values <- data.frame(k = 1:(nrow(gas) - 1), 
                              RMSE = numeric(nrow(gas) - 1), 
                              MAE = numeric(nrow(gas) - 1), 
                              MAPE = numeric(nrow(gas) - 1))

for (k in 1:(nrow(gas) - 1)) {
  # Calculate MA
  gas$gas_sma <- SMA(gas$Sales, n = k)
  
  # Calculate accuracy measures and store
  acc <- accuracy(gas$gas_sma, gas$Sales)
  accuracy_values[k, c("RMSE", "MAE", "MAPE")] <- acc[1, c("RMSE", "MAE", "MAPE")]
}

# Print accuracy values
print(accuracy_values)

# Another example for moving average from fpp2 
library(fpp2) # Forecasting: principles and practice by Rob J Hyndman 

# Residential electricity sales (excluding hot water) for South Australia: 1989–2008.
glimpse (elecsales)

# plot
autoplot(elecsales) + xlab("Year") + ylab("GWh") +
  ggtitle("Annual electricity sales: South Australia")

# moving average of order 3
ma(elecsales, 3)

# plot 3 and 5 MA along with the original data
autoplot(elecsales, series="Data") + # plots time series label as "Data"
  autolayer(ma(elecsales,3), series="3-MA") + # add alayer, plots 3-MA
  autolayer(ma(elecsales,5), series="5-MA") + # add alayer, plots 5-MA
  xlab("Year") + ylab("GWh") +
  ggtitle("Annual electricity sales: South Australia")
# We observe that k=5 is smoother than k=3

# This smoothing effect for high values of k could be used to remove seasonality

# Let's look at (elecequip): quarterly data on electrical equipment manufacturing
data(elecequip)
autoplot(elecequip)

autoplot(elecequip, series="Data") +
  autolayer(ma(elecequip, 12), series="12-MA") +
  xlab("Year") + ylab("New orders index") +
  ggtitle("Electrical equipment manufacturing (Euro area)") 
# Notice that the smooth line shows no seasonality (only trend and cycle)

# Section 3 Weighted Moving Average -----
library(TTR)
weights <- c(.17, .33, .5)
gas$gas_wma <- WMA(gas$Sales, n=3, wts = weights) 
print(gas$gas_wma)
#The last entry is the forecast for period 13.

# Calculate accuracy measures for this model
library(forecast)
accuracy(gas$gas_wma, gas$Sales)

# Visualization
gas %>% 
  ggplot(aes(Week, Sales)) + 
  ylim(0,25) +
  geom_point(size = 5, alpha =.2) + 
  geom_line(lwd = 2, color = "lightsalmon") + 
  geom_point(y = gas$gas_wma, size = 5, alpha =0.1) +
  geom_line(lwd = 2, y = gas$gas_wma, color = "cadetblue") +
  ggtitle("Weighted Moving Average")


# Section 4 Exponential Smoothing -----

#Exponential Smoothing
library(TTR)
gas$gas_exp <- EMA(gas$Sales, n=1, ratio = .2) 
# n = number of periods to average over, in our case n should be set equal to 1.

print(gas$gas_exp)
#The last entry is the forecast for period 13.
  
# Accuracy
accuracy(gas$gas_exp, gas$Sales)

# Visualization
gas %>% 
  ggplot(aes(Week, Sales)) + 
  ylim(0,25) +
  geom_point(size = 5, alpha =.2) + 
  geom_line(lwd = 2, color = "lightsalmon") + 
  geom_point(y = gas$gas_exp, size = 5) +
  geom_line(y = gas$gas_exp, lwd = 2, color = "cadetblue") +
  ggtitle("Exponential Smoothing")
  

# More comprehensive example from fpp2 library
library(fpp2)
oil
?oil

oildata <- window(oil, start=1996) # Oil production in Saudi Arabia from 1996 to 2013.

# Time Series Plot
autoplot(oildata) +
  ylab("Oil (millions of tonnes)") + xlab("Year")

# Estimate parameters
fc <- ses(oildata, h=5) # ses: single exponential smoothing from fpp2
# 'h' (forecasting horizon) refers to the number of forecast periods you want to generate.
# Alpha is estimated automatically from the data by minimizing sum of squares

summary(fc) # Auto calculated alpha is 0.83
# Large value of alpha (0.83) means large adjustment the estimates each time.
# We are assigning a lot of weight to the most recent observation.
# A smaller value of alpha would lead to smaller changes over time (smoother).

# We can set alpha manually 
fc1 <- ses(oildata, 
           alpha = 0.1, 
           h = 5) # Estimate parameters, set alpha = 0.1

summary(fc1)

# Visualization to compare two forecast methods
autoplot(oildata) +  # Original time series
  autolayer(fitted(fc), series = "Alpha = 0.83") +  # Default SES forecast
  autolayer(fitted(fc1), series = "Alpha = 0.1") +  # Smoother model
  ylab("Oil (millions of tonnes)") + xlab("Year")
# autolayer creates multiple layers of plots, based on the contents of a list or data frame.
# It is particularly useful for creating visualizations of time series data.

# Section 5 Trend projection (simple linear regression) -----

library(readxl)
bike <- read_excel("bicycle.xlsx")
glimpse(bike)
head(bike)

# Visualization
bike %>% 
  ggplot(aes(Year, Sales)) + 
  geom_point() + 
  geom_smooth(method=lm, color = "hotpink")

# Simple Linear Regression
bike_slr <- lm(Sales ~ Year, data = bike)
coefficients (bike_slr)
summary(bike_slr)

# MSE
anova(bike_slr)

#forecast for period 11
predict(bike_slr, list(Year=11))


# Section 6 Trend projection (Non-linear regression) -----
library(readxl)
cholesterol <- read_excel("cholesterol.xlsx")
glimpse(cholesterol)
head(cholesterol)

# Visualization
cholesterol %>% 
  ggplot(aes(Year, Revenue)) + 
  geom_point() + 
  geom_smooth(method=lm, formula = y ~ poly(x, 2), color = "peachpuff")

#Simple Linear Regression (Raw=F Orthogonal Polynomials)
cholestrol_model <- lm(Revenue ~ poly(Year, degree=2, raw=T), data = cholesterol)
summary(cholestrol_model)
coefficients(cholestrol_model)

#MSE
anova(cholestrol_model)

#forecast for period 11
predict(cholestrol_model, list(Year=11))


# Section 7 Seasonality without Trend -----

library(readxl)
umbrella <- read_excel("umbrella.xlsx")
glimpse(umbrella)
head(umbrella)

# Convert Quarter to Factor
umbrella$Quarter <- as.factor(umbrella$Quarter)
levels(umbrella$Quarter)

# Dummy Var Building
umbrella_dummy <- as.data.frame(model.matrix(~ Quarter -1, umbrella)) # -1 removes intercept

# Combine data frames
umbrella <- cbind(umbrella_dummy, umbrella)

# Simple Linear Regression
umbrella_slr <- lm(Sales ~ Quarter1 + Quarter2 + Quarter3, data = umbrella)
summary(umbrella_slr)
coef(umbrella_slr)

# forecast
predict(umbrella_slr)

# Visulaization
umbrella %>% 
  ggplot(aes(Period, Sales)) + 
  ylim(0,200) +
  geom_point() + 
  geom_line(color="slateblue") +
  geom_smooth(method = "lm", color = "seagreen") # no trend

# Section 8 Seasonality with Trend -----

library(readxl)
phone <- read_excel("smartphonesales.xlsx")
glimpse(phone)
head(phone)

# Change column name
colnames(phone)[3] <- "Sales"

# Convert Quarter to Factor
phone$Quarter <- as.factor(phone$Quarter)
phone_dummy <- as.data.frame(model.matrix(~ Quarter -1, phone)) 

# Dummy Var Building
phone <- cbind(phone_dummy, phone)

# build the period column
phone$Period <- c(1:16)

# Simple Linear Regression
phone_slr <- lm(Sales ~ Quarter1 + Quarter2 + Quarter3 + Period, data = phone)
summary(phone_slr)
coef(phone_slr)

#forecast
predict(phone_slr)

#forecast next year Q1
predict(phone_slr, data.frame(Quarter1=1, Quarter2=0, Quarter3=0, Period=17) )

phone %>% 
  ggplot( 
    aes(Period, Sales)) + 
  ylim(0,10) +
  geom_point() + 
  geom_line(color="coral") +
  geom_smooth(method = "lm", color = "royalblue") # no trend

# Section 9 Time Series Decomposition ----

# Building time series
phone_ts <- ts(phone$Sales, start=2015, frequency = 4)
print(phone_ts)
autoplot(phone_ts) + xlab("Year") + ylab("Smartphone Sales") 

# Decomposition
phone_decomp <- decompose(phone_ts, type = "multiplicative" ) # in bus we use multiplicative
autoplot(phone_decomp)
print(phone_decomp)

# A more interesting Time Series
AirPassengers # Monthly totals of international airline passengers, 1949 to 1960.
# Note: this is a monthly data (not quarterly)

# Visualization
autoplot(AirPassengers) # Airline Passengers from

# Decomposition
AirPassengers_decomp <- decompose(AirPassengers, type= "multiplicative")
autoplot(AirPassengers_decomp)
print(AirPassengers_decomp)

# Each plot separately
autoplot(AirPassengers_decomp$seasonal) # These are seasonal indexes
autoplot(AirPassengers_decomp$trend)
autoplot(AirPassengers_decomp$random)

# Decomposition example from fpp2 (x11 method)
autoplot(elecequip)
# Monthly production of electrical equipment. January 1996 - March 2012

# Decomposition (Classical Method)
elecequip %>% 
  decompose(type="multiplicative") %>%
  autoplot() + xlab("Year") +
  ggtitle("Multiplicative decomposition of electrical equipment index")
# The classical method captured the sudden fall in 2009 as an error

# Decomposition (X11)
# It handles both additive and multiplicative decomposition (U.S. Census Bureau)
# https://otexts.com/fpp3/methods-used-by-official-statistics-agencies.html

library(seasonal)
fit <- seas(elecequip, x11="") # x11="" default settings for the X-11 decomposition method.
autoplot(fit) +
  ggtitle("X11 decomposition of electrical equipment index")
# The X11 captured the sudden fall in the data in early 2009 better than classical method

# Overlayed plot
library(ggplot2)
autoplot(elecequip, color="gray") + 
  autolayer(trendcycle(fit), color="black") + # trendcycle()  extract trend component
  autolayer(seasadj(fit), color="tomato") + # seasadj() compute seasonally adjusted (deseasonalized) time series
  ggtitle("Electrical equipment manufacturing (Euro area)")

