# Clear the environment
rm(list = ls())

# Set the working directory
setwd("~/Documents/5504 Spring 2025/Ch_14")

# Section 1 ----

# Read Excel file in R
library(readxl)
armands <- read_excel("armands.xlsx")
summary(armands)

# Examine Correlation between variables (Multiple R in Excel)
cor(armands$Population, armands$Sales)

# Simple linear regression
armands_lm <- lm(Sales ~ Population, data = armands)
armands_lm$coefficients # coefficients
summary(armands_lm)

# Plotting data and regression line
library(tidyverse)
armands %>% ggplot(aes(x = Population, y = Sales)) + # coordinate system
  geom_point() + # scatter plot
  geom_smooth(method = 'lm', se = TRUE) # ab-line with std. error (gray)

# Section 2 ----

# R Squared Value
summary(armands_lm)$r.squared

# Adjusted R Squared Value
summary(armands_lm)$adj.r.squared

# In Class Exercise ----
# Data
bike <- read_excel("RacingBicycles.xlsx")
summary(bike)

# a. Regression analysis
bike_lm <- lm(Price ~ Weight, data = bike)

# Coefficients
bike_lm$coefficients

# b. Compute r^2
summary(bike_lm)$r.squared

# Better representation
r_squared <- summary(bike_lm)$r.squared
cat("R-squared value:", r_squared) # concatenate and print

# Display the summary of the regression model
summary(bike_lm)

# c. Predict the price for a bike that weighs 15 pounds
predicted_price <- predict(bike_lm, newdata = data.frame(Weight = 15))
cat("Predicted price for a 15-pound bike:", predicted_price)

# Scatter plot with regression line
library(ggplot2)
library(ggrepel) # extra geoms for ggplot2.

ggplot(bike, aes(Weight, Price, label = Brand)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_text_repel(box.padding = 0.5) +
  labs(title = "Bike Weight vs Price", x = "Weight (lbs)", y = "Price ($)") +
  annotate("text", x = 14, y = 5000, 
           label = paste("y =", 
                         round(coef(bike_lm)[2], 2), "x +", 
                         round(coef(bike_lm)[1], 2))) +
  annotate("text", x = 14, y = 4500, 
           label = paste("R² =", round(r_squared, 2)))

# Section 3 ----

# Complete regression analysis
summary(armands_lm)
confint(armands_lm) # CI for slope and intercept

# ANOVA Table 
anova(armands_lm)

# point estimate
predict(armands_lm, newdata = data.frame(Population = 10))         

# Confidence Interval
predict (armands_lm, data.frame(Population = 10), 
         interval = "confidence", conf.level = 0.95)

# Prediction Interval
predict (armands_lm, data.frame(Population = 10), 
         interval = "prediction", conf.level = 0.95)

# Section 4 ----

# Residual analysis (method 1)
armands$predicted <- fitted(armands_lm)
armands$residuals <- residuals(armands_lm)
armands$std_residuals <- rstandard(armands_lm)

# Residual plot against x
armands %>% ggplot(aes(x = Population, y = residuals)) + 
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)  # we observe no pattern

# Residual plot against y-hat
armands %>% ggplot(aes(x = predicted, y = residuals)) + 
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)  # we observe no pattern

# Standard residual plot against x
armands %>% ggplot(aes(x = Population, y = std_residuals)) +
  geom_point() +
  geom_hline(yintercept = 2, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = -2, linetype = "dashed", color = "red") +
  labs(title = "Standardized Residuals Plot",
       x = "Population",
       y = "Standardized Residuals")
  
# QQ Plot 
armands %>% ggplot(aes(sample = std_residuals)) +
  geom_qq() + 
  geom_qq_line()

# Section 5 ----

# Calculating Leverage (Hat Values)
armands$hat <- hatvalues(armands_lm) #none greater than 6/10=0.6

qplot (armands$Population, armands$hat) + # quickplot
  geom_hline(yintercept=0.6, linetype="dashed", color = "red")

# Residual analysis (method 2)
plot(armands_lm, which = 1) # 1: residuals versus fitted

plot(armands_lm, which = 2) # 2: QQ plot

plot(armands_lm, which = 3) # 3: Scale-Location

plot(armands_lm, which = 4) # 4: Cook's Distance (none greater than 0.5)
# Cook's distance is a combination of leverage and residual values
# The higher the leverage and residuals, the higher the Cook’s distance.
# Investigate any point over 0.5, values over 1.0 are influential

plot(armands_lm, which = 5) # 5: Williams-like Graph
# in this plot we are looking for values lying outside dashed line

