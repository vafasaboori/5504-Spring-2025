# Clear the environment
rm(list = ls())

# Set the working directory
setwd("~/Documents/5504 Spring 2025/Ch_14")

# Collapse All — Option+Command+O or Alt+O for PC
# Expand All — Shift+Option+Command+O or Shift+Alt+O for PC

# 14-3 ----
Q3 <- data.frame(x = c(2, 6, 9, 13, 20), 
                 y = c(7, 18, 9, 26, 23))

# Part a, Scatter Plot
plot (Q3$x, Q3$y)
abline(lm(Q3$y ~ Q3$x))

# Using ggplot (recommended)
library(tidyverse)
library(ggpubr)
library(ggpmisc)
?ggpubr
?ggpmisc

Q3 %>% ggplot(aes(x, y)) + # coordinate system
  geom_point() + # scatter plot
  geom_smooth(method = 'lm', se = TRUE) + # ab-line with std. error (gray)
  stat_regline_equation() +
  stat_poly_eq() #R-squared

# Part b, regression
slr3 <- lm(y ~ x, data = Q3)
summary(slr3)
coef(slr3)

predict(slr3, data.frame(x=6))

# 14-5 ----
# Creating the data frame
Q5 <- data.frame(
  LineSpeed = c(20, 20, 30, 30, 40, 40, 50, 50),
  DefectivePartsFound = c(23, 21, 19, 16, 15, 17, 14, 11)
)

# Displaying the data frame
print(Q5)
summary(Q5)

# Rename one variables (too long)
colnames(Q5)[1] <- "speed"

# rename all variables
colnames(Q5) <- c("speed", "defective")

# alt method
names(Q5) <- c("speed", "defective")

# Part a, Scatter Plot
plot (Q5$speed, Q5$defective)
abline(lm(Q5$defective ~ Q5$speed))

# Using ggplot (recommended)
library(tidyverse)
Q5 %>% ggplot(aes(speed, defective)) + # coordinate system
  geom_point() + # scatter plot
  geom_smooth(method = 'lm', se = TRUE) + # ab-line with conf. interval (gray)
  stat_regline_equation() +
  stat_poly_eq()  

# part b, plot shows a negative relationship

# part c, regression
slr5 <- lm(defective ~ speed, data = Q5)
summary(slr5)

predict(slr5, data.frame(speed=25))

# 14-9 ----
library(readxl)
Q9 <- read_excel("landscape.xlsx")
summary(Q9)

# Rename one variables (too long)
colnames(Q9) <- c("value", "exp")

# Part a, Scatter Plot
plot (Q9$value, Q9$exp)
abline(lm(Q9$exp ~ Q9$value))

# Using ggplot (recommended)
library(tidyverse)
Q9 %>% ggplot(aes(value, exp)) + # coordinate system
  geom_point() + # scatter plot
  geom_smooth(method = 'lm', se = TRUE) + # ab-line with conf. interval (gray)
  stat_regline_equation() +
  stat_poly_eq()  

# part b, plot shows a positive relationship

# part c, regression
slr9 <- lm(exp ~ value, data = Q9)
summary(slr9)
coefficients(slr9)

# part d, variable cost
coefficients(slr9)[2]*1000  # should be multiplied by 1000

# part e predict
predict(slr9, data.frame(value=575))*1000 # should be multiplied by 1000

# 14-14 ----

# Creating the data frame
Q14 <- data.frame(
  DistanceToWork = c(1, 3, 4, 6, 8, 10, 12, 14, 14, 18),
  NumberOfDaysAbsent = c(8, 5, 8, 7, 6, 3, 5, 2, 4, 2)
)

# Displaying the data frame
print(Q14)

# Rename one variables (too long)
colnames(Q14) <- c( "distance", "absent")

# Part a, Scatter Plot
plot (Q14$distance, Q14$absent)
abline(lm(Q14$absent ~ Q14$distance))

# Using ggplot (recommended)
library(tidyverse)
Q14 %>% ggplot(aes(distance, absent)) + # coordinate system
  geom_point() + # scatter plot
  geom_smooth(method = 'lm', se = TRUE) + # ab-line with conf. interval (gray)
  stat_regline_equation() +
  stat_poly_eq()  

# plot shows a negative relationship

# part b, regression
slr14 <- lm(absent ~ distance, data = Q14)
summary(slr14)

# part c predict
predict(slr14, data.frame(distance=5))

# part e round
round (predict(slr14, data.frame(distance=5)), 2)

# 14-17 ----
Q17 <- data.frame(x=c(2, 6, 9, 13, 20), 
                  y=c(7, 18, 9, 26, 23))

# part a, SSR SSE SST
slr17 <- lm(y ~ x, data = Q17)
summary(slr17)

# r_squared
r_squared <- summary(slr17)$r.squared
r_squared

# r_value
sqrt(r_squared) * sign(coef(slr17)[2])

# Manual Calculations (Optional)
library("car") # Companion to Applied Regression
anova <- anova(slr17)
anova

# extract values
ssr <- anova$`Sum Sq`[1]
sse <- anova$`Sum Sq`[2]
sst <- ssr + sse

# r_squared
ssr/sst 

# r_value
sqrt(ssr/sst) * sign(coef(slr17)[2])

# 14-21 ----
Q21 <- data.frame(x = c (400, 450, 550, 600, 700, 750), 
                 y = c(4000, 5000, 5400, 5900, 6400, 7000))

# part a, regression
slr21 <- lm(y ~ x, data = Q21)
summary(slr21)

# part b, variable cost
coefficients(slr21)[2]

# part c, R-squared
summary (slr21)$r.squared 

# part d, predict
predict(slr21, data.frame(x=500))


# 14-25 ----
Q25 <- data.frame(x = c(2, 6, 9, 13, 20),
                 y = c(7, 18, 9, 26, 23))

slr25 <- lm(y ~ x, data = Q25)
summary(slr25)

# part a, standard error of estimate
summary(slr25)$sigma

#part b and c sig. test
summary(slr25)
summary(slr25)$coefficients[2, 4]
# Conclusion: the relationship is not statistically significant

# 14-29 ----
Q29 <- data.frame(x <- c(400, 450, 550, 600, 700, 750),
                 y <- c(4000, 5000, 5400, 5900, 6400, 7000))
slr29 <- lm(y ~ x, data = Q29)
summary(slr29)

# Anova Table
library(car)
anova(slr29)

# 14-33----
Q33 <- data.frame(x = c(3, 12, 6, 20, 14),
                 y = c(55, 40, 55, 10, 15))

# a. (optional) Estimate the standard deviation of y^* when x = 8
# SE of the predicted (fitted) values at x = 8)
# Formula 14.23 Page 686 for standard deviation of y^* (manual calc)
slr33 <- lm(y ~ x, data = Q33)
summary(slr33)

predict(slr33, newdata = data.frame(x = 8), se.fit = TRUE)$se.fit
# se.fit = TRUE means SE (or SD) of the predicted values should be calculated. 

# b. 95% CI interval for the expected value of y when x = 8
predict(slr33, data.frame(x=8), interval = "confidence")

# c. (optional) Estimate the standard deviation of an individual value of y when x = 8
# SD of the actual observed y values at x = 8
# Formula 14.26 Page 687 for standard deviation of y^* (manual calc)
sqrt(sum(residuals(slr33)^2) / (length(y) - 2))

# d. Develop a 95% prediction interval for y when x = 8
predict(slr33, data.frame(x=8), interval = "prediction")

# Question: What is Prediction Interval? How is it different from CI?

# 14-37 ----
Q37 <- data.frame(x = c(22, 27, 32, 48, 65, 85, 120),
                 y = c(9.6, 9.6, 10.1, 11.1, 13.5, 17.7, 25.5))
slr37 <- lm(y ~ x, data = Q37)
summary(slr37)

# part a and b
CI <- predict(slr37, data.frame(x=52.5), interval = "confidence")
CI

PI <- predict(slr37, data.frame(x=52.5), interval = "prediction")
PI

# part c
# Check if claimed deductions fall within the prediction interval
claim <- 20.4
audit <- claim < PI[1] | claim > PI[2] 
# pipe (|) is an (OR) logical test to check if claim is outside PI bounds
audit

# d. based on part (b) give IRS a guideline for income of $52,500 
# If claim falls within prediction interval, avoid triggering an audit.

# 14-39 ----
library(readxl)
Q39 <- read_excel("BusinessTravel.xlsx")
summary(Q39)

# rename all variables
colnames(Q39) <- c("city", "Rate", "Ent")

slr39 <- lm(Ent ~ Rate, data = Q39)
summary(slr39)

# point estimate
predict(slr39, data.frame(Rate=89))

# intervals (optional)
predict(slr39, data.frame(Rate=89), interval = "confidence")
predict(slr39, data.frame(Rate=89), interval = "prediction")

# 14-45 ----
Q45 <- data.frame(x = c(6, 11, 15, 18, 20),
                  y = c(6, 8, 12, 20, 30))
slr45 <- lm(y ~ x, data = Q45)

#part a
coefficients(slr45)

# part b residuals
slr45$residuals

# Residual plot against y hat
plot(slr45, which = 1)  # 1: residuals versus fitted

plot(slr45, which = 2) # 2: QQ plot

plot(slr45, which = 3) # 3: Scale-Location

plot(slr45, which = 4) # 4: Cook's Distance
# Investigate any point over 0.5, values over 1.0 are influential

plot(slr45, which = 5) # 5: Williams-like Graph
# in this plot we are looking for values lying outside dashed line

# Residual plot against x and y-hat
Q45$predicted <- fitted(slr45)
Q45$residuals <- residuals(slr45)

library(tidyverse)

# Residual plot against y hat 
Q45 %>% ggplot(aes(predicted , residuals)) + 
  geom_point()   # we observe a U pattern

# Residual plot against x
Q45 %>% ggplot(aes(x , residuals)) + 
  geom_point()   # we observe a U pattern

# Std Residual plot against y hat
Q45$std_residuals <- rstandard(slr45)
Q45 %>% ggplot(aes(x = predicted, y =std_residuals)) + 
  geom_point()  # we observe a U pattern


# 14-51 ----

Q51 <- data.frame (x = c(4, 5, 7, 8, 10, 12, 12, 22),
                  y = c(12, 14, 16, 15, 18, 20, 24, 19))

slr51 <- lm(y ~ x, data = Q51)
summary(slr51)

# part b std res
Q51$std_rediduals <- rstandard(slr51) # last observation is an outlier
Q51

# part c hat values
Q51$hat <- hatvalues(slr51)  
qplot (Q51$x, Q51$hat) +
  geom_hline(yintercept=0.75, linetype="dashed", color = "red")
# last observation is greater than 6/n (6/8=0.75)

# cook's distance
plot(slr51, which = 5) #last observation greater than 0.5

# 14-53 ----
library(readxl)
Q53 <- read_excel("checkout.xlsx")
summary(Q53)

# rename all variables
colnames(Q53) <- c( "arrival", "shopping")

# part a and b, scatter plot
Q53 %>% ggplot(aes(x = arrival, y = shopping)) + # coordinate system
  geom_point() + # scatter plot
  geom_smooth(method = 'lm', se = TRUE) # some outliers

# part c regression
slr53 <- lm(shopping ~ arrival, data = Q53)
summary(slr53)

# part d residual analysis
plot (slr53, which = 1) # obs 32 is an outlier
plot (slr53, which = 5) # obs 32 is an outlier

# using std residuals
Q53$std_residuals <- rstandard(slr53) #obs 32 is an outlier

# Using Leverage (Hat Values)
Q53$hat <- hatvalues(slr53) 
qplot (Q53$arrival, Q53$hat) +
  geom_hline(yintercept=0.1875, # 6/32 = 0.1875 
             linetype="dashed", color = "red") + # obs 32 is an outlier
# Other sources suggest 2(p+1)/n (p: number of independent variables)
   geom_hline(yintercept=0.125, # 2(p+1)/n, 4/32 = 0.125 
             linetype="dashed", color = "red") # obs 32 is an outlier

# Removing Last Observation
Q53_Rev <- Q53[-32,] # row 32, all columns

# scatter plot
Q53_Rev %>% ggplot(aes(x = arrival, y = shopping)) + # coordinate system
  geom_point() + # scatter plot
  geom_smooth(method = 'lm', se = TRUE) # abline with conf. interval (gray)

slr53_Rev <- lm(shopping ~ arrival, data = Q53_Rev)
summary(slr53_Rev)

# part d residual analysis
plot (slr53_Rev, which = 1)
plot (slr53_Rev, which = 5) #no outlier

# using std residuals
Q53_Rev$std_residuals <- rstandard(slr53_Rev) # no outlier

# 14-68 ----
library(readxl)
Q68 <- read_excel("camry.xlsx")
summary(Q68)

# rename all variables
colnames(Q68) <- c( "miles", "price")

# part a scatter plot
Q68 %>% ggplot(aes(x = miles, y = price)) + # coordinate system
  geom_point() + # scatter plot
  geom_smooth(method = 'lm', se = TRUE) # abline with conf. interval (gray)

# part c regression
slr68 <- lm( price ~ miles, data = Q68)
summary(slr68)
predict(slr68, data.frame(miles=60))

# Buckeye Case ----
library(readxl)
buck <- read_excel("buckeyecreek.xlsx")

# Descriptive Statistics
summary(buck)

# rename all variables
colnames(buck) <- c( "zip", "population", "pass")

# part 1 scatter plot and descriptive statistics
buck %>% ggplot(aes(x = population, y = pass)) + # coordinate system
  geom_point() + # scatter plot
  geom_smooth(method = 'lm', se = TRUE) # abline with conf. interval (gray)

# part 2, 3, 4 Regression
buckeye_slr <- lm(pass ~ population, data = buck)
summary (buckeye_slr)
anova(buckeye_slr)

summary(buckeye_slr)$r.squared
# R-squared:  0.6374 not a very good fit

# part 5 Residuals Analysis
plot(buckeye_slr, which =1) # Fan (Not Good!)
plot(buckeye_slr, which =5) # No outliers

# 1 = residuals versus fitted
# 2 = QQ plot
# 3 = Scale-Location
# 4 = Cook's Distance
# 5 = Williams-like Graph

# part 6, 7 should regression be used? what other data?
# Class discussion
