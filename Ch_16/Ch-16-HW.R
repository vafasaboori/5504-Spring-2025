# Clear the environment and set the working directory ----
rm(list = ls())
setwd("~/Documents/5504 Spring 2025/Ch_16")

# 16-01 ----

# build data frame
Q1 <- data.frame(
  x = c(22,	24,	26,	30,	35,	40),
  y = c(12, 21,	33,	35,	40,	36))

# part a
Q1_model_a <- lm(y ~ x, data = Q1) 
coef(Q1_model_a)

# part b
summary(Q1_model_a) # summary
summary(Q1_model_a)$r.squared # extract r-squared
summary(Q1_model_a)$fstatistic # extract F Statistic
summary(Q1_model_a)$coefficients[2,4] # extract p-value of t-test (same as F in slr)
anova(Q1_model_a)[1,5] # extract p-value of F-test directly
# The relationship between x and y is not significant; the fit is weak

# part c
library(tidyverse)
Q1 %>% ggplot(aes(x, y)) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "tomato") # much better fit
# scatter diagram suggest a form  y = b0 + b1(x) + b2(x^2)
# Because the points are arranged approximately on the parabola

# part d
Q1_model_b <- lm(y ~ poly(x, degree = 2, raw = T), data = Q1)
coef(Q1_model_b)

# part e
summary(Q1_model_b)
summary(Q1_model_b)$r.squared # extract r-squared
summary(Q1_model_b)$fstatistic # extract F Statistic
summary(Q1_model_a)$coefficients[2,4] # extract p-value of 
anova(Q1_model_b)[1,5] # extract p-value of F-test directly
# curvilinear relationship is significant and provides a very good fit

# part f
predict(Q1_model_b, data.frame(x=25))

# 16-02 ----

# build data frame
Q2 <- data.frame(
  x = c(9, 32, 18, 15, 26),
  y = c(10, 20, 21, 16, 22))

# part a
Q2_model_a <- lm(y ~ x, data = Q2) 

summary(Q2_model_a) # not significant
coef(Q2_model_a) # coefficients
summary(Q2_model_a)$r.squared # extract r-squared
summary(Q2_model_a)$adj.r.squared # extract r-squared
anova(Q2_model_a) # anova table

# let's visualize this model
Q2 %>% ggplot(aes(x, y)) + 
  geom_point() +
  geom_smooth(method = "lm") # not so much of a good fit

# part b
Q2_model_b <- lm(y ~ poly(x, degree = 2, raw = T), data = Q2)

summary(Q2_model_b)
coef(Q2_model_b) # coefficients
summary(Q2_model_b)$r.squared # extract r-squared
summary(Q2_model_b)$adj.r.squared # extract r-squared
anova(Q2_model_b) # anova table
# strong relationship with 99.13428% of variation in y explained by x

# let's visualize the quadratic relationship
Q2 %>% ggplot(aes(x, y)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2)) # much better fit

# Comparison between the two models
summary(Q2_model_a)$sigma # s or Residual standard error (RSE)
summary(Q2_model_b)$sigma # s or Residual standard error (RSE)
# RSE square root of the mean squared error (MSE)
# RSE is a measure of variability of the residuals 
# RSE provides an indication of how well the regression model fits the data
# A smaller (comparative) RSE indicates a better fit (smaller residuals)

# part c
predict(Q2_model_b, data.frame(x=20))

# 16-03 ----

# Define the data
Q3 <- data.frame(x = c(2, 3, 4, 5, 7, 7, 7, 8, 9),
                   y = c(4, 5, 4, 6, 4, 6, 9, 5, 11))

# Create a scatterplot with a trend line
library(tidyverse)
ggplot(Q3, aes(x = x, y = y)) +
  geom_point() +  # Add points for the scatterplot
  geom_smooth(method = "lm")
# The scatter diagram shows some evidence of a possible linear relationship.

# b. Fit a linear regression model
Q3_model <- lm(y ~ x, data = Q3)

# Print the summary of the regression model
summary(Q3_model)

# c. Plot standardized residuals versus predicted values
plot(Q3_model, which=3)
# 1 = residuals versus fitted
# 2 = QQ plot
# 3 = Scale-Location
# 4 = Cook's Distance
# 5 = Williams-like Graph

# alt method
# Calculate standardized residuals and predicted values
Q3$s_residuals <- rstandard(Q3_model)
Q3$predicted_values <- fitted(Q3_model)

# Create the plot with ggplot2
ggplot(Q3, aes(x = predicted_values, y = s_residuals)) +
  geom_point() +  # Add points for the scatterplot
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # line at y = 0
  labs(title = "Standardized Residuals vs. Predicted Values",
       x = "Predicted Values", y = "Standardized Residuals")
# residual plot indicates the constant variance assumption is not satisfied (megaphone)

# d. Perform logarithmic transformation on y
Q3$log_y <- log(Q3$y)

# Fit a linear regression model with the transformed variable
Q3_log_model <- lm(log_y ~ x, data = Q3)
summary(Q3_log_model)

plot(Q3_log_model, which=3)
# 1 = residuals versus fitted
# 2 = QQ plot
# 3 = Scale-Location
# 4 = Cook's Distance
# 5 = Williams-like Graph

# alt method
# Calculate standardized residuals and predicted values
Q3$log_residuals <- rstandard(Q3_log_model)
Q3$log_predicted_values <- fitted(Q3_log_model)

# Create the plot with ggplot2
ggplot(Q3, aes(x = log_predicted_values, y = log_residuals)) +
  geom_point() +  # Add points for the scatterplot
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # line at y = 0
  labs(title = "Standardized Residuals vs. Predicted Values",
       x = "Predicted Values", y = "Standardized Residuals")
# residual plot shows that constant variance assumption is not satisfied (megaphone)
# After logarithmic transformation, we still see wedged-shaped pattern in residual plot. 

# Perform reciprocal transformation on y
Q3$reciprocal_y <- 1/Q3$y
# Fit a linear regression model with the reciprocal transformed variable
Q3_reciprocal_model <- lm(reciprocal_y ~ x, data = Q3)
summary(Q3_reciprocal_model)

plot(Q3_reciprocal_model, which=3)
# 1 = residuals versus fitted
# 2 = QQ plot
# 3 = Scale-Location
# 4 = Cook's Distance
# 5 = Williams-like Graph

# alt method
# Calculate standardized residuals and predicted values
Q3$rec_residuals <- rstandard(Q3_reciprocal_model)
Q3$rec_predicted_values <- fitted(Q3_reciprocal_model)

# Create the plot with ggplot2
ggplot(Q3, aes(x = rec_predicted_values, y = rec_residuals)) +
  geom_point() +  # Add points for the scatterplot
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # line at y = 0
  labs(title = "Standardized Residuals vs. Predicted Values",
       x = "Predicted Values", y = "Standardized Residuals")
# The reciprocal transformation remove the wedge-shaped pattern.
# However Neither transformation provides a good fit.

# 16-06 ----

# build data frame
Q6 <- data.frame(
  fac = c(9, 11, 16, 21, 27, 30),
  dist = c(1.66, 1.12, 0.83, 0.62, 0.51, 0.47))

# part b
plot(Q6)
# slr not appropriate, the relationship appears to be curvilinear

# Let's visualize linear, quadratic, and reciprocal fit
library(tidyverse)

ggplot(Q6, aes(x = fac, y = dist)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "skyblue") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "dodgerblue") +
  geom_smooth(method = "lm", formula = y ~ I(1 / x), se = FALSE, color = "midnightblue")  
# I(1 / x) ensures R treats 1/x as a mathematical transformation, not a formula operator

# R offers about 657 colors. You can see all of them using:
colors()

# port c
# quadratic
Q6_model_a <- lm(dist ~ poly(fac, degree = 2, raw = T), data = Q6)
summary(Q6_model_a)
coef(Q6_model_a) # coefficients

# let's try reciprocal
Q6_model_b <- lm(dist ~ I(1/fac), data = Q6)
summary(Q6_model_b)
coef(Q6_model_b) # coefficients

# let's compare r-squard values
summary(Q6_model_a)$adj.r.squared # Adj r-squared 91%
summary(Q6_model_b)$adj.r.squared # Adj r-squared 96%


# 16-07 ----
# Load the necessary library for reading Excel files
library(readxl)

# Read the data from the Excel file
Q7 <- read_excel("WashingMachines.xlsx")

# Check the structure of the data
str(Q7) # no. of observations, no. of variables, names and type of variables

colnames(Q7)[3] <- "Price"

# a. Develop a scatter plot
plot(Q7$Capacity, Q7$Price, 
     main = "Scatterplot of Washing Machine Capacity vs. Price",
     xlab = "Capacity (cubic feet)", ylab = "Price ($)")

# alt visulaization:
library(tidyverse)

# Develop a scatter plot using ggplot
ggplot(Q7, aes(x = Capacity, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "salmon") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, col = "firebrick")
# 2nd-order poly curve did a better fit, simple linear regression may not be appropriate.

# b. Fit a simple linear regression model
Q7_model_linear <- lm(Price ~ Capacity, data = Q7)
summary(Q7_model_linear)

# Plot standardized residuals
plot(Q7_model_linear, which=3)
# 1 = residuals versus fitted
# 2 = QQ plot
# 3 = Scale-Location
# 4 = Cook's Distance
# 5 = Williams-like Graph

# alt method
Q7$residuals <- rstandard(Q7_model_linear)
Q7$predicted_values <- fitted(Q7_model_linear)

# Create the plot with ggplot2
ggplot(Q7, aes(x = predicted_values, y = residuals)) +
  geom_point() +  # Add points for the scatterplot
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # line at y = 0
  labs(x = "Predicted Values", y = "Standardized Residuals") 
# The curvature suggests that a simple linear regression may not be appropriate 

# c. Fit a second-order polynomial regression model
Q7_model_poly <- lm(Price ~ poly(Capacity, degree = 2, raw = T), data = Q7)
summary(Q7_model_poly)

# Plot standardized residuals
plot(Q7_model_poly, which=3)
# 1 = residuals versus fitted
# 2 = QQ plot
# 3 = Scale-Location
# 4 = Cook's Distance
# 5 = Williams-like Graph

# alt method
Q7$poly_residuals <- rstandard(Q7_model_poly)
Q7$poly_predicted_values <- fitted(Q7_model_poly)

# Create the plot with ggplot2
ggplot(Q7, aes(x = poly_predicted_values, y = poly_residuals)) +
  geom_point() +  # Add points for the scatterplot
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # line at y = 0 +
  labs(x = "Predicted Values", y = "Standardized Residuals")
# The curvature is gone


# 16-08----

# read excel file
library(readxl)
classic <- read_excel("ClassicCars.xlsx")
glimpse(classic)
head(classic)

# change column names
colnames(classic)[5] = "Price"
head(classic)

# part a, scatter plot 
classic %>% ggplot(aes(x=Rating, y=Price)) + 
  geom_point() +
  geom_smooth(method = "lm")
# slr model does not appear to be appropriate.

# part b
classic_model_a <- lm(Price ~ poly(Rating, degree = 2, raw = T),
                      data = classic)

summary(classic_model_a) # coefficients
summary(classic_model_a)$r.squared # r-squared
summary(classic_model_a)$fstatistic # F
anova(classic_model_a) # anova table
anova(classic_model_a)[1,5] # p-value for F

# Visualization
classic %>% ggplot(aes(x=Rating, y=Price)) + 
  geom_point() +
  geom_smooth(method = "lm", col="lightgreen") +
  geom_smooth(method = "lm",formula = y ~ poly(x, 2), col="darkgreen") 

# part c
# Consider the nonlinear relationship in equation 16.7, log both DV and IVs
# Use logarithms to develop an estimated regression equation for this model.

classic_model_b <- lm(log(Price) ~ log(Rating), data = classic)
summary(classic_model_b) # coefficients
summary(classic_model_b)$r.squared # r-squared (better model)
summary(classic_model_b)$fstatistic # F
anova(classic_model_a)[1,5] # p-value for F almost 0

# part d, The model in part (c) is preferred because it provides a better fit.

# 16-10 ----

# Part A (y = 25.2 + 5.5X1)
# Given values
SST_reduced <- 1550
SSE_reduced <- 520
n <- 27
p_reduced <- 1

# Calculate degrees of freedom
DF1_reduced <- p_reduced
DF2_reduced <- n - p_reduced - 1

# Calculate F-statistic
F_reduced <- ((SST_reduced - SSE_reduced) / DF1_reduced) / (SSE_reduced / DF2_reduced)
F_reduced

# Calculate p-value
p_value_reduced <- 1 - pf(F_reduced, DF1_reduced, DF2_reduced)
p_value_reduced # Because p-value ≤ alpha, x1 is significant.

# Part B (y = 16.3 + 2.3X1 + 12.1X2 - 5.8X3)
# Given values
SST_full <- 1550
SSE_full <- 100
p_full <- 3

# Calculate degrees of freedom
DF1_full <- p_full
DF2_full <- n - p_full - 1

# Calculate F-statistic for comparing two models (Formula 16.15)
F_comp <- (((SSE_reduced - SSE_full) / (p_full - p_reduced)) / (SSE_full / DF2_full))
F_comp

# Calculate p-value
p_value <- 1 - pf(F_comp, p_full - p_reduced, DF2_full)  # Fixed the degrees of freedom
p_value # Because p-value ≤ alpha, addition of x2 and x3 is significant.

# 16-11 ----
# Full Model (y = 17.6 + 3.8X1 - 2.3X2 + 7.6X3 + 2.7X4)
# Given values 
SST_a <- 1805
SSR_a <- 1760
n <- 30
p <- 4  # number of independent variables in the model

# Calculate the degrees of freedom
DF1_a <- p
DF2_a <- n - p - 1

# Compute the F-statistic for full model
F_a <- (SSR_a / DF1_a) / ((SST_a - SSR_a) / DF2_a)
F_a

# Calculate the p-value for full model
p_value_a <- 1 - pf(F_a, DF1_a, DF2_a)
p_value_a # Because p-value ≤ alpha, the overall relationship is significant.

# Compute SSE for full model
SSE_a <- SST_a - SSR_a
SSE_a

# Reduced Model (y =11.1 - 3.6X2 + 8.1X3)
# Given values 
SST_b <- 1805
SSR_b <- 1705

# Compute SSE for the reduced model
SSE_b <- SST_b - SSR_b
SSE_b

# Compare two models
DF1_b <- 2  # Since x1 and x4 are dropped from the model
DF2_b <- n - DF1_b - 1

# Compute the F-statistic for comparing two models
# F = [SSE_reduced - SSE_full/No. of extra terms] / MSE_full
MSE_a = SSE_a / DF2_a
MSE_a
F_comp <- ((SSE_b - SSE_a) / 2) / MSE_a
F_comp

#Calculate the p-value for comparing two models
p_value_comp <- 1 - pf(F_comp, DF2_b, DF2_a)
p_value_comp


# 16-15 ----

# Load the required library
library(readxl)

# Load the data from the Excel file
Q15 <- read_excel("NaturalGas.xlsx")

# Check the structure of the data
str(Q15)

#Change column names
colnames(Q15) <- c("bill", "age", "sqft", "rooms")

# a. Develop an estimated regression equation using only age
Q15_model_a <- lm(bill ~ age, data=Q15)
summary(Q15_model_a)

# b. Develop an estimated regression equation using age, square footage, and number of rooms
Q15_model_b <- lm(bill ~ age + sqft + rooms, data=Q15)
summary(Q15_model_b)

# c. additional variables (sqft and rooms) contribute significantly to the model?
anova(Q15_model_a, Q15_model_b)

# Refresher on DOE ----
# First let's review the concepts (slides and excel)

# Section 13-5 DOE (GMAT) ----

library(readxl)
gmat <- read_excel("gmatstudy.xlsx")
head(gmat)

# This format is not useful in R, we need to reshape
library(reshape2)
gmat_a <- melt(gmat)

# change column names
colnames(gmat_a) <- c("Prep", "Major", "GMAT")

# convert prep to factor
gmat_a$Prep <- as.factor(gmat_a$Prep)
levels(gmat_a$Prep)

# convert major to factor
gmat_a$Major <- as.factor(gmat_a$Major)
levels(gmat_a$Major)

gmat_anova <- aov(GMAT ~ Major*Prep, data = gmat_a) # Analysis of Variance
summary(gmat_anova)

# DOE Using Regression
gmat_model <- lm(GMAT ~ Prep*Major, data = gmat_a) # both variables and interaction
anova(gmat_model)
#Preparation: not sig. / Major: sig. / Interaction: not sig.

# Section 9-2 DOE using Regression (Chemitech) ----

# First let's review the concept (slides and excel)

library(readxl)
chem <- read_excel("chemitech.xlsx")
head(chem)

# This format is not useful in R, we need to reshape
library(reshape2)
chem_a <- melt(chem)

# Rename "Method A" to "A"
chem_a$method <- c(rep("A",5), rep("B",5), rep("C", 5))

# convert "method" to factor
chem_a$method <- as.factor(chem_a$method)
levels(chem_a$method)

colnames(chem_a)[2] <- "units"
chem_model <- lm(units ~ method, data = chem_a)
anova(chem_model)

# 16-23 ----

# create data set
jacobs <- data.frame(manuf = c(rep("A",4), rep("B",4), rep("C", 4)),
            time = c(20,26,24,22, 28,26,31,27,20,19,23,22))

# convert to factor (categorical var)
jacobs$manuf <- as.factor(jacobs$manuf)
levels(jacobs$manuf)

# In case you want to relevel
# jacobs$manuf <- relevel(jacobs$manuf, ref = "B")
# levels(jacobs$manuf)

# method 1 (without dummy variables)
jacobs_model <- lm(time ~ manuf, data = jacobs)
anova(jacobs_model)

# method 2 (create dummy variables -- optional)
dummy_matrix <- model.matrix(~ manuf , data=jacobs)

jacob_dummies <- as.data.frame(dummy_matrix)
colnames(jacob_dummies) = c("I", "D1", "D2")

# combine dummies with original data
jacobs_1 <- cbind (jacobs, jacob_dummies)

jacobs_1_model <- lm(time ~ D1 + D2 , data = jacobs_1)
summary(jacobs_1_model)
anova(jacobs_1_model)

# null hyp: b1 = b2 = 0
summary(jacobs_1_model)$fstatistic # F (sig at 0.01) reject null

# 16-25 ----

# build data frame
auto <- data.frame( 
  analyzer = factor(rep(c("computer", "electronic"), times = 3)),
       car = factor(c(rep("compact", 2), rep("inter", 2), rep("full", 2))),
      time = c(50, 42, 55, 44, 63, 46))# create data set
  
# ANOVA
auto_model <- lm(time ~ analyzer + car, data = auto)
anova(auto_model)

# 16-28 ----

# Cravens
library(readxl)
cravens <- read_excel("cravens.xlsx")
glimpse(cravens)
head(cravens)

# Model with Accounts, AdvExp, Poten, and Share
cravens_model <- lm(Sales ~ Accounts + AdvExp + Poten + Share , data = cravens)
summary(cravens_model)

# DW Test for Auto-correlation
library(lmtest)
dwtest(cravens_model) 
# At the .05 level of significance, dL = 1.04 and dU = 1.77.
# Because dL ≤ d ≤ dU, the test is inconclusive. 
# R says that we cannot reject null (null is "we do not have autocorrelation")
# However, these data are not a time series!
# Autocorrelation is not a concern and it is not appropriate to perform a Durbin-Watson test.

# 16-29 ----

library(readxl)
bonds <- read_excel("CorporateBonds.xlsx")
glimpse(bonds)

# Change col. names
colnames(bonds) = c("tic", "yrs", "yld")

# part a. scatter plot
bonds %>% ggplot(aes(x= yrs, y= yld)) +
  geom_point() +
  geom_smooth(method = lm)

# A simple linear regression model does not appear to be appropriate.

# part b. quadratic regression
bonds_model_a <- lm(yld ~ poly(yrs, degree = 2, raw = T), data = bonds)

summary(bonds_model_a)
summary(bonds_model_a)$r.squared 
summary(bonds_model_a)$fstatistic
anova(bonds_model_a)
anova(bonds_model_a)[1,5] # extract p-value of F-test directly

# part c. log regression
bonds_model_b <- lm(yld ~ log(yrs), data = bonds)

summary(bonds_model_b)
summary(bonds_model_b)$r.squared # r-squared (better model)
summary(bonds_model_b)$fstatistic # F
anova(bonds_model_b)[1,5] # p-value for 

# Log model provides a better fit than second order, r-sq higher

# Visualization
bonds %>% ggplot(aes(x= yrs, y= yld)) +
  geom_point() +
  geom_smooth(method = lm, col="lightgray", se=FALSE) +
  geom_smooth(method = lm, formula = y ~ poly(x, 2), col="red", se=FALSE) +  
  geom_smooth(method = lm, formula = y ~ log(x), col="blue", se=FALSE)  
  
  
# 16-32 ----
library(readxl)
audit <- read_excel("audit.xlsx")
glimpse(audit)

# part a, b regression
audit_model_a <- lm(Delay ~ Industry + Public + Quality + Finished, data = audit)
summary(audit_model_a)
summary(audit_model_a)$r.squared # not a good fit, non-linear

# part c, scatter plot
audit %>% ggplot(aes(x=Finished, y=Delay)) + 
  geom_point() +
  geom_smooth(method = lm) +
  geom_smooth(method = lm, formula = y ~ poly(x, 2), col = "red")
# The scatter diagram suggests a curvilinear relationship between these two variables.
  
# part d

# add fifth IV Finished^2
audit$Finished2 <- audit$Finished^2

# regression
audit_model_b <- lm(Delay ~ Industry + Public + Quality + Finished + Finished2, data = audit)
summary(audit_model_b)

# best subset
# olsrr package
library(olsrr) # ordinary least squares regression models

# Forward Selection using p-values
ols_step_forward_p(audit_model_b, penter=.05, details = TRUE)

# Backward Elimination using p-values
ols_step_backward_p(audit_model_b, prem=.05, details = TRUE)

# Stepwise regression using p-values
ols_step_both_p(audit_model_b, pent=.05, prem=.05, details = TRUE)

# Best subsets regression
ols_step_best_subset(audit_model_b, details = TRUE)
# We look for high r-squared and low AIC, SBIC, and SBC

# best model with 2 IV: Finished and Finished-Squared
# best model with 3 IV: Industry, Finished and Finished-Squared
# Using the best subset regression procedure, 4 IVs in the highest adjusted  model
# R^2 for model 4 is: 48.29%

# 16-34 ----

library(readxl)
audit <- read_excel("audit.xlsx")
glimpse(audit)

# part a, regression
audit_model_c <- lm(Delay ~ Industry + Quality, data = audit)
summary(audit_model_c) # coefficients

# part b, residual plot against order of data
plot(audit_model_c, which =1) # 1 = residuals versus fitted
# This is not really what the question asks for, on x we need to have order of data

# alt method
audit$Residuals <- residuals(audit_model_c)

ggplot(audit, aes(x = 1:nrow(audit), y = Residuals)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Order of Data", y = "Residuals") 
# There is no obvious pattern in the data indicative of positive autocorrelation.
# Although this is not a time series data, however
# Even in non-time-series data, autocorrelation in residuals can occur due to hidden patterns 

# part c, DW Test for Auto-correlation
library(lmtest)
dwtest(audit_model_c)
# At alpha = .05 and n = 40, and 2 IVs, dL = 1.39 and dU = 1.60 (Table 16-10)
# our dw = 1.4261, because dL ≤ d ≤ dU, the test is inconclusive
# Caveat!
# R says: at the .05 level of significance, there is evidence of auto-correlation

# 16-35 ----

library(readxl)
fuel <- read_excel("FuelEconomy2019.xlsx")
glimpse(fuel)

# change column names
colnames(fuel)[4] = "MPG"

# convert Class to factor (categorical var)
fuel$Class <- as.factor(fuel$Class)
levels(fuel$Class)

# Regression DOE
fuel_model <- lm(MPG ~ Class , data = fuel)
summary(fuel_model)
anova(fuel_model) 
# overall model significant
# p-values for all of the independent variables are significant except Standard SUV 2WD.
# Therefore, no difference in mpg between the Standard SUV 2WD and Standard SUV 4WD
# But there is a difference between the Standard SUV 4WD and the other eight car classes. 


# Case Study Piedmont Wine ----

library(readxl)
wine <- read_excel("WineRatings.xlsx")
glimpse(wine)

# Q1 1.	A table showing the number of wines and average price for each rating

# R for Excel Users Book: https://rstudio-conf-2020.github.io/r-for-excel/
# Pivot table in R: https://rstudio-conf-2020.github.io/r-for-excel/pivot-tables

library(dplyr) # PAckage for data wrangling
?dplyr

# Pivot table using "group_by()", then "summarize()"
wine %>%
  group_by(Rating) %>%
  summarise(count = n(), avg_price = mean(Price))
# None of the wines reviewed received a Not Recommended rating 
# Only one wine was rated Mediocre.
# Overall, 85% of the wines received a Very Good or Outstanading rating. 
# With  exception of 1 mediocre wine, price is greater for wines that are rated higher.

# Q2 Scatter diagram
plot(wine$Price, wine$Score)

# Alt Method
wine %>% ggplot(aes(x = Price, y = Score)) + 
  geom_point() 
# The relationship between price and score does not appear to be linear. 
# The scatter diagram indicates a curviliniear relationship between the two variables.

# Q3 linear regression
wine_model_1 <- lm(Score ~ Price, data = wine)
summary(wine_model_1)

# visualization
wine %>% ggplot(aes(x = Price, y = Score)) + 
  geom_point() +
  geom_smooth(method = "lm")

# Q4 Second Order (Quadratic) Regression
wine_model_2 <- lm(Score ~ poly(Price, degree=2, raw=T), data = wine)
summary(wine_model_2)

# Q5 Model 2 is better fit, R-Sq (adj) = 51.35% compared to R-Sq = 40.62% for slr

# let's plot them on the same coordiane
wine %>% ggplot(aes(x = Price, y = Score)) + 
  geom_point() +
  geom_smooth(method = "lm", se=FALSE, col = "lightgray") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se=FALSE)

# Q6 log model
wine_model_3 <- lm(Score ~ log(Price), data = wine)
summary(wine_model_3)

# Log model gives slightly better fit with R-Sq (adj) = 57.2%.

# Q7, Spending more for a bottle of wine will, in general, provide a better wine. 

# Q8 buying a wine below $30

library(dplyr)
wine_1 <- filter(wine, Price <=30)
plot(wine_1$Price, wine_1$Score)
# not much relationship between price and score for this group of wines. 
# You could probably pick a wine at random from this group!

# Extra Steps
# Let's do a visualization for price and score separated by rating
library("ggplot2")

wine %>% 
  ggplot(aes(x = Price, y = Score, color = Rating)) + #different ratings in different color
  geom_point(size=3, alpha=.5) +
  geom_smooth(method = "lm", se= FALSE) 

# Let's do multiple graphs (facet wrap)  for price and score separated by rating
wine %>% 
  ggplot(aes(x = Price, y = Score)) +
  geom_point(size=1, alpha=.5) + # data points with size 1 and semi-transparent
  geom_smooth(method = "lm") +
  facet_wrap(~ Rating, scales = "free") # breaks data into different groups and plots them
# scales = "free" allows both the x and y axes to be on different scales for each facet


# In Class Practice (Case Study1: LPGA) ----

# Include the following in your report
# Descriptive statistics and the sample correlation coefficients for the data (interpret)
# SLR model with best IV (Interpret r-squared)
# Investigate other IVs using all methods including stepwise regression procedure (interpret)
# What IVs combination give you the best result?
# Prepare a report that summarizes your analysis, conclusions, and recommendations.

# Solution (Case Study1: LPGA) ----
# Step 1: Load the dataset
library(readxl)
LPGA <- read_excel("TourLPGA2012.xlsx")

# Step 2: Explore the structure and summary statistics
str(LPGA)
summary(LPGA)
colnames(LPGA) <- c("Player", "ScoringAverage", "DrDist", "DrAccu", "GIR", "SandSaves", "PPR")

# Step 3: Correlation analysis
round(cor(LPGA[-1]),3)

# alt method
# Correlation Matrix (alt method)
library(PerformanceAnalytics)
chart.Correlation(LPGA[-1],
                  histogram = TRUE)

# Correlation matrix shows that GIR is highly correlated with scoring average
# Thus, the best slr uses GIR to predict scoring average. 

# Fit a simple linear regression model
model_gir <- lm(ScoringAverage ~ GIR, data=LPGA)
summary(model_gir)
# This slr is able to explain approximately 71% of the variation in scoring average.

# For var selection we used stepwise regression procedure with Alpha = .05 
library(olsrr)

# Fit the initial regression model (full)
full_model <- lm(ScoringAverage ~ DrDist + DrAccu + GIR + SandSaves + PPR, data=LPGA)
summary(full_model)

# Perform forward selection using p-values
ols_step_forward_p(full_model, penter=0.05, details = TRUE)
# GIR and PPR can be used with an adjusted R2 = .9598, significant

# Backward Elimination using p-values
ols_step_backward_p(full_model, prem=.05, details = TRUE)
# SandSaves, DrAccu, and DrDist can be used with an adjusted R2 = .9604, significant

# Stepwise regression using p-values
ols_step_both_p(full_model, pent=.05, prem=.05, details = TRUE)
# GIR and PPR can be used with an adjusted R2 = .96, significant

# Best subsets regression
ols_step_best_subset(full_model, details = TRUE)
# We look for high r-squared and low AIC, SBIC, MSEP, FPE, and SBC

# Model Selection Criteria (C(p), AIC, SBIC, SBC)
#	C(p): Ideally close to the number of IVs. Model 2 C(p) = 5.4392 is closest
# AIC: Lower values indicate better models. Model 3 has the lowest (37.7957)
#	SBIC & SBC: Lower values are better. Model 2 and 3 have the lowest values.

# Prediction Performance (MSEP, FPE, HSP, APC)
# MSEP: Lower values shows how well the model predicts new data. Model 5 has the lowest value (10.1989)
# FPE: Lower value is better. Model 3 performs slightly better than Model 5.
# HSP & APC: Lowe values are better, Model 3 has the lowest values, supporting its optimal predictive capability.

# Interpretation & Best Model Choice
# Model 1 (Single Predictor) has weak performance (low R², high prediction error).
# Model 2 and 3 offer the best trade-off between predictive power and complexity
# Models 4 and 5 do not provide substantial improvements but introduce additional complexity.

# Thus, Model 2 seems to be the best choice, offering high explanatory power while avoiding overfitting.

LPGA_model_2 <- lm(ScoringAverage ~ GIR + PPR, data=LPGA)
summary(LPGA_model_2)
anova(LPGA_model_2)

# Residual Analysis
# Check for assumptions (e.g., linearity, homoscedasticity, normality of residuals)
plot(LPGA_model_2, which = 1) # 1 = residuals versus fitted
plot(LPGA_model_2, which = 2) # 2 = QQ plot
plot(LPGA_model_2, which = 3) # 3 = Scale-Location
plot(LPGA_model_2, which = 4) # 4 = Cook's Distance
plot(LPGA_model_2, which = 5) # 5 = Williams-like Graph

