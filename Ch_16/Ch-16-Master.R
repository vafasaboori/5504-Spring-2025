# Clear the environment and Set the working directory ----
rm(list = ls())
setwd("~/Documents/5504 Spring 2025/Ch_16")

# Section 1 Modeling Curvilinear Relationships ----

# Read Excel file in R
library(readxl)
reynolds <- read_excel("reynolds.xlsx")
head(reynolds)

# Simple linear regression
reynolds_model <- lm(Sales ~ Months, data = reynolds)
reynolds_model$coefficients # coefficients
summary(reynolds_model)

# Residual Analysis
plot(reynolds_model, which = 1) # 1 = residuals versus fitted (violation)
plot(reynolds_model, which = 3) # 3 = Scale-Location
# 1 = residuals versus fitted
# 2 = QQ plot
# 3 = Scale-Location
# 4 = Cook's Distance
# 5 = Williams-like Graph

# Residual Analysis Alt Method
reynolds$predicted <- fitted(reynolds_model)
reynolds$residuals <- residuals(reynolds_model)
reynolds$std_residuals <- rstandard(reynolds_model)

library(tidyverse)
reynolds %>% ggplot(aes(x = predicted, y = std_residuals)) + 
  geom_point() +
  geom_smooth(method = "gam", se = TRUE)  # std residual analysis shows curvilinear pattern
# Generalized Additive Model (GAM) 
# GAM is a flexible model for complex non-linear relationships without a specific form.

# Other methods:
# loess: The locally weighted scatterplot smoothing (LOESS) non-parametric regression method 
# glm: The generalized linear model (GLM) flexible various types of response variables
# rlm: Robust regression robust to outliers data with a high degree of noise.
# lm: Linear regression is a basic regression method that fits a linear relationship 
# auto: Automatically choose a smoothing method based on the size and structure of the data.

# Modeling Curvelinear relationship

# First Method
reynolds$Months2 <- reynolds$Months^2
reynolds_model_a <- lm(Sales ~ Months + Months2, data = reynolds)
summary(reynolds_model_a)
plot(reynolds_model_a, which = 1) # 1 = residuals versus fitted (violation)
plot(reynolds_model_a, which = 3) # 3 = Scale-Location

# Second Method (More Efficient Method)
reynolds_model_b <- lm(Sales ~ poly(Months, degree = 2, raw = T), data = reynolds)
# raw = T is the polynomial model
# If raw = F Orthogonal Model (Beyond Scope)
?poly
summary(reynolds_model_b)
plot(reynolds_model_b, which = 1) # 1 = residuals versus fitted (violation)
plot(reynolds_model_b, which = 3) # 3 = Scale-Location

# Section 2 Modeling Interaction ----

# Read Excel file in R
library(readxl)
tyler <- read_excel("tyler.xlsx")
glimpse(tyler)

# visualization (scatter plot)
tyler %>% ggplot(aes(x= Price, y= Sales,
                     color = Advert)) +
  geom_point() +
  geom_smooth(method = lm)

# alt method (seperate scatterplots)
tyler$Advert_cat <- as.factor(tyler$Advert)

tyler %>% ggplot(aes(x= Price, y= Sales,
                     color = Advert_cat)) +
  geom_point() +
  geom_smooth(method = lm)

# visualization (clustered column)
tyler %>% ggplot(aes(x = Price, y = Sales, fill = Advert_cat)) +
  geom_col(position = "dodge") + # dodge: columns placed side by side rather than stacked.
  labs(x = "Price", y = "Sales")

# regression model
tyler_model <- lm(Sales ~ Price + Advert + Price*Advert, data = tyler)
summary(tyler_model)
# Price:Advert p-value is 0.0000, interaction is significant 
# Effect of advertising expenditure on sales depends on the price.
# For a unit increase in Price.Advert (all other constant) Sales decrease by 6.08 units.

# Section 3 Transformation ----
library(readxl)
mpg <- read_excel("mpgweight.xlsx")
glimpse(mpg)
head(mpg)

# visualization
mpg %>% ggplot(aes(x= Weight, y= MPG)) +
  geom_point() +
  geom_smooth(method = lm)

# regression model
mpg_model <- lm(MPG ~ Weight, data = mpg)
summary(mpg_model)

# Residual Analysis
plot(mpg_model, which = 3)
# 1 = residuals versus fitted
# 2 = QQ plot
# 3 = Scale-Location
# 4 = Cook's Distance
# 5 = Williams-like Graph

# Residual Analysis Alt Method
mpg$predicted <- fitted(mpg_model)
mpg$residuals <- residuals(mpg_model)
mpg$std_residuals <- rstandard(mpg_model)

library(tidyverse)
mpg %>% ggplot(aes(x = predicted, y = std_residuals)) + 
  geom_point() + 
 scale_y_continuous(limits = c(-3, 3))# we observe wedge pattern

# This can be corrected by transforming the dependent variable to ln scale.
mpg$LnMPG <- log(mpg$MPG)
# log() returns the natural in R (base e)
# The log10() function in R uses base 10 to calculate the logarithm of a number. 

# revised regression
mpg_model_a <- lm(LnMPG ~ Weight, data = mpg)
summary(mpg_model_a)

# Residual Analysis
plot(mpg_model_a, which = 3)
# 1 = residuals versus fitted
# 2 = QQ plot
# 3 = Scale-Location
# 4 = Cook's Distance
# 5 = Williams-like Graph

# Residual Analysis Alt Method
mpg$predicted_ln <- fitted(mpg_model_a)
mpg$residuals_ln <- residuals(mpg_model_a)
mpg$std_residuals_ln <- rstandard(mpg_model_a)

library(tidyverse)
mpg %>% ggplot(aes(x = predicted_ln, y = std_residuals_ln)) + 
  geom_point() + 
  scale_y_continuous(limits = c(-3, 3))# we do not observe wedge pattern anymore

# Section 4 In Class Practice (Q5: Vehicle Speed and Traffic Flow) ----
traffic <- data.frame(
  speed = c(35, 40, 30, 45, 50, 25),
  flow = c(1256, 1329, 1226, 1335, 1349, 1124))

# visualization
traffic %>% ggplot(aes(x= speed, y= flow)) +
  geom_point() +
  geom_smooth(method = lm)

# simple linear regression model
traffic_model <- lm(flow ~ speed, data = traffic)
summary(traffic_model)

# Residual Analysis
plot(traffic_model, which = 1)
# 1 = residuals versus fitted
# 2 = QQ plot
# 3 = Scale-Location
# 4 = Cook's Distance
# 5 = Williams-like Graph

# Residual Analysis (Alt Methodm, Std Residuals)
traffic$predicted <- fitted(traffic_model)
traffic$residuals <- residuals(traffic_model)
traffic$std_residuals <- rstandard(traffic_model)

library(tidyverse)
traffic %>% ggplot(aes(x = predicted, y = std_residuals)) + 
  geom_point() + 
  geom_smooth(se = FALSE) # we observe a curviliniar pattern 

# curvilinear estimated regression equation.
traffic$Speed2 <- traffic$speed^2
traffic_model_a <- lm(flow ~ speed + Speed2, data = traffic)
summary(traffic_model_a)

# Alt Method
traffic_model_b <- lm(flow ~ poly(speed, degree = 2, raw = T), data = traffic)
# raw=T is the polynomial model
#If raw=F Orthogonal Model (Beyond Scope)

summary(traffic_model_b)

# prediction
predict(traffic_model_b, data.frame(speed=38), interval = "confidence")

# Residual Analysis
plot(traffic_model_b, which = 1)
# 1 = residuals versus fitted
# 2 = QQ plot
# 3 = Scale-Location
# 4 = Cook's Distance
# 5 = Williams-like Graph

# Residual Analysis Alt Method
traffic$predicted_b <- fitted(traffic_model_b)
traffic$residuals_b <- residuals(traffic_model_b)
traffic$std_residuals_b <- rstandard(traffic_model_b)

library(tidyverse)
traffic %>% ggplot(aes(x = predicted_b, y = std_residuals_b)) + 
  geom_point() +
  geom_hline(yintercept = 0, 
             linetype="dotted", 
             color = "blue", 
             size=1.5) # we do not observe a curviliniar pattern 


# Section 5 Variable Selection Procedures (Q14: Stroke) ----

# part a
library(readxl)
stroke <- read_excel("stroke.xlsx")
glimpse(stroke)
head(stroke)

# Rename column
colnames(stroke)[3] = "BP"

# visualization
stroke %>% ggplot(aes(x= Age, y= Risk,
                     color = BP)) +
  geom_point() +
  geom_smooth(method = lm)

# alt method
stroke$BP_cat <- cut(stroke$BP, breaks = c(0, 120, 140, 500),
                     labels = c("Normal", "Stage 1 Hyp", "Stage 2 Hyp"))

stroke %>% ggplot(aes(x= Age, y= Risk,
                 col = BP_cat)) +
  geom_point() +
  geom_smooth(method = "glm")

# Regression
stroke_model_a <- lm(Risk ~ Age + BP, data = stroke)
summary(stroke_model_a)

# part b add two IVs: 1-interaction between age and BP, 2-smoker
stroke_model_b <- lm(Risk ~ Age + BP + Smoker + Age*BP, data = stroke)
summary(stroke_model_b)

#You can turn off scientific notation for numbers using the option below;
options(scipen = 999)
#That would make all the numbers to appear as decimals.
#If you want to revert it back to the default, use
options(scipen = 0)

#scipen: A penalty to be applied when deciding to print numeric values in exponential notation. 
# Positive values bias towards fixed and negative towards scientific notation
# fixed notation will be preferred unless it is more than scipen digits wider.

options(scipen = 999)
summary(stroke_model_b)

# part c, test if the addition of the interaction and the smoker contribute significantly

library(car) # companion to applied regression
anova(stroke_model_a)
anova(stroke_model_b)
# At this point you can read SSE(reduced), SSE(full), calculate MSE, etc.

# alt method (more efficient)
# first build a new interaction variable
stroke$Age.BP <- stroke$Age * stroke$BP
stroke_model_c <- lm(Risk ~ Age + BP + Smoker + Age.BP, data = stroke)
summary(stroke_model_c)

# install.packages("car")
library(car)
Added_IV <- c("Smoker", "Age.BP")
linearHypothesis(stroke_model_c, Added_IV)
# Addition of the two terms is significant
# linearHypothesis computes a F statistic for comparing a full model and a reduced model

# Section 6 Large Sample Var Selection (Cravens Dataset) ----

# part a
library(readxl)
cravens <- read_excel("cravens.xlsx")
glimpse(cravens)
head(cravens)

# Correlation Matrix
round(cor(cravens), 3)

# Visualization
library(corrplot)
corrplot(cor(cravens), method = "number")

# Correlation Matrix (alt method)
library(PerformanceAnalytics)
chart.Correlation(cravens,
                  histogram = TRUE)
# correlation between Time and Accounts is .758 (multicolinearity)
# If Accounts is an IV, Time would not add much more explanatory power to the model.

# Regression with all variables anyway
cravens_model_1 <- lm(Sales ~ ., data = cravens)
summary(cravens_model_1)

# Include only significant IVs
cravens_model_2 <- lm(Sales ~ Poten + AdvExp + Share, data = cravens)
summary(cravens_model_2)
# How can we find a regression that fits best?
# One approach is to compute all possible regressions and compare them.
# But doing so involves a great amount of computation and review.
# For Cravens data, 255 different estimated regression equations exist
# Statisticians prefer a more systematic approach called variable selection procedure.

# Section 7 Variable Selection: Best-Subsets Regression (RegData Dataset) ----

# load data file
load("regdata.RData")

# DV is achievement, and IVs are mastery, anxiety, etc.
ob_model <- lm(achieve ~ mastery + interest + anxiety + perfgoal + genderid, data=regdata)
summary(ob_model)

# olsrr package
library(olsrr) # ordinary least squares regression models

# Forward Selection using p-values
ols_step_forward_p(ob_model, penter=.05, details = T)

# Backward Elimination using p-values
ols_step_backward_p(ob_model, prem=.05, details = T)

# Stepwise regression using p-values
ols_step_both_p(ob_model, pent=.05, prem=.05, details = T)

# Best subsets regression
ols_step_best_subset(ob_model)
# We look for high r-squared and low AIC, SBIC, and SBC
# Using this criteria, model 3 seems to be the best model

# Section 8 DOE using Regression (Chemitech) ----
library(readxl)
chem <- read_excel("chemitech.xlsx")
glimpse(chem)

# This format is not useful in R, we need to reshape
library(reshape2)
chem_a <- melt(chem)

# Rename Method A to A
chem_a$method <- c(rep("A",5), rep("B",5), rep("C", 5))

# convert to factor
chem_a$method <- as.factor(chem_a$method)
levels(chem_a$method)

colnames(chem_a)[2] <- "units"
chem_model <- lm(units ~ method, data = chem_a)
summary(chem_model)
anova(chem_model)

# Section 9 DOE (GMAT) ----

library(readxl)
gmat <- read_excel("gmatstudy.xlsx")
glimpse(gmat)
head(gmat)

# This format is not useful in R, we need to reshape
library(reshape2)
gmat_a <- melt(gmat, id="Preparation")

# change column names
colnames(gmat_a) <- c("Prep", "Major", "GMAT")

# convert Prep to factor
gmat_a$Prep <- as.factor(gmat_a$Prep)
levels(gmat_a$Prep)

# convert Major to factor
gmat_a$Major <- as.factor(gmat_a$Major)
levels(gmat_a$Major)

# DOE Using aov function
Anova <- aov(GMAT ~ Major*Prep, data = gmat_a)
summary(Anova)

# DOE Using Regression
gmat_model <- lm(GMAT ~ Prep*Major, data = gmat_a) # both variables and interaction
anova(gmat_model)
#Preparation: not sig. / Major: sig. / Interaction: not sig.

# Section 10 Autocorrelation and Durbin Watson Test (Q27 Closing Price) ----
library(readxl)
closing <- read_excel("closingprice.xlsx")
glimpse(closing)

# Part a. Define the independent variable Period
closing$Period <- c(1:20)

# rename columns
names(closing)[2] <- "Price"

closing_model <- lm(Price ~ Period, data = closing)
summary(closing_model)

# Part b. DW Test for Auto-correlation
library(lmtest)
dwtest(closing_model)
# Remember that null here is "we do not have autocorrelation". When reject null: Unhappy!

# Critical Value Method

# DW statistic is always between 0 and 4. 
# The closer the value is to 2, the stronger the evidence of the absence of autocorrelation.
# If it is close to 0 or 4, it suggests of positive or negative autocorrelation, respectively.
# The Durbin–Watson statistic is .7981. At the .05 level of significance.
# At alpha of 0.05 with one IV and n = 20, dL = 1.20, and dU = 1.41 (Table 16-10)
# our d=0.79, d < dL, there is significant positive autocorrelation.

# Notes:
# There are several strategies to address autocorrelation in regression analysis:
# 1. Include Lagged Variables: Incorporate past values of variables into the model.
# 2. Differencing: Subtract each observation from the previous one.
# 3. ARIMA Models: Auto-regressive integrated moving average models (time series data)
# 4. Heteroscedasticity-Robust Standard Errors: Adjust standard errors.
# 5. Co-integration Analysis: Identify long-term relationships among variables.
# 6. Time Series Cross-Validation: Evaluate model performance by cross-validation.
# 7. Other Advanced Techniques: state-space models or dynamic regression models