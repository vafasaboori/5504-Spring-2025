# Clear the environment
rm(list = ls())

# Set the working directory
setwd("~/Documents/5504 Spring 2025/Ch_15")

# Section 1 Multiple Regression Coefficients and R-Squared ----

# Read Excel file in R
library(readxl)
butler <- read_excel("butler.xlsx")
summary(butler)

# multiple regression
butler_model <- lm(Time ~ Miles + Deliveries, data = butler)
butler_model$coefficients # coefficients

# R2 and Adjusted R2
summary(butler_model) 
summary(butler_model)$r.squared # R2
summary(butler_model)$adj.r.squared # Adjusted R2

# alt method rsq package
library(rsq)
rsq(butler_model, adj = FALSE) # R2
rsq(butler_model, adj = TRUE) # adjusted RR2
?rsq

# Section 2 Test of Significance ----
summary(butler_model) # t Test for each IV and F Test for whole model

# alt method
library(car)
Anova(butler_model) # Separate F Tests for each IV

# alt method (optional)
# drop1 tests significance by comparing the model fit with and without each term.
drop1(butler_model) #  SSR for each IV
# AIC: Akaike Information Criterion, Lower AIC = Better Model
# RSS: Residual Sum of Squares (Same as SSE)
drop1(butler_model, test = "F") # Separate F Tests
drop1(butler_model, test = "Chisq") # Separate Chi-Sq Tests (logistic regression)
?drop1

# Section 3 In Class Practice (Q15-9 House Prices) ----

#Read Excel File
library(readxl)
house <- read_excel("springhouses.xlsx")
print(house)
summary(house)

# Change col names
colnames(house) <- c("price" , "bath", "sqft", "beds")

# part a
plot(house)
# scatter plot matrix, showing relationships between pairs of variables in a data frame.

# alt method
pairs(house, lower.panel = panel.smooth) # same plot
# in the lower triangle of the matrix, show a smooth (fitted) plot

# alt method
library(GGally) # Extension to ggplot2
?GGally

ggscatmat(house, alpha = 0.5) # scatterplot matrix better visuals
# r values in the upper panel
# aplpha = 0.5 points slightly transparent
# diagonal: histograms or density plots for each variable

# add linear fit lines by using ggplot2 layers to customize the plot
ggscatmat(house, alpha = 0.5) +
geom_smooth(method = "lm", se = FALSE, color = "red")

# Square feet and bedrooms show strong increasing relationships with  price. 
# Relationship between bathrooms and price is not as strong as the other two.

# part b
house_model <- lm(price ~ bath + sqft + beds, data = house)
summary(house_model)
# we observe that "bath" is not significant

# part c drop "bath"
house_model_rev <- lm(price ~ sqft + beds, data = house)
summary(house_model_rev)

# part d prediction
predict(house_model_rev, data.frame(sqft = 2650, beds = 4))

# Section 4 Multicollinearity ----

# Let's load the mtcars dataset
?mtcars # fuel consumption and 10 aspects of automobile design for 32 cars

# Correlation matrix (shows correlation coeff between multiple variables in a dataset)
cor(mtcars)
round(cor(mtcars), 2)

# alt method
?corrplot # visual exploratory tool on correlation matrix
library(corrplot)
corrplot(cor(mtcars)) # graphical shell for cor-plot

# added visualization
corrplot(cor(mtcars), method = "number")
corrplot(cor(mtcars), method = "circle")
corrplot(cor(mtcars), method = "square")
corrplot(cor(mtcars), method = "shade")
corrplot(cor(mtcars), method = "pie")

# alt method
library(GGally)
ggcorr(mtcars, label = TRUE)
?ggcorr # help file

# alt method
library(PerformanceAnalytics)
chart.Correlation(mtcars, histogram = TRUE)

# Section 5 In Class Practice (Q15-25 Auto-Resale) ----

# Read Excel file in R
library(readxl)
auto <- read_excel("autoresale.xlsx")
print (auto)
summary(auto)

# part a, multiple regression
auto_model <- lm(Price ~ Mileage + Age, data = auto)
summary(auto_model)

# getting rid of the scientific notations
options(scipen = 999)
summary(auto_model)

# bringing scientific notations back
options(scipen = 0)
summary(auto_model)

# part b, Multicollinearity
plot(auto)
cor(auto)

# alt method
chart.Correlation(auto,
                  histogram = TRUE)
# Mileage and Age (IVs) Cor Coeff is 0.67 which is less than 0.7 (OK)

# parts c and d
summary(auto_model)
anova(auto_model)

# Section 6 Prediction and Confidence Interval NFL ----
# Read Excel file in R
library(readxl)
nfl <- read_excel("nfl2011.xlsx")
print(nfl)
summary(nfl)

#change column names
colnames(nfl) <- c("team", "offense", "defense", "win")

# Regression
nfl_model <- lm(win ~ offense + defense, data = nfl)
summary(nfl_model)

# Coefficient and CI for Coefficients (not for dependent variable)
coef(nfl_model)
confint(nfl_model, level = .95)

# Part a: CI  for specific data points
predict(nfl_model, 
        data.frame(offense=225, defense=300), 
        interval = "confidence", level = .95)

# Part b: PI for specific data points

predict(nfl_model, 
        data.frame(offense=225, defense=300), 
        interval = "prediction", level = .95)

# Section 7 Categorical Independent Variables ----
# Read Excel file in R
library(readxl)
repair <- read_excel("repair.xlsx")
print(repair)
summary(repair)

#change column names
colnames(repair) <- c("time", "months", "type", "person")

# Regression
repair_model <- lm(time ~ type + months, data = repair)
summary(repair_model) # coeff for type negative
# R creates dummy automatically and chooses baseline alphabetic or numerical
# Default baseline is Electrical type=0 (alphabetically)
# In this case we want to change baseline for Mechanical to type=0

repair$type <- relevel(factor(repair$type), ref = "mechanical")
repair_model <- lm(time ~ type + months, data = repair)
summary(repair_model)

# Section 8 In Class Practice (Q15-37 Fridge) ----
# Read Excel file in R
library(readxl)
fridge <- read_excel("refrigeratorsizeprice.xlsx")
print(fridge)
summary(fridge)

#change column names
colnames(fridge) <- c("model", "door", "cubft", "price")

# part a 
fridge_slm <- lm(price ~ cubft, data = fridge)
summary (fridge_slm)

# part b
# p-value = .0002, significant relationship between list price and cubic feet.

# part c dummy variable (in R we don't need to develop a dummy variable!)
# Regression
fridge_model <- lm(price ~ door + cubft, data = fridge)
summary(fridge_model)
#	p-value = .7508 > α = .05
# there is no sig. relationship between list price and thru-the-door feature.

# Here No is door=0, and yes is door=1
# If we want to change the baseline to Yes is door=0
fridge$door <- relevel(factor(fridge$door), ref = "Yes")

# then run the model again
fridge_model <- lm(price ~ door + cubft, data = fridge)
summary(fridge_model)

# Section 9 Residual Analysis ----

# Read Excel file in R
library(readxl)
butler <- read_excel("butler.xlsx")
summary(butler)

# multiple regression
butler_model <- lm(Time ~ Miles + Deliveries, data = butler)
summary(butler_model)
butler_model$coefficients # coefficients

# residual analysis
plot(butler_model, which = 1) # 1 = residuals versus fitted
plot(butler_model, which = 2) # 2 = QQ plot
plot(butler_model, which = 3) # 3 = Scale-Location
plot(butler_model, which = 4) # 4 = Cook's Distance
plot(butler_model, which = 5) # 5 = Williams-like Graph

# Section 10 Logistic Regression ----

# Read Excel file in R
library(readxl)
simmons <- read_excel("Simmons.xlsx")
print(simmons)
summary(simmons)

# plot logistic regression with only one IV
library(tidyverse)

simmons %>% ggplot(aes(Spending, Coupon)) +
  geom_point(size = 3, alpha = 0.3) + # reduced transparency for overlapping values
  geom_smooth(method = "lm") # for now we plot the linear model

# Regression analysis
simmons_model <- glm(Coupon ~ Card + Spending, data = simmons, 
                     family ='binomial')
summary(simmons_model)
# glm is general linear model
# Family binomial indicates that it's a logistic regression problem
# If we have categorical IV, it should be converted to a factor first.
# You can do this using the factor function in R.

#let's focus on the coefficients
coefficients(simmons_model)

# coefficients represent change in log odds of the dependent variable
# By themselves are hard to interpret
# we need to convert them to odds ratio using exp() function 
exp(simmons_model$coefficients) # we don't need intercept
log(exp(simmons_model$coefficients)) 
# just to show the relationship between odds and log-odds

# odds-ratios interpretation for IVs
# categorical IV (card): the odds of cardholders using coupon vs non-cardholders
  # card = 3, odds of a cardholder using the coupon are 3x a non-cardholder
# continuous IV (spending): odds of using coupon increased by an additional 1k
  # spending = 1.4, for a 1k increase in spending, odds of using coupon is 40% higher

# To predict individual probabilities we use this:
predict(simmons_model, data.frame(Card = 1, Spending = 4.2), type = "response")
# type = 'response' specifies that you want predicted probabilities
# probability of a cardholder with 4.2k spending is %59.6

predict(simmons_model, data.frame(Card = 1, Spending = 4.2))
# without "type = 'response'" you will get the log odds, aka logits (ln of odds).
# logit of a cardholder with 4.2k spending is %38.7

# To convert logit "ln(p/1-p)" to probability use the inverse logit function
logit <- predict(simmons_model, data.frame(Card = 1, Spending = 4.2))

p <- 1 / (1 + exp(-logit))
print (p)
# logit value of %38 corresponds to probability value of of %59.6

# Section 11 In class Practice 15-46 Direct Deposit ----

# Read Excel file in R
library(readxl)
bank <- read_excel("Bank.xlsx")
print(bank)
summary(bank)

# a. logitic regression: log(p/1−p) = b0 + b1x
# Where: p is the probability of signing up for direct payroll deposit.
# b0 is the intercept
# b1 is the coefficient for the average monthly balance.

# b. Use statistical software to compute the estimated logistic regression equation.
bank_model <- glm(Direct ~ Balance, data = bank, family = "binomial")
# Family binomial indicates that it's a logistic regression problem

# Display the estimated coefficients
summary(bank_model)

# c. Conduct a test of significance using the χ^2 test statistic.
anova(bank_model, test = "Chisq")

# d. Estimate the prob for customers with an average monthly balance of $1000.
predict(bank_model, newdata = data.frame(Balance = 10), type = "response")
# prob of customers with a balance of 1k signing up for direct deposit is 0.3939.

# e. Find the average monthly balance required for a probability of 0.5 or higher.
# Iterate over different values of x (monthly balance from 0 to 30k)
for (x_value in seq(0, 30, by = 0.1)) {
  predicted_prob <- predict(bank_model, newdata = data.frame(Balance = x_value), 
                            type = "response")
  
  if (predicted_prob >= 0.5) {
    required_balance <- x_value
    break  # Stop the loop when the condition is met
  }
}

cat("Required Balance for P of 0.5 or Higher:", required_balance*100)

# f. Calculate the estimated odds ratio and its interpretation.
# we need to convert the coefficients (log-odds) to odds ratio using exp() function 
exp(bank_model$coefficients) # we don't need intercept
# odds of direct deposit increase by 1.2463 times for every $100 increase in balance
