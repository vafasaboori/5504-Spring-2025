# Clear the environment and set working directory ----
rm(list = ls())
setwd("~/Documents/5504 Spring 2025/Ch_15")

# 15-5 ----
# read excel file
library(readxl)
Q5 <- read_excel("showtime.xlsx")
head(Q5)
summary(Q5)

# change column names
colnames(Q5) <- c( "revenue", "tv", "news")

# Part a, simple regression
slr5a <- lm(revenue ~ tv, data = Q5)
summary(slr5a)
coef(slr5a)

# Part b, multiple regression
slr5b <- lm(revenue ~ tv + news, data = Q5)
summary(slr5b)
coef(slr5b)

# part c, coefficients for tv are not the same in part a and b
  # part a: tv coef. is change in revenue due to a unit change in tv ads
  # part b: tv coef. is change in revenue due to a unit change in tv ads with news ads held constant.

# predict tv = 3600 and news = 1600
round(
  predict(slr5b, data.frame(tv=3.5, news = 2.3))
  *1000,
  0)

# 15-8 ----
# read excel file
library(readxl)
Q8 <- read_excel("Ships.xlsx")
head(Q8)

# change column names
colnames(Q8) <- c( "ship", "overall", "shore", "food")

# Part a, simple regression
slr8a <- lm(overall ~ shore, data = Q8)
summary(slr8a)
coef(slr8a)

# Part b, multiple regression
slr8b <- lm(overall ~ shore + food, data = Q8)
summary(slr8b)
coef(slr8b)

# part c, predict shore 80 and food 90
round(
  predict(slr8b, data.frame(shore=80, food = 90)),
  2)

# 15-12 ----

# method a, using data file Exer2.xlsx
# read excel file
library(readxl)
Q12 <- read_excel("Exer2.xlsx")
head(Q12)

# multiple regression
slr12 <- lm(Y ~ X1 + X2, data = Q12)
slr12$coefficients # coefficients

# part a, b R2 and Adj. R2
summary(slr12) 
summary(slr12)$r.squared # R2
summary(slr12)$adj.r.squared # Adjusted R2

# part c, R2 > 0.7 regression equation explain a large amount of the variability in the data

# method b, (using SSR, SST)

# Given data
SST <- 15182.9
SSR <- 14052.2
n <- 10  # Number of observations
p <- 2   # Number of independent variables

# Compute R-squared
R2 <- SSR / SST

# Compute Adjusted R-squared
R2_a <- 1 - ((1 - R2) * (n - 1) / (n - p - 1))

# Print results
cat("R-squared:", R2, "\n")
cat("Adjusted R-squared:", R2_a, "\n")

# Interpretation
if (R2 > 0.7) {
  cat("The regression equation explains a large amount of variability in the data.\n")
} else {
  cat("The regression equation does not explain a large amount of variability in the data.\n")
}

# 15-18 ----
library(readxl)
Q18 <- read_excel("PitchingMLB.xlsx")
head(Q18)

# change some column names
colnames(Q18)[6] <- "SO_IP"
colnames(Q18)[7] <- "HR_IP"
colnames(Q18)[8] <- "R_IP"

head(Q18)

# part a: multiple regression
slr18a <- lm(R_IP ~ SO_IP + HR_IP, data = Q18)
slr18a$coefficients # coefficients

# part b: R2 and Adj. R2
summary(slr18a) 
summary(slr18a)$r.squared # R2
summary(slr18a)$adj.r.squared # Adjusted R2

# The fit is not bad, because it explains around 50% of the variability

# part c: multiple regression ERA as IV
slr18c <- lm(ERA ~ SO_IP + HR_IP, data = Q18)
slr18c$coefficients # coefficients

# part a: R2 and Adj. R2
summary(slr18c) 
summary(slr18c)$r.squared # R2
summary(slr18c)$adj.r.squared # Adjusted R2

# The fit is not bad, because it explains 58.00% of the variability

# 15-23 ----
# read excel file
library(readxl)
Q23 <- read_excel("showtime.xlsx")
head(Q23)

# change column names
colnames(Q23) <- c( "revenue", "tv", "news")

# Part a, F test in multiple regression 
slr23 <- lm(revenue ~ tv + news, data = Q23)
summary(slr23)

# Extract F-statistic (optional)
summary(slr23)$fstatistic
summary(slr23)$fstatistic[1] # F-statistic

# Extract F test p-value (optional)
# pf() calculates the cumulative distribution function of the F-distribution
pf(summary(slr23)$fstatistic[1], 
   summary(slr23)$fstatistic[2], summary(slr23)$fstatistic[3], 
   lower.tail = FALSE) 
# lower.tail = TRUE (default): P(X <= q) cumulative probability up to q.
# lower.tail = FALSE: P(X > q) complementary probability (1-cumulative) p-value

# The overall model is  significant

# Part b, c: t tests in multiple regression 
summary(slr23)
summary(slr23)$coefficients[ , 3] # Returning t-value (column 3)
summary(slr23)$coefficients[ , 4] # Returning p-value (column 4)
# both "tv" and "news" are significant, should not be dropped

# 15-26 ----
library(readxl)
Q26 <- read_excel("PitchingMLB.xlsx")
head(Q26)

# change some column names
colnames(Q26)[6] <- "SO_IP"
colnames(Q26)[7] <- "HR_IP"
colnames(Q26)[8] <- "R_IP"

head(Q26)

# multiple regression
slr26 <- lm(R_IP ~ SO_IP + HR_IP, data = Q26)
slr26$coefficients # coefficients

# part a, b: F and t Test
summary(slr26) 

# There is a significant overall relationship.
# bot IVs are also significant.

# 15-31 ----
library(readxl)
Q31 <- read_excel("autoresale.xlsx")
head(Q31)

# Part a: prediction
slr31 <- lm(Price ~ Mileage + Age, data = Q31)
summary(slr31)

predict(slr31, data.frame(Mileage = 40000, Age = 4))

# Part b, confidence and prediction interval
predict(slr31, data.frame(Mileage = 40000, Age = 4), interval = "confidence", level = 0.95)
predict(slr31, data.frame(Mileage = 40000, Age = 4), interval = "prediction", level = 0.95)

# 15-36 ----
# Read Excel file in R
library(readxl)
repair <- read_excel("repair.xlsx")
head(repair)

#change column names
colnames(repair) <- c("time", "months", "type", "person")

# Re-level categorical data
repair$type <- relevel(factor(repair$type), ref = "mechanical") # set mechanical = 0
repair$person <- relevel(factor(repair$person), ref = "Bob Jones") # not necessary, bob is already 0

# parts a, b) Regression
repair_model <- lm(time ~ months + type + person, data = repair)

# coefficients, statistics, and sig levels
summary(repair_model)

# part c) The addition of Person is not statistically significant.
    # Why? (In class discussion) ----
    
    # Correlation matrix  
    library(tidyverse)
    repair$type_numeric <- as.numeric(repair$type)
    repair$person_numeric <- as.numeric(repair$person)
    numeric_data <- repair %>% select(time, months, type_numeric, person_numeric)
    cor(numeric_data)
    
    # correlation coeff
    cor(repair$months, as.numeric(repair$person))
    cor.test(repair$months, as.numeric(repair$person))
    # Person is highly correlated with months since last service
    # Some repairpersons handle overdue repairs while others focus on routine maintenance
    # Once effect of "Months" is accounted for, "Person" will not add much to the model.

# 15-38 ----
# Read Excel file in R
library(readxl)
stroke <- read_excel("Stroke.xlsx")
head(stroke)

# part a and b Regression
stroke_model <- lm(Risk ~ Age + Pressure + Smoker, data = stroke)
summary(stroke_model)
# p-value for smoking is .010 < α = .05, smoking is a significant factor.

# part c predict
predict(stroke_model, data.frame(Age = 68, Pressure = 175, Smoker = "Yes"))

# The physician would recommend quit smoking (b3) and begin bp treatment (b2)

# 15-41 ----
# read excel file
library(readxl)
Q41 <- read_excel("showtime.xlsx")
head(Q41)

# change column names
colnames(Q41) <- c( "revenue", "tv", "news")
head(Q41)

# Part a: regression
slr41 <- lm(revenue ~ tv + news, data = Q41)
summary(slr41)
coefficients(slr41)

# part b) std residual plot for assumptions
plot(slr41, which = 3) 
# few observations, can't tell if assumptions are violated
# the plot could be interpreted as either random or curvilinear.

# alt method
# Residual analysis 
library(tidyverse)
Q41$predicted <- fitted(slr41)
Q41$residuals <- residuals(slr41)
Q41$std_residuals <- rstandard(slr41)

Q41 %>% ggplot(aes(x = predicted, y = std_residuals)) + 
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")  # we observe no pattern

# few observations, difficult to determine error assumptions violated.
# For instance, we can say there does not appear to be any pattern in the plot
# alternatively we can say there is a curvilinear pattern in the plot.

# part c) std residual plot for outliers
plot(slr41, which = 3) # no outliers no values over 1.4 

# part d) influential observations
# hat values method
hatvalues(slr41)
hat_values <- hatvalues(slr41)
hat_threshold <- 1.125  # h > 3(P+1)/n outlier
any(hat_values > hat_threshold) # No high leverage points

# cook's distance method
plot(slr41, which = 4) # D1 > 1, first observation is influential
plot(slr41, which = 5) # D1 > 1, first observation is influential

# 1 = residuals versus fitted
# 2 = QQ plot
# 3 = Scale-Location
# 4 = Cook's Distance
# 5 = Williams-like Graph


# 15-42----
# Read Excel file in R
library(readxl)
auto <- read_excel("Auto2.xlsx")
head(auto)

# Curb weight: weight including a full tank of fuel and all standard equipment.

# change col names
colnames(auto) <- c( "car", "price", "weight", "hp", "speed")

# part a Regression
auto_model <- lm(speed ~ price + hp, data = auto)
summary(auto_model)
coefficients(auto_model)

# parts b,c,d residual analysis
plot(auto_model, which = 1) 
plot(auto_model, which = 3) # part c. no outliers
plot(auto_model, which = 4) # part d.	observation 2 as an influential observation.
plot(auto_model, which = 5)

# 1 = residuals versus fitted
# 2 = QQ plot
# 3 = Scale-Location
# 4 = Cook's Distance
# 5 = Williams-like Graph

# alt method
# Residual analysis 
auto$predicted <- fitted(auto_model)
auto$residuals <- residuals(auto_model)
auto$std_residuals <- rstandard(auto_model)

auto %>% ggplot(aes(x = predicted, y = std_residuals)) + 
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")  

# There appears to be a highly unusual trend (megaphone) in the standardized residuals.
# residual plot does not support the assumption about error
# There are no std residuals above +2 or below -2


# 15-47----

# Part a) Regression Equation
# P: probability of return for the sophomore year
# logit(P/1-P) = b0 + b1(GPA) + b2(Program) 

# Part b) What is the interpretation of E(y) when x2(Program) = 0?
# For a given GPA, it's the prob of a student who not attended prog will return for sophomore year.

# Part c) Use both IVs to compute the estimated logit.
# Estimated Logit (equation 15.37) [ln(P/1-P) = b0 + b1(GPA) + b2(Program)]

library(readxl)
lakeland <- read_excel("Lakeland.xlsx")
head(lakeland)

# logistic regression
lakeland_model <- glm(Return ~ Program + GPA, data = lakeland, family = 'binomial')
coefficients(lakeland_model)
# Estimated Logit is ln(P/1-P) = -8.89 + 2.539 GPA + 1.561 Program

# Coefficients represent changes in log-odds ln(odds) of DV (hard to interpret)
# We need to convert them to Odds Ratio using exp() function 
exp(lakeland_model$coefficients[-1]) # we don't need intercept

# odds-ratios interpretation for IVs
# categorical IV (e.g. program): the odds of program attendees returning vs non-attendees
# program = 4.76, odds of an attendee returning are 4.76x a non-attendee

# continuous IV (e.g. GPA): the odds of returning increased for every additional unit of GPA
# GPA = 12.66, for every unit increase in GPA, odds of returning as a sophomore is 12.67x higher

# Part d) Overall Significance
summary(lakeland_model) # we don't get the overall significance
# R does not report overall model significance (beyond scope)

# The overall significance of model is tested by "Analysis of Deviance"
# It could be obtained by a Chi-Squared test (likelihood ratio test, LRT)
library(lmtest)
lrtest(lakeland_model) # this gives us the deviance table (overall model sig.)

# Part e) Each IV Significance
summary(lakeland_model)

# We can also conduct Chi-Squared test for each IV
library(car)
Anova(lakeland_model, test = "LR") # likelihood ratio test

# Part f) predict
predict(lakeland_model, data.frame(GPA = 2.5, Program = 0), type = "response")
predict(lakeland_model, data.frame(GPA = 2.5, Program = 1), type = "response")

# Part g) Interpretation of odds ratio for the orientation program?
lakeland_model$coefficients
exp(lakeland_model$coefficients[-1])
# odds of continuing for students who attended orient 4.7624 x greater those not.

# Part h) Recommendation
# We recommend making the orientation program required. 
# Odds of continuing are much higher for students who have attended the orientation program.

# case (NASCAR) ----
library(readxl)
nascar <- read_excel("nascar.xlsx")
head(nascar)
# “Poles” is the number of times a driver starts in first place, determined by the fastest qualifying lap.

# Part 1
round(cor(nascar[,-1]),2) # correlation matrix (remove first column)
# The variable most highly correlated with Winnings ($) is the number of top-ten finishes.

# alt method
library(PerformanceAnalytics)
chart.Correlation(nascar[,-1],
                  histogram = TRUE)

# simple regression winnings and top10

# change col names
colnames(nascar)[5] = "Top5"
colnames(nascar)[6] = "Top10"
colnames(nascar)[7] = "Winnings"

nascar_model_1<- lm(Winnings ~ Top10, data = nascar)
summary(nascar_model_1)

# Part 2
nascar_model_2 <- lm(Winnings ~ Poles + Wins + Top5 + Top10, data = nascar)
summary(nascar_model_2)

# t-test: The only significant variable is Top 10, with a p-value of .0015.
# R2 = 0.82, whereas the model with only Top 10 had an R2 = .799. 
# Adding more IVs added little to the model’s ability to explain variation in Winnings.

# Part 3, Create two new independent variables: Top 2–5 and Top 6–10. Top 2–5 
# Top 2-5
nascar$Top2_5 <- nascar$Top5 - nascar$Wins

# Top 6-10
nascar$Top6_10 <- nascar$Top10 - nascar$Top5

# regression Winnings ($) ~ Wins, Top 2–5, and Top 6–10.
nascar_model_3 <- lm(Winnings ~ Poles + Wins + Top2_5 + Top6_10, data = nascar)
summary(nascar_model_3)

# T test: only IV not significant is Poles, with a p-value of .9047.
# Pole position (starting first) does not guarantee a win or high finish.

# Multicollinearity is reduced by replacing Top 5 with Top 2–5 and Top 10 with Top 6–10.

# Correlation Matrix below provides evidence of this (lower correlations)
round(cor(nascar[, -c(1,5,6)]),2) # excluding driver, top5, and top10

# alt method
library(PerformanceAnalytics)
chart.Correlation(nascar[, -c(1,5,6)],
                  histogram = TRUE)

# part 4, What regression equation you recommend to predict Winnings ($)?

# Keep Wins, Top2-5, and Top6-10
nascar_model_4 <- lm(Winnings ~ Wins + Top2_5 + Top6_10, data = nascar)
summary(nascar_model_4)

# Wins: one unit increase in wins, other IV constant, increases winnings by $204,735.
# Top 2–5: one unit increase in Top 2–5, other IV constant, increases winnings by $186,778.
# Top 6–10: one unit increase in Top 6–10, other IV constant, increases winnings by $116,189.

# In Class Practice (15-48) ----
library(readxl)
TireRatings <- read_excel("TireRatings.xlsx")
head(TireRatings)

#Rename column with space
colnames(TireRatings)[4] <- "BuyAgain"

# Make sure your variables are numeric
TireRatings$Wet <- as.numeric(TireRatings$Wet)
TireRatings$Noise <- as.numeric(TireRatings$Noise)
TireRatings$Buy_Again <- as.numeric(TireRatings$BuyAgain)

# Create the logistic regression model
tire_model <- glm(Purchase ~ Wet + Noise, data = TireRatings, family = "binomial")

# Print the summary to get the estimated coefficients
summary(tire_model)

# (a) Logistic regression equation
# The logistic regression equation is given by:
# logit(Purchase) = b0 + b1 * Wet + b2 * Noise
# where b0 is the intercept, b1 is the coefficient for Wet, and b2 is the coefficient for Noise

# (b) Estimated logit

# Coefficients represent changes in log-odds (ln(odds) of DV (hard to interpret)
# We need to convert them to Odds Ratio using exp() function 
exp(tire_model$coefficients[-1]) # we don't need intercept

# for a 1-unit increase in Wet rating, odds of purchasing are multiplied by  29.2. 
# For a 1-unit increase in Noise rating, odds of purchasing are multiplied by 6.15.
# In general odds ratio >1 indicates a positive association between IV and DV
# while an odds ratio <1 indicates a negative association between IV and DV

# (c) Estimate probability for Wet rating of 8 and Noise rating of 8
predict(tire_model, data.frame(Wet = 8, Noise = 8), type = "response")

# (d) Estimate probability for Wet rating of 7 and Noise rating of 7
predict(tire_model, data.frame(Wet = 7, Noise = 7), type = "response")

# (e) Interpretation
# Wet and Noise ratings of 7 are both considered excellent performance ratings
# Nonetheless, the probability of repurchasing such tire is extremely low (0.0406)
# But a one-point increase in both ratings increases the probability to 0.8837
# So achieving the highest possible levels of performance is essential for manufacturer
