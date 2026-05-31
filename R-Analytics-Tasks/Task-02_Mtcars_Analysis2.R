## Task 2: Exploratory Data Analysis and Regression Modeling (MTCars)

# Loading Library
library(tidyverse)

## Converting continuous categorical variables into factor variables 
mtcars$cyl  <- as.factor(mtcars$cyl)
mtcars$gear <- as.factor(mtcars$gear)
mtcars$am   <- as.factor(mtcars$am)

## Total count of cars by transmission type (0 = automatic, 1 = manual)
mtcars %>% count(am)
table(mtcars$am)

## Finding the maximum and minimum horsepower limits
min(mtcars$hp)
max(mtcars$hp)

## Interquartile range of mpg
IQR(mtcars$mpg)

## Feature Engineering: Converting vehicle weight from lbs to kilograms
mtcars <- mtcars %>% 
  mutate(wt_kg = wt * 453.592)

## Contingency table of cylinders vs transmission type
contingency_table <- table(mtcars$cyl, mtcars$am)
print(contingency_table)

## Calculating average miles per gallon segmented by transmission type
avg_mpg_by_am <- mtcars %>% 
  group_by(am) %>% 
  summarise(mean_mpg = mean(mpg))
print(avg_mpg_by_am)

## Extracting high-performance cars exceeding the global horsepower average
avg_hp <- mean(mtcars$hp)
high_hp_cars <- mtcars[mtcars$hp > avg_hp, ]
head(high_hp_cars)

## Creating a binary flag classification for cars exceeding 150 horsepower
mtcars$high_hp <- ifelse(mtcars$hp > 150, 1, 0)

## Calculating the exact sample percentage of manual transmission vehicles
manual_pct <- mean(mtcars$am == 1) * 100
print(manual_pct)

## Evaluating mean MPG matrix grouped by Cylinder count and Transmission setup
avg_mpg_by_cyl_am <- mtcars %>% 
  group_by(cyl, am) %>%
  summarise(mean_mpg = mean(mpg), .groups = 'drop')
print(avg_mpg_by_cyl_am)

## Data Visualization: Histogram distribution of vehicle fuel efficiency
hist(mtcars$mpg,
     main = "Distribution of mpg",
     xlab = "Miles per gallon",
     col = "lightblue",
     border = "black")

## Variance Analysis: Boxplot of Horsepower across Cylinder setups
boxplot(hp ~ cyl, data = mtcars, 
        main = "Horsepower by Number of Cylinders",
        xlab = "Number of Cylinders",
        ylab = "Horsepower",
        col = "lightgreen")

## Linear Correlation Plot: Fuel Efficiency vs Engine Horsepower with Trendline
plot(mtcars$hp, mtcars$mpg, 
     main = "MPG vs Horsepower", 
     xlab = "Horsepower", 
     ylab = "Miles Per Gallon", 
     pch = 19)
abline(lm(mpg ~ hp, data = mtcars), col = "red")

## Calculating absolute statistical covariance between efficiency and weight
cov_mpg_wt <- cov(mtcars$mpg, mtcars$wt)
print(cov_mpg_wt)

## Data Normalization: Generating standardized Z-Scores for Horsepower
mtcars$hp_zscore <- (mtcars$hp - mean(mtcars$hp)) / sd(mtcars$hp)

## Cross-categorical validation: identifying cars with identical MPG values but distinct cylinder metrics
identical_mpg_diff_cyl <- mtcars %>%
  group_by(mpg) %>%
  filter(n_distinct(cyl) > 1)
print(identical_mpg_diff_cyl)

## Fitting a Multivariable Ordinary Least Squares (OLS) Linear Regression Model
model1 <- lm(mpg ~ hp + wt, data = mtcars)
summary(model1)

## Extracting fitted expected assumptions against residual errors 
fitted_mpg <- fitted(model1)
residuals_mpg <- residuals(model1)
head(data.frame(fitted_mpg, residuals_mpg))

## Residual Diagnostics: Checking assumptions of homoscedasticity 
plot(fitted(model1), residuals(model1),
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values (Predicted MPG)",
     ylab = "Residuals (Errors)",
     pch = 19, col = "darkblue")
abline(h = 0, col = "red", lty = 2)
