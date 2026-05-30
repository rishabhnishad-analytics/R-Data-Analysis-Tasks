## Load the mtcars dataset and display the first six observations
data(mtcars) 
head(mtcars) 

## Display the structure of the dataset
str(mtcars)

## Find the number of rows and columns in the dataset
dim(mtcars) 
nrow(mtcars) 
ncol(mtcars) 

## Display all variable names in the dataset
names(mtcars)

## Generate summary statistics for all variables
summary(mtcars) 

## Calculate mean, median, and standard deviation of mpg
mean(mtcars$mpg) 
median(mtcars$mpg) 
sd(mtcars$mpg) 

## Identify the car with the lowest mileage 
mtcars[which.min(mtcars$mpg), ]

## Count the number of cars with mpg greater than 20
sum(mtcars$mpg > 20) 

## Extract all cars having 6 cylinders
mtcars[mtcars$cyl == 6, ]

## Select only mpg, cyl, and hp variables
mtcars[, c("mpg", "cyl", "hp")]

## Create a new variable for power-to-weight ratio
mtcars$power_to_weight <- mtcars$hp / mtcars$wt 
head(mtcars)

## Sort the dataset in descending order of mpg 
mtcars[order(mtcars$mpg, decreasing = TRUE), ] 

## Calculate average mpg for each cylinder category
aggregate(mpg ~ cyl, data = mtcars, mean) 

## Calculate average horsepower for each gear category
aggregate(hp ~ gear, data = mtcars, mean) 

## Create mileage categories based on mpg values
mtcars$mpg_class <- ifelse(mtcars$mpg < 15, "Low",ifelse(mtcars$mpg <= 25, "Medium", "High")) 
table(mtcars$mpg_class) 

## Find correlation between mpg and horsepower 
cor(mtcars$mpg, mtcars$hp)

## Build a simple linear regression model using weight 
model1 <- lm(mpg ~ wt, data = mtcars) 
summary(model1)                                  # View Summary

## Build a multiple regression model using wt, hp, and cyl 
model2 <- lm(mpg ~ wt + hp + cyl, data = mtcars) 
summary(model2)                                  # View Summary

## Create a scatter plot for mpg vs weight
plot(mtcars$wt, mtcars$mpg, 
     xlab = "Weight", ylab = "mpg", 
     main = "mpg vs Weight")

## Create a boxplot for mpg by number of cylinders
boxplot(mpg ~ cyl, data = mtcars, 
        xlab = "Cylinders", ylab = "mpg", 
        main = "mpg by Cylinders")



