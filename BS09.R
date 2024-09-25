library(tidyverse)

## Two-Tail Hypothesis testing
pnorm(622.85,606.4,65/sqrt(30),lower.tail = FALSE)*2 ## use x_bar
pnorm(1.3862,lower.tail = FALSE)*2 ## use z-score

#### Exercise 9.2 - 28
## Generate data
# Set the seed for reproducibility
set.seed(123)

# Define the number of weeks
num_weeks <- 52

# Generate normally distributed stock prices with a mean of 0 (can be adjusted)
# and a standard deviation of 3
stock_prices <- rnorm(num_weeks, mean = 0, sd = 3)

# Add a hypothetical base price to the generated values (e.g., $100)
base_price <- 30
stock_prices <- stock_prices + base_price

# Create a data frame with week numbers and stock prices
stock_data <- data.frame(Week = 1:num_weeks, Price = stock_prices)
View(stock_data)

## a. State the null and the alternative hypotheses in order to test whether or not the average weekly stock price differs from $30.
## ANS: H0: mu0 equal 30, Ha = mu0 not equal 30

## b. Find the value of the test statistic and the p-value.
## This is Two-tailed test -> 2*P(Z>z)
mu0 = 30
sd_pop = 3
x_bar = mean(stock_data$Price) ## 30.11221
p_value = 2*pnorm(x_bar,mu0,sd_pop/sqrt(52),lower.tail = FALSE) ## 0.7873788

z = (x_bar-mu0)/(sd_pop/sqrt(52)) ##  0.2697159
p_value_check = 2*pnorm(z,0,1,lower.tail = FALSE) ## 0.7873788

## c. At α = 0.05, can you conclude that the average weekly stock price does not equal $30?
## ANS: p_value = 0.7873788, which is greater than alpha 0.05, So do not reject H0. This means we cannot conclude that the stock price is not equal 30 USD. 


#### Exercise 9.2 - 30
### Gen data
# Set the seed for reproducibility
set.seed(123)

# Define the number of students
num_students <- 40

# Generate normally distributed student debt with a mean of 0 (can be adjusted)
# and a standard deviation of 5000
student_debt <- rnorm(num_students, mean = 0, sd = 5000)

# Add a hypothetical base debt to the generated values (e.g., $30,000)
base_debt <- 30000
student_debt <- student_debt + base_debt

# Create a data frame with student IDs and debt amounts
debt_data <- data.frame(StudentID = 1:num_students, Debt = student_debt)
View(debt_data)

### a. Specify the competing hypotheses to test the researcher’s belief.
## ANS: H0: debt <= 25000, Ha = debt > 25000

### b. Find the value of the test statistic and the p-value.
## This is One tailed test -> Right test
mu0 = 25000
x_bar = mean(debt_data$Debt) ## 30225.92
sd_pop = 5000
n = 40
p_value = pnorm(x_bar,mu0,sd_pop/sqrt(n),lower.tail = FALSE) ## 1.917454e-11

### c. Do the data support the researcher’s claim, at α = 0.10?
alpha = 0.1
## ANS: p_value < alpha, So we rejected H0. This mean average debt is greater than 25000 as researcher claim.









