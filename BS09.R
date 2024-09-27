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


### Exercise 9.3 - 46
## The Hypothesis is below
## H0: mu <= 6.6, Ha: mu > 6.6
## n = 36 > 30 assume x_bar Normal Distribution but do not known population sd, so use t-distribution
mu0 = 6.6
alpha = 0.05
n = 36
df = n -1
x_bar = 7.5
s = 2
t_df = (x_bar - mu0)/(s/sqrt(n)) ## 2.7
## This is right-tailed test -> Find P(T>t_df)
p_value = pt(t_df,df,lower.tail = FALSE) ## 0.005304

### ANS: p_value < 0.05 so we rejected H0, this can conclude that the average increase in home prices in the West is greater than the increase in the Midwest

 
### Exercise 9.3 - 51
## Generate data
# Set the seed for reproducibility
set.seed(123)

# Define the number of metropolitan areas
num_areas <- 26

# Generate random average debt payments (adjust mean and standard deviation as needed)
debt_payments <- rnorm(num_areas, mean = 1000, sd = 300)

# Ensure all values are positive (debt payments cannot be negative)
debt_payments <- abs(debt_payments)

# Create a data frame with metropolitan area names and debt payments
metropolitan_areas <- c("New York", "Los Angeles", "Chicago", "Houston", "Phoenix", "Philadelphia",
                        "San Antonio", "San Diego", "Dallas", "Detroit", "San Jose", "Austin",
                        "Indianapolis", "Jacksonville", "Fort Worth", "Columbus", "Charlotte",
                        "Las Vegas", "Baltimore", "Memphis", "Oklahoma City", "Portland",
                        "Milwaukee", "Tucson", "Fresno","Bangkok")

debt_data <- data.frame(MetropolitanArea = metropolitan_areas, Debt = debt_payments)

head(debt_data)

## a. State the null and the alternative hypotheses in order to test whether average monthly debt payments are greater than $900.
## ANS: H0: debt_mu <= 900, Ha: debt_mu > 900

## b. What assumption regarding the population is necessary in order to implement part a?
## ANS: The assumption must be x_bar is normally distributed (n > 30)

## c. Calculate the value of the test statistic and the p-value.
## We have no ideas about population sd so we use t-distribution
mu0 = 900
n = 26
df = n-1
x_bar = mean(debt_data$Debt) ## 970.9
sd = sd(debt_data$Debt) ## 294.8
t_df = (x_bar-mu0)/(sd/sqrt(n)) ## 1.227
## This is right tailed test -> P(T>t_df)
p_value = pt(t_df,df) ## 0.8843

## Use t.test function
t.test(debt_data$Debt,alternative = "greater",mu=900)

## d. At α = 0.05, are average monthly debt payments greater than $900? Explain.
## ANS: p_value > alpha so we do not reject H0. This means we cannot conclude that average debt is greater than 900USD.


### Exercise 9.4 - 66
## Hypothesis is below
## H0: p <= 0.75, Ha: p > 0.75
n = 214 ## Passed np>=5 and n(1-p)>=5 so this is assumed normally distributed
p0 = 0.75
p_bar = 0.78
z = (p_bar-p0)/sqrt(p0*(1-p0)/n) ## 1.014
p_value = pnorm(z,lower.tail = FALSE) ## 0.1554 This is right tailed test

## ANS: p_value > 0.05 so do not rejected H0. We cannot conclude that more than three of four (75%) of these websites are prone to fraud.


### Exercise 9.4 - 71
### Data
employees = read_csv("data/employees_info.csv")
View(employees)

### a. At the 5% level of significance, determine if the proportion of women in Tara’s firm is different from 0.30.
## Hypothesis is below
## H0: p == 0.30, Ha: p != 0.30
p0 = 0.30
n = count(employees) # 50
## n = 50 is satisfied np>=5 and n(1-p)>=5 So we can assume normally distribution
woman = employees |> 
  filter(Sex=="Woman")
n_woman = count(woman)
p_woman = n_woman/n
z = (p_woman-p0)/sqrt(p0*(1-p0)/n) ## -3.429
## This is two-tailed test -> Find 2*P(P<zp)
p_value = 2*pnorm(-3.429) ## 0.0006058

## ANS: p-value > 0.05 So do not reject H0. This means cannot conclude that proportion of woman in Tara's firm is different from 0.30

### b. At the 5% level of significance, determine if the proportion of whites in Tara’s firm is more than 0.50.
## Hypothesis is below
## H0: p <= 0.50, Ha: p > 0.50
p0 = 0.5
n = nrow(employees) # 50 -> is satisfied np>=5 and n(1-p)>=5 So we can assume normally distribution
white = employees |> 
  filter(Ethnicity=="White")
n_white = nrow(white) # 11
p_white = n_white/n # 0.22

## ANS: Because p_white(0.22) is less than p0(0.50) So we cannot rejected H0. So the proportion of whites in Tara's firm is not greater than 0.50


### Case Study - Report 9.1
### Data
# Set the seed for reproducibility
set.seed(123)

# Define the number of residents
num_residents <- 35

# National average well-being in 2017
national_avg <- 61.5

# Assuming a standard deviation of 5 (adjust as needed)
std_dev <- 5

# Generate normally distributed well-being scores with a mean slightly higher than the national average
# to reflect South Dakota's higher well-being
wellbeing_scores <- rnorm(num_residents, mean = national_avg + 1, sd = std_dev)

# Ensure scores are within the 0-100 range (adjust min and max as needed)
wellbeing_scores <- pmin(pmax(wellbeing_scores, 0), 100)

# Create a data frame with resident IDs and well-being scores
resident_data <- data.frame(ResidentID = 1:num_residents, WellBeingScore = wellbeing_scores)

View(resident_data)

### (1) determine whether the well-being score of South Dakotans is more than the national average of 61.5
## Hypothesis is below
## H0: mu <= 61.5, Ha: mu > 61.5
mu0 = 61.5
n = nrow(resident_data) ## 35, is greater than 30 so assume normal distribution
## we do not know population sd so use t-distribution, and this is one-tailed test -> P(X_bar>t_df)
df = n-1 # 34
x_bar = mean(resident_data$WellBeingScore) # 62.68759
sd = sd(resident_data$WellBeingScore) # 4.731296
t_df = (x_bar-mu0)/(sd/sqrt(n)) # 1.484978
p_value = pt(t_df,df,lower.tail = FALSE) # 0.07338155
## ANS: Assume alpha=0.05, p_value > 0.05 so we do not reject H0. So we cannot conclude that the well-being of South Dakotans is more than the national average of 61.5

### (2) determine if fewer than 40% of South Dakotans report a score below 60.
## Hypothesis is below
## H0: p >= 0.4, Ha: p < 0.4
## n=35, this satisfied np>=5 and n(1-p)>=5 -> Assume normal distribution
## This is one-tailed test -> Find P(P_bar<p_bar)
score_less_60 = resident_data |> 
  filter(WellBeingScore<60)
n_less_60 = nrow(score_less_60) # 11
p0 = 0.4
p_bar = n_less_60/n # 0.3142857
zp = (p_bar-p0)/sqrt(p0*(1-p0)/n) # -2.197401
p_value = pnorm(zp) # 0.01399591
## ANS: Assume alpha=0.05, p_value < 0.05, so rejected H0. We can concluded that fewer than 40% of South Dakotans report a score below 60


### Additional Exercise - 8.7
## Data
# Set the seed for reproducibility
set.seed(123)

# Define the number of students in each group
num_business_students <- 50
num_nonbusiness_students <- 50

# Generate random responses for business students (adjust probability as needed)
business_responses <- sample(c("yes", "no"), num_business_students, replace = TRUE, prob = c(0.3, 0.7))

# Generate random responses for non-business students (adjust probability as needed)
nonbusiness_responses <- sample(c("yes", "no"), num_nonbusiness_students, replace = TRUE, prob = c(0.6, 0.4))

# Combine responses into a single data frame
student_data <- data.frame(
  Group = c(rep("Business", num_business_students), rep("Non-Business", num_nonbusiness_students)),
  Response = c(business_responses, nonbusiness_responses)
)

View(student_data)

### a. At the 5% level of significance, determine if the percentage of business majors who study hard is less than 20%.
## Hypothesis is below
## H0: p >= 0.2, Ha: p < 0.2
n = nrow(student_data) # 100 -> satisfied np>=5 and n(1-p)>=5 -> Assume normal distribution
## This is one-tailed test to find P(Z<zp) [Left tailed]
p0 = 0.2
n_bus_yes = nrow(
  student_data |> 
    filter(Group=="Business" & Response=="yes")
) # 15
p_bar = n_bus_yes/n # 0.15
zp = (p_bar-p0)/sqrt(p0*(1-p0)/n) # -1.25
p_value = pnorm(zp) # 0.1056498
### ANS: p_value > 0.05, so we cannot rejected H0. This means the percentage of business majors who study hard is less than 20%

### b. At the 5% level of significance, determine if the percentage of nonbusiness majors who study hard is more than 20%.
## Hypothesis is below
## H0: p <= 0.2, Ha: p > 0.2
n = nrow(student_data) # 100 -> satisfied np>=5 and n(1-p)>=5 -> Assume normal distribution
## This is one-tailed test to find P(Z>zp) [Right tailed]
p0 = 0.2
n_nonebus_yes = nrow(
  student_data |> 
    filter(Group=="Non-Business" & Response=="yes")
) # 30
p_bar = n_nonebus_yes/n # 0.3
zp = (p_bar-p0)/sqrt(p0*(1-p0)/n) # 2.5
p_value = pnorm(zp,lower.tail = FALSE) # 0.006209665
### ANS: p_value < 0.05, so rejected H0. We can concluded that the percentage of nonbusiness majors who study hard is more than 20%.









