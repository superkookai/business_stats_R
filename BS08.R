library(tidyverse)

## Example 8.1
qnorm(0.025,1.02,0.03/sqrt(25),lower.tail = FALSE) ## 1.03176
qnorm(0.025,1.02,0.03/sqrt(25)) ## 1.00824

### Exercise 8.1 - 15
speeds = read_csv("data/highway_speeds.csv")
View(speeds)

## Construct the 95% confidence interval for the mean speed of all cars on that section of the turnpike. Are the safety officer’s concerns valid if the speed limit is 55 mph? Explain.
n = 40
sd_pop = 5
## 95% confidence interval -> x_bar +- z*sd_pop/sqrt(n)
alpha = 1-0.95
x_bar = mean(speeds$`Speed (mph)`)

upper = qnorm(alpha/2,x_bar,sd_pop/sqrt(n),lower.tail = FALSE) ## 61.30374
lower = qnorm(alpha/2,x_bar,sd_pop/sqrt(n)) ## 58.20476

### Ans: with 95% confident interval, the average speed will fall in [58.20476,61.30374]
### Ans: The safety officer concerns is true if speed limit is 55 mph because the average speed interval is greater than limit at 55 mph.

### Exercise 8.2 - 22
n=18
x_bar=12.5
sd_s=9.2

## a. Calculate the margin of error with 95% confidence.
## alpha = 0.025
qt(0.025,n-1,lower.tail = FALSE) ## 2.109816
## Ans: margin of error = 2.109816*sd_s/sqrt(n)

## b. Calculate the 95% confidence interval for the population mean.
## Find x_bar +/- margin error
lower = x_bar - 2.109816*sd_s/sqrt(n) ## 7.924947
upper = x_bar + 2.109816*sd_s/sqrt(n) ## 17.07505
## Ans: 95% confident interval for the population mean = [7.924947,17.07505]


### Exercise 8.2 - 38
## Create data
# Set the seed for reproducibility
set.seed(123)

# Sample size
n <- 143

# Research expenditure (assuming a lognormal distribution)
research <- rlnorm(n, meanlog = 10, sdlog = 2)

# Duration of technology transfer office (assuming a uniform distribution)
duration <- runif(n, min = 5, max = 30)

# Create a data frame
university_data <- data.frame(Research = research, Duration = duration)
View(university_data)

## a. Construct and interpret the 95% confidence interval for the mean research expenditure of all universities.
research_x_bar = mean(university_data$Research) ## 111145.5
research_sd = sd(university_data$Research) ## 271490.5
qt(0.025,n-1,lower.tail = FALSE) ## 1.976811
lower = research_x_bar - 1.976811*research_sd/sqrt(n) ## 66265.6
upper = research_x_bar + 1.976811*research_sd/sqrt(n) ## 156025.4
## ANS: 95% confidence interval for the mean research expenditure = [66265.6,156025.4]

## b. Construct and interpret the 95% confidence interval for the mean duration of all universities.
duration_x_bar = mean(university_data$Duration) ## 17.06061
duration_sd = sd(university_data$Duration) ## 7.384579
qt(0.025,n-1,lower.tail = FALSE) ## 1.976811
lower = duration_x_bar - 1.976811*duration_sd/sqrt(n) ## 15.83987
upper = duration_x_bar + 1.976811*duration_sd/sqrt(n) ## 18.28135
## ANS: 95% confidence interval for the mean duration = [15.83987,18.28135]

### Exercise 8.3 - 46
n=5324
p_bar=0.37

## a. Construct the 90% confidence interval for the proportion of Americans who feel that good job opportunities matter most in a place to live.
qnorm(0.05,lower.tail = FALSE) ## z_0.05 =  1.644854
lower = p_bar - 1.644854*sqrt(p_bar*(1-p_bar)/n) ## 0.3591162
upper = p_bar + 1.644854*sqrt(p_bar*(1-p_bar)/n) ## 0.3808838
## ANS: 90% confidence interval = [0.3591162,0.3808838], margin error = 0.01088377

## b. Construct the 99% confidence interval for the proportion of Americans who feel that good job opportunities matter most in a place to live.
qnorm(0.005,lower.tail = FALSE) ## 2.575829
lower = p_bar - 2.575829*sqrt(p_bar*(1-p_bar)/n) ## 0.3529561
upper = p_bar + 2.575829*sqrt(p_bar*(1-p_bar)/n) ## 0.3870439
## ANS: 99% confidence interval = [0.3529561,0.3870439], margin error = 0.01704391

## c. Which of the above two intervals has a higher margin of error? Explain why.
## ANS: Margin error of (b) is greater than (a) because they use higher confidence interval


### Exercise 8.3 - 54
p_pop=1/5 ## 0.2
n=80
## a. Compute the point estimate for the proportion of 18-year-olds who have not graduated from high school in this city.
p_bar = 20/n ## 0.25
## ANS: point estimate for the proportion = 0.25

## b. Use this point estimate to derive the 95% confidence interval for the population proportion.
qnorm(0.025,lower.tail = FALSE) ## 1.959964
lower = p_bar - 1.959964*sqrt(p_bar*(1-p_bar)/n) ## 0.1551137
upper = p_bar + 1.959964*sqrt(p_bar*(1-p_bar)/n) ## 0.3448863
## ANS: 95% confidence interval = [0.1551137,0.3448863], margin error = 0.09488635

## c. Can the mayor’s comment be justified at 95% confidence?
## ANS: The rate of not graduate from high school of Northeastern city is the same interval with the rest of the country with 95% confidence.

### Exercise 8.4 - 64
A_sd_pop=0.206
B_sd_pop=0.128
## a. What is the minimum sample size required by the analyst if she wants to restrict the margin of error to 4% for Industry A?
## Find n = (Z_0025*sd_hat/E)**2 
E=0.04
Z_0025=qnorm(0.025,lower.tail = FALSE) ## 1.959964
n = (Z_0025*A_sd_pop/E)**2
## ANS: n = 102

## b. What is the minimum sample size required by the analyst if she wants to restrict the margin of error to 4% for Industry B?
## Find n = (Z_0025*sd_hat/E)**2 
E=0.04
Z_0025=qnorm(0.025,lower.tail = FALSE) ## 1.959964
n = (Z_0025*B_sd_pop/E)**2
## ANS: n = 40

## c. Why do the results differ if they use the same margin of error?
## ANS: Because standard deviation is difference. With higher standard deviation, Fund A requires a larger sample size to achieve the same margin of error.


### Exercise 8.4 - 69
## a. Identify the relevant parameter of interest for this categorical variable and compute its point estimate as well as the margin of error with 90% confidence.
## Assume The Sampling Distribution of the Sample Proportion
## Find Point estimate and se
p_bar=0.7
n=400
se_p_bar=sqrt(p_bar*(1-p_bar)/n) ## 0.02291288
z_0.05=qnorm(0.05,lower.tail = FALSE) ## 1.644854
## Find Margin error
E=z_0.05*se_p_bar ## 0.03768833

## b. You decide to redo the analysis with the margin of error reduced to 2%. How large a sample do you need to draw? State your assumptions in computing the required sample size.
## Find n = (z_0.05/E)**2*p_hat*(1-p_hat)
E=0.02
p_hat=p_bar
n = (z_0.05/E)**2*p_hat*(1-p_hat)
## ANS: n = 1420 to reduce margin error to 2%


### Case study Report 8.1
## Generate data
# Set the seed for reproducibility
set.seed(123)

# Sample size
n <- 30

# Hourly wages for each group
bachelors <- rnorm(n, mean = 35, sd = 5)
high_school <- rnorm(n, mean = 25, sd = 4)
no_high_school <- rnorm(n, mean = 20, sd = 3)

# Combine data into a data frame
data <- data.frame(
  Education = c(rep("Bachelor's", n), rep("High School", n), rep("Less than High School", n)),
  Hourly_Wage = c(bachelors, high_school, no_high_school)
)
View(data)

## summary measures to compare the hourly wages for the three education levels.
ba = data |> 
  filter(Education=="Bachelor's")

high = data |> 
  filter(Education=="High School")

no_high = data |> 
  filter(Education=="Less than High School")

summary(ba)
summary(high)
summary(no_high)

## construct and interpret confidence intervals for the mean hourly wage at each education level.
## Construct 95% Confidence interval by using z and t to compare
## For Bachelor degree
ba_x_bar = mean(ba$Hourly_Wage) ## 34.76448
ba_sd = sd(ba$Hourly_Wage) ## 4.905154
z_0.025 = qnorm(0.025,lower.tail = FALSE) ## 1.959964
t_0.025 = qt(0.025,30-1,lower.tail = FALSE) ## 2.04523
lower_z = ba_x_bar - z_0.025*ba_sd/sqrt(30) ## 33.00923
upper_z = ba_x_bar + z_0.025*ba_sd/sqrt(30) ## 36.51974
lower_t = ba_x_bar - t_0.025*ba_sd/sqrt(30) ## 32.93287
upper_t = ba_x_bar + t_0.025*ba_sd/sqrt(30) ## 36.5961
## ANS: 95% Confidence interval of mean hourly wage for Bachelor's degree: 
## z-distribution: [33.00923,36.51974] with margin error = 1.755254
## t-distribution: [32.93287,36.5961] with margin error = 1.831614

## For High School
high_x_bar = mean(high$Hourly_Wage) ## 25.71335
high_sd = sd(high$Hourly_Wage) ## 3.340512
z_0.025 = qnorm(0.025,lower.tail = FALSE) ## 1.959964
t_0.025 = qt(0.025,30-1,lower.tail = FALSE) ## 2.04523
lower_z = high_x_bar - z_0.025*high_sd/sqrt(30) ## 24.51799
upper_z = high_x_bar + z_0.025*high_sd/sqrt(30) ## 26.90872
lower_t = high_x_bar - t_0.025*high_sd/sqrt(30) ## 24.46599
upper_t = high_x_bar + t_0.025*high_sd/sqrt(30) ## 26.96072
## ANS: 95% Confidence interval of mean hourly wage for High School: 
## z-distribution: [24.51799,26.90872] with margin error = 1.195365
## t-distribution: [24.46599,26.96072] with margin error = 1.247368


## For Less than High School
no_high_x_bar = mean(no_high$Hourly_Wage) ## 20.07326
no_high_sd = sd(no_high$Hourly_Wage) ## 2.609381
z_0.025 = qnorm(0.025,lower.tail = FALSE) ## 1.959964
t_0.025 = qt(0.025,30-1,lower.tail = FALSE) ## 2.04523
lower_z = no_high_x_bar - z_0.025*no_high_sd/sqrt(30) ## 19.13952
upper_z = no_high_x_bar + z_0.025*no_high_sd/sqrt(30) ## 21.007
lower_t = no_high_x_bar - t_0.025*no_high_sd/sqrt(30) ## 19.0989
upper_t = no_high_x_bar + t_0.025*no_high_sd/sqrt(30) ## 21.04762
## ANS: 95% Confidence interval of mean hourly wage for Less than High School: 
## z-distribution: [19.13952,21.007] with margin error = 0.933738
## t-distribution: [19.0989,21.04762] with margin error = 0.974359

### Additional Exercise - 89
### Generate data
library(dplyr)

# Create a data frame with 30 rows
data <- data.frame(row.names = 1:30)

# Add a column for gender with random values (Male or Female)
data$Gender <- sample(c("Male", "Female"), size = 30, replace = TRUE)

# Add a column for field preference with random values (Science, Business, or Other)
data$Preference <- sample(c("Science", "Business", "Other"), size = 30, replace = TRUE)

# Print the data frame
print(data)

data |> 
  group_by(Preference) |> 
  summarise(
    n = n()
  )

## a. Compare the 95% confidence intervals for the proportion of students who would like to pursue science with the proportion who would like to pursue business.
sci = data |> 
  filter(Preference=="Science")
buz = data |> 
  filter(Preference=="Business")
N = count(data)
## For Science
n = count(sci)
p_sci = n / N
z_0.025 = qnorm(0.025,lower.tail = FALSE) ## 1.959964
lower = p_sci - z_0.025*sqrt(p_sci*(1-p_sci)/N) ## 0.2246955
upper = p_sci + z_0.025*sqrt(p_sci*(1-p_sci)/N) ## 0.5753045
## ANS: 95% Confidence interval of Science = [0.2246955,0.5753045] with margin error = 0.1753045

## For Business
n = count(buz)
p_buz = n / N
z_0.025 = qnorm(0.025,lower.tail = FALSE) ## 1.959964
lower = p_buz - z_0.025*sqrt(p_buz*(1-p_buz)/N) ## 0.2246955
upper = p_buz + z_0.025*sqrt(p_buz*(1-p_buz)/N) ## 0.5753045
## ANS: 95% Confidence interval of Business = [0.2246955,0.5753045] with margin error = 0.1753045

## b. Construct and interpret the 90% confidence interval for the proportion of female students who are college bound.
female = data |> 
  filter(Gender=="Female")
p_female = count(female)/N ## 0.3333333
z_0.05 = qnorm(0.05,lower.tail = FALSE) ## 1.644854
lower = p_female - z_0.05*sqrt(p_female*(1-p_female)/N) ## 0.1917669
upper = p_female + z_0.05*sqrt(p_female*(1-p_female)/N) ## 0.4748998
## ANS: 90% Confidence interval for Female students = [0.1917669,0.4748998] with margin error =  0.1415665


















