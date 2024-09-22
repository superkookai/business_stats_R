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














