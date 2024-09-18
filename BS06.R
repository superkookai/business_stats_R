library(tidyverse)

### Example 6.1 - 10
a = 7
b = 16
## What is the probability that a randomly selected tulip is tall enough to pick?
## Find P(X>10)
base = b-10
height = 1/(b-a)
prob = base*height ## Ans = 0.6666667

F(2.5)-F(0) = 0.54
F(4)-F(2.5) = 0.16
upper = 4

### Exercise 6.2 - 44
mu=60
sd=20

## a. Find the probability that a randomly selected student scores between 50 and 80.
## Find P(50<=X<=80) = P(X<=80)-P(X<=50)
pnorm(80,mu,sd)-pnorm(50,mu,sd) ## Ans = 0.5328072

## b. Find the probability that a randomly selected student scores between 20 and 40.
## Find P(20<=X<=40) = P(X<=40)-P(X<=20)
pnorm(40,mu,sd)-pnorm(20,mu,sd) ## Ans = 0.1359051

## c. The syllabus suggests that the top 15% of the students will get an A in the course. What is the minimum score required to get an A?
## Find P(X<=x) = 0.85
qnorm(0.85,mu,sd) ## Ans = 80.72867
qnorm(0.15,mu,sd,lower.tail = FALSE) ## Ans = 80.72867

## d. What is the passing score if 10% of the students will fail the course?
## Find P(X<=x) = 0.1
qnorm(0.1,mu,sd) ## Ans = 34.36897

### Exercise 6.2 - 51
mu=40
sd=16

## a. What is the probability that a battery will break down during the warranty period?
## Warranty is 2x12 = 24 months -> Find P(X<=24)
pnorm(24,mu,sd) ## Ans = 0.1586553

## b. What is the expected profit of the auto store on a battery?
## The probability battery life longer than 2 years(24 months) is 1-P(X<=2) = 1-0.1586553 = 0.8413447 which is 84.13% -> So expected profit is (0.8413)*20 = 16.83 USD/piece

## c. What is the expected monthly profit on batteries if the auto store sells an average of 500 batteries a month?
## 84.13% - life long, 15.87% - break down
## Average monthly profit = Profit - Loss = (0.8413*500*20) - (0.1587*500*10) = 7619.5 USD/Month


### Exercise 6.3 - 68
## Exponentially distributed problem
## lambda = 1/E(X)
lambda = 1/500

## a. What is the probability that the standby generator fails during the next 24-hour blackout?
## Find P(X<=24)
pexp(24,lambda,lower.tail = TRUE) ## Ans = 0.04686621

## b. Suppose the hospital owns two standby generators that work independently of one another. What is the probability that both generators fail during the next 24-hour blackout?
## From a, probability that one generator will be failed in 24 hours is 0.04686621. We have two generators working independently, let's say A and B, so P(A|B) = P(A) = P(B) = 0.04686621. Then the result of both generators will fail is P(A ^ B) = P(A|B)P(B) = P(A)P(B) = 0.04686621*0.04686621 = 0.002196442


### Exercise 6.3 - 73
## Lognormal distribution problem
mu_y = 8
sd_y = 4

## a. Compute the mean and the standard deviation of X.
mu_x = log(mu_y**2 / sqrt(mu_y**2+sd_y**2)) ## Ans = 1.96787
sd_x = sqrt(log(1 + (sd_y**2/mu_y**2))) ## Ans = 0.4723807

## b. What proportion of the washing machines will last for more than 10 years?
## Find P(Y>10)
plnorm(10,mu_x,sd_x,lower.tail = FALSE) ## Ans = 0.2392953

## c. What proportion of the washing machines will last for less than 6 years?
## Find P(Y<=6)
plnorm(6,mu_x,sd_x,lower.tail = TRUE) ## Ans = 0.3546433

## d. Compute the 90th percentile of the life of the washing machines.
## Find P(Y<=y) = 0.9
qlnorm(0.9,mu_x,sd_x) ## Ans = 13.10836


### Case Study Report 6.1
bmi_us = tibble(
  Weight_Status = c("Underweight","Healthy Weight","Overweight","Obese"),
  Range = c("Less than 14.2","Between 14.2 and 19.4","Between 19.4 and 22.2","More than 22.2"),
  Probability_base = c(0.05,0.8,0.1,0.05)
)

## BMI Midwestern town
mu = 19.2
sd = 2.6
p_less_142 = pnorm(14.2,mu,sd)
p_between_142_194 = pnorm(19.4,mu,sd) - pnorm(14.2,mu,sd)
p_between_194_222 = pnorm(22.2,mu,sd) - pnorm(19.4,mu,sd)
p_more_22.2 = pnorm(22.2,mu,sd, lower.tail = FALSE)

Probability_Midwestern = c(p_less_142,p_between_142_194,p_between_194_222,p_more_22.2)

bmi_us$Probability_Midwestern = Probability_Midwestern

sum(bmi_us$Probability_Midwestern)


### Case Study Report 6.2
invest_table = tibble(
  w_1575 = c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,1)
)

invest_table = invest_table |> 
  mutate(
    w_12 = 1-w_1575,
    Total_Profit = 15.75*w_1575 + 12*w_12
  )

invest_table |> 
  arrange(desc(Total_Profit))

### Additional Exercise 6.96
mu_x = 11.1
sd_x = 0.4

## a. What is the probability that a household’s income is less than $50,000?
plnorm(50000,mu_x,sd_x) ## Ans =  0.2417906

## b. What is the probability that a household’s income is greater than $60,000?
plnorm(60000,mu_x,sd_x,lower.tail = FALSE) ## Ans = 0.5966751

## c. What is the lowest income that places a household in the 99th percentile?
qlnorm(0.99,mu_x,sd_x) ## Ans = 167801.9

## d. What is the highest income that places a household in the 10th percentile?
qlnorm(0.1,mu_x,sd_x) ## Ans = 39631.49




























