library(tidyverse)

### Exercise 7.2 - 15
mu_pop = 30
sd_pop = 6
## How is n???
## a. How unusual would it be to get a sample average of 35 or more customers if the manager had not offered the discount?
## Find P(X>35) Not find P(X_bar>35)!!
pnorm(35,mu_pop,sd_pop,lower.tail = FALSE) ## Ans = 0.2023284


### Exercise 7.2 - 20
mu_pop = 100
sd_pop = 16

## a. What is the probability that a randomly selected person will have an IQ score of less than 90?
## Find P(X<=90)
pnorm(90,mu_pop,sd_pop) ## Ans = 0.2659855

## b. What is the probability that the average IQ score of four randomly selected people is less than 90?
## Find P(X_bar<=90) by n = 4
n=4
pnorm(90,mu_pop,sd_pop/sqrt(n)) ## Ans = 0.1056498

## c. If four people are randomly selected, what is the probability that all of them have an IQ score of less than 90?
## Probability of a selected person has score less than or equal 90 is 0.2659855
## So Probability of 4 persons have score less than or equal 90 is 0.2659855**4 = 0.00500532


### Exercise 7.3 - 26
p = 1/5 ## = 0.2
## a. What is the expected value and the standard error for the sampling distribution of the sample proportion based on a sample of 200 Britains. Is it appropriate to use the normal distribution approximation for the sample proportion? Explain.
## E(P_bar)=p, se(P_bar)=sqrt(p*(1-p)/n) 
n=200
se_p_bar=sqrt(p*(1-p)/n) ## Ans = 0.02828427
## the normal approximation criteria are met because 
## np = 200*(0.20) = 40 > 5 and n(1−p) = 200*(1−0.20)= 160 > 5

## b. What is the probability that more than 25% of Britains in the sample are not using the internet? 
## Find P(P_bar>0.25)
pnorm(0.25,p,se_p_bar,lower.tail = FALSE) ## Ans = 0.03854994

### Exercise 7.4 - 36
N = 500
mu_pop = 10.32
sd_pop = 9.78
n = 12

## a. Is it necessary to apply the finite population correction factor? Explain.
## Ans n=12 which is 0.024 of N -> Not to apply finite population correction factor (n>=0.05N)

## b. Calculate the expected value and the standard error of the sample mean.
## Ans E(X_bar) = mu_pop = 10.32
## Ans se(X_bar) = sd_pop/sqrt(n) = 9.78/sqrt(12) = 2.823243

## c. Can you use the normal approximation to calculate the probability that the sample mean is more than $12 million? Explain.
## Ans = The normal approximation is not justified because we do not know if the population has a normal distribution and n < 30.

### Exercise 7.5 - 42
n = 500
p = 0.34

## a. Construct the centerline and the upper and lower control limits for the p chart.
## n*p=170 > 5 and n*(1-p)=330 > 5 So it is Normal distribution approximation
centerline = p ## 0.34
upper = p + 3*sqrt(p*(1-p)/n) ## 0.4035547
lower = p - 3*sqrt(p*(1-p)/n) ## 0.2764453

## b. Suppose six samples of size 500 produced the following sample proportions: 0.28, 0.30, 0.33, 0.34, 0.37, and 0.39. Plot these values on the p chart.
data_table = tibble(
  sample = 1:6,
  sample_p = c(0.28,0.30,0.33,0.34,0.37,0.39)
)

ggplot(data_table,aes(x=sample,y=sample_p)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = centerline, color = "blue") +
  geom_hline(yintercept = upper, color = "red") +
  geom_hline(yintercept = lower, color = "red") +
  labs(
    title = "p chart",
    x = "Sample Number",
    y = "Defect Rate"
  )

## c. Are any points outside the control limits? Does it appear that the process is under control? Explain.
## Ans: No any points outside control limits. But it looks like the production trends will be increased, so we need to look up in the line of production to find the causes.


### Exercise 7.5 - 50
complaints_data = tibble(
  month = c(1,2,3,4,5,6),
  num_complaints = c(20,12,24,14,25,22)
)

complaints_table = complaints_data |> 
  mutate(
    prob_complaints = num_complaints / sum(num_complaints)
  )
sum(complaints_table$prob_complaints)

## a. Construct the centerline and the upper and lower control limits for the p chart if the corporation allows a 15% complaint rate.
p = 0.15
n = 80
centerline = p ## 0.15
upper = p + 3*sqrt(p*(1-p)/n) ## 0.2697654
lower = p - 3*sqrt(p*(1-p)/n) ## 0.0302346

ggplot(complaints_table,aes(x=month,y=prob_complaints)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = centerline, color="blue") +
  geom_hline(yintercept = upper, color="red") +
  geom_hline(yintercept = lower, color="red") +
  labs(
    title = "p Chart for Customers' Complaints",
    x = "Month",
    y = "Complaints rate"
  )

## b. Can you justify the corporation’s decision to direct customers to call centers outside of Country X?
## Ans: Yes, I recommend company to direct customers to call centers outside of Country X, due to the trends is in within UCL and LCL.







