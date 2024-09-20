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

### Case Study Report 7.1
p_pop = 7.9/100
sd_pop = 1.4/100
n1 = 160
p1 = 8.18/100
n2 = 40
p2 = 8.3/100

## Which of the two samples is likely to be more representative of the United States as a whole?
## Check for Normal distribution approximation of these two samples -> n*p>=5 and n*(1-p)>=5
# Sample1
n1*p_pop ## = 12.64
n1*(1-p_pop) ## = 147.36
#Sample2
n2*p_pop ## = 3.16
n2*(1-p_pop) ## = 36.84
## Sample1 is approximate normal distribution but not Sample2

## Sample1 -> Find P(P_bar>p1) 
se_p1 = sqrt(p*(1-p)/n1) ## = 0.02822897
pnorm(p1,p,se_p1,lower.tail = FALSE) ## Ans = 0.9921531

## Sample2 -> Find P(P_bar>p2) if it is not approximate Normal distribution
se_p2 = sqrt(p*(1-p)/n2) ## 0.05645795
pnorm(p2,p,se_p2,lower.tail = FALSE) ## 0.8823317

## Conclude = Sample1 is likely to be more representative of US as a whole with 99.21% Probability that Average saving rate is 8.18%


### Additional Exercise - 64
p = 0.1

## a. What is the probability that the production machines will be adjusted if the batch consists of 50 items?
## Find P(P_bar>0.15) by n=50
n=50
## Check for this distribution is approximating Normally distributed -> n*p>=5 and n*(1-p)>=5
n*p ## 5
n*(1-p) ## 45
## Ans -> It is approximating Normally distributed
se_p_bar = sqrt(p*(1-p)/n) ## 0.04242641
pnorm(0.15,p,se_p_bar,lower.tail = FALSE) ## = 0.1192964
## ANS = There is 11.92% Probability that machines need to be adjusted if the batch consists of 50 items.

## b. What is the probability that the production machines will be adjusted if the batch consists of 100 items?
## Find P(P_bar>0.15) by n=100
n=100
## Check for this distribution is approximating Normally distributed -> n*p>=5 and n*(1-p)>=5
n*p ## 10
n*(1-p) ## 90
## Ans -> It is approximating Normally distributed
se_p_bar = sqrt(p*(1-p)/n) ## 0.03
pnorm(15,p,se_p_bar,lower.tail = FALSE) ## 0
## ANS = There is 0% Probability that machines need to be adjusted if the batch consists of 100 items.

### Additional Exercise - 65
weekly_batches = tibble(
  week = 1:6,
  nonconform_percent = c(5.5,13.1,16.8,13.6,19.8,2.0)
)

### For 50 items per batches weekly
weekly_batches_50 = weekly_batches |> 
  mutate(
    nonconform_prob = nonconform_percent/100
  )

p = 0.1
n = 50
sd_p_bar = sqrt(p*(1-p)/n)
upper = p + 3*sd_p_bar
lower = p - 3*sd_p_bar

ggplot(weekly_batches_50,aes(x=week,y=nonconform_prob)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = p, color="blue") +
  geom_hline(yintercept = upper, color="red") +
  geom_hline(yintercept = lower, color="red") +
  labs(
    title = "Nonconforming For 50 items per batch weekly",
    x = "Week",
    y = "Nonconforming Probability"
  )

### For 100 items per batches weekly
weekly_batches_100 = weekly_batches |> 
  mutate(
    nonconform_prob = nonconform_percent/100
  )

p = 0.1
n = 100
sd_p_bar = sqrt(p*(1-p)/n)
upper = p + 3*sd_p_bar
lower = p - 3*sd_p_bar

ggplot(weekly_batches_100,aes(x=week,y=nonconform_prob)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = p, color="blue") +
  geom_hline(yintercept = upper, color="red") +
  geom_hline(yintercept = lower, color="red") +
  labs(
    title = "Nonconforming For 100 items per batch weekly",
    x = "Week",
    y = "Nonconforming Probability"
  )

