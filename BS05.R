library(tidyverse)

### Exercise 5.1 - 11
grade_data = tibble(
  Grade = c("A","B","C","D","F"),
  Numerical_score = c(4,3,2,1,0),
  Prob = c(0.10,0.30,0.40,0.10,0.10)
)

## a. Probability distribution -> Positive Skew
ggplot(grade_data,aes(x=factor(Grade),y=Prob)) + 
  geom_bar(stat = "identity",fill="lightblue",color="gray") +
  geom_text(aes(label=Prob),vjust=-0.5) +
  labs(
    x = "Grade",
    y = "Probability"
  )

## b. Convert the probability distribution to a cumulative probability distribution
grade_data |> 
  mutate(
    Cumu_Prob = cumsum(Prob)
  )

## c. What is the probability of earning at least a B in Professor Sanchez’s course?
## This is P(X>=B) = P(X=B) + P(X=A) = 0.3 + 0.1 = 0.4

## d. What is the probability of passing Professor Sanchez’s course?
## This is P(X>=C) = P(X=C) + P(X=B) + P(X=A) = 0.1 + 0.3 + 0.4 = 0.8

### Exercise 5.2 - 27
invest_data = tibble(
  us_eco = c("Good","Fair","Poor"),
  us_prob = c(0.2,0.5,0.3),
  return_eu = c(10,6,-6),
  return_asia = c(18,10,-12)
)

## a. Find the expected value and the standard deviation of returns in Europe and Asia.
eu_summary = invest_data |> 
  mutate(
    x_multiply_prob = return_eu * us_prob
  )
mu_eu = sum(eu_summary$x_multiply_prob) ## = 3.2
eu_summary = eu_summary |> 
  mutate(
    x_minus_mu_square_multiply_prob = (return_eu - mu_eu)**2 * us_prob
  )
var_eu = sum(eu_summary$x_minus_mu_square_multiply_prob) ## = 38.56
sd_eu = sqrt(var_eu) ## = 6.20967

asia_summary = invest_data |> 
  mutate(
    x_multiply_prob = return_asia * us_prob
  )
mu_asia = sum(asia_summary$x_multiply_prob) # = 5
asia_summary = asia_summary |> 
  mutate(
    x_minus_mu_square_multiply_prob = (return_asia - mu_asia)**2 * us_prob
  )
var_asia = sum(asia_summary$x_minus_mu_square_multiply_prob) ## = 133
sd_asia = sqrt(var_asia) ## = 11.53256

## b. What will Janice pick as an investment if she is risk neutral?
## Ans: She will pick Asia over EU as a Risk Neutral, because mu_asia=5 is over mu_eu=3.2

## c. Discuss Janice’s decision if she is risk averse.
## Ans: If she is Risk Averse, she may choose EU over Asia eventhough the average return on EU is lower, because EU is lower risk than Asia (sd_eu=6.20967 is less than sd_asia=11.53256) 


### Exercise 5.4 - 51
minor_fraud_p = 0.0131
serious_fraud_p = 0.0087

### a. What is the probability that there are minor fraudulent activities in fewer than 2 out of 100 transactions? -> Find P(X<=2) by 100 samples
minor_n = 100
minor_x = 2

# Find P(X<=2) by 100 samples - Minor
pbinom(minor_x,minor_n,minor_fraud_p) ## Ans = 0.8558646

# Find P(X<=2) by 100 samples - Minor = P(X=0)+P(X=1)+P(X=2) 
dbinom(0,minor_n,minor_fraud_p)+dbinom(1,minor_n,minor_fraud_p)+dbinom(2,minor_n,minor_fraud_p) ## Ans = 0.8558646

### b. What is the probability that there are serious fraudulent activities in fewer than 2 out of 100 transactions? -> Find P(X<=2) by 100 samples
serious_n = 100
serious_x = 2

# Find P(X<=2) by 100 samples - Serious
pbinom(serious_x,serious_n,serious_fraud_p) ## Ans = 0.9427766

# Find P(X<=2) by 100 samples - Serious = P(X=0)+P(X=1)+P(X=2)
dbinom(0,serious_n,serious_fraud_p)+dbinom(1,serious_n,serious_fraud_p)+dbinom(2,serious_n,serious_fraud_p) ## Ans = 0.9427766
  
### Exercise 5.5 - 67

## a. Calculate the average number of claims filed on a working day.
cliams_per_day = 24584 / 260

## b. What is the probability that exactly 100 claims were filed on a working day?
## Find P(X=100)
dpois(100,cliams_per_day) ## Ans = 0.03417454

## c. What is the probability that no more than 100 claims were filed on a working day?
## Find P(X<=100)
ppois(100,cliams_per_day) ## Ans = 0.7331074


### Exercise 5.6 - 74
S = 8 # Number of Success in Population
N = 40 # Number of Population
n = 3 # Number of randomly selected

## a. Find the probability that none of these students has landed an internship.
## Find P(X=0)
x = 0
dhyper(x,S,N-S,n) ## Ans = 0.5020243

## b. Find the probability that at least one of these students has landed an internship.
## Find P(X>=1) = 1 - P(X=0)
1 - dhyper(x,S,N-S,n) ## Ans = 0.4979757


### Additional Exercise - 82
investment_table = tibble(
  Condition = c("Successful","Some Successful","Failure"),
  Prob = c(0.1,0.4,0.5),
  Return = c(5,2,-4)
)

investment_table = investment_table |> 
  mutate(
    x_Px = Return*Prob
  )

sum(investment_table$x_Px) ### Ans is Expected to loss -0.7 million dollars

### Additional Exercise - 92
## This is Binomial Distribution problem
p = 0.19
n = 30
## a. What is the probability that exactly ten of the employers have eliminated or plan to eliminate perks? -> Find P(X=10)
dbinom(10,n,p) ## Ans = 0.02722757

## b. What is the probability that at least ten employers, but no more than 20 employers, have eliminated or plan to eliminate perks? -> P(X<=20) - P(X<=9)
pbinom(20,n,p) - pbinom(9,n,p) ## Ans = 0.04507909


## c. What is the probability that at most eight employers have eliminated or plan to eliminate perks? -> Find P(X<=8)
pbinom(8,n,p) ## Ans = 0.8996469


### Additional Exercise - 98
## This is Hypergeometric Distribution
## Let's success case is Republican
S = 25
N = 50
n = 10

## a. What is the probability that there will be five Republicans and five Democrats?
## Find P(X=5)
dhyper(5,S,N-S,n) ## Ans = 0.2223625

## b. What is the probability that a majority of the committee will be Republicans?
## Find P(X>5) = P(X<=10) - P(X<=5)
phyper(10,S,N-S,n) - phyper(5,S,N-S,n) ## Ans =  0.688265






