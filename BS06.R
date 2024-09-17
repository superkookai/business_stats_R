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
















