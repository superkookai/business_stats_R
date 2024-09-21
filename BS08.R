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

