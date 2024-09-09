library(tidyverse)

#### Example 3.1 - 12
# data
stock_A = tibble(
  date = c("Jan","Jul","Dec"),
  price_per_share = c(19.58,24.06,29.54),
  num_of_share = c(70,80,50)
)

relative_table = stock_A |> 
  mutate(
    relative_freq = num_of_share / sum(num_of_share),
    xi_wi = price_per_share * relative_freq
  )

# Weighted mean
sum(relative_table$xi_wi)
# Mean
mean(relative_table$price_per_share)

## Calculate Percentile
# data
funds = data.frame(
  metals = c(-7.34,18.33,33.35,59.45,8.09,43.79,34.30,36.13,-56.02,76.46),
  income = c(4.07,6.52,9.38,18.62,9.44,3.12,8.15,5.44,-11.37,31.77)
)

# summary function
summary(funds$metals)

# quantile function
quantile(funds$metals, 0.5)

### Example 3.2 - 22
summary(mpg$hwy)
IQR = 27 - 18 # Q3 - Q1
limit = IQR + 1.5
left = 18 - 12 # Q1 - Min
right = 44 - 27 # Max - Q3

ggplot(mpg,aes(x=hwy)) +
  geom_boxplot() +
  geom_vline(xintercept = mean(mpg$hwy), linetype="dashed", color="red")

### Exercise 3.3 - 37
# data
revenues = tibble(
  year = c(1,2,3),
  revenue_1 = c(379.8,404.3,408.2),
  revenue_2 = c(63.4,65.0,65.3)
)

avg_growth_rate = function(revenue){
  n = length(revenue)
  xn = revenue[n]
  x1 = revenue[1]
  avg = (xn/x1)^(1/(n-1)) - 1
  return(avg)
}

### range function
range(revenues$revenue_1)

### mean absolute deviation using function
mean(abs(revenues$revenue_1 - mean(revenues$revenue_1)))

### variance and standard deviation function
var(revenues$revenue_1)
sd(revenues$revenue_1)

### CV
sd(revenues$revenue_1) / mean(revenues$revenue_1)
sd(revenues$revenue_2) / mean(revenues$revenue_2)

## Covariance and Correlation coefficient
cov(revenues)
cor(revenues)

cov(revenues$revenue_1,revenues$revenue_2)
cor(revenues$revenue_1,revenues$revenue_2)


### Exercise 3.7 - 77
# Data
## use rnorm function 
age_vector = rnorm(1000, mean = 42, sd = 5)
age_vector = pmax(26, pmin(75, round(age_vector)))

happy_vector = rnorm(1000, mean = 65, sd = 10)
happy_vector = pmax(0, pmin(100, round(happy_vector)))

happiness_age = tibble(
  age = age_vector,
  happiness = happy_vector
)

# find Correlation coefficient
cor(happiness_age$age,happiness_age$happiness)

# Scatter plot
ggplot(happiness_age,aes(x=age,y=happiness)) +
  geom_point()















