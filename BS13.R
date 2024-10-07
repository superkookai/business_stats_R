library(tidyverse)
options(scipen = 999)

### Exercise 13.1 - 10
### a. Specify the competing hypotheses in order to test the survey’s claim.
### Hypothesis is below (1=Snorkeling,2=Sailing,3=Windsurfing,4=Bowling)
### H0: All incomes are the same (mu1 == mu2 == mu3 == mu4)
### Ha: Household income of recreational athletes varies by sport

### b. Do some average incomes differ depending on the recreational sport? Explain. Assume incomes are normally distributed.

snorkeling = c(90.9,86,93.6,98.8,98.4)
sailing = c(87.6,95,94.6,87.2,82.5)
windsurfing = c(75.9,75.6,83.1,74.4,80.5)
bowling = c(79.3,75.8,79.6,78.5,73.2)
c = 4

x_bar_snorkeling = mean(snorkeling) # 93.54
s_snorkeling = sd(snorkeling) # 5.364513
x_bar_sailing = mean(sailing) # 89.38
s_sailing = sd(sailing) # 5.340599
x_bar_windsurfing = mean(windsurfing) # 77.9
s_windsurfing = sd(windsurfing) # 3.719543
x_bar_bowling = mean(bowling) # 77.28
s_bowling = sd(bowling) # 2.728919

## Between Treatment Estimate
n_T = length(snorkeling)+length(sailing)+length(windsurfing)+length(bowling) # 20
grand_mean = (sum(snorkeling)+sum(sailing)+sum(windsurfing)+sum(bowling))/n_T # 84.525
SSTR = length(snorkeling)*(x_bar_snorkeling-grand_mean)**2 + length(sailing)*(x_bar_sailing-grand_mean)**2 + 
  length(windsurfing)*(x_bar_windsurfing-grand_mean)**2 + length(bowling)*(x_bar_bowling-grand_mean)**2 # 1006.109
MSTR = SSTR/(c-1) # 335.3698

## Within Treatment Estimate
SSE = (length(snorkeling)-1)*s_snorkeling**2 + (length(sailing)-1)*s_sailing**2 + (length(windsurfing)-1)*s_windsurfing**2 + (length(bowling)-1)*s_bowling**2 # 314.328
MSE = SSE/(n_T-c) # 19.6455

## Test Statistics
df1 = c-1 # 3
df2 = n_T-c # 16
F_df1_df2 = MSTR/MSE # 17.07108
p_value = pf(F_df1_df2,df1,df2,lower.tail = FALSE) # 0.00003056294

### ANS: p_value < 0.05, so rejected H0: So at 5% significance level we can conclude that Household Income of recreational athletes varies by sport.

### Checking with aov and anova function
sport_data = tibble(
  Income = c(snorkeling,sailing,windsurfing,bowling),
  Sport = c(rep("Snorkeling",5),rep("Sailing",5),rep("Windsurfing",5),rep("Bowling",5))
)

sport_fm = aov(Income~Sport,data = sport_data) 
anova(sport_fm) 

### Exercise 13.1 - 16
### Data
library(forecast)

# Set parameters
num_weeks <- 52
avg_customers <- 100  # Average number of customers
seasonality <- 20  # Seasonal variation (e.g., higher in summer)
weekday_effect <- c(1.1, 1.3, 1.5)  # Multiplier for Friday, Saturday, and Sunday
noise <- 15  # Random noise

# Create a time series object
time <- seq(from = 1, to = num_weeks*3)

# Generate customer data
customers <- avg_customers + seasonality * sin(2 * pi * time / 52) + 
  weekday_effect[c(rep(1, 52), rep(2, 52), rep(3, 52))] * rnorm(num_weeks, 0, noise)

# Create a data frame with weekend days and customer counts
weekend_data <- data.frame(
  Week = time,
  Day = c(rep("Friday", 52), rep("Saturday", 52), rep("Sunday", 52)),
  Customers = customers
)

# View the generated data
View(weekend_data)

### Hypothesis is below
### H0: Average number of customers that frequent restaurant is the same all weekend day (mu1=mu2=mu3)
### Ha: Average number of customers that frequent restaurant is differ by weekend day

### Use aov and anova function
weekend_fm = aov(Customers~Day, data = weekend_data)
anova(weekend_fm)

### Between Treatment Estimate
grand_mean = mean(weekend_data$Customers) # 98.11637
n_T = nrow(weekend_data) # 156
c = 3

friday_customers = weekend_data |> 
  filter(Day=="Friday") |> 
  select(Customers)
x_bar_Fri = mean(friday_customers$Customers) # 98.40616
s_Fri = sd(friday_customers$Customers) # 24.83147

sat_customers = weekend_data |> 
  filter(Day=="Saturday") |> 
  select(Customers)
x_bar_Sat = mean(sat_customers$Customers) # 98.11637
s_Sat = sd(sat_customers$Customers) # 27.53812

sun_customers = weekend_data |> 
  filter(Day=="Sunday") |> 
  select(Customers)
x_bar_Sun = mean(sun_customers$Customers) # 97.82658
s_Sun = sd(sun_customers$Customers) # 30.34175

nrow(friday_customers) # 52
SSTR = 52*(x_bar_Fri-grand_mean)**2 + 52*(x_bar_Sat-grand_mean)**2 + 52*(x_bar_Sun-grand_mean)**2 # 8.733678
MSTR = SSTR/(c-1) # 4.366839

### Within Treatment Estimate
SSE = 51*s_Fri**2 + 51*s_Sat**2 + 51*s_Sun**2 # 117074.2
MSE = SSE/(n_T-c) # 765.1907

### Test Statistics
df1 = c-1 # 2
df2 = n_T-c # 153
F_df1_df2 = MSTR/MSE # 0.005706863
p_value = pf(F_df1_df2,df1,df2,lower.tail = FALSE) # 0.9943096

### ANS: p_value > 0.05, do not rejected H0. So at 5% significance level we cannot conclude that Average number of customers that frequent restaurant is differ by weekend day



