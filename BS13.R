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


### Exercise 13.2 - 28
### Data
# Estimated average days absent per year (source may vary)
avg_absent <- c(First = 7, Second = 9, Third = 10)

# Standard deviation (assuming 10% of average)
sd_absent <- avg_absent * 0.5

# Simulate data for each worker in each shift
set.seed(123)  # Set random seed for reproducibility
workers <- c("First", "Second", "Third")
shift_data <- lapply(workers, function(shift) {
  worker_shifts <- paste(shift, "Shift")
  days_absent <- rnorm(25, avg_absent[shift], sd_absent[shift])
  data.frame(worker_shifts, days_absent)
})

# Combine data for all shifts
all_data <- do.call(rbind, shift_data)

# Print the data
view(all_data)

### a. At the 5% significance level, can the production manager conclude that the mean days of absenteeism differ among the three shifts? Does the conclusion change at the 10% significance level? Assume that days are normally distributed.
### Hypothesis is below (1=First,2=Second,3=Third)
### H0: The mean days of absenteeism is the same among the three shifts (mu1==mu2==mu3)
### Ha: The mean days of absenteeism differ among the three shifts

### Use aov and anova function
shifts_fm = aov(days_absent~worker_shifts, data = all_data)
anova(shifts_fm)
### ANS: p_value=0.02033 less than 0.05, so rejected H0. So at 5% significance level we can conclude that the mean days of absenteeism differ among the three shifts

### Use Test Statistics F_df1_df2
### Find Between-Treatments Estimate of σ2: MSTR
grand_mean = mean(all_data$days_absent) # 8.798056

first_shift = all_data |> 
  filter(worker_shifts=="First Shift")
x_bar_1 = mean(first_shift$days_absent) # 6.883344
sd_1 = sd(first_shift$days_absent) # 3.313563
n1 = nrow(first_shift) # 25

second_shift = all_data |> 
  filter(worker_shifts=="Second Shift")
x_bar_2 = mean(second_shift$days_absent) # 9.459618
sd_2 = sd(second_shift$days_absent) # 4.13493
n2 = nrow(second_shift) # 25

third_shift = all_data |> 
  filter(worker_shifts=="Third Shift")
x_bar_3 = mean(third_shift$days_absent) # 10.05121
sd_3 = sd(third_shift$days_absent) # 4.862107
n3 = nrow(third_shift) # 25

SSTR = n1*(x_bar_1-grand_mean)**2 + n2*(x_bar_2-grand_mean)**2 + n3*(x_bar_3-grand_mean)**2 # 141.8542
c = 3
MSTR = SSTR/(c-1) # 70.92712

### Find Within-Treatments Estimate of σ2: MSE
SSE = (n1-1)*sd_1**2 + (n2-1)*sd_2**2 + (n3-1)*sd_3**2 # 1241.218
n_T = nrow(all_data) # 75
MSE = SSE/(n_T-c) # 17.23915

### Find Test Statistics
df1 = c-1 # 2
df2 = n_T-c # 72
F_df1_df2 = MSTR/MSE # 4.114306
p_value = pf(F_df1_df2,df1,df2,lower.tail = FALSE) # 0.02032899
### ANS: p_value < 0.05, rejected H0. So at 5% significance we can conclude that the mean days of absenteeism differ among the three shifts

### b. If significant differences exist, use Fisher’s LSD method at the 10% significance level to determine which shifts have different mean days of absenteeism.

### Use Fisher’s LSD method at the 10% significance level
alpha = 0.1
df_t = n_T-c # 72
t_alpha_2_df = qt(alpha/2,df_t,lower.tail = FALSE) # 1.666294

### mu1-mu2 Range
lower_1_2 = (x_bar_1-x_bar_2)-t_alpha_2_df*sqrt(MSE*(1/n1+1/n2)) # -4.53311
upper_1_2 = (x_bar_1-x_bar_2)+t_alpha_2_df*sqrt(MSE*(1/n1+1/n2)) # -0.6194384
## mu1-mu2 Found [-4.53311,-0.6194384] -> No zero in the range so mu1 is differ with mu2

### mu1-mu3 Range
lower_1_3 = (x_bar_1-x_bar_3)-t_alpha_2_df*sqrt(MSE*(1/n1+1/n3)) # -5.124697
upper_1_3 = (x_bar_1-x_bar_3)+t_alpha_2_df*sqrt(MSE*(1/n1+1/n3)) # -1.211025
## mu1-mu3 Found [-5.124697,-1.211025] -> No zero in the range so mu1 is differ with mu3

### mu2-mu3 Range
lower_2_3 = (x_bar_2-x_bar_3)-t_alpha_2_df*sqrt(MSE*(1/n2+1/n3)) # -2.548423
upper_2_3 = (x_bar_2-x_bar_3)+t_alpha_2_df*sqrt(MSE*(1/n2+1/n3)) # 1.365249
## mu2-mu3 Found [-2.548423,1.365249] -> Has zero in the range so mu2 is not differ from mu3

### ANS: At 10% significance level we can concluded that mean days of absenteeism on First Shift is differ from Second and Third Shift. Absenteeism on First Shift is lowest.

### Check with TukeyHSD function
TukeyHSD(shifts_fm,conf.level = 0.90) 

### Check with Tukey's Confidence interval by calculate manually - For balance data
alpha = 0.1
df_q = n_T-c # 72
q_alpha_df_q = qtukey(p=0.1,nmeans = c,df=df_q,lower.tail = FALSE) # 2.949345
n = n1 = n2 = n3 # 25

### mu1-mu2 Range
lower_1_2 = (x_bar_1-x_bar_2) - q_alpha_df_q*sqrt(MSE/n) # -5.025413
upper_1_2 = (x_bar_1-x_bar_2) + q_alpha_df_q*sqrt(MSE/n) # -0.1271353
## mu1-mu2 Found [-5.025413,-0.1271353] -> No zero in the range so mu1 is differ with mu2

### mu1-mu3 Range
lower_1_3 = (x_bar_1-x_bar_3) - q_alpha_df_q*sqrt(MSE/n) # -5.617
upper_1_3 = (x_bar_1-x_bar_3) + q_alpha_df_q*sqrt(MSE/n) # -0.7187222
## mu1-mu3 Found [-5.617,-0.7187222] -> No zero in the range so mu1 is differ with mu3

### mu2-mu3 Range
lower_2_3 = (x_bar_2-x_bar_3) - q_alpha_df_q*sqrt(MSE/n) # -3.040726
upper_2_3 = (x_bar_2-x_bar_3) + q_alpha_df_q*sqrt(MSE/n) # 1.857552
## mu2-mu3 Found [-1.105446,-0.07772804] -> Has zero in the range so mu2 is not differ with mu3

### ANS: At 10% significance level we can concluded that mean days of absenteeism on First Shift is differ from Second and Third Shift. Absenteeism on First Shift is lowest.




