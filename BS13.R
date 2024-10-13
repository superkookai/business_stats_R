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


#### Exercise 13.3 - 30
### a. Calculate SST, SSA, SSB, and SSE.
### SSA = r*sum((x_bar_i-grand_mean)**2)
SSA = 4*(
  (2-4.25)**2 + (7-4.25)**2 + (3.75-4.25)**2
) # 51.5

### SSB = c*sum((x_bar_j-grand_mean)**2)
SSB = 3*(
  (8-4.25)**2 + (5-4.25)**2 + (2-4.25)**2 + (2-4.25)**2
) # 74.25

### SST = sum((x_ij-grand_mean)**2)
SST = (
  (4-4.25)**2 + (8-4.25)**2 + (12-4.25)**2 +
    (2-4.25)**2 + (10-4.25)**2 + (3-4.25)**2 +
    (0-4.25)**2 + (6-4.25)**2 + (0-4.25)**2 +
    (2-4.25)**2 + (4-4.25)**2 + (0-4.25)**2
) # 176.25

### SSE = SST - (SSA+SSB)
SSE = SST - (SSA+SSB) # 50.5

### b. Calculate MSA, MSB, and MSE.
### MSA = SSA/(c-1)
MSA = SSA/(3-1) # 25.75

### MSB = SSB/(r-1)
MSB = SSB/(4-1) # 24.75

### MSE = SSE/(n_T-c-r+1)
MSE = SSE/(12-3-4+1) # 8.416667

### d. At the 5% significance level, can you conclude that the column means differ?
### Hypothesis is below
### H0: All column means are the same (mu1==mu2==mu3)
### Ha: Not all column means are the same
df1 = 3-1
df2 = 12-3-4+1
F_df1_df2 = MSA/MSE # 3.059406
p_value = pf(F_df1_df2,df1,df2,lower.tail = FALSE) # 0.1213595
### ANS: p_value > 0.05, cannot rejected H0. So at 5% significance level we cannot conclude that all column means are differ.

### e. At the 5% significance level, can you conclude that the row means differ?
### Hypothesis is below
### H0: All row means are the same (mu1==mu2==mu3)
### Ha: Not all row means are the same
df1 = 4-1
df2 = 12-3-4+1
F_df1_df2 = MSB/MSE # 2.940594
p_value = pf(F_df1_df2,df1,df2,lower.tail = FALSE) #  0.1209108
### ANS: p_value > 0.05, cannot rejected H0. So at 5% significance level we cannot conclude that all row means are differ.

#### Exercise 13.3 - 40
### Factor A (Column) = Advertising strategies [Newspaper,Internet,TV,Internet&TV]
### Factor B (Row) = Store locations [City,Suburban,Rural]
### Data
yumyum = tibble(
  Sales = c(511,644,585,712,458,548,503,614,388,298,347,421),
  Ads = c("Newspaper","Internet","TV","Internet&TV","Newspaper","Internet","TV","Internet&TV","Newspaper","Internet","TV","Internet&TV"),
  Locations = c("City","City","City","City","Suburban","Suburban","Suburban","Suburban","Rural","Rural","Rural","Rural")
)
View(yumyum)

### Hypothesis is below for Factor A (Column) = Advertising strategies
### H0: All Sales means of Advertising strategies are the same [mu1==mu2==mu3==mu4]
### Ha: Not all Sales means of Advertising strategies are the same

### Hypothesis is below for Factor B (Row) = Store locations [City,Suburban,Rural]
### H0: All Sales means of Store locations are the same [mu1==mu2==mu3]
### Ha: Not all Sales means of Store locations are the same

### Using aov and anova functions
yum_fm = aov(Sales~Ads+Locations ,data = yumyum)
anova(yum_fm)


### Calculate Test Statistics Manually: Find SSA,SSB,SST,SSE,MSA,MSB,MSE,F_df1_df2
newspaper = yumyum |> 
  filter(Ads=="Newspaper")
x_bar_newspaper = mean(newspaper$Sales) # 452.3333

internet = yumyum |> 
  filter(Ads=="Internet")
x_bar_internet = mean(internet$Sales) # 496.6667

tv = yumyum |> 
  filter(Ads=="TV")
x_bar_tv = mean(tv$Sales) # 478.3333

inter_tv = yumyum |> 
  filter(Ads=="Internet&TV")
x_bar_inter_tv = mean(inter_tv$Sales) # 582.3333

city = yumyum |> 
  filter(Locations=="City")
x_bar_city = mean(city$Sales) # 613

suburban = yumyum |> 
  filter(Locations=="Suburban")
x_bar_suburban = mean(suburban$Sales) # 530.75

rural = yumyum |> 
  filter(Locations=="Rural")
x_bar_rural = mean(rural$Sales) # 363.5

grand_mean = mean(yumyum$Sales) # 502.4167
r = 3
c = 4
n_T = nrow(yumyum) # 12

SSA = r*((x_bar_newspaper-grand_mean)**2 + (x_bar_internet-grand_mean)**2 + (x_bar_tv-grand_mean)**2 + (x_bar_inter_tv-grand_mean)**2) # 28524.25
SSB = c*((x_bar_city-grand_mean)**2 + (x_bar_suburban-grand_mean)**2 + (x_bar_rural-grand_mean)**2) # 129317.2
SST = sum(
  (yumyum$Sales-grand_mean)**2
) # 173026.9
SSE = SST - (SSA+SSB) # 15185.5

MSA = SSA/(c-1) # 9508.083
MSB = SSB/(r-1) # 64658.58
MSE = SSE/(n_T-c-r+1) # 2530.917

### F for Ads
df1 = c-1 # 3
df2 = n_T-c-r+1 # 6
F_ads = MSA/MSE # 3.756775
p_value_ads = pf(F_ads,df1,df2,lower.tail = FALSE) # 0.07879812

### F for Locations
df1_lo = r-1 # 2
df2_lo = n_T-c-r+1 # 6
F_lo = MSB/MSE # 25.5475
p_value_lo = pf(F_lo,df1_lo,df2_lo,lower.tail = FALSE) # 0.001160539


### a. At the 5% significance level, can you conclude that the mean sales differ among the advertising strategies? What about the 10% significance level?
### ANS According to anova function: p_value of Ads is 0.078798 which greater than 0.05, so cannot rejected H0, so at 5% significance level we cannot conclude that mean sales differ among the advertising strategies. But for 10% significance (p_value < 0.10) so we can rejected H0, so at 10% significance we can conclude that mean sales differ among the advertising strategies. [These answers are corrected after checking with manually]

### b. At the 5% significance level, can you conclude that the mean sales differ across the store locations?
### ANS: p_value is 0.001160539 (same as in anova function) which is less than 0.05, so can rejected H0. So at 5% significance we can concluded that mean sales differ across the store locations.

### c. If significant differences exist across advertising strategies, use Fisher’s LSD method at the 10% significance level to find which strategies have different mean sales.
df_fisher = n_T-c-r+1 # 6
alpha = 0.10
t_alpha_2_df = qt(alpha/2,df_fisher,lower.tail = FALSE) # 1.94318

### Ads - Newspaper v Internet
lower_news_int = (x_bar_newspaper-x_bar_internet) - t_alpha_2_df*sqrt(MSE*(1/3+1/3)) # -124.1524
upper_news_int = (x_bar_newspaper-x_bar_internet) + t_alpha_2_df*sqrt(MSE*(1/3+1/3)) # 35.48569
### Found Newspaper v Internet: [-124.1524,35.48569] -> Has zero, so mean Newspaper not differ mean Internet

### Ads - Newspaper v TV
lower_news_tv = (x_bar_newspaper-x_bar_tv) - t_alpha_2_df*sqrt(MSE*(1/3+1/3)) # -105.819
upper_news_tv = (x_bar_newspaper-x_bar_tv) + t_alpha_2_df*sqrt(MSE*(1/3+1/3)) # 53.81902
### Found Newspaper v TV: [-105.819,53.81902] -> Has zero, so mean Newspaper not differ mean TV

### Ads - Newspaper v Internet&TV
lower_news_intertv = (x_bar_newspaper-x_bar_inter_tv) - t_alpha_2_df*sqrt(MSE*(1/3+1/3)) # -209.819
upper_news_intertv = (x_bar_newspaper-x_bar_inter_tv) + t_alpha_2_df*sqrt(MSE*(1/3+1/3)) # -50.18098
### Found Newspaper v Internet&TV: [-209.819,-50.18098] -> No zero, so mean Newspaper differ mean Internet&TV

### Ads - Internet v TV
lower_inter_tv = (x_bar_internet-x_bar_tv) - t_alpha_2_df*sqrt(MSE*(1/3+1/3)) # -61.48569
upper_inter_tv = (x_bar_internet-x_bar_tv) + t_alpha_2_df*sqrt(MSE*(1/3+1/3)) # 98.15235
### Found Internet v TV: [-61.48569,98.15235] -> Has zero, so mean Internet not differ mean TV

### Ads - Internet v Internet&TV
lower_inter_intertv = (x_bar_internet-x_bar_inter_tv) - t_alpha_2_df*sqrt(MSE*(1/3+1/3)) # -165.4857
upper_inter_intertv = (x_bar_internet-x_bar_inter_tv) + t_alpha_2_df*sqrt(MSE*(1/3+1/3)) # -5.847647
### Found Internet v Internet&TV: [-165.4857,-5.847647] -> No zero, so mean Internet differ mean Internet&TV

### Ads - TV v Internet&TV
lower_tv_intertv = (x_bar_tv-x_bar_inter_tv) - t_alpha_2_df*sqrt(MSE*(1/3+1/3)) # -183.819
upper_tv_intertv = (x_bar_tv-x_bar_inter_tv) + t_alpha_2_df*sqrt(MSE*(1/3+1/3)) # -24.18098
### Found TV v Internet&TV: [-183.819,-24.18098] -> No zero, so mean TV differ mean Internet&TV

### ANS: At 10% significance level we can conclude that Sales mean for Internet&TV is differ among other advertising strategies. Internet&TV has Sales mean = 582 (USD1000) which is the greatest among others.

### Checking with TukeyHSD function
TukeyHSD(yum_fm,conf.level = 0.90)


