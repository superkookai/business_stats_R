library(tidyverse)
options(scipen = 999)

### Exercise 10.1 - 10
### a. Set up the hypotheses to test if the report’s conclusion also applies to Lucille’s university.
### Hypothesis is below
### H0: mu1-mu2 >= 0, Ha: mu1-mu2 < 0
### This is left-tailed test with sd_pop known and normally distributed, CTL:n>30

### b. Calculate the value of the test statistic and the p-value.
d0 = 0
x_bar1 = 52000
n1 = 100
sd_pop1 = 4400
x_bar2 = 54700
n2 = 100
sd_pop2 = 1500

z = ((x_bar1-x_bar2) - d0)/sqrt(sd_pop1**2/n1 + sd_pop2**2/n2) ### -5.808131
p_value = pnorm(z) ## 0.00000000315871

### c. At the 5% significance level, can we conclude that there is a “community college penalty” at Lucille’s university?
### ANS: p_value < 0.05, So we rejected H0. This means at 5% significance level we conclude that there is community college penalty for Lucille’s university

### Exercise 10.1 - 16
### a. Construct the null and the alternative hypotheses to contradict the manager’s claim.
### Hypothesis is below
### H0: mu_suv-mu_sm <= 30, Ha: mu_suv-mu_sm > 30

### b. Compute the value of the test statistic and the p-value. Assume that the populations are normally distributed and that the variability of selling time for the SUVs and the small cars is the same.
### This is right-tailed test, normally distributed, and pop_sd is the same
d0 = 30
n_suv = 18
x_bar_suv = 95
s_suv = 32
n_sm = 38
x_bar_sm = 48
s_sm = 24

df = n_suv+n_sm-2 # 54
sp_square = ((n_suv-1)*s_suv**2 + (n_sm-1)*s_sm**2)/df # 717.037
t_df = ((x_bar_suv-x_bar_sm) - d0)/sqrt(sp_square*(1/n_suv + 1/n_sm)) # 2.218769
p_value = pt(t_df,df,lower.tail = FALSE) # 0.01536303

### c. Implement the test at α = 0.10 and interpret your results.
### ANS: p_value < 0.10, so we rejected H0. This means selling SUV is 30 days more than selling Small car as Sales manager claim.


### Exercise 10.1 - 21
### Data
# Set the seed for reproducibility
set.seed(123)

# Define the number of players in each group
num_with_nicknames <- 30
num_without_nicknames <- 30

# Average lifespan without nickname
avg_lifespan_without_nickname <- 75

# Average lifespan with nickname (2.5 years longer)
avg_lifespan_with_nickname <- avg_lifespan_without_nickname + 2.5

# Standard deviation (adjust as needed)
std_dev <- 5

# Generate lifespans for players with nicknames
lifespan_with_nickname <- rnorm(num_with_nicknames, mean = avg_lifespan_with_nickname, sd = std_dev)

# Generate lifespans for players without nicknames
lifespan_without_nickname <- rnorm(num_without_nicknames, mean = avg_lifespan_without_nickname, sd = std_dev)

# Create a data frame with player data
player_data <- data.frame(
  Nickname = c(rep("Yes", num_with_nicknames), rep("No", num_without_nicknames)),
  Lifespan = c(lifespan_with_nickname, lifespan_without_nickname)
)

View(player_data)

### a. Create two subsamples consisting of players with and without nicknames. Calculate the average longevity for each subsample.
yes_lifespan = player_data |> 
  filter(Nickname=="Yes") |> 
  select(Lifespan)
x_bar_yes = mean(yes_lifespan$Lifespan) # 77.26448
s_yes = sd(yes_lifespan$Lifespan) # 4.905154
n_yes = nrow(yes_lifespan) # 30

no_lifespan = player_data |> 
  filter(Nickname=="No") |> 
  select(Lifespan)
x_bar_no = mean(no_lifespan$Lifespan) # 75.89169
s_no = sd(no_lifespan$Lifespan) # 4.17564
n_no = nrow(no_lifespan) # 30

### b. Specify the hypotheses to contradict the claim made by the researchers.
### Hypothesis is below
### H0: mu_yes-mu_no <= 2.5 ,Ha: mu_yes-mu_no > 2.5

### c. Calculate the value of the test statistic and the p-value. Assume that the population variances are equal.
## Right-tailed test with pop_sd unknown but equally
d0 = 2.5
df = n_yes+n_no-2 # 58
sp_square = ((n_yes-1)*s_yes**2+(n_no-1)*s_no**2)/df # 20.74825
t_df = ((x_bar_yes-x_bar_no)-d0)/sqrt(sp_square*(1/n_yes+1/n_no)) # -0.9584289
p_value = pt(t_df,df,lower.tail = FALSE) # 0.8290876

### d. What is the conclusion of the test using a 5% level of significance?
### ANS: p_value > 0.05 So cannot rejected H0: So at 5% level of significance it is contradicted to researcher claim that players with nickname has lives longer than players with nonickname.

### Check with t.test function
t.test(yes_lifespan,no_lifespan,alternative = "greater",mu=2.5,var.equal = TRUE,conf.level = 0.95)


### Exercise 10.2 - 28
### a. Specify the appropriate hypotheses to test if the mean difference is greater than zero.
### Hypothesis is below
### H0: mu_d <= 0, Ha: mu_d > 0

### b. Calculate the value of the test statistic and the p-value.
## This is right-tailed test, n>30 assume normally distributed
d0 = 0
d_bar = 1.2
sd_d = 3.8
n = 35
df = n-1 # 34
t_df = (d_bar-d0)/(sd_d/sqrt(n)) # 1.868236
p_value = pt(t_df,df,lower.tail = FALSE) # 0.03518339

### c. At the 5% significance level, can you conclude that the mean difference is greater than zero? Explain.
### ANS: p_value < 0.05, so rejected H0. We can concluded that at 5% significance level the mean difference is greater than zero

### Exercise 10.2 - 35
### Data
students_sat = read_csv("data/students_sat.csv")
View(students_sat)

### a. Specify the competing hypotheses that determine whether completion of the test-prep course increases a student’s score on the real SAT.
### Hypothesis is below
### H0: mu_d >= 0, Ha: mu_d < 0

### b. Calculate the value of the test statistic and the p-value. Assume that the SAT scores difference is normally distributed.
### This is left tailed
d0 = 0
students_sat = students_sat |> 
  mutate(
    Diff = `Mock SAT` - `Real SAT`
  )
d_bar = mean(students_sat$Diff) # -48.75
sd_d = sd(students_sat$Diff) # 31.42679
n = 8
df = n-1 # 7
t_df = (d_bar-d0)/(sd_d/sqrt(n)) # -4.387525
p_value = pt(t_df,df) # 0.00160251

### c. At the 5% significance level, do the sample data support the test-prep providers’ claims?
### ANS: p_value < 0.05, so rejected H0. We can conclude that at 5% significance level completion of the test-prep course increases a student’s score on the real SAT.

### Check with t.test
t.test(students_sat$`Mock SAT`,students_sat$`Real SAT`,alternative = "less", mu=0, paired = TRUE)


### Exercise 10.2 - 36
### Data
# Set the seed for reproducibility
set.seed(123)

# Define the number of policyholders
num_policyholders <- 50

# Average premium difference (Insure-Me - Competitor)
avg_difference <- -100  # Negative indicates Insure-Me is cheaper

# Standard deviation (adjust as needed)
std_dev <- 50

# Generate random differences in premiums
premium_differences <- rnorm(num_policyholders, mean = avg_difference, sd = std_dev)

# Generate competitor premiums (assuming a base premium of $1000)
competitor_premiums <- rnorm(num_policyholders, mean = 1000, sd = 100)

# Calculate Insure-Me premiums based on differences
insure_me_premiums <- competitor_premiums + premium_differences

# Create a data frame with policyholder IDs, Insure-Me premiums, and Competitor premiums
policyholder_data <- data.frame(
  PolicyholderID = 1:num_policyholders,
  InsureMePremium = insure_me_premiums,
  CompetitorPremium = competitor_premiums
)

head(policyholder_data)
View(policyholder_data)

### a. Specify the competing hypotheses to determine whether the mean difference between the competitor’s premium and Insure-Me’s premium is over $100.
### Hypothesis is below
### H0: mu_d <= 100, Ha: mu_d > 100

### b. Calculate the value of the test statistic and the p-value.
### This is right-tailed
d0 = 100
policyholder_data = policyholder_data |> 
  mutate(
    Diff = CompetitorPremium - InsureMePremium
  )
d_bar = mean(policyholder_data$Diff) # 98.27982
sd_d = sd(policyholder_data$Diff) # 46.2935
n = nrow(policyholder_data) # 50
df = n-1 # 49
t_df = (d_bar-d0)/(sd_d/sqrt(n)) # -0.2627473
p_value = pt(t_df,df,lower.tail = FALSE) #  0.6030765

### c. What is the conclusion at the 5% significance level? What is the conclusion at the 10% significance level?
### ANS: p_value > 0.05 and p_value > 0.10. So both we cannot rejected H0. So we cannot conclude that at 5% or 10% significance level the mean difference between the competitor’s premium and Insure-Me’s premium is over $100.

### Check with t.test
t.test(policyholder_data$CompetitorPremium,policyholder_data$InsureMePremium,alternative = "greater", mu=100, paired = TRUE)


### Exercise 10.2 - 36 What-if CompetitorPremium is independently with InsureMePremium
### Hypothesis is below
### H0: mu_com - mu_in <= 100, Ha: mu_com - mu_in > 100
### This is right tailed test, n>30 is normally distributed, don't know population sd and not assume is equally
d0 = 100
n_com = 50
n_in = 50
x_bar_com = mean(policyholder_data$CompetitorPremium) # 1014.641
sd_com = sd(policyholder_data$CompetitorPremium) # 90.54472
x_bar_in = mean(policyholder_data$InsureMePremium) # 916.361
sd_in = sd(policyholder_data$InsureMePremium) # 100.2034
df = (sd_com**2/n_com+sd_in**2/n_in)**2 / (((sd_com**2/n_com)**2/(n_com-1))+((sd_in**2/n_in)**2/(n_in-1))) # 97.01015 round to 97
t_df = ((x_bar_com-x_bar_in)-d0)/sqrt(sd_com**2/n_com + sd_in**2/n_in) # -0.09006517
p_value = pt(t_df,df,lower.tail = FALSE) # 0.5357894

### ANS: p_value > 0.05 and > 0.10 -> Cannot rejected H0. Both 5% or 10% significance level we cannot conclude that the mean difference between the competitor’s premium and Insure-Me’s premium is over $100.

### Check with t.test function
t.test(policyholder_data$CompetitorPremium,policyholder_data$InsureMePremium,alternative = "greater",mu=100)





















