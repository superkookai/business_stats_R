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





