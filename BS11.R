library(tidyverse)
options(scipen = 999)

### Find the value x for which P(chi_square_10<x) = 0.05
qchisq(0.05,10,lower.tail = TRUE) # 3.940299
## This one is equivalent to P(chi_square_10>x) = 1-0.05 = 0.95
qchisq(0.95,10,lower.tail = FALSE) # 3.940299

### Find the value x for which P(chi_square_10>x) = 0.05
qchisq(0.05,10,lower.tail = FALSE) # 18.30704

### Remark when find value of x in Table -> There is only a right-tailed P(chi_square_df>x) = alpha

### Exercise 11.1 - 8
### Hypothesis is below
### H0: sigma_sq <= 2, Ha: sigma_sq > 2
data = c(2, 4, 1, 3, 2, 5, 2, 6, 1, 4)
sigma_0 = 2
sd_sample = sd(data) # 1.699673
n = length(data) # 10
df = n-1 # 9
chi_sq_df = (n-1)*sd_sample**2/sigma_0 # 13
### Right-tailed test
p_value = pchisq(chi_sq_df,df,lower.tail = FALSE) # 0.1626063
### ANS: p_value > 0.10, cannot rejected H0. So at 10% significance level we cannot conclude that the variance of population is greater than 2 squared unit.

### Exercise 11.1 - 22
### Data
# Set the seed for reproducibility
set.seed(123)

# Define the number of apartments in each town
num_apartments <- 50

# Average rent for Town A (adjust as needed)
avg_rent_town_a <- 1000

# Average rent for Town B (adjust as needed)
avg_rent_town_b <- 1150

# Standard deviation (adjust as needed)
std_dev_a <- 100
std_dev_b <- 200

# Generate rents for Town A
rents_town_a <- rnorm(num_apartments, mean = avg_rent_town_a, sd = std_dev_a)

# Generate rents for Town B
rents_town_b <- rnorm(num_apartments, mean = avg_rent_town_b, sd = std_dev_b)

# Create a data frame with town and rent data
rent_data <- data.frame(
  Town = c(rep("Town A", num_apartments), rep("Town B", num_apartments)),
  Rent = c(rents_town_a, rents_town_b)
)

View(rent_data)

### Prepare data
town_a = rent_data |> 
  filter(Town=="Town A")
n_a = nrow(town_a) # 50
s_a = sd(town_a$Rent) # 92.587

town_b = rent_data |> 
  filter(Town=="Town B")
n_b = nrow(town_b) # 50
s_b = sd(town_b$Rent) # 181.0894

### a. At the 5% level of significance, test if the standard deviation of the rent in Town 1 is below 220. Should the investment company invest in this campus town? Explain
### Hypothesis is below
### H0: sigma_sq >= 48400, Ha: sigma_sq < 48400
sigma_sq_0 = 48400
df = n_a - 1 # 49
chi_sq_df = (n_a-1)*s_a**2/sigma_sq_0 # 8.678621
## Left-tailed test
p_value = pchisq(chi_sq_df,df) # 0.00000000002104302
### ANS: p_value < 0.05, rejected H0. So at 5% significance level we can conclude that the standard deviation of the rent in Town 1 is below 220. Investment company should invest in this Town 1 due to standard deviation is below 220 as expected.

### b. At the 5% level of significance, test if the standard deviation of the rent in Town 2 is below 220. Should the investment company invest in this campus town? Explain.
### Hypothesis is below
### H0: sigma_sq >= 48400, Ha: sigma_sq < 48400
sigma_sq_0 = 48400
df = n_b - 1 # 49
chi_sq_df = (n_b-1)*s_b**2/sigma_sq_0 # 33.19992
## Left-tailed test
p_value = pchisq(chi_sq_df,df) # 0.04092251
### ANS: p_value < 0.05, so rejected H0. So we can conclude that at 5% significance level the standard deviation of the rent in Town 2 is below 220. Company can also invest in this Town2 but invest in Town1 is lower risk than Town2 due to p_value of Town2 is greater than Town1.


### Exercise 11.2 - 30
### a. Develop the hypotheses to test whether the population variances differ.
### Hypothesis is below (1=older, 2=new)
### H0: sigma_sq_1/sigma_sq_2 == 1, Ha: sigma_sq_1/sigma_sq_2 != 1

### b. Calculate the value of the test statistic and the p-value.
n1 = 26
s_sq_1 = 0.28
n2 = 26
s_sq_2 = 0.48
df1 = n1-1 # 25
df2 = n2-1 # 25
F_df1_df2 = s_sq_1/s_sq_2 # 0.5833333
## Two-tailed F-test
p_value = pf(F_df1_df2,df1,df2,lower.tail = FALSE)*2 # 1.815234

### c. Can you conclude that the variances are different at the 5% significance level? Given that all other criteria are satisfied, should the company adopt the new technology?
### ANS: p_value > 0.05, so cannot rejected H0. Company should not adopt new technology.

### Exercise 11.2 - 32
### a. State the hypotheses to test whether the variance in discharge time for the lithium-ion batteries is less than the variance for the nickel-cadmium batteries.
### Hypothesis is below (1=lithium-ion,2=nickel-cadmium)
### H0: sigma_sq_1/sigma_sq_2 >= 1, sigma_sq_1/sigma_sq_2 < 1

### b. Compute the value of the test statistic. What assumption did you make?
n1 = 16
n2 = 26
s_sq_1 = 0.44
s_sq_2 = 0.89
df1 = n1-1 # 15
df2 = n2-1 # 25
F_df1_df2 = s_sq_1/s_sq_2 # 0.494382
## This is Left-tailed F-test

### c. Find the p-value.
p_value = pf(F_df1_df2,df1,df2,lower.tail = TRUE) # 0.07902489

### d. Make a conclusion at the 5% significance level.
### ANS: p_value > 0.05, do not rejected H0. So at 5% significance level we cannot conclude that the variance in discharge time for the lithium-ion batteries is less than the variance for the nickel-cadmium batteries.

### e. Would your conclusion change at the 10% significance level?
### ANS: p_value < 0.10, so rejected H0. So at 10% significance level we can conclude that the variance in discharge time for the lithium-ion batteries is less than the variance for the nickel-cadmium


### Exercise 11.2 - 37
### Data
# Set the seed for reproducibility
set.seed(123)

# Define the number of boxes for each material type
num_boxes <- 16

# Average temperature (adjust as needed)
avg_temp <- 20

# Standard deviation for old packaging (adjust as needed)
std_dev_old <- 2

# Standard deviation for new packaging (smaller to reflect reduced variation)
std_dev_new <- 1

# Generate temperatures for old packaging
old_package_temps <- rnorm(num_boxes, mean = avg_temp, sd = std_dev_old)

# Generate temperatures for new packaging
new_package_temps <- rnorm(num_boxes, mean = avg_temp, sd = std_dev_new)

# Create a data frame with package type and temperature data
package_data <- data.frame(
  PackageType = c(rep("Old", num_boxes), rep("New", num_boxes)),
  Temperature = c(old_package_temps, new_package_temps)
)

View(package_data)

### Prepare data
old = package_data |> 
  filter(PackageType=="Old")
n_old = nrow(old) # 16
s_old = sd(old$Temperature) # 1.826405
df_old = n_old-1 # 15

new = package_data |> 
  filter(PackageType=="New")
n_new = nrow(new) # 16
s_new = sd(new$Temperature) # 0.9276535
df_new = n_new-1 # 15

### a. State the hypotheses to test whether the new packaging material reduces the variation of temperatures in the box.
### Hypothesis is below
### H0: sigma_sq_new/sigma_sq_old >= 1, Ha: sigma_sq_new/sigma_sq_old < 1

### b. Compute the value of the test statistic and the p-value.
### This is Left-tailed F-test
F_dfnew_dfold = s_new**2/s_old**2 # 0.2579748
p_value = pf(F_dfnew_dfold,df_new,df_old,lower.tail = TRUE) # 0.006346038

### c. Make a conclusion at the 5% significance level.
### ANS: p_value < 0.05, rejected H0. So we can conclude that the new packaging material reduces the variation of temperatures in the box.











