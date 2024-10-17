library(tidyverse)
options(scipen = 999)

#### Exercise 14.1 - 11
### Data
# Define player names
player_names <- c("LeBron James", "Giannis Antetokounmpo", "Luka Doncic", 
                  "Stephen Curry", "Joel Embiid", "Kevin Durant", 
                  "James Harden", "Ja Morant", "Nikola Jokic", "Jayson Tatum")

# Define means, standard deviations, and correlation coefficient
avg_ppg_mean <- 25
avg_ppg_sd <- 5

avg_mpg_mean <- 35
avg_mpg_sd <- 3

correlation <- 0.7 # Positive correlation

# Generate correlated PPG and MPG data
library(MASS)
data <- mvrnorm(n = 10, mu = c(avg_ppg_mean, avg_mpg_mean), 
                Sigma = matrix(c(avg_ppg_sd^2, correlation * avg_ppg_sd * avg_mpg_sd,
                                 correlation * avg_ppg_sd * avg_mpg_sd, avg_mpg_sd^2), 
                               nrow = 2, ncol = 2))

# Round the values and create a data frame
basketball_data <- data.frame(Player = player_names,
                   PPG = round(data[, 1], digits = 1),
                   MPG = round(data[, 2], digits = 1))

View(basketball_data)

# Scatter Plot
ggplot(data=basketball_data, aes(x=PPG,y=MPG)) +
  geom_point(size=2,color="blue") +
  geom_smooth(method = "lm") +
  labs(
    title = "Relationship between PPG and MPG",
    x = "Point Per Game (PPG)",
    y = "Minute Per Game (MPG)"
  )

### a.Calculate and interpret the sample correlation coefficient between PPG and MPG.
### Find rxy 
n = nrow(basketball_data) # 10
x_bar = mean(basketball_data$PPG) # 26.72
s_x = sd(basketball_data$PPG) # 3.152706
y_bar = mean(basketball_data$MPG) # 33.41
s_y = sd(basketball_data$MPG) # 2.21382

sxy = sum((basketball_data$PPG-x_bar)*(basketball_data$MPG-y_bar))/(n-1) # 2.524222
rxy = sxy/(s_x*s_y) # 0.3616611
## check with cor function
cor(basketball_data$PPG,basketball_data$MPG) # 0.3616611
### ANS: The sample correlation coefficient between PPG and MPG is 0.36, positive correlation but not strong.

### b.Specify the competing hypotheses in order to determine whether the population correlation coefficient between PPG and MPG is positive.
### H0: pop_cor <= 0
### Ha: pop_cor > 0

### c.Calculate the value of the test statistic and the p-value.
### Find t_df = rxy*sqrt(n-2)/sqrt(1-rxy**2), df=n-2
t_df = rxy*sqrt(n-2)/sqrt(1-rxy**2) # 1.097202
df=n-2 # 8
p_value = pt(t_df,df,lower.tail = FALSE) # 0.1522388
## check with cor.test function
cor.test(basketball_data$PPG,basketball_data$MPG,alternative = "greater")

### d.At the 5% significance level, what is the conclusion to the test? Is this result surprising? Explain.
### ANS: p_value > 0.05, cannot rejected H0. So at 5% significance level we cannot conclude that population correlation between PPG and MPG is positive. 


### Exercise 14.2 - 35
### Data
# Set up parameters
num_houses <- 36
avg_sqft <- 2000
sqft_sd <- 300
avg_beds <- 3
beds_sd <- 1
avg_baths <- 2
baths_sd <- 0.5
avg_price <- 200000
price_sd <- 15000

# Generate random values for each variable
sqft <- round(rnorm(num_houses, mean = avg_sqft, sd = sqft_sd))
beds <- round(rnorm(num_houses, mean = avg_beds, sd = beds_sd))
baths <- round(rnorm(num_houses, mean = avg_baths, sd = baths_sd))
prices <- round(rnorm(num_houses,mean = avg_price, sd = price_sd))

# Create a data frame
house_data <- data.frame(Price = prices, Sqft = sqft, Beds = beds, Baths = baths)

View(house_data)

### a. Estimate the model Price = β0 + β1Sqft + β2Beds + β3Baths + ε.
fit_model = lm(Price ~ Sqft+Beds+Baths, data = house_data)
summary(fit_model)
### ANS: Price = 182586.7615 + 0.5377*Sqft + 26.3648*Beds + 8174.4235*Baths

### b. Interpret the slope coefficients.
### ANS: b0 is more significance to the house price, b1 is small affected by Sqft to the house price is small, b2 is also small so Beds is small affected to house price, b3 has some amount so Baths is affected to the house price greatest of all explanatory variables.

### c. Predict the price of a 2,500-square-foot home with three bedrooms and two bathrooms.
predict(fit_model,data.frame(Sqft=2500,Beds=3,Baths=2)) # 200358.9
### ANS: House Price is 200,358.9USD for a 2,500-square-foot home with three bedrooms and two bathrooms.


### Exercise 14.2 - 39
### Data
num_quarterback = 32
avg_salary = 12 # million USD
sd_salary = 9
avg_pc = 60 # Percentage
sd_pc = 4
avg_td = 20 # Points
sd_td = 2
avg_age = 20 # Year
sd_age = 2

salaries = round(rlnorm(num_quarterback, meanlog = log(avg_salary), sdlog = log(sd_salary)),2)
pc = round(rnorm(num_quarterback,avg_pc,sd_pc),2)
td = round(rnorm(num_quarterback,avg_td,sd_td),2)
age = round(rnorm(num_quarterback,avg_age,sd_age),2)

quaterback_data = data.frame(
  Salaries = salaries,
  PC = pc,
  TD = td,
  Age = age
)
View(quaterback_data)

### a. Estimate the model defined as Salary = β0 + β1PC + β2TD + β3Age + ε.
fit_quaterback = lm(Salaries ~ PC+TD+Age, data = quaterback_data)
summary(fit_quaterback)
### ANS: Salaries = 84.44 + (-9.75)*PC + 23.12*TD + 4.49*Age 

### b. Are you surprised by the estimated coefficients?
### ANS: Surprise on Pass Completion Percent (PC) coefficient is negative, but should be positive (same direction with Salaries amount)

### c. Quarterback 8 earned 12.9895 million dollars. According to the model, what is his predicted salary if PC = 70.6, TD = 34, and Age = 30?
predict(fit_quaterback,data.frame(PC=70.6,TD=34,Age=30)) # 316.8433
### ANS: 316.84 million USD

### d. Quarterback 16 earned 8.0073 million dollars. According to the model, what is his predicted salary if PC = 65.7, TD = 28, and Age = 32?
predict(fit_quaterback,data.frame(PC=65.7,TD=28,Age=32)) # 234.8653
### ANS: 234.87 million USD

### e. Compute and interpret the residual salary for Quarterback 8 and Quarterback 16.
### Data generated in this problem is different with data comes with the book.





















































