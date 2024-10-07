library(tidyverse)
options(scipen = 999)

### Exercise 12.1 - 8
### 1=Europe, 2=North America, 3=Other
p1=1/3
p2=1/3
p3=1/3
k=3
df=k-1
n=380
o=c(148,106,126)
e=c(p1*n,p2*n,p3*n)

### a.A recent visitor to Zimbabwe believes that the tourism director’s claim is wrong. Set up the competing hypotheses to test the visitor’s belief.
### Hypothesis is below
### H0: p1=1/3,p2=1/3,p3=1/3, Ha: Not all population proportion equal their hypothesized values (p=1/3)

### b.Conduct the test at the 5% significance level. Do the sample data support the visitor’s belief?
chi_sq_df = sum((o-e)**2/e) # 6.968421
p_value = pchisq(chi_sq_df,df,lower.tail = FALSE) # 0.03067797
### ANS: p_value < 0.05, rejected H0. So at 5% significance level we can conclude that not all population proportion equal to 1/3


### Exercise 12.1 - 9
### Data
n = 500
region = c("Europe","North America","Asia Pacific","Latin America","Middle East","Africa")
p_2003 = c(0.357,0.314,0.229,0.043,0.043,0.014)
o_now = c(153,163,139,20,20,5)
millionaires_table = data.frame(
  Region = region,
  P_2003 = p_2003,
  O_now = o_now
)
millionaires_table = millionaires_table |> 
  mutate(
    E_2003 = P_2003*n,
    O_minus_E = O_now - E_2003
  )

### Hypothesis is below
### H0: Population proportion as in p_2003, Ha: Not all population proportion is as p_2003

### a. Test whether the distribution of millionaires today is different from the distribution in 2003 at α = 0.05.
k = 6
df = k-1 # 5
chi_sq_df = sum(millionaires_table$O_minus_E**2/millionaires_table$E_2003) # 9.895245
p_value = pchisq(chi_sq_df,df,lower.tail = FALSE) # 0.07825842
### ANS: p_value > 0.05, do not rejected H0. So at 5% significance level we cannot conclude that Not all population proportion is as p_2003.

### b. Would the conclusion change if we tested it at α = 0.10?
### ANS: p_value < 0.10, so rejected H0, So at 10% significance level we can conclude that Not all population proportion is as p_2003

### Check with chisq.test function
chisq.test(millionaires_table$O_now,p=millionaires_table$P_2003) 


### Exercise 12.2 - 16
### Data
auto_data = tibble(
  id = 1:6,
  sex = c("Male","Female","Male","Female","Male","Female"),
  color = c("Silver","Silver","Black","Black","Red","Red"),
  freq = c(470,280,535,285,495,350)
)

sum_male = sum(auto_data |> 
                 filter(sex=="Male") |> 
                 select(freq)
                 ) # 1500

sum_female = sum(auto_data |> 
                 filter(sex=="Female") |> 
                 select(freq)
                ) # 915

sum_silver = sum(
  auto_data |> 
    filter(color=="Silver") |> 
    select(freq)
) # 750
 
sum_black = sum(
  auto_data |> 
    filter(color=="Black") |> 
    select(freq)
) # 820

sum_red = sum(
  auto_data |> 
    filter(color=="Red") |> 
    select(freq)
) # 845

total = sum_male + sum_female # 2415

Eij = c(sum_silver*sum_male/total,sum_silver*sum_female/total,sum_black*sum_male/total,sum_black*sum_female/total,sum_red*sum_male/total,sum_red*sum_female/total)

df = (3-1)*(2-1) # 2

chi_sq_2 = sum((auto_data$freq-Eij)**2/Eij) # 7.995596
p_value = pchisq(chi_sq_2,df,lower.tail = FALSE) # 0.01835602

### a. Specify the competing hypotheses to determine whether color preference depends on the automobile buyer’s sex.
### Hypothesis is below
### H0: Color preference and Automobile buyer's sex is independent
### Ha: Color preference and Automobile buyer's sex is dependent

### b. Calculate the value of the test statistic and the p-value.
### ANS: p_value < 0.05, rejected H0. So at 5% significance level we can concluded that Color preference and Automobile buyer's sex is dependent

### c. Does your conclusion suggest that the company should target advertisements differently for males versus females? Explain
### ANS: Yes, the company should target advertisements differently for males versus females. Because sex has relationship with color of automobile, so for example we may advertise Red car to Female but Black car to Male.


### Exercise 12.2 - 24
### Data
# Set the seed for reproducibility
set.seed(123)

# Define the number of participants
n <- 290

# Generate happiness data (assuming a binomial distribution)
happiness <- sample(c("Happy", "Not Happy"), n, replace = TRUE, prob = c(0.7, 0.3))

# Generate income data (assuming a multinomial distribution)
income <- sample(c("Low", "Medium", "High"), n, replace = TRUE, prob = c(0.3, 0.4, 0.3))

# Combine data into a data frame
survey_data <- data.frame(
  Individual = 1:n,
  Happiness = happiness, 
  Income = factor(income,levels = c("High","Medium","Low"))
)

# View the generated data
View(survey_data)

### a. Use the data to construct a contingency table.
survey_table = survey_data |> 
  group_by(Income,Happiness) |> 
  summarise(
    Freq = n()
  )

survey_contingency = survey_table |> 
  pivot_wider(
    names_from = Happiness,
    values_from = Freq
  )

col_sums = colSums(survey_contingency[,2:3])
survey_contingency = rbind(survey_contingency,col_sums)
row_sums = rowSums(survey_contingency[,2:3])
survey_contingency$Total_Income = row_sums
survey_contingency$Income = coalesce(survey_contingency$Income,"Total_Happy")

### b. Specify the competing hypotheses to determine whether happiness is related to income.
### H0: Happiness is not related to Income
### Ha: Happiness is related to Income

### c. Conduct the test at the 5% significance level and make a conclusion.
Oij = c(62,15,83,41,66,23)
Eij = c(77*211/290,77*79/290,124*211/290,124*79/290,89*211/290,89*79/290)
df = (3-1)*(2-1) # 2
chi_sq_2 = sum((Oij-Eij)**2/Eij) # 4.54914
p_value = pchisq(chi_sq_2,df,lower.tail = FALSE) #  0.1028411
### ANS: p_value > 0.05, not rejected H0. So at 5% significance level we cannot concluded that Happiness is related to Income


### Exercise 12.3 - 28
### a. Using the goodness-of-fit test for normality, state the competing hypotheses in order to determine if the normal distribution is inappropriate for making grades.
### H0: The grading is normal distribution with x_bar=72, sd=10
### Ha: The grading is not normal distribution with x_bar=72, sd=10

### b. Calculate the value of the test statistic and the p-value.
### P(X<50)
p_F = pnorm(50,72,10) # 0.01390345
### P(50<=X<70)
p_D = pnorm(70,72,10)-pnorm(50,72,10) # 0.4068368
### P(70<=X<80)
p_C = pnorm(80,72,10)-pnorm(70,72,10) # 0.3674043
### P(80<=X<90)
p_B = pnorm(90,72,10)-pnorm(80,72,10) # 0.1759251
### P(X>=90)
p_A = pnorm(90,72,10,lower.tail = FALSE) # 0.03593032

n = 300
grades_freq_table = tibble(
  Range = c("F","D","C","B","A"),
  Freq = c(5,135,105,45,10),
  Prob = c(p_F,p_D,p_C,p_B,p_A)
)
grades_freq_table = grades_freq_table |> 
  mutate(
    E = n*Prob,
    O_minus_E = Freq-E
  )
sum(grades_freq_table$E) # 300
df = 5-3 # 2
chi_sq_2 = sum(grades_freq_table$O_minus_E**2/grades_freq_table$E) # 2.988343
p_value = pchisq(chi_sq_2,df,lower.tail = FALSE) # 0.2244345

### c. At the 5% significance level, what is the conclusion to the test?
### ANS: p_value > 0.05, do not rejected H0. So at 5% significance level we cannot conclude that the normal distribution is inappropriate for making grades.

### Exercise 12.3 - 32
### Hypothesis is below
### H0: CEO compensation follow normal distribution with mean=19.03, standard deviation=27.61
### Ha: CEO compensation not follow normal distribution with mean=19.03, standard deviation=27.61

### a.Conduct a goodness-of-fit test for normality of CEO compensation at the 1% significance level. Does total compensation of CEOs not follow the normal distribution?
p_less5 = pnorm(5,19.03,27.61) # 0.3056743
p_5to10 = pnorm(10,19.03,27.61)-pnorm(5,19.03,27.61) # 0.06613864
p_10to15 = pnorm(15,19.03,27.61)-pnorm(10,19.03,27.61) # 0.07016286
p_15to20 = pnorm(20,19.03,27.61)-pnorm(15,19.03,27.61) # 0.07203699
p_more20 = pnorm(20,19.03,27.61,lower.tail = FALSE) # 0.4859872

n = 238
ceo_table = tibble(
  Range = c("Less than 5","Between 5 to 10","Between 10 to 15","Between 15 to 20","More than 20"),
  O_Freq = c(43,65,32,38,60),
  Prob = c(p_less5,p_5to10,p_10to15,p_15to20,p_more20)
)
sum(ceo_table$O_Freq) # 238
ceo_table = ceo_table |> 
  mutate(
    E = n*Prob,
    O_minus_E = O_Freq-E
  )
sum(ceo_table$E) # 238

df = 5-3 #2
chi_sq_2 = sum(ceo_table$O_minus_E**2/ceo_table$E) # 232.4931
p_value = pchisq(chi_sq_2,df,lower.tail = FALSE) # 0.00000
### ANS: p_value < 0.01, rejected H0. At 1% significance level we can conclude that ceo compensation not follow normal distribution.

### b. Conduct the Jarque-Bera test at the 1% significance level. Does total compensation of CEOs not follow the normal distribution?
### H0: S=0 and K=0
### H1: S!=0 and K!=0
S=5.26
K=35.53
chi_sq_2 = (n/6)*(S**2+(K**2)/4) # 13616.09
p_value = pchisq(chi_sq_2,2,lower.tail = FALSE) # 0
### ANS: p_value < 0.01, rejected H0. At 1% significance level we can conclude that ceo compensation not follow normal distribution.

### Exercise 12.3 - 34
### Data
library(forecast)
library(ggplot2)

# Set parameters
n <- 100  # Number of weeks
start_price <- 50  # Initial stock price
trend <- 0.1  # Weekly upward trend
seasonality <- 0.5  # Seasonal variation (e.g., higher in spring/summer)
noise <- 0.2  # Random noise

# Create a time series object
time <- seq(from = 1, to = n)

# Generate the stock prices
stock_prices <- start_price + trend * time + seasonality * sin(2 * pi * time / 52) + rnorm(n, 0, noise)

# DataFrame
weekly_stock_prices = data.frame(time, stock_prices)

# Plot the stock prices
ggplot(weekly_stock_prices, aes(x = time, y = stock_prices)) +
  geom_line() +
  labs(title = "Weekly Stock Prices for a Home Improvement Firm",
       x = "Week",
       y = "Stock Price")

### a. Using the Jarque-Bera test, state the competing hypotheses in order to determine whether or not the firm's weekly stock prices follow the normal distribution.
### H0: S=0, K=0
### Ha: S!=0, K!=0

### b. Calculate the value of the Jarque-Bera test statistic and the p-value.
library(moments)
# Calculate skewness and kurtosis
S = skewness(weekly_stock_prices$stock_prices) # 0.008412622
K = kurtosis(weekly_stock_prices$stock_prices) # 1.602802
df = 2
chi_sq_2 = (n/6)*(S**2+(K**2)/4) # 10.70523
p_value = pchisq(chi_sq_2,df,lower.tail = FALSE) # 0.004735742

### c. At α = 0.05, can you conclude that stock prices are not normally distributed?
### ANS: p_value < 0.05, rejected H0. At 5% significance level we can conclude that weekly stock prices for home improvement firm not follow normal distribution.

### jarque.bera.test() from the tseries package.
library(tseries)
jarque.bera.test(weekly_stock_prices$stock_prices)



### Suggest case study Report 12.1
### Data
library(dplyr)

# Set parameters
num_students <- 50
problems <- c("Drug Addiction", "Gun Violence", "Terrorism", "College Affordability")

# Generate random responses
survey_data <- data.frame(
  Student = 1:num_students,
  MostImportantProblem = sample(problems, num_students, replace = TRUE),
  stringsAsFactors = FALSE
)

# View the generated data
View(survey_data)

### Prepare data
survey_problems_table = survey_data |> 
  group_by(MostImportantProblem) |> 
  summarise(
    Count = n()
  )

### Test on Terrorism and Other
### Hypothesis is below (1=Terrorism,2=Other)
### H0: p1=0.53, p2=0.47, Ha: Not all proportion population equal hypothesized value
terrorism = survey_problems_table |> 
  mutate(
    Problems = if_else(MostImportantProblem!="Terrorism","Others","Terrorism")
  ) |> 
  select(Problems,Count) |> 
  group_by(Problems) |> 
  summarise(
    Freq = sum(Count)
  )

terrorism_table = terrorism |> 
  mutate(
    Prop = c(0.47,0.53),
    E = 50*Prop,
    std_sq_deviation = (Freq-E)**2/E
  )

df=2-1 # 1
chi_sq_1 = sum(terrorism_table$std_sq_deviation) # 10.61823
p_value = pchisq(chi_sq_1,df,lower.tail = FALSE) # 0.001119784
### ANS: p_value < 0.05, rejected H0. At 5% significance level students view on Terrorism problem is not equal hypothesized value

### Test on Drug Addiction and Other
### Hypothesis is below (1=Drug Addiction,2=Other)
### H0: p1=0.68, p2=0.32, Ha: Not all proportion population equal hypothesized value
drug = survey_problems_table |> 
  mutate(
    Problems = if_else(MostImportantProblem!="Drug Addiction","Others","Drug Addiction")
  ) |> 
  select(Problems,Count) |> 
  group_by(Problems) |> 
  summarise(
    Freq = sum(Count)
  )

drug_table = drug |> 
  mutate(
    Prop = c(0.68,0.32),
    E = 50*Prop,
    std_sq_deviation = (Freq-E)**2/E
  )
df=2-1 # 1
chi_sq_1 = sum(drug_table$std_sq_deviation) # 44.48529
p_value = pchisq(chi_sq_1,df,lower.tail = FALSE) # 0.0000000000256275
### ANS: p_value < 0.05, rejected H0. At 5% significance level students view on Drug Addiction problem is not equal hypothesized value


### Data2
library(dplyr)

# Set parameters
num_students <- 50
problems <- c("DrugAddiction", "GunViolence", "Terrorism", "CollegeAffordability")
response_scale <- c("StronglyDisagree", "Disagree", "Neutral", "Agree", "StronglyAgree")

# Generate random responses
survey_data <- data.frame(
  Student = 1:num_students,
  stringsAsFactors = FALSE
)

for (problem in problems) {
  survey_data[[problem]] <- sample(response_scale, num_students, replace = TRUE)
}

# View the generated data
View(survey_data)

### Drug Addiction
survey_data$DrugAddiction = 
  factor(survey_data$DrugAddiction, levels = c("StronglyDisagree", "Disagree", "Neutral", "Agree", "StronglyAgree"))
drug_addiction = survey_data |> 
  select(Student,DrugAddiction) |> 
  group_by(DrugAddiction) |> 
  summarise(
    Freq = n()
  )
## Hypothesis is below (1=StronglyDisagree,2=Disagree,3=Neutral,4=Agree,5=StronglyAgree) 
## H0: p1=p2=p3=p4=0.08,p5=0.68; Ha: Not all population proportion equals hypothesized values
drug_addiction = drug_addiction |> 
  mutate(
    Prop = c(0.08,0.08,0.08,0.08,0.68),
    E = num_students*Prop,
    std_sq_deviation = (Freq-E)**2/E
  )
k=5
df=k-1 # 4
chi_sq_df = sum(drug_addiction$std_sq_deviation) # 70.13235
p_value = pchisq(chi_sq_df,df,lower.tail = FALSE) # 0.000000000000021284
### ANS: p_value < 0.05, rejected H0. At 5% significance level we can conclude that Drug addict proportion is not the same with year 2018.

### E of StronglyDisagree,Disagree,Neutral,Agree is less than 5
## Hypothesis is below (1=StronglyDisagree+Disagree+Neutral,2=Agree+StronglyAgree) 
## H0: p1=0.24,p2=0.76; Ha: Not all population proportion equals hypothesized values
drug_addiction2 = drug_addiction |> 
  mutate(
    DrugAddiction2 = case_when(
      DrugAddiction %in% c("StronglyDisagree","Disagree","Neutral") ~ "NotAgree",
      DrugAddiction %in% c("Agree","StronglyAgree") ~ "Agree"
    )
  ) |> 
  select(DrugAddiction2,Freq) |>
  group_by(DrugAddiction2) |>
  summarise(
    Freq = sum(Freq)
  ) |> 
  mutate(
    Prop = c(0.76,0.24),
    E = num_students*Prop,
    std_sq_deviation = (Freq-E)**2/E
  )
k=2
df=k-1 # 1
chi_sq_1 = sum(drug_addiction2$std_sq_deviation) # 28.07018
p_value = pchisq(chi_sq_1,df,lower.tail = FALSE) # 0.000000116995
### ANS: p_value < 0.05, rejected H0. At 5% significance level we can conclude that Drug addict proportion value is not the same as of year 2018.


### Additional Exercise - 38
### Data
breaks_data = tibble(
  Delivery = c("In Person","Phone","Email","Facebook","Instant Message"),
  FB_results = c(0.47,0.3,0.04,0.05,0.14),
  Research_results = c(0.55,0.25,0.06,0.05,0.09)
)
sum(breaks_data$FB_results)
sum(breaks_data$Research_results)

### Conduct Goodness-of-fit test
### Hypothesis is below.
### H0: p1=0.47,p2=0.3,p3=0.04,p4=0.05,p5=0.14
### Ha: Not all population proportion follow FB_results

n = 200
k = 5 
df = k-1 # 4

breaks_table = breaks_data |> 
  mutate(
    O = Research_results*n,
    E = FB_results*n,
    std_sq_diviation = (O-E)**2/E
  )

chi_sq_4 = sum(breaks_table$std_sq_diviation) # 9.961499
p_value = pchisq(chi_sq_4,df,lower.tail = FALSE) # 0.04108124

### ANS: p_value < 0.05, rejected H0. So at 5% significance level we can conclude that Researcher's results is difference with FB results.But if we lower significance level to 1%, p_value > 0.01, not rejected H0, so we can conclude that Reseracher's results is not difference with FB results.


### Additional Exercise - 43
### a. State the competing hypotheses to test whether a person’s sex and race are dependent when making a choice to serve in the military.
### H0: Person’s sex and race are independent when making a choice to serve in the military.
### H1: Person’s sex and race are dependent when making a choice to serve in the military.

### b. Conduct the test at the 5% significance level.
military_data = tibble(
  Item = 1:6,
  O_Freq = c(1098,678,549,484,355,64),
  Race = c("Hispanic","Black","White","Hispanic","Black","White"),
  Sex = c("Male","Male","Male","Female","Female","Female")
)
n = sum(military_data$O_Freq) # 3228

sum_male = sum(
  military_data |> 
    filter(Sex=="Male") |> 
    select(O_Freq)
) # 2325

sum_female = sum(
  military_data |> 
    filter(Sex=="Female") |> 
    select(O_Freq)
) # 903

sum_hispanic = sum(
  military_data |> 
    filter(Race=="Hispanic") |> 
    select(O_Freq)
) # 1582

sum_black = sum(
  military_data |> 
    filter(Race=="Black") |> 
    select(O_Freq)
) # 1033

sum_white = sum(
  military_data |> 
    filter(Race=="White") |> 
    select(O_Freq)
) # 613

df = (2-1)*(3-1) # 2
Eij = c(sum_male*sum_hispanic/n,sum_male*sum_black/n,sum_male*sum_black/n,
        sum_female*sum_hispanic/n,sum_female*sum_black/n,sum_female*sum_black/n)
chi_sq_df = sum((military_data$O_Freq-Eij)**2/Eij) # 252.6052
p_value = pchisq(chi_sq_df,df,lower.tail = FALSE) # 0

### ANS: p_value < 0.05, rejected H0. So at 5% significance level we can conclude that race and sex are dependent for serving military's services.



### Additional Exercise - 51
### Hypothesis is below
### H0: Program generate random numbers follow normally distributed with mean=100, sd=10
### Ha: Program generate random numbers not follow normally distributed with mean=100, sd=10

### Data
program_data = tibble(
  Value = factor(
    c("Under 70","70 to 80","80 to 90","90 to 100","100 to 110","110 to 120","120 to 130","130 or more"), 
    levels = c("Under 70","70 to 80","80 to 90","90 to 100","100 to 110","110 to 120","120 to 130","130 or more"  )),
  Freq = c(12,99,658,1734,1681,697,112,7)
)
n = sum(program_data$Freq) # 5000

p_under70 = pnorm(70,100,10) # 0.001349898
p_vectors = c(p_under70)
for (point in seq(from = 70, to = 120, by = 10)){
  p_norm = pnorm(point+10,100,10)-pnorm(point,100,10)
  p_vectors = append(p_vectors,p_norm)
}
p_over130 = pnorm(130,100,10,lower.tail = FALSE)
p_vectors = append(p_vectors,p_over130)
sum(p_vectors)

program_data$Prop = p_vectors
program_table = program_data |> 
  mutate(
    E = Prop*n,
    std_sq_div = (Freq-E)**2/E
  )
k = 8 
df = k-1 # 7
chi_sq_df = sum(program_table$std_sq_div) # 6.880432
p_value = pchisq(chi_sq_df,df,lower.tail = FALSE) #  0.4414346

### ANS: p_value > 0.05, cannot rejected H0. So at 5% significance level we cannot conclude that Program generate random number not follow normal distribution with mean=100, sd=10. 








