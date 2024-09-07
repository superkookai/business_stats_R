library(tidyverse)

## data
region = c("Europe","North America","Asia","Latin America")
sales2000 = c(2860,1906,875,171)
sales2009 = c(4384,2360,2614,1006)
adidas = data.frame(region,sales2000,sales2009)

table(adidas$region)

## Exercise 2.1 - 3
day_levels = c("Mon","Tue","Wed","Thur","Fri","Sat","Sun")
purchase_by_day = tibble(
  day = factor(c("Mon","Tue","Wed","Thur","Fri","Sat","Sun"), levels = day_levels),
  frequency = c(2504,2880,3402,3566,4576,5550,5022)
)

purchase_by_day_relative = purchase_by_day |> 
  mutate(
    relative_freq = frequency / sum(frequency)
  ) |> 
  select(day,relative_freq)

ggplot(purchase_by_day_relative, aes(x=day, y=round(relative_freq*100))) +
  geom_bar(stat = "identity", fill = "lightblue", color = "gray") +
  labs(
    title = "Percent(%) purchase by Day",
    y = "Percent(%)",
    x = "Day"
  ) +
  theme_classic()

## Exercise 2.1 - 9
# Prepare data
fresh = rep("Freshman",20)
sop = rep("Sophomore",20)
junior = rep("Junior",20)
senior = rep("Senior",20)
status_full = c(fresh,sop,junior,senior)
status_60 = sample(status_full,60,prob = c(rep(0.15,20),rep(0.25,20),rep(0.5,20),rep(0.1,20)), replace = TRUE)
length(status_60)

students = tibble(
  student = 1:60,
  status = status_60
)
View(students)

students |> 
  count(status)

# Create summarise table
summarise_students = students |> 
  group_by(status) |> 
  summarise(
    freq = n()
  ) |> 
  mutate(
    rel_freq = freq / sum(freq),
    percent = round(rel_freq * 100)
  )

# Create pie-chart with base R
Frequency = table(students$status) 
pie(Frequency, main = "Pie Chart for Status of Students")

# Create Bar chart
status_levels = c("Freshman","Sophomore","Junior","Senior")
ggplot(summarise_students, aes(x=factor(status, levels = status_levels), y=freq)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "gray") +
  labs(
    title = "Bar Chart for Status of Students",
    y = "Number of Students",
    x = "Status of Students"
  ) +
  theme_classic() +
  coord_polar()

# Create Coxcomb chart from Bar chart
status_levels = c("Freshman","Sophomore","Junior","Senior")
ggplot(summarise_students, aes(x=factor(status, levels = status_levels), y=freq)) +
  geom_bar(stat = "identity", fill = "red", color = "gray") +
  labs(
    title = "Coxcomb chart for Status of Students",
    y = "Number of Students",
    x = "Status of Students"
  ) +
  coord_polar()

## Exercise 2.2 - 16
## Prepare data
shift1 = rep("1",150)
shift2 = rep("2",150)
shift3 = rep("3",150)
shift_full = c(shift1,shift2,shift3)
shift_300 = sample(shift_full,300,prob = c(rep(0.25,150),rep(0.45,150),rep(0.3,150)), replace = TRUE)
length(shift_300)

defect_yes = rep("Yes",200)
defect_no = rep("No",200)
defect_full = c(defect_yes,defect_no)
defect_300 = sample(defect_full,300,prob = c(rep(0.35,200),rep(0.65,200)), replace = TRUE)
length(defect_300)

metalworks = tibble(
  component = 1:300,
  shift = shift_300,
  defective = defect_300
)
View(metalworks)

metalworks |> 
  count(shift)
metalworks |> 
  count(defective)

## Contingency table and Bar plot with base R
contigency_table = table(metalworks$shift,metalworks$defective)
rownames(contigency_table) = c("Shift1","Shift2","Shift3")
colnames(contigency_table) = c("No","Yes")
prop.table(contigency_table)

contigency_table2 = table(metalworks$defective,metalworks$shift)
barplot(contigency_table2, main="Shift and Defective",col=c('blue','red'), 
        legend=colnames(contigency_table), xlab='Shift',ylab='Defective')

## Contingency table and Bar plot with tidyverse
con_table_dplyr = metalworks |> 
  group_by(shift,defective) |> 
  summarise(
    n = n()
  ) |> 
  pivot_wider(
    names_from = defective,
    values_from = n
  )

ggplot(metalworks,aes(x=shift,fill=defective)) +
  geom_bar() +
  labs(
    title = "Shift vs Defective",
    x = "Shift",
    y = "Number of components",
  ) +
  theme_minimal()


















