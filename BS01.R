library(tidyverse)

data()

## data set
gig_employees = tibble(
  emp_id = c(1,2,3,4,5,6,7,8,9,10),
  wage = c(32.81,29.5,23.5,60,45.6,34.4,55.5,48.7,55.6,23.5),
  industry = c("Construction","Automotive", NA,"Automotive","Construction", NA,"Construction","Construction","Automotive","Construction"),
  job = c("Analyst","Engineer","Engineer","Analyst","Other","Other","Analyst","Other","Analyst","Engineer")
)

### display data
dim(gig_employees)
head(gig_employees)
tail(gig_employees, n = 3)
View(gig_employees)

## check for NA
is.na(gig_employees$industry)
which(is.na(gig_employees$industry))
gig_employees[3,]
gig_employees[6,]

# dplyr
gig_employees |> 
  filter(is.na(gig_employees$industry))

## check length
length(which(gig_employees$industry == "Automotive"))
length(which(gig_employees$wage > 30))
length(which(gig_employees$industry == "Automotive" & gig_employees$wage > 30))

# dplyr
gig_employees |> 
  filter(industry == "Automotive") |> 
  count()

## sort data
sortedData = gig_employees[order(gig_employees$wage), ]
sortedData_desc = gig_employees[order(gig_employees$wage, decreasing = TRUE), ]
sortedData_multi = gig_employees[order(gig_employees$industry, gig_employees$job, -gig_employees$wage), ]

# dplyr
gig_employees |> 
  arrange(wage)

gig_employees |> 
  arrange(desc(wage))

gig_employees |> 
  arrange(industry,job,desc(wage))


### subsetting
job = split(gig_employees, gig_employees$job)
job$Analyst
job$Engineer
job$Other

# dplyr
analyst_only = gig_employees |> 
  filter(job=="Analyst")
analyst_only



