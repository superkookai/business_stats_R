library(tidyverse)

### Introductory case
# data
set.seed(123)  # Set a seed for reproducibility
fitness = data.frame(
  Attendee = 1:400,
  Age_Group = sample(c("Under 30", "Between 30 and 50", "Over 50"), 400, replace = TRUE),
  Outcome = sample(c("Enroll", "Not Enroll"), 400, replace = TRUE, prob = c(0.8, 0.2))
)

# Create group by table
fitness_table = fitness |> 
  group_by(Outcome,Age_Group) |> 
  summarise(
    freq = n()
  )

# Create Contingency table
fitness_contingency_table = fitness_table |> 
  pivot_wider(
    names_from = Age_Group,
    values_from = freq
  )

# Create Joint probability table
fitness_joint_prob_table = fitness_table |> 
  mutate(
    prob = freq / 400
  ) |> 
  select(-freq) |> 
  pivot_wider(
    names_from = Age_Group,
    values_from = prob
  )
  


