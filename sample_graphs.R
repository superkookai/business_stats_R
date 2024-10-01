library(tidyverse)

### Normal Distribution with ggplot2

# Generate a sequence of x values for the distribution
x <- seq(-3, 3, length.out = 100)

# Calculate the corresponding y values for the normal distribution
y <- dnorm(x, mean = 0, sd = 1)

# Create a data frame with x and y values
df <- data.frame(x, y)

# Create the plot using ggplot2
ggplot(df, aes(x = x, y = y)) +
  geom_line() +
  geom_vline(xintercept = 1.66, linetype="dashed") +
  labs(title = "Standard Normal Distribution", x = "x", y = "Density")
 

### Normal Distribution with base R
# Generate a sequence of x values for the distribution
x <- seq(-3, 3, length.out = 100)

# Calculate the corresponding y values for the normal distribution
y <- dnorm(x, mean = 0, sd = 1)

# Plot the normal distribution
plot(x, y, type = "l", xlab = "x", ylab = "Density", main = "Standard Normal Distribution")

# Example with a Different Mean and Standard Deviation
x <- seq(1, 9, length.out = 100)
y <- dnorm(x, mean = 5, sd = 2)
plot(x, y, type = "l", xlab = "x", ylab = "Density", main = "Normal Distribution with Mean = 5, SD = 2")