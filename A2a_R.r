# Load necessary libraries
library(readr)   # For reading CSV files
library(dplyr)   # For data manipulation
library(ggplot2) # For plotting
library(car)     # For regression diagnostics

# Load the dataset
df <- read_csv("C:/Users/gauri/OneDrive/Documents/VCU/SCMA/NSSO68.csv")

# Check the structure of the dataset and column names
str(df)

# Summary of the dataset
summary(df)

# Define the dependent variable (foodtotal_v) and independent variables
Y <- df$foodtotal_v
X <- df[, c("pickle_v", "sauce_jam_v", "Beveragestotal_v")]

# Fit the regression model
model <- lm(Y ~ pickle_v + sauce_jam_v + Beveragestotal_v, data = df)

# Print the summary of the regression
summary(model)

# Regression diagnostics
# Plot residuals vs fitted values
par(mfrow = c(2, 2))
plot(model)

# Q-Q plot of residuals
qqPlot(model, main="Q-Q Plot")

# Breusch-Pagan test for heteroscedasticity
bptest(model)

# Check for multicollinearity using VIF
vif(model)

# Plot residuals vs fitted values
plot(model, which = 1)

# Q-Q plot of residuals
plot(model, which = 2)

# Scale-location plot (to check homoscedasticity)
plot(model, which = 3)

# Cook's distance plot (to identify influential points)
plot(model, which = 4)

# Check for multicollinearity using VIF
library(car)
vif(model)

# Example: If multicollinearity is an issue, remove highly correlated variables or use PCA

# Step 4.1: If transformation is needed for non-linearity
# Log-transform the dependent variable (example)
df$log_foodtotal_v <- log(df$foodtotal_v)

# Check for missing values in foodtotal_v
sum(is.na(df$foodtotal_v))
sum(is.na(df$pickle_v))
sum(is.na(df$sauce_jam_v))
sum(is.na(df$Beveragestotal_v))

# Check for infinite values in foodtotal_v
sum(!is.finite(df$foodtotal_v))
sum(!is.finite(df$pickle_v))
sum(!is.finite(df$sauce_jam_v))
sum(!is.finite(df$Beveragestotal_v))

df <- df[complete.cases(df$foodtotal_v), ]

# Check for negative values in foodtotal_v
sum(df$foodtotal_v <= 0)

# Add a small constant to avoid log of zero or negative values
epsilon <- 1e-6  # Small positive constant
df$log_foodtotal_v <- log(df$foodtotal_v + epsilon)

# Exclude rows where log_foodtotal_v is NA, NaN, or Inf
df <- df[is.finite(df$log_foodtotal_v), ]

# Check the data type of log_foodtotal_v
class(df$log_foodtotal_v)

# Check summary statistics of log_foodtotal_v
summary(df$log_foodtotal_v)

# Fit the regression model with log-transformed dependent variable
model_corrected <- lm(log_foodtotal_v ~ pickle_v + sauce_jam_v + Beveragestotal_v, data = df)

# Print the summary of the corrected regression
summary(model_corrected)

# Perform regression diagnostics again
par(mfrow = c(2, 2))
plot(model_corrected)
qqPlot(model_corrected, main="Q-Q Plot")

# Breusch-Pagan test for heteroscedasticity
bptest(model_corrected)

# Check for multicollinearity using VIF
vif(model_corrected)

# Check the R-squared and adjusted R-squared values
rsquared <- summary(model_corrected)$r.squared
adjusted_rsquared <- summary(model_corrected)$adj.r.squared

cat("R-squared:", rsquared, "\n")
cat("Adjusted R-squared:", adjusted_rsquared, "\n")

# Review the coefficients and their p-values
coefficients <- coef(model_corrected)
pvalues <- summary(model_corrected)$coefficients[, 4]

results <- data.frame(Coefficient = coefficients, P_Value = pvalues)
print(results)

# Analyze residual plots for normality and homoscedasticity
plot(model_corrected, which = c(1, 2))

# Q-Q plot to check residuals for normality
plot(model_corrected, which = 2)

# Perform Breusch-Pagan test for heteroscedasticity
library(lmtest)
bptest(model_corrected)

# Evaluate VIF values to identify multicollinearity
library(car)
vif(model_corrected)

# Print the final model summary
summary(model_corrected)
