# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(caret)
library(readxl)

# Load the dataset
data <- read_csv("C:/Users/gauri/OneDrive/Documents/VCU/SCMA/IPL_ball_by_ball_updated till 2024.csv")

# Display basic information about the dataset
print(str(data))
print(head(data))

# Data Cleaning
# Check for missing values
print(colSums(is.na(data)))

# Dropping rows with missing critical values (e.g., 'runs_scored', 'Bowler', 'Striker')
data <- data %>%
  filter(!is.na(runs_scored), !is.na(Bowler), !is.na(Striker))

# Fill missing values for non-critical columns with appropriate values
data <- data %>%
  mutate(
    extras = ifelse(is.na(extras), 0, extras),
    `type of extras` = ifelse(is.na(`type of extras`), 'None', `type of extras`),
    wicket_confirmation = ifelse(is.na(wicket_confirmation), 'No', wicket_confirmation),
    wicket_type = ifelse(is.na(wicket_type), 'None', wicket_type),
    fielders_involved = ifelse(is.na(fielders_involved), 'None', fielders_involved),
    `Player Out` = ifelse(is.na(`Player Out`), 'None', `Player Out`)
  )

# Convert 'Date' to Date type
data$Date <- as.Date(data$Date, format="%Y-%m-%d")

# Feature Engineering
# Create a player performance dataset
performance <- data %>%
  group_by(Striker) %>%
  summarise(
    runs_scored = sum(runs_scored),
    balls_faced = n(),
    wickets_lost = sum(wicket_confirmation == 'Yes')
  )

performance <- performance %>%
  mutate(strike_rate = (runs_scored / balls_faced) * 100)

# Load player salaries data from the Excel file
salaries <- read_excel("C:/Users/gauri/OneDrive/Documents/VCU/SCMA/IPL SALARIES 2024.xlsx")

# Function to convert salary strings to numeric values
convert_salary <- function(salary) {
  if (grepl('crore', salary)) {
    return(as.numeric(gsub(' crore', '', salary)) * 10^7)
  } else if (grepl('lakh', salary)) {
    return(as.numeric(gsub(' lakh', '', salary)) * 10^5)
  } else {
    return(as.numeric(salary))
  }
}

# Apply conversion function to salary column
salaries$Salary <- sapply(salaries$Salary, convert_salary)

# Merge performance data with salaries
player_data <- merge(performance, salaries, by.x = 'Striker', by.y = 'Player')

# Exploratory Data Analysis (EDA)
# Plotting distributions of key variables
ggplot(player_data, aes(x = runs_scored)) +
  geom_histogram(bins = 30, fill = 'blue', alpha = 0.7) +
  labs(title = 'Distribution of Runs Scored')

ggplot(player_data, aes(x = strike_rate)) +
  geom_histogram(bins = 30, fill = 'green', alpha = 0.7) +
  labs(title = 'Distribution of Strike Rate')

ggplot(player_data, aes(x = Salary)) +
  geom_histogram(bins = 30, fill = 'red', alpha = 0.7) +
  labs(title = 'Distribution of Salaries')

# Scatter plots to visualize relationships
ggplot(player_data, aes(x = runs_scored, y = Salary)) +
  geom_point() +
  labs(title = 'Runs Scored vs Salary')

ggplot(player_data, aes(x = strike_rate, y = Salary)) +
  geom_point() +
  labs(title = 'Strike Rate vs Salary')

# Regression Analysis
# Define the feature matrix (X) and target vector (y)
X <- player_data[, c("runs_scored", "strike_rate")]
y <- player_data$Salary

# Split the data into training and testing sets
set.seed(42)
trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

# Create a linear regression model
model <- lm(y_train ~ ., data = X_train)

if (!require('car')) install.packages('car')
# Load the library
library(car)

summary(model)

# Make predictions
y_pred <- predict(model, newdata = X_test)

# Evaluate the model
mse <- mean((y_test - y_pred)^2)
r2 <- 1 - (sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2))

print(paste('Mean Squared Error:', mse))
print(paste('R^2 Score:', r2))

# Plotting the regression line
ggplot(data.frame(y_test, y_pred), aes(x = y_test, y = y_pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = 'red', linetype = 'dashed') +
  labs(title = 'Actual vs Predicted Salaries', x = 'Actual', y = 'Predicted')
