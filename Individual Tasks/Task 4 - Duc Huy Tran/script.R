#install and load packages
packages <- c("readr", "dplyr", "ggplot2", "lubridate", "randomForest", "caret", "tidyr", "tsibble")


install_if_missing <- function(p) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
  }
  library(p, character.only = TRUE)
}

lapply(packages, install_if_missing)




# Load the data
data <- read_csv('sales_clean(draft).csv')

# Convert 'Order.Date' to Date format
names(data) <- make.names(names(data))
data$Order.Date <- as.POSIXct(data$Order.Date, format="%Y-%m-%d %H:%M:%S")

# Remove rows where Order.Date year is NA or 2020
data <- data[!is.na(year(data$Order.Date)) & year(data$Order.Date) != 2020, ]
data$Total.Sales <- data$Quantity.Ordered * data$Price.Each

# Add a new column with just the date part of Order.Date
data <- data %>% mutate(Order.Date.NoTime = as.Date(Order.Date))

# Group by the new column to get daily sales
daily_sales <- data %>%
  group_by(Order.Date.NoTime) %>%
  summarise(Total.Sales = sum(Total.Sales))

# Remove rows with NA values if any
daily_sales <- na.omit(daily_sales)

# Create weekly sales
weekly_sales <- daily_sales %>%
  mutate(Week = floor_date(Order.Date.NoTime, "week")) %>%
  group_by(Week) %>%
  summarise(Total.Sales = sum(Total.Sales))

# Remove rows with NA values if any
weekly_sales <- na.omit(weekly_sales)

# Define start date components for time series conversion
start_year <- year(min(weekly_sales$Week))
start_week <- week(min(weekly_sales$Week))

# Convert dataframe to time-series
ts_data <- ts(weekly_sales$Total.Sales, start = c(start_year, start_week), frequency = 52)

# Define the proportion for training
train_prop <- 0.8

# Calculate the index that splits the data
split_index <- round(length(ts_data) * train_prop)

# Create the training and test sets
train <- window(ts_data, end = c(start_year + floor(split_index / 52), split_index %% 52))
test <- window(ts_data, start = c(start_year + floor(split_index / 52), (split_index %% 52) + 1))

# Determine the forecast horizon
forecast_horizon <- length(test)

# Apply linear trend model
lt_model <- tslm(train ~ trend)

# Predict using linear trend model
lt_forecast <- forecast(lt_model, h = forecast_horizon)

# Apply SES model with sufficient horizon
ses_model <- ses(train, h = forecast_horizon, alpha = NULL)

# Predict using SES model
ses_forecast <- forecast(ses_model, h = forecast_horizon)

# Check accuracy of Linear Trend Model
lt_accuracy <- accuracy(lt_forecast, test)
print(lt_accuracy)

# Check accuracy of SES Model
ses_accuracy <- accuracy(ses_forecast, test)
print(ses_accuracy)

# Plot the actual and forecasted values for Linear Trend Model
p1 <- autoplot(lt_forecast) + 
  autolayer(test, series = "Test", PI = FALSE) +
  ggtitle("Linear Trend Model Forecast") +
  xlab("Time") + ylab("Sales") +
  theme_minimal()

# Plot the actual and forecasted values for SES Model
p2 <- autoplot(ses_forecast) + 
  autolayer(test, series = "Test", PI = FALSE) +
  ggtitle("SES Model Forecast") +
  xlab("Time") + ylab("Sales") +
  theme_minimal()

# Print the plots
print(p1)
print(p2)

# Compare models based on accuracy metrics
comparison <- data.frame(
  Model = c("Linear Trend", "SES"),
  ME = c(lt_accuracy[1, "ME"], ses_accuracy[1, "ME"]),
  RMSE = c(lt_accuracy[1, "RMSE"], ses_accuracy[1, "RMSE"]),
  MAE = c(lt_accuracy[1, "MAE"], ses_accuracy[1, "MAE"]),
  MPE = c(lt_accuracy[1, "MPE"], ses_accuracy[1, "MPE"]),
  MAPE = c(lt_accuracy[1, "MAPE"], ses_accuracy[1, "MAPE"]),
  ACF1 = c(lt_accuracy[1, "ACF1"], ses_accuracy[1, "ACF1"]),
  TheilU = c(lt_accuracy[1, "TheilU"], ses_accuracy[1, "TheilU"])
)

print(comparison)