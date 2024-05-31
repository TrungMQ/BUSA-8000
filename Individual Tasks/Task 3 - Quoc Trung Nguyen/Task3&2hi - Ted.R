## TASK 2 part h & i + TASK 3: TED
# Import library
library(zoo)
library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

## TASK 2: h & i
# 2.h. Overall which product sold the most and why do you think it has sold the most?
product_sales <- sales %>%
  group_by(Product) %>%
  summarize(total_quantity = sum(`Quantity Ordered`)) %>%
  arrange(desc(total_quantity))

top_selling_product <- product_sales$Product[1]

# 2.i. What is the least sold product in the best year of sales?
# Calculate total sales for each year
total_sales_by_year <- sales %>%
  group_by(Year) %>%
  summarise(Total_Sales = sum(`Total Purchase Amount`))
# Find the year with the highest total sales
best_year <- total_sales_by_year$Year[which.max(total_sales_by_year$Total_Sales)]
# Filter data for the best year
best_year_data <- sales %>% filter(Year == best_year)

# Least sold product:
product_sales_best_year <- best_year_data %>%
  group_by(Product) %>%
  summarize(total_quantity = sum(`Quantity Ordered`)) %>%
  arrange(total_quantity)

least_sold_product <- product_sales_best_year$Product[1]
least_sold_product_sales <- product_sales_best_year$total_quantity[1]


## TASK 3: DATA VISUALIZATION
# 3.1: Monthly sales trend vs Average monthly sales
# 3.1.1: 
# Convert Month column:
# Convert: 1 to Jan, 2 to Feb, etc.
sales$Month <- factor(sales$Month, levels = 1:12, labels = month.abb)
# Summarize total sales by month
monthly_sales <- sales %>%
  group_by(Month) %>%
  summarize(`Total Purchase Amount` = sum(`Total Purchase Amount`, na.rm = TRUE))

# Plot the monthly sales trend using a bar chart with formatted labels
ggplot(data = monthly_sales, aes(x = Month, y = `Total Purchase Amount` / 1e6)) +  # Convert y values to millions
  geom_bar(stat = "identity", fill = "lightsteelblue2") +  # Bar chart
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +  # Format y-axis labels as integers
  geom_text(aes(label = round(`Total Purchase Amount` / 1e6, 1)), vjust = -0.5) +  # Add formatted text labels above bars
  labs(title = "Monthly Sales Trend", x = "Month", y = "Total Sales (in million $)") +
  theme_minimal()

## 3.1.2: Monthly Average Sales
# Summarize average sales by month
monthly_average_sales <- sales %>%
  group_by(Month) %>%
  summarize(AverageSales = mean(`Total Purchase Amount`, na.rm = TRUE))
# Find the month with the highest average sales
max_month <- monthly_average_sales$Month[which.max(monthly_average_sales$AverageSales)]

# Find the month with the second highest average sales
second_max_month <- monthly_average_sales$Month[order(monthly_average_sales$AverageSales, decreasing = TRUE)[2]]

# Plotting
ggplot(monthly_average_sales, aes(x = Month, y = AverageSales)) +
  geom_rect(aes(xmin = as.numeric(max_month) - 0.5, xmax = as.numeric(max_month) + 0.5, 
                ymin = -Inf, ymax = Inf), 
            fill = "papayawhip", alpha = 0.3) +  # Highlight month with highest Average sale
  geom_rect(aes(xmin = as.numeric(second_max_month) - 0.5, xmax = as.numeric(second_max_month) + 0.5, 
                ymin = -Inf, ymax = Inf), 
            fill = "papayawhip", alpha = 0.3) +  # Highlight month with second highest Average sale
  geom_line(group=1, color="lightsteelblue3") +  # Line plot
  geom_point(color="lightsteelblue4") +  # Points on the line plot
  geom_text(aes(label = round(AverageSales, 2)), vjust = -0.5, hjust = 0.5) +  
  theme_minimal() +  # Minimal theme
  labs(title = "Average Monthly Sales in 2019",
       x = "Month",
       y = "Average Sales ($)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust the angle of month labels for clarity


# 3.2: Total Sales by State:
# Summarize total sales by state
total_sales_by_state <- sales %>%
  group_by(State) %>%
  summarise(Total_Sales = sum(`Total Purchase Amount`) / 1000000) %>%
  arrange(desc(Total_Sales))

# Reorder State factor levels based on Total Sales and reverse the order
total_sales_by_state$State <- factor(total_sales_by_state$State, levels = rev(total_sales_by_state$State))

# Create a color gradient for the bars
color_palette <- scales::gradient_n_pal(c("lightsteelblue1", "lightsteelblue4"))

# Plot Total Sales by State in millions with numbers on top of bars
ggplot(total_sales_by_state, aes(x = Total_Sales, y = State, label = round(Total_Sales, 2))) +
  geom_bar(stat = "identity", aes(fill = Total_Sales)) +
  geom_text(hjust = -0.2, color = "black") +
  scale_fill_gradient(low = "lightsteelblue1", high = "lightsteelblue4") +
  theme_minimal() +
  labs(title = "Total Sales by State (in millions $)",
       x = "Total Sales (Millions $)",
       y = "State")

# 3.3: Top 10 products sold in the best year of sales
# Calculate total sales for each product in the best year
top_products <- best_year_data %>%
  group_by(Product) %>%
  summarise(Total_Sales = sum(`Total Purchase Amount`)) %>%
  arrange(desc(Total_Sales)) %>%
  top_n(10)

# Format total sales in millions
top_products$Total_Sales <- top_products$Total_Sales / 1000000
# Create a rank variable for the top products
top_products <- mutate(top_products, Rank = row_number())

# Define color gradient
color_palette <- scales::gradient_n_pal(c("lightsteelblue1", "lightsteelblue4"))(length(top_products$Total_Sales))

# Visualize the top 10 products in the best year with gradient colors
ggplot(top_products, aes(x = Total_Sales, y = reorder(Product, Total_Sales), fill = Total_Sales)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::number(Total_Sales, accuracy = 0.01)), hjust = -0.2, size = 3) +  # Add total sales value for each bar
  scale_fill_gradient(low = "lightsteelblue1", high = "lightsteelblue4") +  # Apply gradient color scale
  theme_minimal() +
  labs(title = paste("Top 10 Products Sold in", best_year),
       x = "Total Sales (in Millions $)",
       y = "Product") +
  scale_x_continuous(breaks = seq(1, max(top_products$Total_Sales), by = 1), labels = seq(1, max(top_products$Total_Sales), by = 1)) +  # Set x-axis 
  theme(axis.text.x = element_text(angle = 0, hjust = 1))  # Adjust angle of x-axis


## 3.3.1: visualize the other years (2020 and 2021)
# Filter for the years 2020 and 2021
sales_filtered <- sales %>%
  filter(Year %in% c(2020, 2021))

# Summarize total sales by Product and Year
top_products <- sales_filtered %>%
  group_by(Year, Product) %>%
  summarize(Total_Sales = sum(`Total Purchase Amount`, na.rm = TRUE)) %>%
  arrange(Year, desc(Total_Sales)) %>%
  group_by(Year) %>%
  slice_head(n = 10) %>%
  ungroup()

# Reorder Product levels within each Year
top_products <- top_products %>%
  group_by(Year) %>%
  mutate(Product = factor(Product, levels = Product[order(-Total_Sales)])) %>%
  ungroup()

# Create the bar plot
ggplot(top_products, aes(x = Product, y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "lightsteelblue2") +
  coord_flip() + # Flip coordinates to make the y-axis the products
  labs(title = "Top 10 Products by Total Sales for 2020 and 2021",
       x = "Product",
       y = "Total Sales (in $)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold")) +
  facet_wrap(~ Year, scales = "free_y") # Facet by Year


# 3.4: Monthly Order Trend vs Monthly Average order
# Aggregate data to get monthly total orders and average order
monthly_summary <- best_year_data %>%
  group_by(Month) %>%
  summarise(Total_Orders = sum(`Quantity Ordered`),
            Avg_Order = mean(`Quantity Ordered`))

overall_avg <- mean(monthly_summary$Total_Orders)

#Plotting the graph
ggplot(monthly_summary, aes(x = Month, y = Total_Orders)) +
  geom_bar(stat = "identity", fill = "lightsteelblue2") +
  geom_hline(yintercept = overall_avg, linetype = "dashed", color = "firebrick2") +
  annotate("text", x = Inf, y = overall_avg, label = paste("Average:", scales::comma(overall_avg)), hjust = 1, vjust = 1.2, color = "firebrick2") +
  geom_text(aes(label = comma_format()(Total_Orders)), vjust = -0.5, size = 3) +
  ylim(0, overall_avg * 1.5) +
  labs(title = "Monthly Order Trend vs Monthly Average Order",
       x = "Month",
       y = "Total Orders") +
  scale_y_continuous(labels = comma_format())


# 3.5:
# Aggregate data to get daily total orders and average order
daily_summary <- best_year_data %>%
  group_by(Day) %>%
  summarise(Total_Orders = sum(`Quantity Ordered`))

# Calculate overall average quantity ordered for all days
overall_avg_daily <- mean(daily_summary$Total_Orders)


# Plotting daily order trend with overall average line and index labels
ggplot(daily_summary, aes(x = Day, y = Total_Orders, group = 1)) +
  geom_line(color = "lightsteelblue3") +
  geom_point(color = "lightsteelblue4") +
  geom_text(aes(label = scales::comma(Total_Orders)), vjust = -0.5, hjust = 1, size = 3, angle = 45) +
  geom_hline(yintercept = overall_avg_daily, linetype = "dashed", color = "firebrick2") +
  annotate("text", x = Inf, y = overall_avg_daily, label = paste("Average:", scales::comma(overall_avg_daily)), hjust = 1, vjust = 1.2, color = "firebrick2") +
  labs(title = "Daily Order Trend vs Daily Average Order",
       x = "Day",
       y = "Total Orders") +
  scale_y_continuous(labels = scales::comma)


# 3.6: Hourly order trend vs hourly average order
# Aggregate data to get hourly total orders and average order
hourly_summary <- sales %>%
  group_by(Hour) %>%
  summarise(Total_Orders = sum(`Quantity Ordered`))

# Calculate overall average quantity ordered for all hours
overall_avg_hourly <- mean(hourly_summary$Total_Orders)

# Plotting hourly order trend with overall average line
# Plotting hourly order trend with overall average line
ggplot(hourly_summary, aes(x = factor(Hour), y = Total_Orders)) +
  geom_bar(stat = "identity", fill = "lightsteelblue2") +  # Bar chart for hourly total orders
  geom_hline(yintercept = overall_avg_hourly, linetype = "dashed", color = "firebrick2") + # Add overall average line
  geom_text(aes(label = scales::comma(Total_Orders)), vjust = -0.5, size = 3) + # Add index labels for each bar
  labs(title = "Hourly Order Trend vs Hourly Average Order",
       x = "Hour",
       y = "Total Orders") +
  scale_y_continuous(labels = scales::comma) # Format y-axis labels with commas for better readability

# Plotting hourly order trend with overall average line and index label
ggplot(hourly_summary, aes(x = factor(Hour), y = Total_Orders)) +
  geom_bar(stat = "identity", fill = "lightsteelblue2") +
  geom_hline(yintercept = overall_avg_hourly, linetype = "dashed", color = "firebrick2") +
  geom_text(aes(label = scales::comma(Total_Orders)), vjust = -0.5, size = 3) +
  annotate("text", x = Inf, y = overall_avg_hourly, label = paste("Average:", scales::comma(overall_avg_hourly)), hjust = 1, vjust = 1.2, color = "firebrick2") +
  labs(title = "Hourly Order Trend vs Hourly Average Order",
       x = "Hour",
       y = "Total Orders") +
  scale_y_continuous(labels = scales::comma)




         
      












  