# TASK 1: DATA PREPROCESSING & TRANSFROMATION
# PIC: Nguyen Khuat Son Tra

# To begin, we set up the working directory and import neccessary library to prepare for the work.

# import libraries for data manipulation and analysis.
library(tidyverse)
library(dplyr)
library(data.table)

#0. Prepare
# import libraries for formatting tables and captions
library(knitr)
library(pander)

# setting default settings for visualization
panderOptions('table.alignment.default', 'left') # specifies the default alignment for table columns.
panderOptions('table.split.table', 80) # sets the maximum table width before spliting into multiple lines

panderOptions('round',2) # control how rounding is handled
panderOptions('digits',2) # control the number of digits displayed in numeric output

# Observe
# Importing Files from the "./Data/" directory
data = list.files("./Data/")  
data_paths = file.path("./Data/", data)  

# Reading and Combining Data
sales_raw = rbindlist(lapply(data_paths, fread))  

# Count null values in each column
null = colSums(is.na(sales_raw))
null

# ORDER ID: Since as observed, this column are clean. so we will move on and clean Product column

# PRODUCT
# Checking Unique Values in Product Column
products = unique(sales_raw$Product)
products

# Data Transformation (typo and null)
sales <- sales_raw %>%
  mutate(
    Product = case_when(
      Product %in% c("", "#### syste error###", "##system error##", "### syste error###", "Fault error", "Product") ~ NA,
      Product == "IPhone" ~ "iPhone",
      Product == "AAA Batteries (4pack)" ~ "AAA Batteries (4-pack)",
      Product == "Goo0gle Phone" ~ "Google Phone",
      Product == "Wired Headphoness" ~ "Wired Headphones",
      Product == "USBC Charging Cable" ~ "USB-C Charging Cable",
      Product == "LightCharging Cable" ~ "Lightning Charging Cable",
      TRUE ~ Product
    ))

# Remove NA values
sales <- na.omit(sales)

# Format
sales <- sales %>% 
  mutate(`Order ID` = as.integer(`Order ID`))


# QUANTITY ORDER
# Data Transformation (quantity = 0)
sales <- sales[sales$`Quantity Ordered`!= 0, ]

# Format
sales <- sales %>% 
  mutate(`Quantity Ordered` = as.integer(`Quantity Ordered`))

# PRICE EACH
# Data Transformation (dollar sign included)
sales <- sales %>% 
  mutate(
    `Price Each` = case_when(
      `Price Each` == "$11.95" ~ "11.95",
      `Price Each` == "$149.99" ~ "149.99",
      TRUE ~ `Price Each`
    ))

# Format
sales <- sales %>%
  mutate(`Price Each` = as.numeric(`Price Each`))



# ORDER DATE
# Data Transformation (wrong year entries)
sales <- sales %>%
  mutate(
    `Order Date` = case_when(
      `Order Date` == "11/17/28 12:38" ~ "11/17/19 12:38",
      `Order Date` == "12/28/01 17:19" ~ "12/28/19 17:19",
      TRUE ~ `Order Date`
    ))

# Create new columns for each 'time-indicating' variables
# Convert "Order Date" to Date-time object
sales$`Order Date` <- as.POSIXct(sales$`Order Date`, format = "%m/%d/%Y %H:%M", tz = "GMT")

# Extract day, month, year, hour, and minute
sales$Day <- as.numeric(format(sales$`Order Date`, "%d"))
sales$Month <- as.numeric(format(sales$`Order Date`, "%m"))
sales$Year <- as.numeric(format(sales$`Order Date`, "%Y"))
sales$Hour <- as.numeric(format(sales$`Order Date`, "%H"))
sales$Minute <- as.numeric(format(sales$`Order Date`, "%M"))

# Remove NA values any of the extracted columns
sales <- sales[complete.cases(sales$Day, sales$Month, sales$Year, sales$Hour, sales$Minute), ]

sales <- sales %>%
  mutate(
    `Year` = case_when(
      `Year` == 19 ~ 2019,
      `Year` == 20 ~ 2020,
      `Year` == 21 ~ 2021,
      TRUE ~ `Year`
    ))


# ADDRESS
# Create new columns for `City` and `State` from `Purchase Address`
city_state <- str_match(sales$`Purchase Address`, "(.+), (.+), (.+) (.+)")[, c(3,4)]

sales <- cbind(sales, 
               City = city_state[, 1],
               State = city_state[, 2])

# Data Transformation (typo correcting)
sales <- sales %>%
  mutate(
    City = case_when(
      City == "Las Angeles" ~ "Los Angeles",
      City == "SanFrancisco" ~ "San Francisco",
      TRUE ~ City
    ))


# NEW COLUMN: TOTAL PURCHASE AMOUNT

sales <- sales %>%
  mutate(`Total Purchase Amount` = `Quantity Ordered` * `Price Each`) # add a new column for subsequent stages 


# DATA EXPORT
write.csv(sales, file = "sales_clean.csv", row.names = FALSE)


# TASK 2: DESCRIPTIVE ANALYSIS 
# PIC: Nguyen Khuat Son Tra
# a. What is the worst year of sales and how much sales was earned? 2021, $3927
sales_per_year <- sales %>%
  group_by(Year) %>%
  summarize(sales = sum(`Total Purchase Amount`))
print(sales_per_year)

worst_year_sales = sales_per_year$sales[sales_per_year$Year == 2021]
print(worst_year_sales)

# b. How much was earned in the best Year of sales? Answer: $34,484,368.66
best_year_sales = sales_per_year$sales[sales_per_year$Year == 2019]
print(best_year_sales)

# c. In the best year of sales which was the best month for sales?  Answer: December
month_sales <- sales %>%
  filter(Year == 2019) %>%
  group_by(Month) %>%
  summarize(sales = sum(`Total Purchase Amount`)) %>%
  arrange(desc(sales))

print(month_sales)

# d. In the best year of sales how much was earned in the best month? Answer: $4,614,443.00
best_month_sales <- month_sales$sales[month_sales$Month == 12]
print(best_month_sales)

# e. Which City had the most sales in the best year of sales? Answer: San Francisco
city_sales <- sales %>%
  filter(Year == 2019) %>%
  group_by(City) %>%
  summarize(sales = sum(`Total Purchase Amount`)) %>%
  arrange(desc(sales))

print(city_sales)

# f. To maximise the likelihood of customers buying a product, what time should Dibs business 
# be displaying advertisements in the best year of sales?

library(ggplot2)

sales_hours <- sales %>%
  filter(Year == 2019) %>%
  group_by(`Order ID`, Hour) %>%
  summarize(Products = paste(Product, collapse = ","))

hourly_orders <- sales_hours %>%
  group_by(Hour) %>%
  tally()

ggplot(hourly_orders, aes(x = factor(Hour), y = n)) +
  geom_bar(stat = "identity", fill = "lightsteelblue") +
  labs(title = "Number of Orders by Hour",
       x = "Hour",
       y = "Number of Orders") +
  theme_minimal()

# 11-13 and 18-20 are two most ideal time slot for Dibs to advertise

# g. What products are most often sold together? Iphone, Lightning
multiple_products <- sales %>%
  group_by(`Order ID`) %>%
  summarize(Products = paste(Product, collapse = ",")) %>%
  filter(str_count(Products, ",") >= 1)

product_combinations <- multiple_products %>%
  group_by(Products) %>%
  summarize(Frequency = n()) %>%
  arrange(desc(Frequency))

head(product_combinations, n = 3) # Show top 3 combination





         
      












  