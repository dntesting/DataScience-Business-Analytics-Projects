install.packages('tidyverse')
install.packages('lubridate')
library(tidyverse)
library(lubridate)
trans <- read.csv('Online Retail.csv', header = TRUE, 
                  stringsAsFactors = FALSE, colClasses = 'character')
head(trans)
str(trans)
sapply(trans, function(x) sum(is.na(x))) # check the number of NA - or missing - values in each column
summary(trans)

#--------------------------------------------------------------
#read in the dataset with NA values identified
#--------------------------------------------------------------

#Read in the same data into dataframe called trans_clean with an 
#additional clause in the read.csv statement - na.strings = c("", " ") 
trans_clean <-  read.csv('Online Retail.csv', header = TRUE, 
                         stringsAsFactors = FALSE, colClasses = 'character', na.string = c('', ' '))
#Explore if there are any missing values
sapply(trans_clean, function(x) sum(is.na(x)))
# There are 1454 null values in Description columns and 135080 in CustomerID Column

#Assigning the right data types - convert variables Quantity and UnitPrice to a numeric
quantity_index <- grep('Quantity', names(trans)) # Grab Index of Quantity column
price_index <- grep('UnitPrice', names(trans)) # Grab Index of UnitPrice column

#Use integer indeces to subset the columns and change their types with sapply function
trans[,c(quantity_index, price_index)] <- sapply(trans[,c(quantity_index, price_index)], function(x) as.numeric(x)) 
str(trans) # check if the data types are changed

#Dealing with dates using the lubridate package
head(unique(trans$InvoiceDate)) # To Check how are the dates currently formatted
#format of month/day/year hour:minutes 
#so we can use the mdy_hm command from the lubridate package to convert it to a date type.
trans$InvoiceDate <- mdy_hm(trans$InvoiceDate)
str(trans)

#Using dplyr to build practical features for data exploration
#create variables that represent the year, month, year+month pair and day of the invoice of that transaction.
trans <- trans %>% 
  mutate(InvoiceYear = floor_date(InvoiceDate, 'year'),
         InvoiceMonth = month(InvoiceDate),
         InvoiceYearMonth = floor_date(InvoiceDate, 'month'),
         InvoiceDay = floor_date(InvoiceDate, 'day')
  )
head(trans)

# Plot distributions with ggplot to get insights into seasonality and store operations
ggplot(trans, aes(x = factor(InvoiceYear))) + geom_bar()
# Insight 1 : Most of the data that we have was logged on 2011 and some - on 2010
# going Granular to month
ggplot(trans, aes(x = factor(InvoiceYearMonth))) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Insight 2: November seems highest but there is a possibility December being higher 
# as it is the holiday season. There maybe more data and questions to ask about possible
# missing data for 2011 December

# analyzing hourly count
ggplot(trans, aes(x = factor(hour(InvoiceDate)))) + geom_bar()
#Insight 3 : The peak purchases happen between 11 a.m. and 4 p.m.

# finding the percentage of sales happening in the peak time
sum((hour(trans$InvoiceDate) >= 11 & hour(trans$InvoiceDate) <= 16)) / nrow(trans) * 100
# Insight 4 : This slightly above 75.31%

# checking to see if there are negative values for qty
ggplot(trans, aes(x = Quantity)) + geom_density() + xlim(c(-100,100))

# share of negative transactions.
sum(trans$Quantity < 0) / nrow(trans) * 100
#Insight 5 : 1.96 % negative quantity values

# share of zero transactions.
sum(trans$Quantity == 0) / nrow(trans) * 100
# Insight 6 : no zero quantity values

# create a variable called OrderType which will have two possible values
# Return if the Quantity is less than zero and Purchase otherwise.
trans <- trans %>% mutate(OrderType = ifelse(Quantity < 0, 'Return', 'Purchase'))
head(trans)

#--------------------------------------------------------------
#explore the UnitPrice variable
#--------------------------------------------------------------
# Plot the UnitPrice variable 
ggplot(trans, aes(x = UnitPrice)) + geom_density() + xlim(c(-100,100))

# share of negative UnitPrice.
NegUnitPrice <- sum(trans$UnitPrice<0)
NegUnitPrice
NegUnitPrice / nrow(trans) * 100

# share of zero UnitPrice.
ZeroUnitPrice <- sum(trans$UnitPrice == 0)
ZeroUnitPrice
ZeroUnitPrice / nrow(trans) * 100

# Remove of negative UnitPrice.
trans <-trans %>% filter(UnitPrice == 0|UnitPrice > 0)
NegUnitPrice1 <- sum(trans$UnitPrice<0) # Check if the negative price is removed
NegUnitPrice1

# new variable called TotalSum which is a product of Quantity and UnitPrice 
# Will represent the total amount spent with that transaction.
trans <- trans %>% mutate(TotalSum = Quantity * UnitPrice)
head(trans)

# average value of the new variable TotalSum plotted on a monthly basis.
trans %>% 
  filter(OrderType == 'Purchase') %>% 
  group_by(InvoiceYearMonth) %>% 
  summarize(OrdersTotalSum = mean(TotalSum)) %>% 
  ggplot + aes(x = as.factor(InvoiceYearMonth), y = OrdersTotalSum, group = 1) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Insight 7 : the basket average transaction value increaes over time and has been the highest in the last December. 

# Explore distribution and attributes of products
# Count Unique number of different products 
length(unique(trans$Description))

# investigate a sample of the products.
trans %>% 
  group_by(Description) %>% 
  summarise(avg_price = mean(UnitPrice)) %>% 
  arrange(avg_price)

# Insight 8 : There’s one product Adjust bad debt that stands out with a large negative average price value. 
# Also there are some different product names with question marks and other symbols that have an average price of zero

#--------------------------------------------------------------
# Excluding zero priced items and analyze the product names
#--------------------------------------------------------------

# Filter the products leaving only the ones that have avg_price equal to zero

trans %>% 
  group_by(Description) %>% 
  summarise(avg_price = mean(UnitPrice)) %>% 
  filter(avg_price == 0) %>% 
  arrange(avg_price)%>% 
  print(n = 1e3)

# Insight 9 : The description of these 182 rows looks like remarks on the condition of the products . 
# It does not identify the product in any manner. 
# Thus it is going to be difficult to link these to any products. 
# Including them in the dataset will capture 182 untraceable data , which may affect the results of the other products or end up being outliers. 
# It is better to exclude from further analysis

# Quantities and names of the products that have recorded zero price transactions in descending order.
trans %>% 
  filter(UnitPrice == 0) %>%
  group_by(Description) %>% 
  summarise(quantity = sum(Quantity)) %>% 
  arrange(-quantity) %>% 
  as.data.frame() %>% 
  head(20)

# Insight 10 : ave very different product names between positive and negative Quantity values.


# Quantities and names of the products that have recorded zero price transactions in descending order.
trans %>% 
  filter(UnitPrice == 0) %>%
  group_by(Description) %>% 
  summarise(quantity = sum(Quantity)) %>% 
  arrange(-quantity) %>% 
  as.data.frame() %>% 
  tail(20)


######################################################
# Building customer summary table
######################################################

customer_summary <- trans %>% 
  group_by(CustomerID) %>% 
  summarise(
    purchase_count = n(),
    avg_price = mean(UnitPrice),
    avg_quantity = mean(Quantity),
    avg_basket_sum = mean(TotalSum),
    total_quantity = sum(Quantity),
    total_value = sum(TotalSum),
    first_purchase = min(InvoiceDate),
    last_purchase = max(InvoiceDate),
    months_active = length(unique(InvoiceYearMonth)),
    days_active = length(unique(InvoiceDay)),
    n_products = length(unique(Description))
  ) %>% 
  arrange(-total_quantity)
 
head(customer_summary)
print(customer_summary,width = Inf)

# Share of transactions belong to the customer whose ID is ’’
sum(trans$CustomerID == '')/nrow(trans)*100

# exclude this CustomerID and transactions from both trans and customer_summary datasets
trans <- trans %>% filter(CustomerID != '')
customer_summary <- customer_summary %>% filter(CustomerID != '')
head(customer_summary)

#########################################################################
# Feature engineering - calculate tenure for each customer
#########################################################################

# Set date of today is the maximum value of the date recorded in the dataset
current_date <- max(trans$InvoiceDate)

# Find Tenure ( Length in time units -days in this case) since the customer has made their first transaction
customer_summary$tenure_days <- as.integer(difftime(current_date , customer_summary$first_purchase, units = c("days")))
head(customer_summary)

# Find percentage share of the total tenure days that this customer has been active in
customer_summary$days_active_share <- customer_summary$days_active / customer_summary$tenure_days
range(customer_summary$days_active_share) # find min & Max to check for errors during the new variable creation
min(customer_summary$tenure_days) # to check whether the denominator is the issue

# Special Note #
# add 1 to the tenure_days variable. And this is ok, 
# you’re not inflating the numbers. In the real world 
# you will be creating customer summary table with data 
# that is no later than yesterday’s, so the minimum value 
# for the tenure_days variable will be 1. 

customer_summary$tenure_days <- customer_summary$tenure_days + 1 
customer_summary$days_active_share <- customer_summary$days_active / customer_summary$tenure_days
range(customer_summary$days_active_share)

# Saving for later Use
saveRDS(trans, 'trans_lesson1.rds')
saveRDS(customer_summary, 'customer_sum_lesson1.rds')


############################################################
#calculate recency
############################################################

# Find Recency ( Length in time units -days in this case) since the customer has made most recent transaction
customer_summary$recency_days <- as.integer(difftime(current_date , customer_summary$last_purchase, units = c("days")))
head(customer_summary)

min(customer_summary$recency_days) # min value of the new recency_days variable
max(customer_summary$recency_days) # max value of the new recency_days variable

mean(customer_summary$recency_days)# average value of the new recency_days variable without adding 1

# No of customers have made a their latest purchase within 30 days or less
customer_summary %>% 
  filter(recency_days <= 30) %>%
  summarize(Customer_count = n())

