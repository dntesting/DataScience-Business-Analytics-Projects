# loading the packages and removing everything from the environment
library(tidyverse)
library(lubridate)
rm(list = ls())

#load all objects saved as RDS files
trans <- readRDS('trans_lesson2.rds')
customer_summary <- readRDS('customer_sum_lesson2.rds')
agg_fx <- readRDS('agg_fx.rds')
multiplot <- readRDS('multiplot.rds')

#Dont limit the number of columns it outputs
options(dplyr.width = Inf)

#Aggregate the data for each customer on a monthly level
monthly_summary <- trans %>% 
  group_by(CustomerID, SnapshotMonth = InvoiceYearMonth) %>% 
  summarise(purchase_count = n(),
            avg_price = mean(UnitPrice),
            avg_quantity = mean(Quantity),
            avg_basket_sum = mean(TotalSum),
            total_quantity = sum(Quantity),
            total_value = sum(TotalSum),
            days_active = length(unique(InvoiceDay)),
            n_products = length(unique(Description))
  ) %>% 
  arrange(-total_quantity)

head(monthly_summary)

#define the month when the customer “joined” the company 
#which is basically the month that the customer made their first transaction.
customer_joindate <- trans %>%
  group_by(CustomerID) %>%
  summarise(join_date = min(InvoiceYearMonth))



#join these dates to the main monthly_summary
monthly_summary <- merge(monthly_summary, customer_joindate, by = 'CustomerID', all.x = TRUE)

# no month in which the customer was active should be earlier than the first month they joined
sum(monthly_summary$SnapshotMonth < monthly_summary$join_date)

head(monthly_summary, 10)



#Aggregate customer transactions on a weekly basis 
#trans <- trans %>% 
#  mutate(InvoiceWeek = ceiling_date(InvoiceDate, 'week')
#      )
options(max.print=999999)
#head(trans, 200)

#Aggregate the data for each customer on a weekly level

#weekly_summary <- trans %>% 
#  group_by(CustomerID, SnapshotWeek = InvoiceWeek) %>% 
#  summarise(purchase_count = n(),
#            avg_price = mean(UnitPrice),
#            avg_quantity = mean(Quantity),
#            avg_basket_sum = mean(TotalSum),
#            total_quantity = sum(Quantity),
#            total_value = sum(TotalSum),
#            days_active = length(unique(InvoiceDay)),
#            n_products = length(unique(Description))
#  ) %>% 
#  arrange(-total_quantity)

#head(weekly_summary, 100)



#monthly_summary <- trans %>% 
#  group_by(CustomerID,SnapshotMonth = InvoiceYearMonth ) %>% 
#  mutate(
#    join_date_new = min(InvoiceYearMonth)
#  ) 
#head(monthly_summary, 100)

#sum(monthly_summary$SnapshotMonth < monthly_summary$join_date_new)

#list of all months
temp_join <- data.frame(SnapshotMonth_temp = unique(monthly_summary$SnapshotMonth))

#monthly_summary table merge with the temp_join table
new_monthly <- merge(monthly_summary, temp_join, all = TRUE)

#check how the top rows look like but first arrange the dataset by CustomerID and SnapshotMonth
new_monthly %>% arrange(CustomerID, SnapshotMonth) %>% slice(1:20)

#keep only those values where SnapshotMonth_temp are either equal or higher than the joining date of the customer
new_monthly <- new_monthly %>% filter(SnapshotMonth_temp >= join_date)

#keep the existing values and impute the values where customers have been inactive with zeros
#create 2 datasets - one with real existing values (i.e. months where customers have actually been active), and another one where we need to add zeros.
real_data <- new_monthly %>% filter(SnapshotMonth == SnapshotMonth_temp)
impute_data <- new_monthly %>% filter(SnapshotMonth != SnapshotMonth_temp)

#when the SnapshotMonth matches the SnapshotMonth_temp then this means it has real values, and when they 
#don’t match - it is a product of cross-join and needs to be filled with zeros.
head(impute_data)

#all values except CustomerID and SnapshotMonth_temp should be excluded.
# First, we will assign NA to them.
impute_data[,2:11] <- NA

#merge the two datasets into one by using the rbind function 
new_data <- rbind(real_data, impute_data)

#summarise the dataframe using the SnapshotMonth_temp variable
#and renaming it to SnapshotMonth
new_data <- new_data %>% 
  group_by(CustomerID, SnapshotMonth = SnapshotMonth_temp) %>% 
  summarise(purchase_count = max(purchase_count, na.rm = TRUE),
            avg_price = max(avg_price, na.rm = TRUE),
            avg_quantity = max(avg_quantity, na.rm = TRUE),
            avg_basket_sum = max(avg_basket_sum, na.rm = TRUE),
            total_quantity = max(total_quantity, na.rm = TRUE),
            total_value = max(total_value, na.rm = TRUE),
            days_active = max(days_active, na.rm = TRUE),
            n_products = max(n_products, na.rm = TRUE)
  )

#Since only one unique row per CustomerID and SnapshotMonth, 
#we can change the NA values to zeros.
start_index <- grep('purchase_count',names(new_data))
end_index <- grep('n_products',names(new_data))
new_data[,start_index:end_index] <- sapply(new_data[,start_index:end_index], function(x) ifelse(is.na(x),0,x))
new_data[,start_index:end_index] <- sapply(new_data[,start_index:end_index], function(x) ifelse(is.infinite(x),0,x))

#make sure of two things - 
# 1) we didn’t create new non-existing data, either by duplicating values or some other #unfortunate mistake, and 
# 2) we didn’t destroy data. Since we only added zeros, the total sum of all values should be identical to the original monthly_summary data.
sapply(new_data[,3:10],sum) - sapply(monthly_summary[,3:10], sum)

#calculate the lag metrics correctly and be sure that the difference will be 1 month for each customer on each month’s snapshot.
final_monthly <- new_data %>% 
  arrange(CustomerID, SnapshotMonth) %>% 
  mutate(SnapshotMonth_prev = lag(SnapshotMonth),
         purchase_count_prev = lag(purchase_count),
         avg_price_prev = lag(avg_price),
         avg_quantity_prev = lag(avg_quantity),
         avg_basket_sum_prev = lag(avg_basket_sum),
         total_quantity_prev = lag(total_quantity),
         total_value_prev = lag(total_value),
         days_active_prev = lag(days_active),
         n_products_prev = lag(n_products)
  )

#join the join_date to this dataframe, and then run the same table command 
#on the difference between the snapshot month and previous (lagged) snapshot month
#- all the values should only either 28, 30 or 31
final_monthly <- merge(final_monthly, customer_joindate, by = 'CustomerID', all.x = TRUE)
table(final_monthly$SnapshotMonth - final_monthly$SnapshotMonth_prev)

#remove the temporary dataframes we created on the go.
rm(impute_data, real_data, temp_join, monthly_summary, new_monthly, new_data)
head(final_monthly)

#There are some NA values in the SnapshotMonth_prev - we can easily solve it by just rewriting 
#it with SnapshotMonth that is one month earlier. We can do that by subtracting a 1 month from 
#it with this command.
final_monthly[, 'SnapshotMonth_prev'] <- final_monthly$SnapshotMonth - months(1)
head(final_monthly)

#check if there are any more NA values in the dataset 
sapply(final_monthly, function(x) sum(is.na(x)))

#we have fixed the SnapshotMonth_prev, some other _prev values still have NA. These are basically
#non-existent previous months’ data when the current month is the first month of the customer’s 
#transactions with the company. So we can easily assume that since there were no transactions 
#in the previous month, we can impute them with zeros.

start_index <- grep('purchase_count_prev', names(final_monthly))
end_index <- grep('n_products_prev', names(final_monthly))
final_monthly[, start_index : end_index] <- sapply(final_monthly[, start_index : end_index], function(x) ifelse(is.na(x), 0, x))

#final check if there are any NA values left.
sapply(final_monthly, function(x) sum(is.na(x)))

head(final_monthly)

# Explore the new dataset


library(ggplot2)
library(dplyr)

#avg_price density
ggplot(final_monthly, aes(x = avg_price)) + geom_density(size = 1) + coord_cartesian(xlim = c(0,50))

#avg_basket sum
ggplot(final_monthly, aes(x = avg_basket_sum)) + geom_density(size = 1) + coord_cartesian(xlim = c(0,50))

#total_quantity
ggplot(final_monthly, aes(x = total_quantity)) + geom_density(size = 1) + coord_cartesian(xlim = c(-50,150))


#highest average total_quantity
final_monthly %>% 
  group_by(SnapshotMonth) %>% 
  summarize(Month_avg_total_quantity = max(avg_quantity)) %>% 
  ggplot + aes(x = as.factor(SnapshotMonth), y = Month_avg_total_quantity, group = 1) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1))


#highest sum of n products
final_monthly %>% 
  group_by(SnapshotMonth) %>% 
  summarize(Month_sum_n_products = sum(n_products)) %>% 
  ggplot + aes(x = as.factor(SnapshotMonth), y = Month_sum_n_products, group = 1) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

saveRDS(trans, 'trans_lesson3.rds')
saveRDS(final_monthly, 'final_monthly3.rds')
