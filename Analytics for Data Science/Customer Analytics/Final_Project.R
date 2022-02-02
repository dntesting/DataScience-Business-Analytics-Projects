library(tidyverse)
library(lubridate)
rm(list = ls())
#load all the objects we have saved as RDS files
trans <- readRDS('trans_lesson3.rds')
final_monthly <- readRDS('final_monthly3.rds')
#Don’t limit the number of columns that are printed out.
options(dplyr.width = Inf)
#############################################################
#####Creating customer segments New and recent from a monthly summary dataset
############################################################
final_monthly[, 'customer_status'] <- ifelse(final_monthly$SnapshotMonth == final_monthly$join_date, 'New', 
                                             ifelse(final_monthly$SnapshotMonth <= final_monthly$join_date + months(3), 'Recent', 'Existing'
                                             ))
###summarize the dataset quickly on a monthly cadence and newly created customer_status
final_monthly %>% 
  group_by(SnapshotMonth, customer_status) %>% 
  summarise(total_quantity = sum(total_quantity), purchase_count = sum(purchase_count)) %>% 
  arrange(SnapshotMonth) %>% 
  as.data.frame()

#simple plot to see how many different types of customers are active over time 
ggplot(final_monthly, aes(x = SnapshotMonth, fill = customer_status)) + geom_bar(position = 'dodge')

#######################################
#create a retention status segment
#######################################
final_monthly[, 'retained_status'] <- ifelse(final_monthly$purchase_count > 0 
                                             & final_monthly$purchase_count_prev > 0 , 'Retained', 
                                             
                                             ifelse(final_monthly$purchase_count > 0 
                                                    & final_monthly$purchase_count_prev == 0 
                                                    & final_monthly$customer_status != 'New', 'Reactivated',
                                                    
                                                    ifelse(final_monthly$purchase_count == 0 
                                                           & final_monthly$purchase_count_prev > 0, 'Inactive',
                                                           
                                                           ifelse(final_monthly$purchase_count == 0 
                                                                  & final_monthly$purchase_count_prev == 0 
                                                                  & final_monthly$SnapshotMonth >= final_monthly$join_date + months(2), 'Churned', 'New' 
                                                           ))))
#check if all of our formulas worked and the last assignment to New matches the customer_status which also has the same segment.
#We will count the number of rows for each segment where the value is New.
final_monthly %>% filter(customer_status == 'New') %>% group_by(retained_status) %>% summarise(n())
final_monthly %>% filter(retained_status == 'New') %>% group_by(customer_status) %>% summarise(n())

##########################################################################
#   create Segment product_status
#   we will check whether the customer has increased the number
#   of monthly items in the basket this month, or bought less, or bought the same number.
###########################################################################

final_monthly[, 'product_status'] <- ifelse(final_monthly$customer_status == 'New', 'New incremental products',
                                            ifelse(final_monthly$n_products == final_monthly$n_products_prev, 'Same number of products',
                                                   ifelse(final_monthly$n_products - final_monthly$n_products_prev > 0, 'More products', 'Less products'
                                                   )))
# Calculate retention  % rates
final_monthly[, 'active_flag_prev'] <- ifelse(final_monthly$purchase_count_prev > 0, 'Active', 'Inactive')
final_monthly %>% group_by(SnapshotMonth) %>% summarise(Retention_rate = round(sum(retained_status == 'Retained') / sum(active_flag_prev == 'Active'), 3) * 100)%>%ggplot(aes(x=SnapshotMonth, y=Retention_rate, group=1)) +
  geom_line(color="red")+
  geom_point()

# Calculate Reactivation  % rates
final_monthly %>% group_by(SnapshotMonth) %>% summarise(Reactivation_rate = round(sum(retained_status == 'Reactivated') / sum(active_flag_prev == 'Inactive'), 3) * 100)%>%ggplot(aes(x=SnapshotMonth, y=Reactivation_rate, group=1)) +
  geom_line(color="blue")+
  geom_point()

#simple plot to see  retention status of customers over time 
ggplot(final_monthly, aes(x = SnapshotMonth, fill = retained_status)) + geom_bar(position = 'dodge')

#simple plot to see  product status of customers over time 
ggplot(final_monthly, aes(x = SnapshotMonth, fill = product_status)) + geom_bar(position = 'dodge')


##########################################################################
# Top Quantity Segment
#################################################################

final_monthly[, 'segment_total_quantity'] <- ifelse(final_monthly$total_quantity >= 2000, '1. Top Quantity Segment',
                                                    ifelse(final_monthly$total_quantity >= 500, '2. Frequency Segment',
                                                           ifelse(final_monthly$total_quantity >= 100, '3. Average segment',
                                                                  ifelse(final_monthly$total_quantity >= 0, '4. Low quantity segment', '5. Negative quantity segment'
                                                                  ))))
ggplot(final_monthly,aes(x = SnapshotMonth, fill = segment_total_quantity)) + geom_bar(position = 'dodge')


##########################################################################
# “Basket Segment”- average value of a typical transaction
##########################################################################

final_monthly[, 'segment_avg_basket_sum'] <- ifelse(final_monthly$avg_basket_sum >= 150, '1. Merry Shopper',
                                                    ifelse(final_monthly$avg_basket_sum >= 50, '2. Big Shopper',
                                                           ifelse(final_monthly$avg_basket_sum >= 10, '3. Average Shopper',
                                                                  ifelse(final_monthly$avg_basket_sum >= 0, '4. Thrifty Shopper', '5. Fussy Shopper'
                                                                  ))))

ggplot(final_monthly,aes(x = SnapshotMonth, fill = segment_avg_basket_sum)) + geom_bar(position = 'dodge')


##########################################################################
# Total value Segment”
##########################################################################

final_monthly[, 'segment_totalvalue'] <- ifelse(final_monthly$total_value >= 150, '1. Diamond Customer',
                                                ifelse(final_monthly$total_value >= 50, '2. Gold Shopper',
                                                       ifelse(final_monthly$total_value >= 10, '3. Silver Shopper',
                                                              ifelse(final_monthly$total_value >= 0, '4. Bronze Shopper', '5. Casual Shopper'
                                                              ))))


ggplot(final_monthly,aes(x = SnapshotMonth, fill = segment_totalvalue)) + geom_bar(position = 'dodge')

# A.   Customer_Status Segment  comparison with total quantity and avg basket sum

ggplot(final_monthly,aes( x = SnapshotMonth, y= total_quantity, fill =customer_status)) + geom_bar(position = 'dodge',stat='identity')
ggplot(final_monthly,aes( x = SnapshotMonth, y= avg_basket_sum, fill =customer_status)) + geom_bar(position = 'dodge',stat='identity')

# B.	retained_status  Segment  comparison with total quantity and avg basket sum

ggplot(final_monthly,aes( x = SnapshotMonth, y= total_quantity, fill =retained_status)) + geom_bar(position = 'dodge',stat='identity')
ggplot(final_monthly,aes( x = SnapshotMonth, y= avg_basket_sum, fill =retained_status)) + geom_bar(position = 'dodge',stat='identity')

# C.	product_status  Segment  comparison with total quantity and avg basket sum

ggplot(final_monthly,aes( x = SnapshotMonth, y= total_quantity, fill =product_status)) + geom_bar(position = 'dodge',stat='identity')
ggplot(final_monthly,aes( x = SnapshotMonth, y= avg_basket_sum, fill =product_status)) + geom_bar(position = 'dodge',stat='identity')

# D.	segment_totalvalue  Segment  comparison with total quantity and avg basket sum

ggplot(final_monthly,aes( x = SnapshotMonth, y= total_quantity, fill =segment_totalvalue)) + geom_bar(position = 'dodge',stat='identity')
ggplot(final_monthly,aes( x = SnapshotMonth, y= avg_basket_sum, fill =segment_totalvalue)) + geom_bar(position = 'dodge',stat='identity')
