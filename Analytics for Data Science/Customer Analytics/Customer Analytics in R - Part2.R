library(tidyverse)
library(lubridate)
rm(list = ls())
trans <- readRDS('trans_lesson1.rds')
customer_summary <- readRDS('customer_sum_lesson1.rds')

# Calculate recency days
current_date <- max(trans$InvoiceDate)
customer_summary$recency_days <- as.integer(difftime(current_date,customer_summary$last_purchase, units = c("days")))

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#Plotting Graphs

g1 <- ggplot(customer_summary, aes(x = days_active_share)) + geom_density()
g2 <- ggplot(customer_summary, aes(x = days_active)) + geom_density()
g3 <- ggplot(customer_summary, aes(x = factor(months_active))) + geom_bar()
g4 <- ggplot(customer_summary, aes(x = avg_basket_sum)) + geom_density()
multiplot(g1, g2, g3, g4, cols = 2)

#Insight 1 : From the charts you can observe that the variables days_active_share,
#days_active and months_active are skewed to the right while the variable avg_basket_sum
#is concentrated around zero and is not skewed neither to the left nor to the right.

#make another 2x2 plot for the 4 variables - 
# avg_quantity, avg_price, total_quantity, total_value
install.packages("gridExtra")
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

g5 <- ggplot(customer_summary, aes(x = avg_quantity)) + geom_density()
g6 <- ggplot(customer_summary, aes(x = avg_price)) + geom_density()
g7 <- ggplot(customer_summary, aes(x = total_quantity)) + geom_density()
g8 <- ggplot(customer_summary, aes(x = total_value)) + geom_density()
multiplot(g5, g6, g7, g8, cols = 2)

grid.arrange(g5,g6,g7,g8,ncol=2) # Plotting with GridExtra

# Explore the customer_summary table
ggplot(customer_summary, aes(x = tenure_days)) + geom_density()

#percentile cutoffs -  10%, 15%, 25% ad 50%.
round(quantile(customer_summary$tenure_days, c(.1, .15, .25, .5)), 1)
#Insight 2: 15% of customers have a tenure of less than 64 days

#new customer is defined as a customer whose tenure in days is lower than 60
customer_summary$customer_status <- ifelse(customer_summary$tenure_days < 60, 'New', 'Existing')

options(dplyr.width = Inf)
customer_summary

#Explore how do New customers differ from the Existing ones - Mean
customer_summary %>% 
  group_by(customer_status) %>% 
  summarise(avg_price = round(mean(avg_price), 1),
            avg_quantity = round(mean(avg_quantity), 1),
            avg_basket_sum = round(mean(avg_basket_sum), 1),
            avg_total_quantity = round(mean(total_quantity), 1),
            avg_total_value = round(mean(total_value), 1),
            avg_n_products = round(mean(n_products), 1),
            avg_recency_days = round(mean(recency_days), 1),
            avg_days_active = round(mean(days_active), 1)
  )

#Explore how do New customers differ from the Existing ones - Max
customer_summary %>% 
  group_by(customer_status) %>% 
  summarise(max_price = max(avg_price),
            max_quantity = max(avg_quantity),
            max_basket_sum = max(avg_basket_sum),
            max_total_quantity = max(total_quantity),
            max_total_value = max(total_value),
            max_n_products = max(n_products),
            max_recency_days = max(recency_days),
            max_days_active = max(days_active)
)

#Explore how do New customers differ from the Existing ones - Median
customer_summary %>% 
  group_by(customer_status) %>% 
  summarise(med_price = median(avg_price),
            med_quantity = median(avg_quantity),
            med_basket_sum = median(avg_basket_sum),
            med_total_quantity = median(total_quantity),
            med_total_value = median(total_value),
            med_n_products = median(n_products),
            med_recency_days = median(recency_days),
            med_days_active = median(days_active)
  )
customer_summary

# create the segment by the average price
# Plot Segment by the average price for existing and New customer

ggplot(customer_summary, aes(x = avg_price, col = customer_status)) + geom_density(size = 1) + coord_cartesian(xlim = c(0,50))

#number of customers in several ranges
sum(customer_summary$avg_price >= 20) / nrow(customer_summary) * 100 #High Price segment
sum(customer_summary$avg_price >= 3 & customer_summary$avg_price < 20) / nrow(customer_summary) * 100 #Medium Price segment
sum(customer_summary$avg_price < 3) / nrow(customer_summary) * 100 #Low Price segment

#create new variable segment_avg_price
customer_summary <- customer_summary %>% 
  mutate(
    segment_avg_price = ifelse(avg_price >= 20, '1. High Price', 
                               ifelse(avg_price >= 3, '2. Medium Price', '3. Low Price'))
  ) 
customer_summary

#segmentation with the total_quantity variable.

ggplot(customer_summary, aes(x = total_quantity, col = customer_status)) + geom_density(size = 1) + coord_cartesian(xlim = c(-200,5000))
sum(customer_summary$total_quantity >= 2000) / nrow(customer_summary) * 100 #Top Quantity Segment
sum(customer_summary$total_quantity < 2000 & customer_summary$total_quantity >= 500) / nrow(customer_summary) * 100 #Frequency segment
sum(customer_summary$total_quantity < 500 & customer_summary$total_quantity >= 100) / nrow(customer_summary) * 100 #Average segment
sum(customer_summary$total_quantity < 100 & customer_summary$total_quantity >= 0) / nrow(customer_summary) * 100 #Low quantity segment
sum(customer_summary$total_quantity < 0) / nrow(customer_summary) * 100 #Negative quantity segment

customer_summary <- customer_summary %>% 
  mutate(
    segment_total_quantity = ifelse(total_quantity >= 2000, '1. Top Quantity Segment',
                                    ifelse(total_quantity >= 500, '2. Frequency Segment',
                                           ifelse(total_quantity >= 100, '3. Average segment',
                                                  ifelse(total_quantity >= 0, '4. Low quantity segment', '5. Negative quantity segment'
                                                  ))))
  )
customer_summary

#segmentation with the avg_basket_sum variable.

ggplot(customer_summary, aes(x = avg_basket_sum, col = customer_status)) + geom_density(size = 1) + coord_cartesian(xlim = c(-200,500))
sum(customer_summary$avg_basket_sum >= 150) / nrow(customer_summary) * 100 #Merry Shopper
sum(customer_summary$avg_basket_sum < 150 & customer_summary$avg_basket_sum >= 50) / nrow(customer_summary) * 100 #Big Shopper
sum(customer_summary$avg_basket_sum < 50 & customer_summary$avg_basket_sum >= 10) / nrow(customer_summary) * 100 #Average Shopper
sum(customer_summary$avg_basket_sum < 10 & customer_summary$avg_basket_sum >= 0) / nrow(customer_summary) * 100 # Thrifty Shopper
sum(customer_summary$avg_basket_sum < 0) / nrow(customer_summary) * 100 #Fussy Shopper

customer_summary <- customer_summary %>% 
  mutate(
    segment_avg_basket_sum = ifelse(avg_basket_sum >= 150, '1. Merry Shopper',
                                    ifelse(avg_basket_sum >= 50, '2. Big Shopper',
                                           ifelse(avg_basket_sum >= 10, '3. Average Shopper',
                                                  ifelse(avg_basket_sum >= 0, '4. Thrifty Shopper', '5. Fussy Shopper'
                                                  ))))
  )
customer_summary


#segmentation with the total_value variable.

ggplot(customer_summary, aes(x = total_value, col = customer_status)) + geom_density(size = 1) + coord_cartesian(xlim = c(-1000,7000))
sum(customer_summary$total_value >= 2500) / nrow(customer_summary) * 100 #Diamond Customer
sum(customer_summary$total_value < 2500 & customer_summary$total_value >= 1000) / nrow(customer_summary) * 100 #Gold Shopper
sum(customer_summary$total_value < 1000 & customer_summary$total_value >= 500) / nrow(customer_summary) * 100 #Silver Shopper
sum(customer_summary$total_value < 500 & customer_summary$total_value >= 0) / nrow(customer_summary) * 100 # Bronze Shopper
sum(customer_summary$total_value < 0) / nrow(customer_summary) * 100 #Casual Shopper

customer_summary <- customer_summary %>% 
  mutate(
    segment_total_value = ifelse(total_value >= 150, '1. Diamond Customer',
                                    ifelse(total_value >= 50, '2. Gold Shopper',
                                           ifelse(total_value >= 10, '3. Silver Shopper',
                                                  ifelse(total_value >= 0, '4. Bronze Shopper', '5. Casual Shopper'
                                                  ))))
  )
customer_summary

#Absolute number of customers in each of the created segments
table(customer_summary$segment_avg_price)
table(customer_summary$segment_total_quantity)
table(customer_summary$segment_avg_basket_sum)
table(customer_summary$segment_total_value)

#agg_fx() function which has 2 arguments: segment and type. 
#The former one - segment - takes in the name of the categorical 
#variable (in our case we’re interested in the segments we’ve created), 
#uses it as a grouping variable and summarizes the data. The latter argument
#type has three values: mean, median and sum.

agg_fx <- function(segment, type = 'mean') {
  if(type == 'mean') {
    customer_summary %>% 
      group_by_(segment) %>% 
      summarise(n_customers = n(),
                avg_price = round(mean(avg_price), 1),
                avg_quantity = round(mean(avg_quantity), 1),
                avg_basket_sum = round(mean(avg_basket_sum), 1),
                avg_total_quantity = round(mean(total_quantity), 1),
                avg_total_value = round(mean(total_value), 1),
                avg_n_products = round(mean(n_products), 1),
                avg_recency_days = round(mean(recency_days), 1),
                avg_monthts_active = round(mean(months_active), 1),
                avg_days_active = round(mean(days_active), 1),
                avg_tenure_days = round(mean(tenure_days), 1),
                avg_days_active_share = round(mean(days_active_share), 3) * 100
      )
    
  } else if(type == 'median') {
    customer_summary %>% 
      group_by_(segment) %>% 
      summarise(n_customers = n(),
                med_price = round(median(avg_price), 1),
                med_quantity = round(median(avg_quantity), 1),
                med_basket_sum = round(median(avg_basket_sum), 1),
                med_total_quantity = round(median(total_quantity), 1),
                med_total_value = round(median(total_value), 1),
                med_n_products = round(median(n_products), 1),
                med_recency_days = round(median(recency_days), 1),
                med_monthts_active = round(median(months_active), 1),
                med_days_active = round(median(days_active), 1),
                med_tenure_days = round(median(tenure_days), 1),
                med_days_active_share = round(median(days_active_share), 3) * 100
      )
  } else if(type == 'sum') {
    customer_summary %>% 
      group_by_(segment) %>% 
      summarise(n_customers = n(),
                sum_total_quantity = sum(total_quantity),
                sum_total_value = sum(total_value)
      )
  } else {
    print('Error - incorrect "type" - please use one of three: "mean"", "median"" or "sum"')
  }
}
agg_fx('segment_total_quantity', 'median')
agg_fx('segment_total_quantity', 'mean') %>% ggplot(aes(x = segment_total_quantity, y = avg_total_quantity)) + geom_bar(stat = 'identity')

agg_fx('segment_total_value', 'median')
agg_fx('segment_total_value', 'mean') %>% ggplot(aes(x = segment_total_value, y = avg_total_value)) + geom_bar(stat = 'identity')

saveRDS(trans, 'trans_lesson2.rds')
saveRDS(customer_summary, 'customer_sum_lesson2.rds')
saveRDS(agg_fx, 'agg_fx.rds')
saveRDS(multiplot, 'multiplot.rds')