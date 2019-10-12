#set working directory
setwd("/Users/pasto/Documents/Purdue/Fall/Using R for Analytics/Shiny App")

#load necessary packages
library(plyr)
library(data.table)
library(dplyr)
library(tidyr)
library(arules)
library(arulesViz)

#import data
order_products_train <- as.data.frame(read.csv("order_products__train.csv"))
order_products_prior <- as.data.frame(read.csv("order_products__prior.csv"))
products <- as.data.frame(read.csv("products.csv"))
orders <- as.data.frame(read.csv("orders.csv"))
order_products_train <- rbind(order_products_train, order_products_prior)
rm(order_products_prior)

#count instances of each product
count <- count(order_products_train, vars = order_products_train$product_id)
names(count)[1] <- "product_id"
names(count)[2] <- "freq"

#limit products to those that were ordered 50 or more times
products2 <- merge(products, count, by = "product_id")

products <- subset(products2, freq >= 50)

#remove commas from product names
products$product_name <- gsub(',','', products$product_name)

rm(products2)

#get all needed info into one dataframe and sort by order id
order_products_train <- merge(order_products_train, count, by = "product_id")
order_products_train <- merge(order_products_train, products, by = "product_id")

order_products_train <- subset(order_products_train, freq.y >= 50)

tr <- order_products_train[,c(2,6)]
tr <- tr[order(tr$order_id),]

#prepare tr and products datasets with only the columns needed
tr <- merge(orders, tr, by = "order_id")

tr <- tr[,c(1,8)]
products <- products[,1:2]

#merge every product in each order into a single row
tr3 <- tr %>% group_by(order_id) %>% summarise(product_names = paste(product_name, collapse=","))

tr <- tr3
rm(tr3)

tr <- tr[,2]

#create csv of transactions
write.csv(tr, "transactions.csv", quote = FALSE, row.names = FALSE)

#read csv into R in transactions format for arules
transactions <- read.transactions('transactions.csv', format = 'basket', sep = ',')

summary(transactions)

#generate rules, convert into dataframe to use for Shiny App
association.rules <- apriori(transactions, parameter = list(supp=0.000003, conf=0.1, maxlen=2, minlen=2, maxtime = 30))
output <- DATAFRAME(association.rules)

#sort output by confidence and support
output <- arrange(output, desc(confidence), desc(support))

#export csvs for Shiny App
write.csv(output, "Output.csv", row.names = F)
write.csv(products, "Final_Products.csv", row.names = F)

#plot rules
plot(association.rules,method = "two-key plot")
plot(association.rules,shading = "lift", method="scatterplot")
  