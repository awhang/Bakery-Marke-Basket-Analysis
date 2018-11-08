library(lubridate)
library(dplyr)
library(ggplot2)
setwd("D:/School Files/Data Science")

# Importing data
df <- read.csv("BreadBasket_DMS.csv")

# Converte Date and Time to proper formats
df$Date <- as.Date(df$Date)
df$Time <- hms(df$Time)
# Converting dates into weekday names
df$weekday <- wday(df$Date, label = TRUE)
df$month <- month(df$Date, label = TRUE)

tbl_df(df)

# Remove items labeled as "NONE" and select the top 15 most sold items
top_item <- df %>% 
  select(Item) %>% 
  filter(Item != "NONE") %>% 
  group_by(Item) %>% 
  tally() %>% 
  top_n(15) %>% 
  arrange(desc(n))

tbl_df(top_item)

# Which items sell the most
ggplot(top_item, aes(x = reorder(Item, -n), y = n)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Seems like coffee and bread make up the most items sold

top_item_wday <- df %>% 
  select(weekday, Item) %>% 
  filter(Item != "NONE") %>% 
  group_by(weekday) %>% 
  tally()

tbl_df(top_item_wday)

# Which day sells the most items
ggplot(top_item_wday, aes(x = reorder(weekday, -n), y = n)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Saturday sells the most items


# See if there are any differences in which items are sold based on day of the week
# for each day the top 6 items are selected. 
top_item_wday3 <- df %>% 
  select(weekday, Item) %>% 
  filter(Item != "NONE") %>% 
  group_by(weekday, Item) %>%
  tally() %>% 
  top_n(6)

ggplot(top_item_wday3, aes(x = reorder(weekday, -n), y = n)) +
  geom_bar(stat = "identity") +
  facet_grid(~ Item) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Medialuna appears on Sunday 
# as popular item but will be ignored for this analysis as it doesn't for 
# the other days of the week

topitems <- c("Coffee", "Bread", "Tea", "Cake", "Pastry", "Sandwich")

top_item_wday4 <- df %>% 
  select(weekday, Item) %>% 
  filter(Item %in% topitems) %>% 
  group_by(weekday, Item) %>%
  tally() 

tbl_df(top_item_wday4)

# Graph of weekday items sold for 'top 6' items
ggplot(top_item_wday4, aes(x = reorder(weekday, -n), y = n)) +
  geom_bar(stat = "identity") +
  facet_grid(~ Item) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# By month analysis of items sold
# removing April and October since those are the start and end months 
# of data collection which caused data to be skewed
top_item_month <- df %>% 
  select(month, Item) %>% 
  filter(Item != "NONE", month != "Oct", month != "Apr") %>% 
  group_by(month, Item) %>%
  tally() %>% 
  top_n(5)

ggplot(top_item_month, aes(x = month, y = n)) +
  geom_bar(stat = "identity") +
  facet_grid(~ Item) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# It's odd that some months have so little sold in Pastry, Sandwhich, and Medialuna
# Is this observation valid?

# medialuna pastry and sandwich by month
low_item_month <- df %>% 
  select(month, Item) %>% 
  filter(month != "Oct", month != "Apr", Item == "Medialuna" | Item == "Pastry" | Item == "Sandwich") %>% 
  group_by(month, Item) %>%
  tally() 
tbl_df(low_item_month)

ggplot(low_item_month, aes(x = month, y = n)) +
  geom_bar(stat = "identity") +
  facet_grid(~ Item) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Try to graph all items per month again but filter items specifically
# instead of using top_n
topitems2 <- c("Coffee", "Bread", "Tea", "Cake", "Pastry", "Sandwich", "Medialuna")
top_item_month2 <- df %>% 
  select(month, Item) %>% 
  filter(month != "Oct", month != "Apr", Item %in% topitems2) %>% 
  group_by(month, Item) %>%
  tally() 

tbl_df(top_item_month2)

ggplot(top_item_month2, aes(x = month, y = n)) +
  geom_bar(stat = "identity") +
  facet_grid(~ Item) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# The method of using top_n messes up once some months have a different top_n than others
# Here we accurately see the amounts of each item

# Items per transaction plots
items_per_trans <- df %>%
  select(Item, Transaction) %>% 
  group_by(Transaction) %>% 
  tally() %>% 
  summarise(average = mean(n))
items_per_trans
# average of 2.23 items per transaction

# column plot of avg items by month
# itemstrans_month_avg <- df %>%
#   select(Transaction, month) %>% 
#   group_by(Transaction, month) %>% 
#   summarise(count = n()) %>% 
#   group_by(month) %>% 
#   summarise(avg = mean(count))
# 
# ggplot(itemstrans_month_avg, aes(x = month, y = avg)) + geom_col()

# boxplot of items per transaction for each month
itemstrans_month <- df %>%
  select(Transaction, month) %>% 
  group_by(Transaction, month) %>% 
  summarise(count = n())
            
ggplot(itemstrans_month, aes(x = month, y = count)) + geom_boxplot()

# column plot of avg items by weekday
# itemstrans_weekday_avg <- df %>%
#   select(Transaction, weekday) %>% 
#   group_by(Transaction, weekday) %>% 
#   summarise(count = n()) %>% 
#   group_by(weekday) %>% 
#   summarise(avg = mean(count))
# 
# ggplot(itemstrans_weekday_avg, aes(x = weekday, y = avg)) + geom_col()

# boxplot of items per transaction for each day
itemstrans_weekday <- df %>%
  select(Transaction, weekday) %>% 
  group_by(Transaction, weekday) %>% 
  summarise(count = n())

ggplot(itemstrans_weekday, aes(x = weekday, y = count)) + geom_boxplot()       
