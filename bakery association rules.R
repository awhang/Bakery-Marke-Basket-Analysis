library(arules)
library(arulesViz)
library(dplyr)
library(ggplot2)
library(gridExtra)
setwd("D:/School Files/Data Science")

df <- read.csv("BreadBasket_DMS.csv")
df_clean <- df %>% 
  filter(Item != "NONE")
tbl_df(df_clean)

write.csv(df_clean, "BreadBasket_Clean.csv", row.names = FALSE)

trans <- read.transactions("BreadBasket_Clean.csv", format="single", cols=c(3,4), sep=",", rm.duplicates=TRUE)
str(trans)

# Generate rule
rules_final <- apriori(trans, 
                 parameter=list(sup=0.01,
                 conf=0.5))


association.rules <- sort(rules_final, by = 'support', decreasing = TRUE)
inspect(association.rules)

# Plot frequent items
itemFrequencyPlot(trans, topN = 5, type = "absolute", main = "Most sold items, absolute freq")

# Plot of association
plot(rules_final, method = "graph")

# Seems like all multi-item purchases revolve around Coffee at the bakery. 
