unique(df_clean$Item)
item_count <- df_clean %>% 
  select(Item) %>% 
  group_by(Item) %>% 
  tally() %>% 
  filter(n >= 10) %>% 
  arrange(desc(n)) 

View(item_count)
