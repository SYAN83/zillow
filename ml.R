library(caret)

feature <- (prop_train %>% 
  summarise_all(funs(sum(is.na(.))/n())) %>%
  gather(key="feature", value="missing_pct") %>%
  filter(missing_pct %between% c(0, .75)) %>%
  select(feature))$feature

train_data <- train %>% 
  mutate(year=year(transactiondate),
         month=month(transactiondate)) %>%
  select(-transactiondate) %>%
  inner_join(prop %>% select(feature), by="parcelid") 

str(train_data)

nearZeroVar(prop_train[, cols$feature], saveMetrics= TRUE)

