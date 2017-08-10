library(caret)
library(doMC)
registerDoMC(cores = 4)

## features with 75% or less missing values
feature <- (prop_train %>% 
  summarise_all(funs(sum(is.na(.))/n())) %>%
  gather(key="feature", value="missing_pct") %>%
  filter(missing_pct == 0) %>%
  # filter(missing_pct %between% c(0, .75)) %>%
  select(feature))$feature
## train_data <- train inner_join properties 
train_data <- train %>% 
  mutate(year=year(transactiondate),
         month=month(transactiondate)) %>%
  select(-transactiondate) %>%
  inner_join(properties %>% select(feature), by="parcelid") 
## factor columns
factor_cols <- c("airconditioningtypeid", 
                 "fips",
                 "heatingorsystemtypeid", 
                 "propertycountylandusecode",
                 "propertylandusetypeid",
                 "propertyzoningdesc", 
                 "rawcensustractandblock",
                 "regionidcity", 
                 "regionidcounty", 
                 "regionidneighborhood",
                 "regionidzip",
                 "censustractandblock")
## convert columns to factors
train_data <- train_data %>% mutate_at(.funs = as.factor, 
                                       .vars = intersect(names(train_data), factor_cols))
## near zero variance
nzv <- nearZeroVar(train_data, saveMetrics= TRUE, freqCut = 99/1)
train_data <- train_data[, !nzv$nzv]
## factor columns
factor_cols_rm <- (data.frame(lapply(train_data[, intersect(names(train_data), factor_cols)], 
                  FUN = function(x) length(unique(x)))) %>%
  gather(key="feature", value="count_unique") %>% 
  filter(count_unique > 50))$feature
## remove columns with more than 
train_data <- train_data[, setdiff(names(train_data), factor_cols_rm)]
## Data splitting based on the outcome
set.seed(123)
trainIndex <- createDataPartition(train_data$logerror, 
                                  p = .75, 
                                  list = FALSE, 
                                  times = 1)
subTrain <- train_data[ trainIndex,-1]
subTest  <- train_data[-trainIndex,-1]

fitCtrl <- trainControl(method = "cv",
                        number = 3)
gbmFit1 <- train(logerror ~ .,
                 data = subTrain, 
                 method = "gbm", 
                 trControl = fitCtrl,
                 verbose = FALSE)
gbmFit1