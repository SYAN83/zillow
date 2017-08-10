library(caret)
library(doMC)
registerDoMC(cores = 2)

# Maching Learning
## features with 75% or less missing values
feature <- (prop_train %>% 
  summarise_all(funs(sum(is.na(.))/n())) %>%
  gather(key="feature", value="missing_pct") %>%
  filter(missing_pct < .75) %>%
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

## Define metric - MAE
maeSummary <- function(data, lev = NULL, model = NULL) {
  mae_score <- sum(abs(data$obs - data$pred)) / nrow(data)
  names(mae_score) <- "MAE"
  mae_score
}

fitCtrl <- trainControl(method = "cv",
                        number = 3,
                        summaryFunction = maeSummary)
gbmFit1 <- train(logerror ~ .,
                 data = subTrain, 
                 method = "gbm", 
                 metric = "MAE",
                 trControl = fitCtrl,
                 verbose = TRUE)
plot(gbmFit1)

gbmImp <- varImp(gbmFit1, scale = FALSE)
plot(gbmImp, top = 20)
