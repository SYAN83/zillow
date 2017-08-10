library(caret)
# library(doMC)
# registerDoMC(cores = 2)

# Data preparation
## features with 75% or less missing values
feature <- (properties %>% 
              filter(parcelid %in% train$parcelid) %>%
              summarise_all(funs(sum(is.na(.))/n())) %>%
              gather(key="feature", value="missing_pct") %>%
              filter(missing_pct < .75) %>%
              select(feature))$feature

## train_data: train inner_join properties 
train_data <- train %>% 
  mutate(year=year(transactiondate),
         month=month(transactiondate)) %>%
  select(-transactiondate) %>%
  inner_join(properties %>% select(feature), by="parcelid") 

## factor columns from zillow_data_dictionary
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
train_data <- train_data %>% 
  mutate_at(.funs = as.factor, 
            .vars = intersect(names(train_data), factor_cols))

## remove near zero variance
nzv <- nearZeroVar(train_data, saveMetrics= TRUE, freqCut = 99/1)
train_data <- train_data[, !nzv$nzv]

## factor columns with levels more than 50
factor_cols_rm <- (data.frame(lapply(train_data[, intersect(names(train_data), factor_cols)], 
                  FUN = function(x) length(unique(x)))) %>%
  gather(key="feature", value="count_unique") %>% 
  filter(count_unique > 50))$feature

## remove factor columns with too many levels
train_data <- train_data[, setdiff(names(train_data), factor_cols_rm)]

# Handling missingness
## missing percentage
train_data %>% 
  summarise_all(funs(sum(is.na(.))/n())) %>%
  gather(key="feature", value="missing_pct") %>%
  ggplot(aes(x=reorder(feature, missing_pct), y=missing_pct)) +
  geom_bar(stat="identity",
           color="black", fill="blue", alpha=.5) +
  coord_flip()

mode_ <- function(x) {
  names(which.max(table(train_data$buildingqualitytypeid)))
}

## impute factor columns using mode except for:
##  airconditioningtypeid: 5 None
##  heatingorsystemtypeid: 13 None
train_data <- train_data %>% 
  mutate(airconditioningtypeid = ifelse(is.na(airconditioningtypeid), 5, airconditioningtypeid),
         heatingorsystemtypeid = ifelse(is.na(heatingorsystemtypeid), 13, heatingorsystemtypeid),
         buildingqualitytypeid = ifelse(is.na(buildingqualitytypeid), 
                                        mode_(buildingqualitytypeid), 
                                        buildingqualitytypeid),
         unitcnt = ifelse(is.na(unitcnt), mode_(unitcnt), unitcnt),
         fullbathcnt = ifelse(is.na(fullbathcnt), mode_(fullbathcnt), fullbathcnt),
         calculatedbathnbr = ifelse(is.na(calculatedbathnbr), mode_(calculatedbathnbr), calculatedbathnbr),
         yearbuilt = ifelse(is.na(yearbuilt), mode_(yearbuilt), yearbuilt))

## impute numerical columns using 0
train_data[is.na(train_data)] <- 0

# Machine Learning
## Data splitting based on the outcome
set.seed(123)
trainIndex <- createDataPartition(train_data$logerror, 
                                  p = .75, 
                                  list = FALSE, 
                                  times = 1)
## training set
subTrain <- train_data[ trainIndex,-1]
## testing set
subTest  <- train_data[-trainIndex,-1]

## define metric - MAE
maeSummary <- function(data, lev = NULL, model = NULL) {
  mae_score <- sum(abs(data$obs - data$pred)) / nrow(data)
  names(mae_score) <- "MAE"
  mae_score
}

## cross validation
## 1. random hyperparameter
rdmSearch <- trainControl(method = "cv",
                          number = 3,
                          summaryFunction = maeSummary,
                          search = "random")

gbmFit <- train(logerror ~ .,
                data = subTrain, 
                method = "gbm", 
                preProcess = c("center", "scale"),
                metric = "MAE",
                maximize = FALSE,
                tuneLength = 3,
                trControl = rdmSearch,
                verbose = TRUE)
plot(gbmFit)
gbmFit$bestTune
## 2. grid search
gridSearch <- trainControl(method = "cv",
                           number = 3,
                           summaryFunction = maeSummary)

gbmGrid <-  expand.grid(interaction.depth = 3, 
                        n.trees = c(100), 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

gbmFit2 <- train(logerror ~ .,
                 data = subTrain, 
                 method = "gbm", 
                 preProcess = c("center", "scale"),
                 metric = "MAE",
                 maximize = FALSE,
                 tuneGrid = gbmGrid,
                 trControl = gridSearch,
                 verbose = TRUE)
gbmFit2
## parameters
# plot(gbmFit2)
gbmImp <- varImp(gbmFit2, scale = FALSE)
plot(gbmImp, top = 20)

# Prediction
predict(gbmFit2, newdata = subTrain)
