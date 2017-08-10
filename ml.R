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
  filter(count_unique > 100))$feature
## remove columns with more than 100 levels
train_data <- train_data[, setdiff(names(train_data), factor_cols_rm)]
## handle missingness
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

train_data[is.na(train_data)] <- 0
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
## trainControl
fitCtrl <- trainControl(method = "cv",
                        number = 3,
                        summaryFunction = maeSummary)
## parameters
rfGrid <-  expand.grid(mtry = c(1, 5, 9))
## model fit
rfFit <- train(logerror ~ .,
               data = subTrain, 
               method = "rf", 
               preProcess = c("center", "scale"),
               metric = "MAE",
               trControl = fitCtrl,
               verbose = TRUE)
plot(gbmFit)

gbmImp <- varImp(gbmFit, scale = FALSE)
plot(gbmImp, top = 20)
