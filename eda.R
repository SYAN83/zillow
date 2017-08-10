# Import libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(leaflet)
library(lubridate)
library(VIM)

# Import data
properties <- fread(input = "https://s3.us-east-2.amazonaws.com/aws-emr-dedicated/data/zillow/properties_2016.csv", 
              na.strings = "")
train <- fread(input = "https://s3.us-east-2.amazonaws.com/aws-emr-dedicated/data/zillow/train_2016_v2.csv",
               na.strings = "")

prop_train <- properties %>% filter(parcelid %in% train$parcelid)

# EDA
## Transaction volumn by date
train %>% 
  mutate(year_month = make_date(year=year(transactiondate),
                                month=month(transactiondate))) %>% 
  group_by(year_month) %>% count() %>% 
  ggplot(aes(x=year_month, y=n)) +
  geom_bar(stat="identity", fill="blue", alpha=.5) +
  geom_vline(aes(xintercept=as.numeric(as.Date("2016-10-15"))), size=1)

## Distribution of logerror (99% percentile)
train %>% 
  filter(logerror %between% c(quantile(train$logerror, .005), 
                              quantile(train$logerror, .995))) %>%
  ggplot(aes(x=logerror)) +
  geom_histogram(aes(y=..density..), bins=50,
                 color="black", fill="blue", alpha=.5) + 
  geom_density(alpha = .2, fill = "blue")

## Distribution of absolute logerror (99% percentile)
train %>% 
  filter(logerror %between% c(quantile(train$logerror, .005), 
                              quantile(train$logerror, .995))) %>%
  mutate(abslogerr = abs(logerror)) %>%
  ggplot(aes(x=abslogerr)) +
  geom_histogram(aes(y=..density..), bins=50,
                 color="black", fill="blue", alpha=.5) + 
  geom_density(alpha=.2, fill="blue")

## Mean of absolute logerror over time
train %>% 
  mutate(year_month = make_date(year=year(transactiondate),
                                month=month(transactiondate))) %>% 
  group_by(year_month) %>%
  summarise(meanabslogerr = mean(abs(logerror))) %>%
  ggplot(aes(x=year_month, y=meanabslogerr)) +
  geom_line(color="blue")+
  geom_point(size=2, color="blue")
## Distribution of mean absolute logerror by month (99% percentile)
train %>% 
  filter(logerror %between% c(quantile(train$logerror, .005),
                              quantile(train$logerror, .995))) %>%
  mutate(year_month = as.factor(make_date(year=year(transactiondate),
                                          month=month(transactiondate)))) %>% 
  mutate(abslogerr = abs(logerror)) %>%
  ggplot(aes(x=abslogerr)) +
  geom_histogram(aes(y=..density..), alpha=.5, fill="blue", bins=50) + 
  facet_wrap(~ year_month)
## Missing percentage
prop_train %>% 
  summarise_all(funs(sum(is.na(.))/n())) %>%
  gather(key="feature", value="missing_pct") %>%
  ggplot(aes(x=reorder(feature, missing_pct), y=missing_pct)) +
  geom_bar(stat="identity",fill="blue", alpha=.5)+
  coord_flip()

