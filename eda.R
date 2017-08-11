############################################################
# Zillow Prize: Zillow’s Home Value Prediction (Zestimate) #
############################################################

# Import libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(lubridate)
library(leaflet)
library(leaflet.extras)

# Importing Data
properties <- fread(input = "https://s3.us-east-2.amazonaws.com/aws-emr-dedicated/data/zillow/properties_2016.csv", 
                    na.strings = "",
                    colClasses = list(character=c("parcelid", "airconditioningtypeid", "architecturalstyletypeid",
                                                  "buildingclasstypeid", "decktypeid", "fips", "hashottuborspa",
                                                  "heatingorsystemtypeid", "pooltypeid10", "pooltypeid2", "pooltypeid7",
                                                  "propertycountylandusecode", "propertylandusetypeid", 
                                                  "propertyzoningdesc", "rawcensustractandblock", "regionidcity", 
                                                  "regionidcounty", "regionidneighborhood", "regionidzip", "storytypeid", 
                                                  "typeconstructiontypeid", "fireplaceflag", "assessmentyear",
                                                  "taxdelinquencyflag", "censustractandblock")))

train <- fread(input = "https://s3.us-east-2.amazonaws.com/aws-emr-dedicated/data/zillow/train_2016_v2.csv",
               na.strings = "",
               colClasses = list(character=c("parcelid")))

## convert lat/lon
properties <- properties %>%
  mutate(latitude = latitude/1e6, longitude = longitude/1e6)

# Preliminary Data Analysis
## Transaction volumn by date
train %>% 
  mutate(year_month = make_date(year=year(transactiondate),
                                month=month(transactiondate))) %>% 
  group_by(year_month) %>% count() %>% 
  ggplot(aes(x=year_month, y=n)) +
  geom_bar(stat="identity", 
           color="black", fill="blue", alpha=.5) +
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
  summarise(meanerr = mean(abs(logerror)), 
            stderr = sqrt(var(abs(logerror))/n())) %>%
  ggplot(aes(x=year_month, y=meanerr)) +
  geom_line(color="blue", linetype="dashed") +
  geom_errorbar(aes(ymin=meanerr-1.96*stderr, ymax=meanerr+1.96*stderr), 
                color="blue", width=10) +
  geom_point(size=2, color="blue")

## Distribution of mean absolute logerror by month (99% percentile)
train %>% 
  filter(logerror %between% c(quantile(train$logerror, .005),
                              quantile(train$logerror, .995))) %>%
  mutate(year_month = as.factor(make_date(year=year(transactiondate),
                                          month=month(transactiondate)))) %>% 
  ggplot(aes(x=logerror)) +
  geom_histogram(aes(y=..density..), bins=30,
                 color="black", fill="blue", alpha=.5) + 
  facet_wrap(~ year_month)

## logerror geographic distribution
leaflet(data = properties %>% 
          inner_join(train, by="parcelid") %>%
          group_by(longitude, latitude) %>%
          summarise(logerror = mean(abs(logerror)))) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addHeatmap(lng = ~longitude, lat = ~latitude, 
             intensity = .02,
             blur = 3, radius = 3, 
             group = "Property heatmap") %>%
  addHeatmap(lng = ~longitude, lat = ~latitude, 
             intensity = ~logerror*.2,
             blur = 3, radius = 3, 
             group = "logerror heatmap") %>%
  addLayersControl(
    baseGroups = c("Property heatmap", "logerror heatmap"),
    options = layersControlOptions(collapsed = FALSE)
  )

# outliers (<0.5% vs. >99.5%)
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addHeatmap(data = train %>% 
               filter(logerror < quantile(train$logerror, .005)) %>%
               mutate(logerror = -logerror) %>%
               inner_join(properties, by="parcelid") %>% 
               group_by(longitude, latitude) %>%
               summarise(logerror = mean(logerror)),
             lng = ~longitude, lat = ~latitude, 
             intensity = ~logerror,
             blur = 3, radius = 3,
             group = "Underestimated (< 0.5%)") %>%
  addHeatmap(data = train %>% 
               filter(logerror > quantile(train$logerror, .995)) %>%
               inner_join(properties, by="parcelid") %>% 
               group_by(longitude, latitude) %>%
               summarise(logerror = mean(logerror)),
             lng = ~longitude, lat = ~latitude, 
             intensity = ~logerror,
             blur = 3, radius = 3,
             group = "Overestimated (> 99.5%)") %>%
  addLayersControl(
    baseGroups = c("Underestimated (< 0.5%)", "Overestimated (> 99.5%)"),
    options = layersControlOptions(collapsed = FALSE)
  )

## Missing percentage
properties %>% 
  filter(parcelid %in% train$parcelid) %>%
  summarise_all(funs(sum(is.na(.))/n())) %>%
  gather(key="feature", value="missing_pct") %>%
  ggplot(aes(x=reorder(feature, missing_pct), y=missing_pct)) +
  geom_bar(stat="identity",
           color="black", fill="blue", alpha=.5) +
  coord_flip()

