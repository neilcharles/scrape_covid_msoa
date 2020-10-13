library(tidyverse)
library(caret)
library(xgboost)
library(ggmap)

metadata <- read_csv('demogs/merged_desc.csv', col_names = FALSE) %>% 
  rename(ColumnVariableCode	= X1,
         ColumnVariableMeasurementUnit = X2,
         ColumnVariableStatisticalUnit = X3,
         ColumnVariableDescription = X4
)

vars_required <- metadata %>% 
  filter(ColumnVariableMeasurementUnit=="Percentage" |
         ColumnVariableMeasurementUnit=="Ratio" |
         ColumnVariableDescription=="All categories: Sex") %>% 
  pull(ColumnVariableCode)

demogs <- read_csv('demogs/merged.csv', col_names = FALSE) %>% 
  rename(GeographyCode	= X1,
         ColumnVariableCode = X2,
         value = X3
         ) %>% 
  filter(ColumnVariableCode %in% vars_required) %>% 
  left_join(select(metadata, ColumnVariableCode, ColumnVariableDescription)) %>% 
  pivot_wider(GeographyCode, names_from = ColumnVariableDescription, values_from = value)

covid_cases_demogs <- covid_cases %>% 
  select(msoa11cd, cases_count) %>% 
  rename(GeographyCode = msoa11cd) %>% 
  left_join(demogs) %>% 
  mutate(cases_pct = cases_count / `All categories: Sex`) %>% 
  select(-cases_count, -`All categories: Sex`)


correlations <- cor(select(covid_cases_demogs,-GeographyCode, -cases_pct),
    select(covid_cases_demogs, cases_pct))

tibble(label = row.names(correlations), correlation = correlations[,"cases_pct"]) %>% 
  arrange(correlation) %>% 
  top_n(10, correlation) %>% 
  mutate(label = fct_inorder(label)) %>% 
  ggplot(aes(x = label, y = correlation)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    text = element_text(size = 12)
  ) +
  ggtitle('Top Positive Correlations with Covid-19 Rate')
  
tibble(label = row.names(correlations), correlation = correlations[,"cases_pct"]) %>% 
  arrange(-correlation) %>% 
  top_n(10, -correlation) %>% 
  mutate(label = fct_inorder(label)) %>% 
  ggplot(aes(x = label, y = correlation)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    text = element_text(size = 12)
  ) +
  ggtitle('Top Negative Correlations with Covid-19 Rate')




set.seed(100)

inTrain <- as.vector(createDataPartition(y = covid_cases_demogs$cases_pct, p = 0.8, list = FALSE))

training <- covid_cases_demogs[inTrain,]
testing <- covid_cases_demogs[-inTrain,]

X_train = xgb.DMatrix(as.matrix(training %>% select(-GeographyCode, -cases_pct)))

y_train = training$cases_pct

X_test = xgb.DMatrix(as.matrix(testing %>% select(-GeographyCode, -cases_pct)))

y_test = testing$cases_pct

xgb_trcontrol = trainControl(
  method = "cv",
  number = 5,  
  allowParallel = TRUE,
  verboseIter = FALSE,
  returnData = FALSE
)

xgbGrid <- expand.grid(nrounds = c(100,200),
                       max_depth = c(10, 15, 20, 25),
                       colsample_bytree = seq(0.5, 0.9, length.out = 5),
                       ## The values below are default values in the sklearn-api. 
                       eta = 0.1,
                       gamma=0,
                       min_child_weight = 1,
                       subsample = 1
)

set.seed(0) 

xgb_model = train(
  X_train, y_train,  
  trControl = xgb_trcontrol,
  tuneGrid = xgbGrid,
  method = "xgbTree"
)


predicted = predict(xgb_model, X_test)

residuals = y_test - predicted

RMSE = sqrt(mean(residuals^2))

cat('The root mean square error of the test data is ', round(RMSE,3),'\n')

y_test_mean = mean(y_test)
# Calculate total sum of squares
tss =  sum((y_test - y_test_mean)^2 )
# Calculate residual sum of squares
rss =  sum(residuals^2)
# Calculate R-squared
rsq  =  1 - (rss/tss)
cat('The R-square of the test data is ', round(rsq,3), '\n')

options(repr.plot.width=8, repr.plot.height=4)
my_data = as.data.frame(cbind(predicted = predicted,
                              observed = y_test))
# Plot predictions vs test data
ggplot(my_data,aes(predicted, observed)) + geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm)+ ggtitle('Linear Regression ') + ggtitle("Extreme Gradient Boosting: Prediction vs Test Data") +
  xlab("Predicted") + ylab("Observed") + 
  theme(plot.title = element_text(color="darkgreen",size=16,hjust = 0.5),
        axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))

#Chart
msoa <-
  sf::st_read("geo/Middle_Layer_Super_Output_Areas__December_2011__Boundaries.shp") %>%
  sf::st_transform(4326)

msoa_coords <- sf::st_centroid(msoa) %>%
  sf::st_coordinates()

msoa_coords <- tibble(msoa11cd = msoa$msoa11cd,
                      lat = msoa_coords[, 2],
                      lon = msoa_coords[, 1])


predicted = predict(xgb_model, xgb.DMatrix(as.matrix(covid_cases_demogs %>% select(-GeographyCode, -cases_pct))))

covid_cases_demogs_model <- covid_cases_demogs

covid_cases_demogs_model$predicted <- predicted

register_google(Sys.getenv('GOOGLE_MAPS'))

map <-
  get_map(
    location = 'Sheffield, uk',
    zoom = 9,
    maptype = 'toner',
    color = "bw"
  )

covid_cases_model_map <- covid_cases_demogs_model %>%
  left_join(msoa_coords) %>%
  mutate(sim_cases_count = round(1000*predicted,0)) %>% 
  uncount(sim_cases_count)

covid_cases_actual_map <- covid_cases %>%
  left_join(msoa_coords) %>%
  uncount(cases_count)



#Local map - model
ggmap(map, extent = "panel", maprange = TRUE, darken = c(0.5, 'white')) +
  geom_density2d(
    data = covid_cases_model_map,
    aes(
      x = lon,
      y = lat
    ),
    bins = 20,
    colour = "dark orange",
    alpha = 0.5
  ) +
  stat_density2d(
    data = covid_cases_model_map,
    aes(
      x = lon,
      y = lat,
      fill = ..level..,
      alpha = ..level..
    ),
    bins = 30,
    geom = 'polygon'
  ) +
  scale_fill_continuous(low = "light green", high = "red", oob = scales::squish) +
  scale_alpha(range = c(0.1, 0.2), guide = FALSE) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    text = element_text(size = 12)
  ) +
  ggtitle('Covid Cases in England: Modelled')

#Local map - actual
ggmap(map, extent = "panel", maprange = TRUE, darken = c(0.5, 'white')) +
  geom_density2d(
    data = covid_cases_actual_map,
    aes(
      x = lon,
      y = lat
    ),
    bins = 20,
    colour = "dark orange",
    alpha = 0.5
  ) +
  stat_density2d(
    data = covid_cases_actual_map,
    aes(
      x = lon,
      y = lat,
      fill = ..level..,
      alpha = ..level..
    ),
    bins = 30,
    geom = 'polygon'
  ) +
  scale_fill_continuous(low = "light green", high = "red", oob = scales::squish) +
  scale_alpha(range = c(0.1, 0.2), guide = FALSE) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    text = element_text(size = 12)
  ) +
  ggtitle('Covid Cases in England: Actual')
