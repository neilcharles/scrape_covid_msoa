library(tidyverse)
library(caret)
library(xgboost)
library(ggmap)

week_numbers <- read_csv("C:/Users/neild/Documents/dev/covid19_mapping/lsoa_csv/week_numbers.csv")

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

cases_for_model <- read_csv('https://coronavirus.data.gov.uk/downloads/lsoa_data/LSOAs_latest.csv', na = '-99', col_types = cols(.default = "c")) %>% 
  pivot_longer(cols = contains('wk'), names_to = 'week_number', values_to = 'cases_count') %>% 
  mutate(cases_count = as.integer(cases_count)) %>% 
  left_join(week_numbers) %>% 
  rename(LSOA11CD = lsoa11_cd) %>% 
  replace_na(list(cases_count = 0))


covid_cases_demogs <- cases_for_model %>% 
  select(LSOA11CD, week_ending, cases_count) %>% 
  rename(GeographyCode = LSOA11CD) %>% 
  left_join(demogs) %>% 
  mutate(cases_pct = cases_count / `All categories: Sex`) %>% 
  select(-cases_count, -`All categories: Sex`) %>% 
  drop_na() %>% 
  nest(data = -'week_ending') %>% 
  mutate(correlations = map(data, function(df) cor(select(df, -GeographyCode, -cases_pct), select(df, cases_pct))))

covid_cases_demogs$correlations[10]


tibble(label = row.names(correlations), correlation = correlations[,"cases_pct"]) %>% 
  arrange(correlation) %>% 
  top_n(20, correlation) %>% 
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
  top_n(20, -correlation) %>% 
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

