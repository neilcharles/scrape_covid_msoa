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

cases_for_model <- read_rds('msoa_processed_cases.RDS')

correlate <- function(df){
  cors <- cor(select(df, -GeographyCode, -cases_pct), select(df, cases_pct))
  
  tibble(label = row.names(cors),
         correlation = as.vector(cors)
         )
}

covid_cases_demogs <- cases_for_model %>% 
  select(msoa11cd, week_ending, cases_count) %>% 
  rename(GeographyCode = msoa11cd) %>% 
  left_join(demogs) %>% 
  mutate(cases_pct = cases_count / `All categories: Sex`) %>% 
  select(-cases_count, -`All categories: Sex`) %>% 
  drop_na() %>% 
  nest(data = -'week_ending') %>% 
  mutate(correlations = map(data, .f = ~correlate(.x))) %>% 
  arrange(week_ending)

write_rds(covid_cases_demogs, 'msoa_correlations.RDS')

target_week_ending <- '2020-09-13'

chart_data <- covid_cases_demogs %>% 
  filter(week_ending==target_week_ending) %>% 
  select(week_ending, correlations) %>% 
  unnest(cols = c(correlations))

chart_data %>% 
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
  
chart_data %>% 
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

