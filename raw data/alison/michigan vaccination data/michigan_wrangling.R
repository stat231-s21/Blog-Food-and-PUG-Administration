library(tidyverse)
library(janitor)
library(readr)
library(naniar)

#### wrangling state demographic data to create demographic metric 
michigan_data <- read_csv("Covid19 Vaccination data by county.csv") %>%
  clean_names() %>%
  filter(persons_residence_in_county != "No County") %>%
  rename("county" = "persons_residence_in_county") %>% 
  filter(sex != "U") %>%
  group_by(sex, week_ending_date)
  

