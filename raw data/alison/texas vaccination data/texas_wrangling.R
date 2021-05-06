library(tidyverse)
library(janitor)
library(readr)
library(naniar)



#### wrangling texas vaccination
texas_vaccine_data <- read_csv("~/Blog Food and PUG Administration/raw data/alison/texas vaccination data/Texas Admin Vaccination Data by Race and by County .csv")%>%
  clean_names() %>%
  filter(race_ethnicity != "Unknown") %>%
  filter(county_name != "Other") %>%
  filter(county_name != "Grand Total") %>%
  select(-x6)

#### wrangling texas population data
texas_population_data <-read_csv("alldata.csv") %>%
  filter(County != "STATE OF TEXAS") %>%
  filter(Age == "All Ages") %>%
  select(c(County
           , NH_White_Total
           , NH_Black_Total
           , NH_Asian_Total
           , NH_Other_Total
           , Hispanic_Total)) %>%
  rename("Asian" = "NH_Asian_Total"
         , "Black" = "NH_Black_Total"
         , "Hispanic" = "Hispanic_Total"
         , "White" = "NH_White_Total"
         , "Other" = "NH_Other_Total") %>%
  pivot_longer(col = -c(County), names_to = "race_ethnicity", values_to = "population") %>%
  mutate(County = str_to_title(County)) %>%
  separate(County, into=c("county_name", "remove"), remove = FALSE) %>%
  select(-c(remove, County))

texas_data <- texas_vaccine_data %>%
  inner_join(texas_population_data, by = c("county_name", "race_ethnicity")) %>%
  mutate(one_dose = people_vaccinated_with_at_least_one_dose/population
         , fully_vax = people_fully_vaccinated/population)



