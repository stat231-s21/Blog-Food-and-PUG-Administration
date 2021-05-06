library(tidyverse)
library(janitor)
library(readr)
library(naniar)



#### wrangling state demographic data to create demographic metric 
texas_vaccine_data <- read_csv("~/Blog Food and PUG Administration/raw data/alison/texas vaccination data/Texas Admin Vaccination Data by Race and by County .csv")

texas_population_data <-read_csv("alldata.csv") %>%
  filter(County != "STATE OF TEXAS") %>%
  filter(Age == "All Ages") %>%
  select(c(County
           , Total
           , NH_White_Total
           , NH_Black_Total
           , NH_Asian_Total
           , NH_Other_Total
           , Hispanic_Total))
