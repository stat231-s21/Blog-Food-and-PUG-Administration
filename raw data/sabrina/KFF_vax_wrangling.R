library(tidyverse)
library(janitor)
library(readr)
library(naniar)

#### wrangling state demographic data to create demographic metric 
state_demographics <- read_csv("state_demographics.csv", 
                               skip = 2) %>% 
  slice(2:53) %>%
  select(-c(Footnotes, Total, "Multiple Races")) %>%
  rename("State" = "Location") %>%
  clean_names() %>%
  rename("native_pop" = "american_indian_alaska_native",
         "pacific_islander_pop" = "native_hawaiian_other_pacific_islander", 
         "black_pop" = "black",
         "white_pop" = "white", 
         "asian_pop" = "asian", 
         "hispanic_pop" = "hispanic") %>%
  replace_with_na_at(.vars = c("native_pop",
                               "pacific_islander_pop",
                               "asian_pop"),
                     condition = ~ .x == "N/A") %>% # replace N/A with NA
  mutate(asian_pop = as.numeric(asian_pop), # converting chr to num
         native_pop = as.numeric(native_pop),
         pacific_islander_pop = as.numeric(pacific_islander_pop))


####  wrangling  KFF vaccine demographic data for March 1 - April 26

## March 1 
KFF_vax_March1 <- read_csv("KFF_vax_March1.csv", 
                                 col_types = cols(Footnotes = col_character()), 
                                 skip = 2) %>%
  slice(1:51) %>%
  clean_names() %>%
  rename("state" = "location", 
         "pacific_islander_vax" = "native_hawaiian_or_other_pacific_islander_percent_of_vaccinations", 
         "native_vax" = "american_indian_or_alaska_native_percent_of_vaccinations", 
         "white_vax" = "white_percent_of_vaccinations", 
         "black_vax" = "black_percent_of_vaccinations", 
         "hispanic_vax" = "hispanic_percent_of_vaccinations", 
         "asian_vax" = "asian_percent_of_vaccinations") %>%
  select(-c(footnotes)) %>%
  mutate(asian_vax = ifelse(asian_vax == "<.01", "0", asian_vax), # replacing <0.01 with 0
         black_vax = ifelse(black_vax == "<.01", "0", black_vax),
         native_vax = ifelse(native_vax == "<.01", "0", native_vax),
         pacific_islander_vax= ifelse(pacific_islander_vax == "<.01", "0", 
                                      pacific_islander_vax)) %>%
  replace_with_na_at(.vars = c("white_vax",
                               "asian_vax", 
                               "black_vax",
                               "hispanic_vax",
                               "native_vax",
                               "pacific_islander_vax"),
                     condition = ~ .x == "NR") %>% # replace NR (not reported) with NA
  mutate(white_vax = as.numeric(white_vax), # converting from chr to num
         asian_vax = as.numeric(asian_vax),
         black_vax = as.numeric(black_vax),
         hispanic_vax = as.numeric(hispanic_vax),
         native_vax = as.numeric(native_vax),
         pacific_islander_vax = as.numeric(pacific_islander_vax), 
         date = "March 1") %>%
  inner_join(state_demographics, by = "state") %>%
  mutate(white_metric = white_vax/white_pop,
         black_metric = black_vax/black_pop,
         hispanic_metric = hispanic_vax/hispanic_pop, 
         asian_metric = asian_vax/asian_pop,
         native_metric = native_vax/native_pop,
         pacific_islander_metric = pacific_islander_vax/pacific_islander_pop)

## March 15
KFF_vax_March15 <- read_csv("KFF_vax_March15.csv", 
                           col_types = cols(Footnotes = col_character()), 
                           skip = 2) %>%
  slice(1:51) %>%
  clean_names() %>%
  rename("state" = "location", 
         "pacific_islander_vax" = "native_hawaiian_or_other_pacific_islander_percent_of_vaccinations", 
         "native_vax" = "american_indian_or_alaska_native_percent_of_vaccinations", 
         "white_vax" = "white_percent_of_vaccinations", 
         "black_vax" = "black_percent_of_vaccinations", 
         "hispanic_vax" = "hispanic_percent_of_vaccinations", 
         "asian_vax" = "asian_percent_of_vaccinations") %>%
  select(-c(footnotes)) %>%
  mutate(asian_vax = ifelse(asian_vax == "<.01", "0", asian_vax), # replacing <0.01 with 0
         black_vax = ifelse(black_vax == "<.01", "0", black_vax),
         native_vax = ifelse(native_vax == "<.01", "0", native_vax),
         pacific_islander_vax= ifelse(pacific_islander_vax == "<.01", "0", 
                                      pacific_islander_vax)) %>%
  replace_with_na_at(.vars = c("white_vax",
                               "asian_vax", 
                               "black_vax",
                               "hispanic_vax",
                               "native_vax",
                               "pacific_islander_vax"),
                     condition = ~ .x == "NR") %>% # replace NR (not reported) with NA
  mutate(white_vax = as.numeric(white_vax), # converting from chr to num
         asian_vax = as.numeric(asian_vax),
         black_vax = as.numeric(black_vax),
         hispanic_vax = as.numeric(hispanic_vax),
         native_vax = as.numeric(native_vax),
         pacific_islander_vax = as.numeric(pacific_islander_vax), 
         date = "March 15") %>%
  inner_join(state_demographics, by = "state") %>%
  mutate(white_metric = white_vax/white_pop,
         black_metric = black_vax/black_pop,
         hispanic_metric = hispanic_vax/hispanic_pop, 
         asian_metric = asian_vax/asian_pop,
         native_metric = native_vax/native_pop,
         pacific_islander_metric = pacific_islander_vax/pacific_islander_pop)

## March 29
KFF_vax_March29 <- read_csv("KFF_vax_March29.csv", 
                            col_types = cols(Footnotes = col_character()), 
                            skip = 2) %>%
  slice(1:51) %>%
  clean_names() %>%
  rename("state" = "location", 
         "pacific_islander_vax" = "native_hawaiian_or_other_pacific_islander_percent_of_vaccinations", 
         "native_vax" = "american_indian_or_alaska_native_percent_of_vaccinations", 
         "white_vax" = "white_percent_of_vaccinations", 
         "black_vax" = "black_percent_of_vaccinations", 
         "hispanic_vax" = "hispanic_percent_of_vaccinations", 
         "asian_vax" = "asian_percent_of_vaccinations") %>%
  select(-c(footnotes)) %>%
  mutate(asian_vax = ifelse(asian_vax == "<.01", "0", asian_vax), # replacing <0.01 with 0
         black_vax = ifelse(black_vax == "<.01", "0", black_vax),
         native_vax = ifelse(native_vax == "<.01", "0", native_vax),
         pacific_islander_vax= ifelse(pacific_islander_vax == "<.01", "0", 
                                      pacific_islander_vax)) %>%
  replace_with_na_at(.vars = c("white_vax",
                               "asian_vax", 
                               "black_vax",
                               "hispanic_vax",
                               "native_vax",
                               "pacific_islander_vax"),
                     condition = ~ .x == "NR") %>% # replace NR (not reported) with NA
  mutate(white_vax = as.numeric(white_vax), # converting from chr to num
         asian_vax = as.numeric(asian_vax),
         black_vax = as.numeric(black_vax),
         hispanic_vax = as.numeric(hispanic_vax),
         native_vax = as.numeric(native_vax),
         pacific_islander_vax = as.numeric(pacific_islander_vax), 
         date = "March 29") %>%
  inner_join(state_demographics, by = "state") %>%
  mutate(white_metric = white_vax/white_pop,
         black_metric = black_vax/black_pop,
         hispanic_metric = hispanic_vax/hispanic_pop, 
         asian_metric = asian_vax/asian_pop,
         native_metric = native_vax/native_pop,
         pacific_islander_metric = pacific_islander_vax/pacific_islander_pop)


## April 5
KFF_vax_April5 <- read_csv("KFF_vax_April5.csv", 
                            col_types = cols(Footnotes = col_character()), 
                            skip = 2) %>%
  slice(1:51) %>%
  clean_names() %>%
  rename("state" = "location", 
         "pacific_islander_vax" = "native_hawaiian_or_other_pacific_islander_percent_of_vaccinations", 
         "native_vax" = "american_indian_or_alaska_native_percent_of_vaccinations", 
         "white_vax" = "white_percent_of_vaccinations", 
         "black_vax" = "black_percent_of_vaccinations", 
         "hispanic_vax" = "hispanic_percent_of_vaccinations", 
         "asian_vax" = "asian_percent_of_vaccinations") %>%
  select(-c(footnotes)) %>%
  mutate(asian_vax = ifelse(asian_vax == "<.01", "0", asian_vax), # replacing <0.01 with 0
         black_vax = ifelse(black_vax == "<.01", "0", black_vax),
         native_vax = ifelse(native_vax == "<.01", "0", native_vax),
         pacific_islander_vax= ifelse(pacific_islander_vax == "<.01", "0", 
                                      pacific_islander_vax)) %>%
  replace_with_na_at(.vars = c("white_vax",
                               "asian_vax", 
                               "black_vax",
                               "hispanic_vax",
                               "native_vax",
                               "pacific_islander_vax"),
                     condition = ~ .x == "NR") %>% # replace NR (not reported) with NA
  mutate(white_vax = as.numeric(white_vax), # converting from chr to num
         asian_vax = as.numeric(asian_vax),
         black_vax = as.numeric(black_vax),
         hispanic_vax = as.numeric(hispanic_vax),
         native_vax = as.numeric(native_vax),
         pacific_islander_vax = as.numeric(pacific_islander_vax), 
         date = "April 5") %>%
  inner_join(state_demographics, by = "state") %>%
  mutate(white_metric = white_vax/white_pop,
         black_metric = black_vax/black_pop,
         hispanic_metric = hispanic_vax/hispanic_pop, 
         asian_metric = asian_vax/asian_pop,
         native_metric = native_vax/native_pop,
         pacific_islander_metric = pacific_islander_vax/pacific_islander_pop)

## April 12
KFF_vax_April12 <- read_csv("KFF_vax_April12.csv", 
                            col_types = cols(Footnotes = col_character()), 
                            skip = 2) %>%
  slice(1:51) %>%
  clean_names() %>%
  rename("state" = "location", 
         "pacific_islander_vax" = "native_hawaiian_or_other_pacific_islander_percent_of_vaccinations", 
         "native_vax" = "american_indian_or_alaska_native_percent_of_vaccinations", 
         "white_vax" = "white_percent_of_vaccinations", 
         "black_vax" = "black_percent_of_vaccinations", 
         "hispanic_vax" = "hispanic_percent_of_vaccinations", 
         "asian_vax" = "asian_percent_of_vaccinations") %>%
  select(-c(footnotes)) %>%
  mutate(asian_vax = ifelse(asian_vax == "<.01", "0", asian_vax), # replacing <0.01 with 0
         black_vax = ifelse(black_vax == "<.01", "0", black_vax),
         native_vax = ifelse(native_vax == "<.01", "0", native_vax),
         pacific_islander_vax= ifelse(pacific_islander_vax == "<.01", "0", 
                                      pacific_islander_vax)) %>%
  replace_with_na_at(.vars = c("white_vax",
                               "asian_vax", 
                               "black_vax",
                               "hispanic_vax",
                               "native_vax",
                               "pacific_islander_vax"),
                     condition = ~ .x == "NR") %>% # replace NR (not reported) with NA
  mutate(white_vax = as.numeric(white_vax), # converting from chr to num
         asian_vax = as.numeric(asian_vax),
         black_vax = as.numeric(black_vax),
         hispanic_vax = as.numeric(hispanic_vax),
         native_vax = as.numeric(native_vax),
         pacific_islander_vax = as.numeric(pacific_islander_vax), 
         date = "April 12") %>%
  inner_join(state_demographics, by = "state") %>%
  mutate(white_metric = white_vax/white_pop,
         black_metric = black_vax/black_pop,
         hispanic_metric = hispanic_vax/hispanic_pop, 
         asian_metric = asian_vax/asian_pop,
         native_metric = native_vax/native_pop,
         pacific_islander_metric = pacific_islander_vax/pacific_islander_pop)

## April 19
KFF_vax_April19 <- read_csv("KFF_vax_April19.csv", 
                            col_types = cols(Footnotes = col_character()), 
                            skip = 2) %>%
  slice(1:51) %>%
  clean_names() %>%
  rename("state" = "location", 
         "pacific_islander_vax" = "native_hawaiian_or_other_pacific_islander_percent_of_vaccinations", 
         "native_vax" = "american_indian_or_alaska_native_percent_of_vaccinations", 
         "white_vax" = "white_percent_of_vaccinations", 
         "black_vax" = "black_percent_of_vaccinations", 
         "hispanic_vax" = "hispanic_percent_of_vaccinations", 
         "asian_vax" = "asian_percent_of_vaccinations") %>%
  select(-c(footnotes)) %>%
  mutate(asian_vax = ifelse(asian_vax == "<.01", "0", asian_vax), # replacing <0.01 with 0
         black_vax = ifelse(black_vax == "<.01", "0", black_vax),
         native_vax = ifelse(native_vax == "<.01", "0", native_vax),
         pacific_islander_vax= ifelse(pacific_islander_vax == "<.01", "0", 
                                      pacific_islander_vax)) %>%
  replace_with_na_at(.vars = c("white_vax",
                               "asian_vax", 
                               "black_vax",
                               "hispanic_vax",
                               "native_vax",
                               "pacific_islander_vax"),
                     condition = ~ .x == "NR") %>% # replace NR (not reported) with NA
  mutate(white_vax = as.numeric(white_vax), # converting from chr to num
         asian_vax = as.numeric(asian_vax),
         black_vax = as.numeric(black_vax),
         hispanic_vax = as.numeric(hispanic_vax),
         native_vax = as.numeric(native_vax),
         pacific_islander_vax = as.numeric(pacific_islander_vax), 
         date = "April 19") %>%
  inner_join(state_demographics, by = "state") %>%
  mutate(white_metric = white_vax/white_pop,
         black_metric = black_vax/black_pop,
         hispanic_metric = hispanic_vax/hispanic_pop, 
         asian_metric = asian_vax/asian_pop,
         native_metric = native_vax/native_pop,
         pacific_islander_metric = pacific_islander_vax/pacific_islander_pop)

## April 26
KFF_vax_April26 <- read_csv("KFF_vax_April26.csv", 
                            col_types = cols(Footnotes = col_character()), 
                            skip = 2) %>%
  slice(1:51) %>%
  clean_names() %>%
  rename("state" = "location", 
         "pacific_islander_vax" = "native_hawaiian_or_other_pacific_islander_percent_of_vaccinations", 
         "native_vax" = "american_indian_or_alaska_native_percent_of_vaccinations", 
         "white_vax" = "white_percent_of_vaccinations", 
         "black_vax" = "black_percent_of_vaccinations", 
         "hispanic_vax" = "hispanic_percent_of_vaccinations", 
         "asian_vax" = "asian_percent_of_vaccinations") %>%
  select(-c(footnotes)) %>%
  mutate(asian_vax = ifelse(asian_vax == "<.01", "0", asian_vax), # replacing <0.01 with 0
         black_vax = ifelse(black_vax == "<.01", "0", black_vax),
         native_vax = ifelse(native_vax == "<.01", "0", native_vax),
         pacific_islander_vax= ifelse(pacific_islander_vax == "<.01", "0", 
                                      pacific_islander_vax)) %>%
  replace_with_na_at(.vars = c("white_vax",
                               "asian_vax", 
                               "black_vax",
                               "hispanic_vax",
                               "native_vax",
                               "pacific_islander_vax"),
                     condition = ~ .x == "NR") %>% # replace NR (not reported) with NA
  mutate(white_vax = as.numeric(white_vax), # converting from chr to num
         asian_vax = as.numeric(asian_vax),
         black_vax = as.numeric(black_vax),
         hispanic_vax = as.numeric(hispanic_vax),
         native_vax = as.numeric(native_vax),
         pacific_islander_vax = as.numeric(pacific_islander_vax), 
         date = "April 26") %>%
  inner_join(state_demographics, by = "state") %>%
  mutate(white_metric = white_vax/white_pop,
         black_metric = black_vax/black_pop,
         hispanic_metric = hispanic_vax/hispanic_pop, 
         asian_metric = asian_vax/asian_pop,
         native_metric = native_vax/native_pop,
         pacific_islander_metric = pacific_islander_vax/pacific_islander_pop)

## May 3
KFF_vax_May3 <- read_csv("KFF_vax_May3.csv", 
                            col_types = cols(Footnotes = col_character()), 
                            skip = 2) %>%
  slice(1:51) %>%
  clean_names() %>%
  rename("state" = "location", 
         "pacific_islander_vax" = "native_hawaiian_or_other_pacific_islander_percent_of_vaccinations", 
         "native_vax" = "american_indian_or_alaska_native_percent_of_vaccinations", 
         "white_vax" = "white_percent_of_vaccinations", 
         "black_vax" = "black_percent_of_vaccinations", 
         "hispanic_vax" = "hispanic_percent_of_vaccinations", 
         "asian_vax" = "asian_percent_of_vaccinations") %>%
  select(-c(footnotes)) %>%
  mutate(asian_vax = ifelse(asian_vax == "<.01", "0", asian_vax), # replacing <0.01 with 0
         black_vax = ifelse(black_vax == "<.01", "0", black_vax),
         native_vax = ifelse(native_vax == "<.01", "0", native_vax),
         pacific_islander_vax= ifelse(pacific_islander_vax == "<.01", "0", 
                                      pacific_islander_vax)) %>%
  replace_with_na_at(.vars = c("white_vax",
                               "asian_vax", 
                               "black_vax",
                               "hispanic_vax",
                               "native_vax",
                               "pacific_islander_vax"),
                     condition = ~ .x == "NR") %>% # replace NR (not reported) with NA
  mutate(white_vax = as.numeric(white_vax), # converting from chr to num
         asian_vax = as.numeric(asian_vax),
         black_vax = as.numeric(black_vax),
         hispanic_vax = as.numeric(hispanic_vax),
         native_vax = as.numeric(native_vax),
         pacific_islander_vax = as.numeric(pacific_islander_vax), 
         date = "May 3") %>%
  inner_join(state_demographics, by = "state") %>%
    mutate(white_metric = white_vax/white_pop,
           black_metric = black_vax/black_pop,
           hispanic_metric = hispanic_vax/hispanic_pop, 
           asian_metric = asian_vax/asian_pop,
           native_metric = native_vax/native_pop,
           pacific_islander_metric = pacific_islander_vax/pacific_islander_pop)

# join each KFF_vax_[date] so that the column names are the same, want to 
# be able to use reactive dataset in the shiny app to filter for the date the
# user choses. 

KFF_vax_join <- 
