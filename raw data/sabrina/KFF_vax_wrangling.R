library(tidyverse)
library(janitor)
library(readr)
library(naniar)
library(geojsonio)
library(broom)
library(rgeos)

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

## May 10
KFF_vax_May10 <- read_csv("KFF_vax_May10.csv", 
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
         date = "May 10") %>%
  inner_join(state_demographics, by = "state") %>%
  mutate(white_metric = white_vax/white_pop,
         black_metric = black_vax/black_pop,
         hispanic_metric = hispanic_vax/hispanic_pop, 
         asian_metric = asian_vax/asian_pop,
         native_metric = native_vax/native_pop,
         pacific_islander_metric = pacific_islander_vax/pacific_islander_pop)

# join each KFF_vax_[date] so that the column names are the same, want to 
# be able to use reactive dataset in the shiny app to filter for the date the
# user chooses. 

KFF_vax_join <- KFF_vax_March1 %>%
  rbind(KFF_vax_March15) %>%
  rbind(KFF_vax_March29) %>%
  rbind(KFF_vax_April5) %>%
  rbind(KFF_vax_April12) %>%
  rbind(KFF_vax_April19) %>%
  rbind(KFF_vax_April26) %>%
  rbind(KFF_vax_May3) %>%
  rbind(KFF_vax_May10)

#### DATA WRANGLING FOR HEXBIN MAP 

# Hexagones boundaries in geojson format:  
# https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map.
spdf <- geojson_read("us_states_hexgrid.geojson", what = "sp")

# 'fortify' the data to be able to show it with ggplot2 
# (needs to be in data frame format)
# tidy() function is from the broom package
spdf_fortified <- tidy(spdf, region = "google_name") 

# Calculate the centroid of each hexagon to add the label
# gCentroid function is from the rgeos package
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE)
                                       , id=spdf@data$iso3166_2))

# join data_wrangling with spdf_fortified by state
# note: will have many many rows per state still 
# (data_wrangling values will be repeated for each state)
hexbin_wrangling <- spdf_fortified %>%
  mutate(state = str_replace(id, " \\(United States\\)","")) %>%
  left_join(KFF_vax_join, by = "state")

# to plot with categorical value (demographic metric in categories) mapping to color cue:
# create categorical var

hexbin_wrangling <- hexbin_wrangling %>% # change continuous variable to categorical
  mutate(white_metric_cat = cut(white_metric, 
                                breaks = c(-1, 0.5, 0.95, 1.05, 1.5, 10),
                                labels = c("Extrememly below share (<0.50x)", 
                                           "Below share (0.50x-0.95x)", 
                                           "Proportionate share (0.95x-1.05x)",
                                           "Above share (1.05x-1.50x)",
                                           "Extremely above share (>1.50x)")),
         black_metric_cat = cut(black_metric, 
                                breaks = c(-1, 0.5, 0.95, 1.05, 1.5, 10),                                
                                labels = c("Extrememly below share (<0.50x)", 
                                           "Below share (0.50x-0.95x)", 
                                           "Proportionate share (0.95x-1.05x)",
                                           "Above share (1.05x-1.50x)",
                                           "Extremely above share (>1.50x)")),
         hispanic_metric_cat = cut(hispanic_metric, 
                                   breaks = c(-1, 0.5, 0.95, 1.05, 1.5, 10),                                   
                                   labels = c("Extrememly below share (<0.50x)", 
                                              "Below share (0.50x-0.95x)", 
                                              "Proportionate share (0.95x-1.05x)",
                                              "Above share (1.05x-1.50x)",
                                              "Extremely above share (>1.50x)")),
         asian_metric_cat = cut(asian_metric, 
                                breaks = c(-1, 0.5, 0.95, 1.05, 1.5, 10),                                
                                labels = c("Extrememly below share (<0.50x)", 
                                           "Below share (0.50x-0.95x)", 
                                           "Proportionate share (0.95x-1.05x)",
                                           "Above share (1.05x-1.50x)",
                                           "Extremely above share (>1.50x)")),
         native_metric_cat = cut(native_metric, 
                                 breaks = c(-1, 0.5, 0.95, 1.05, 1.5, 10),                                 
                                 labels = c("Extrememly below share (<0.50x)", 
                                            "Below share (0.50x-0.95x)", 
                                            "Proportionate share (0.95x-1.05x)",
                                            "Above share (1.05x-1.50x)",
                                            "Extremely above share (>1.50x)")),
         pacific_islander_metric_cat = cut(pacific_islander_metric, 
                                           breaks = c(-1, 0.5, 0.95, 1.05, 1.5, 10),                                    
                                           labels = c("Extrememly below share (<0.50x)", 
                                                      "Below share (0.50x-0.95x)", 
                                                      "Proportionate share (0.95x-1.05x)",
                                                      "Above share (1.05x-1.50x)",
                                                      "Extremely above share (>1.50x)")))


hexbin_wrangling$white_metric_cat <- factor(hexbin_wrangling$white_metric_cat, 
                                            levels = c("Extrememly below share (<0.50x)", 
                                                       "Below share (0.50x-0.95x)", 
                                                       "Proportionate share (0.95x-1.05x)",
                                                       "Above share (1.05x-1.50x)",
                                                       "Extremely above share (>1.50x)"))

write_csv(hexbin_wrangling, "hexbin_wrangling.csv", append = FALSE)

write_csv(centers, "centers.csv", append = FALSE)

levels(hexbin_wrangling$white_metric_cat)

