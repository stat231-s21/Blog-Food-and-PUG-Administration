library(tidyverse)
library(janitor)
library(readr)
library(datasets)
library(viridis)
library(maps)
library(leaflet)
library(tmap)
library(tigris)

#long and lat information for texas counties
texas_counties <- map_data(map = "county"
                           , region = ".") %>%
  filter(region == "texas") %>%
  mutate(county_name = str_to_title(subregion)) %>%
  select(-c(region, subregion)) %>%
  mutate(county_name = paste0(county_name, " County" ))


#### wrangling texas vaccination data
texas_vaccine_data <- read_csv("texas_vaccine_data_by_race.csv")%>%
  clean_names() %>%
  filter(race_ethnicity != "Unknown") %>%
  filter(county_name != "Other") %>%
  filter(county_name != "Grand Total") %>%
  pivot_wider(names_from = race_ethnicity
              , values_from = c(doses_administered
                                , people_fully_vaccinated
                                , people_vaccinated_with_at_least_one_dose)) %>%
  mutate(county_name = paste0(county_name, " County" )) #making names match

#### wrangling texas population data
texas_population_data <-read_csv("alldata.csv") %>%
  filter(County != "STATE OF TEXAS") %>%
  filter(Age == "All Ages") %>%
  select(c(County
           , Total
           , NH_White_Total
           , NH_Black_Total
           , NH_Asian_Total
           , NH_Other_Total
           , Hispanic_Total)) %>%
  rename("asian_population" = "NH_Asian_Total"
         , "black_population" = "NH_Black_Total"
         , "hispanic_population" = "Hispanic_Total"
         , "white_population" = "NH_White_Total"
         , "other_population" = "NH_Other_Total"
         , "total_population" = "Total") %>%
  mutate(County = str_to_title(County)) %>%
  rename("county_name" = "County") 

#total population vaccinated by state
texas_total <- read_csv("texas_total.csv", 
                        col_types = cols(`Total Doses Allocated` = col_number(), 
                                         `Vaccine Doses Administered` = col_number(), 
                                         `People Vaccinated with at least One Dose` = col_integer(), 
                                         `People Fully Vaccinated` = col_number())) %>%
  clean_names() %>%
  select(c(county_name, people_fully_vaccinated)) %>%
  mutate(county_name = paste0(county_name, " County" ))

#fips numbers for leaflet map  
texas_county_fips <- read_csv("texas_county_fips.csv", 
                              col_types = cols(`FIPS #` = col_character())) %>%
  clean_names() %>%
  mutate("COUNTYFP" = fips_number)




#### joining 
texas_data <- texas_vaccine_data %>% 
  inner_join(texas_population_data, by = "county_name") %>%
  inner_join(texas_total, by = "county_name") %>%
  mutate(asian_fully = people_fully_vaccinated_Asian/asian_population
         , hispanic_fully = people_fully_vaccinated_Hispanic/hispanic_population
         , black_fully = people_fully_vaccinated_Black/black_population
         , white_fully = people_fully_vaccinated_White/white_population
         , other_fully = people_fully_vaccinated_Other/other_population
         , total_fully = people_fully_vaccinated/total_population) %>%
  inner_join(texas_counties, by = "county_name") 


##graphs

ggplot(texas_data, aes(x = long, y = lat, group = group
                       , fill = total_fully)) +
  geom_polygon(color = "white") +
  theme_void() +
  coord_fixed(ratio = 1.3) +
  labs(title = "Total"
       , fill = "Total Population Vaccinated") +
  scale_fill_distiller(palette = "BuPu", direction = "horizantle")

ggplot(texas_data, aes(x = long, y = lat, group = group
                       , fill = asian_fully)) +
  geom_polygon(color = "white") +
  theme_void() +
  coord_fixed(ratio = 1.3) +
  labs(title = "Asian"
       , fill = "Total Population Vaccinated") +
  scale_fill_distiller(palette = "BuPu", direction = "horizantle")

ggplot(texas_data, aes(x = long, y = lat, group = group
                       , fill = black_fully)) +
  geom_polygon(color = "white") +
  theme_void() +
  coord_fixed(ratio = 1.3) +
  labs(title = "Black"
       , fill = "Total Population Fully Vaccinated") +
  scale_fill_distiller(palette = "BuPu", direction = "horizantle")

ggplot(texas_data, aes(x = long, y = lat, group = group
                       , fill = white_fully)) +
  geom_polygon(color = "white") +
  theme_void() +
  coord_fixed(ratio = 1.3) +
  labs(title = "White"
       , fill = "Total Population Fully Vaccinated") +
  scale_fill_distiller(palette = "BuPu", direction = "horizantle")

ggplot(texas_data, aes(x = long, y = lat, group = group
                       , fill = hispanic_fully)) +
  geom_polygon(color = "white") +
  theme_void() +
  coord_fixed(ratio = 1.3) +
  labs(title = "Hispanic"
       , fill = "Total Population Fully Vaccinated") +
  scale_fill_distiller(palette = "BuPu", direction = "horizantle")


#combining names of counties to dataset with geometry information for map
texas_tracts <- tracts(state = 'TX', cb=TRUE) %>%
  inner_join(texas_county_fips, by = "COUNTYFP") %>%
  mutate(county_name = paste0(county_name, " County" ))


#combining all data into one df 
texas_merged <- geo_join(texas_tracts, texas_data, "county_name", "county_name")

popup_texas <- paste0(texas_merged$county_name
                      , "<br>"
                      , "<br>"
                      , "Percent of Total population vaccinated: "
                      , round(texas_merged$total_fully,2)
                      , "<br>"
                      , "<br>"
                      , "Percent of Asian population vaccinated: "
                      , round(texas_merged$asian_fully,2)
                      , "<br>"
                      , "Percent of Black population vaccinated: "
                      , round(texas_merged$black_fully,2)
                      , "<br>"
                      , "Percent of Hispanic population vaccinated: "
                      , round(texas_merged$hispanic_fully,2)
                      , "<br>"
                      , "Percent of White population vaccinated: "
                      , round(texas_merged$white_fully,2))

texas_pal <- colorNumeric(
  palette = "YlGnBu",
  domain = texas_merged$total_fully
)

texas_vax_map <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = texas_merged, 
              fillColor = ~texas_pal(total_fully), 
              color = "#b2aeae",
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup_texas) %>%
  addLegend(pal = texas_pal, 
            values = texas_merged$total_fully, 
            position = "bottomright", 
            title = "Percent of Total Population <br> Fully Vaccinated")

