library(shiny)
library(tidyverse)
library(geojsonio)
library(tidyverse)
library(broom)
library(rgeos)
library(shinythemes)
library(shinyjs)
library(DT)

###############
# import data #
###############

# added a [demographic]_metric_cat variable that creates a categorical variable 
# out of the continuous [demographic]_metric variables, with consistent breaks 
# across the 6 demographic groups:
# <=0.49x, 0.50x-0.74x, 0.75x-0.99x, 1.00x-1.24x, 1.25x-1.50x, >=1.50x
hexbin_wrangling <- read_csv("hexbin_wrangling.csv") %>%
  mutate(white_metric_cat = as.factor(white_metric_cat),
         black_metric_cat = as.factor(black_metric_cat), 
         hispanic_metric_cat = as.factor(hispanic_metric_cat),
         asian_metric_cat = as.factor(asian_metric_cat), 
         native_metric_cat = as.factor(native_metric_cat),
         pacific_islander_metric_cat = as.factor(pacific_islander_metric_cat))

# Calculate the centroid of each hexagon to add the label
# gCentroid function is from the rgeos package
centers <- read_csv("centers.csv")

###################################################
# define choice values and labels for user inputs #
###################################################
## hexbin map 

# users choose demographic of interest
demographic_choice_values <- c("white_metric_cat", 
                               "black_metric_cat", 
                               "hispanic_metric_cat", 
                               "asian_metric_cat", 
                               "native_metric_cat",
                               "pacific_islander_metric_cat")
demographic_choice_names <- c("White",
                              "Black",
                              "Hispanic", 
                              "Asian",
                              "American Indian or Alaska Native",
                              "Native Hawaiian or Other Pacific Islander")
names(demographic_choice_values) <- demographic_choice_names

# user chooses date
date_choice_values <- c("March 1", "March 15", "March 29", "April 5", 
                        "April 12", "April 19", "April 26", "May 3", "May 10")
date_choice_names <- c("March 1", "March 15", "March 29", "April 5", 
                       "April 12", "April 19", "April 26", "May 3", "May 10")
names(date_choice_values) <- date_choice_names


############
#    ui    #
############

ui <- navbarPage(theme=shinytheme("united"),
  
  title = "Covid Vaccinations by Demographic", 
  
  #### HEXBIN MAP (Sabrina)
  # color by demographic metric (white_vax / white_pop)
  # interactivity: demographic of interest
  
  tabPanel(
    title = "Hexbin Map", 
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "map_demographic", 
                    label = "Choose a racial or ethnic group of interest to plot:", 
                    choices = demographic_choice_values, 
                    selected = "white_metric_cat"), 
        radioButtons(inputId = "date_choice"
                     , label = "Choose a date:"
                     , choices = date_choice_values
                     , selected = "March 1"),
      ), 
      mainPanel(
        plotOutput(outputId = "hexbin_map") 
      )
    )
  ))
############
# server   #
############

server <- function(input, output){
  
  #### HEXBIN MAP (Sabrina)
  # color by demographic metric (white_vax / white_pop)
  # interactivity: demographic of interest, select date
  data_for_hexbin <- reactive({
    data <- filter(hexbin_wrangling, date == input$date_choice)
  })
  
  output$hexbin_map <- renderPlot({
    ggplot() +
      geom_polygon(data=data_for_hexbin()
                   , aes_string(x="long", y="lat", group="group", fill=input$map_demographic)) +
      geom_text(data=centers, aes(x=x, y=y, label=id)
                , color="white", size=3, alpha=0.6) +
      theme_void() +
      labs(fill = "Ratio of % vaccinations to % of population", 
           title = "Covid-19 vaccination proportionality by demographic group", 
           caption = paste("Data as of", input$date_choice, ", 2021  \n states shaded in grey had no data available")) +
      scale_fill_manual(breaks = c("Extrememly below share (<0.50x)", 
                                   "Below share (0.50x-0.95x)", 
                                   "Proportionate share (0.95x-1.05x)",
                                   "Above share (1.05x-1.50x)",
                                   "Extremely above share (>1.50x)"), 
                        values = c("#FEBA80FF", 
                                   "#F8765CFF", 
                                   "#D3436EFF",
                                   "#982D80FF",
                                   "#5F187FFF"), 
                        labels = c("Extrememly \nbelow share \n(<0.50x)", 
                                   "Below share \n(0.50x-0.95x)", 
                                   "Proportionate share \n(0.95x-1.05x)",
                                   "Above share \n(1.05x-1.50x)",
                                   "Extremely \nabove share \n(>1.50x)"),
                        drop=FALSE,
                        guide = guide_legend(keyheight = unit(3, units = "mm")
                                             , keywidth=unit(12, units = "mm")
                                             , label.position = "bottom"
                                             , title.position = 'top'
                                             , nrow=1)
                        , na.value = "grey") + 
      theme(legend.position = c(0.5, 0.9)
            , text = element_text(color = "#22211d")
            , plot.background = element_rect(fill = "#f5f5f2", color = NA)
            , panel.background = element_rect(fill = "#f5f5f2", color = NA)
            , legend.background = element_rect(fill = "#f5f5f2", color = NA)
            , plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47"
                                        , margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))
  })}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)
