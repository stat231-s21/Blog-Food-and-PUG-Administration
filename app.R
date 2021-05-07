library(shiny)
library(tidyverse)
library(geojsonio)
library(tidyverse)
library(broom)
library(rgeos)
library(shinythemes)
library(shinyjs)
library(DT)

#### COMBINED SHINY APP




###############
# import data #
###############

# added a [demographic]_metric_cat variable that creates a categorical variable 
# out of the continuous [demographic]_metric variables, with consistent breaks 
# across the 6 demographic groups:
# <=0.49x, 0.50x-0.74x, 0.75x-0.99x, 1.00x-1.24x, 1.25x-1.50x, >=1.50x
hexbin_wrangling <- read_csv("hexbin_wrangling.csv")

# Calculate the centroid of each hexagon to add the label
# gCentroid function is from the rgeos package
centers <- read_csv("centers.csv")

# import wrangled data
data_wrangling <- read_csv("data_wrangling.csv")

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
                        "April 12", "April 19", "April 26", "May 3")
date_choice_names <- c("March 1", "March 15", "March 29", "April 5", 
                       "April 12", "April 19", "April 26", "May 3")
names(date_choice_values) <- date_choice_names

## scatterplot 
metric_choice_values <- c("white_metric", 
                               "black_metric", 
                               "hispanic_metric", 
                               "asian_metric", 
                               "native_metric",
                               "pacific_islander_metric")
demographic_choice_names2 <- c("White",
                              "Black",
                              "Hispanic",
                              "Asian",
                              "American Indian or Alaska Native",
                              "Native Hawaiian or Other Pacific Islander")
names(metric_choice_values) <- demographic_choice_names2

# to choose which location variable in scatterplot
region_choices <- c("region", "division")
region_names <- c("Region", "Division")
names(region_choices) <- region_names


#TAB 3: TABLE 
#to choose state and variables

table_data <- read_csv("table_data.csv")

state_choices <- unique(table_data$State1)

column_choices <- names(table_data)

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
  ),
  
  ##### SCATTERPLOT (Caroline)
  # interactivity: demographic of interest for metric & which location grouping
  
  tabPanel(
    title = "Scatterplot", 
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "metric_demographic", 
                    label = "Choose a demographic of interest to plot:", 
                    choices = metric_choice_values, 
                    selected = "white_metric"),
        selectInput(inputId = "region", 
                    label = "Choose which geographic grouping to color the points by:", 
                    choices = region_choices, 
                    selected = "region")
      ), 
      mainPanel(
        plotOutput(outputId = "scatter")
      )
    )
  ), 
  
  
##### TABLE (Alison)
# interactivity: hyperlinks to state covid websites and allows users to select
#states and variables 

  tabPanel(
    title = "Table",
    
    sidebarLayout(
      sidebarPanel(
        selectizeInput(inputId = "State1"
                       , label = "Choose one or more state:"
                       , choices = state_choices
                       , multiple = TRUE), 
        
        selectizeInput(inputId = "select"
                       , label = "Select variables to display:"
                       , choices = column_choices
                       , selected = "State"
                       , multiple = TRUE),   
        actionButton("button1", "Go!"), 
        shinyjs::useShinyjs(),
        id = "side-panel", 
        actionButton("reset_input", "Reset inputs"),
        helpText("Clicking on the state name will take you to that state's COVID-19 website.")
      ),
      mainPanel(
        DT::dataTableOutput(outputId = "table")
      )
    )
  )
)




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
      geom_polygon(data=data_for_hexbin
                   , aes_string(x="long", y="lat", group="group", fill=input$map_demographic)) +
      geom_text(data=centers, aes(x=x, y=y, label=id)
                , color="white", size=3, alpha=0.6) +
      theme_void() +
      labs(fill = "Ratio of % vaccinations to % of population", 
           title = "Covid-19 vaccination proportionality by demographic group", 
           caption = "  Data as of March 29, 2021  \n states shaded in grey had no data available") +
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
  })
  output$scatter <- renderPlot({
    data_wrangling %>%
      ggplot(aes_string(x="efficiency", y=input$metric_demographic, color = input$region)) +
      labs(x = "Vaccine efficency", y = "Demographic metric", title = "Exploring Vaccine effiency vs. Demographic metric") +
      geom_point()
  })
  
  #### HEXBIN MAP (Alison)
  # interactivity: user selects state and variables to be shown
  data_for_table <- eventReactive(input$button1,{table_data %>% 
      filter(State1 %in% input$State1) %>%
      select(input$select) %>%
      mutate_if(is.numeric, round, 2) %>%
      datatable(table_data
                , escape=FALSE
                , caption = htmltools::tags$caption(
                    style = 'caption-side: bottom; text-align: center;'
                    ,'Table 1: '
                    , htmltools::em('Blank spaces indicate that there is no reported data for selected state.')
      )
    )
  }) 
  output$table <- DT::renderDataTable({
    
    data_for_table()
    
  })
  
  observeEvent(input$reset_input, {
    shinyjs::reset("side-panel")
  }
  )
  
}



####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)
