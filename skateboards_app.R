library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggrepel)
#library(fivethirtyeight)

###############
# import data #
###############
skateboards <- read_csv("electric_skateboards.txt")

#############################################################
# define choice values and labels for widgets (user inputs) #
#############################################################
# define vectors for choice values and labels 
# can then refer to them in server as well (not just in defining widgets)

# for TAB 1 (HISTOGRAM) widgets: 
# for selectInput, 'choices' object should be a NAMED LIST
hist_choice_values <- c("price","range","top_speed","weight","battery")
hist_choice_names <- c("Price","Range","Top Speed","Weight","Battery")
names(hist_choice_values) <- hist_choice_names

# for checkboxGroupInput
drv_choices <-  unique(skateboards$drive)

# for TAB 2 (SCATTERPLOT) widgets: 
# for radio button in scatterplot tab
size_choice_values <- c("price", "weight", "battery")
size_choice_names <- c("Price", "Weight", "Battery")
names(size_choice_values) <- size_choice_names

# for selectizeInput choices for skateboard name, pull directly from data
name_choices <- unique(skateboards$board)

# for TAB 3 (TABLE) widgets: 
# for selectizeInput choices for company name, pull directly from data

price_choices <- skateboards %>% select(price)

############
#    ui    #
############
ui <- fluidPage(
  div(style="padding: 1px 0px; width: '100%'",
      titlePanel(
        title="", windowTitle="Electric Skateboards"
      )
  ),
  navbarPage(
    
    title=div(img(src="skateboard_0.png", height="6%", width="6%"), a("Electric Skateboards", #scales image & hlink for title
                                                                      href="https://youtu.be/iFNTfhLpi4Q?t=39")), theme = shinytheme("simplex"),
    
    tabPanel(
      title = "Histogram",
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "histvar"
                      , label = "Choose a variable of interest to plot:"
                      , choices = hist_choice_values
                      , selected = "price"),
          checkboxGroupInput(inputId = "drv"
                             , label = "Include drive types:"
                             , choices = drv_choices
                             , selected = drv_choices
                             , inline = TRUE)),
        
        mainPanel(plotOutput(outputId = "hist")
        )
      )
    ),
    
    tabPanel(
      title = "Scatterplot",
      
      sidebarLayout(
        
        sidebarPanel(
          radioButtons(inputId = "pt_size"
                       , label = "Size points by:"
                       , choices = size_choice_values
                       , selected = "weight"),
          selectizeInput(inputId = "id_name"
                         , label = "Identify skateboard(s) in the scatterplot:"
                         , choices = name_choices
                         , selected = NULL
                         , multiple = TRUE)
        ),
        mainPanel(
          plotOutput(outputId = "scatter")
        )
      )
    ),
    
    tabPanel(
      title = "Table",
      
      sidebarLayout(
        sidebarPanel(
          sliderInput("price",label= h3("Price Range"),
                      min=min(price_choices),
                      max=max(price_choices), 
                      value=c(180,4000))
        ),
        mainPanel(
          DT::dataTableOutput(outputId = "table")
        )
      )
    ),
    
    tabPanel(
      title = "Fun", 
      
      sidebarLayout(
        sidebarPanel(
          helpText("Here is a fun skateboarding GIF I found! Please enjoy!")),
        mainPanel(img(src="skateboard.gif")
                  
        )
        
        
      )
    )
  )
  
  
  
)

############
# server   #
############
server <- function(input,output){
  
  # TAB 1: HISTOGRAM
  data_for_hist <- reactive({
    data <- filter(skateboards, drive %in% input$drv)
  })
  
  output$hist <- renderPlot({
    ggplot(data = data_for_hist(), aes_string(x = input$histvar)) +
      geom_histogram(color = "#2c7fb8", fill = "#7fcdbb", alpha = 0.7) +
      labs(x = hist_choice_names[hist_choice_values == input$histvar]
           , y = "Number of Skateboards")
  }) 
  
  # TAB 2: INTERACTIVE SCATTERPLOT 
  output$scatter <- renderPlot({
    skateboards %>%
      filter(drive != "Direct") %>%
      ggplot(aes_string(x="range", y="top_speed", size = input$pt_size)) +
      geom_point(color = "#2c7fb8") +
      labs(x = "Range (miles)", y = "Top Speed (mph)"
           , title = "Electric Skateboards", subtitle = "August 2018"
           , size = size_choice_names[size_choice_values == input$pt_size]) +
      geom_label_repel(data = filter(skateboards, board %in% input$id_name)
                       , aes(label = board), show.legend = FALSE) +
      facet_grid(~drive) 
  })
  
  # TAB 3: TABLE
  
  data_for_table <- reactive({
    data <- filter(skateboards, price >= input$price[1] & price <= input$price[2])
  })
  
  output$table <- DT::renderDataTable({ 
    data_for_table()
  })
  
  
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)

# Your turn.  Copy this code as a template into a new app.R file (WITHIN A FOLDER
# named something different than your other Shiny app folders).  Then, either 
# (1) update this template to still explore the skateboards dataset, but with
#     different app functionality (e.g. different widgets, variables, layout, theme...); 
#   OR
# (2) use this as a template to create a Shiny app for a different dataset 
#     from the fivethirtyeight package:
#     either candy_rankings (candy characteristics and popularity)
#            hate_crimes (hate crimes in US states, 2010-2015)
#            mad_men (tv performers and their post-show career), 
#            ncaa_w_bball_tourney (women's NCAA div 1 basketball tournament, 1982-2018), 
#         or nfl_suspensions (NFL suspensions, 1946-2014)
#      these five datasets are part of the fivethirtyeight package
#      and their variable definitions are included in pdfs posted to the Moodle course page