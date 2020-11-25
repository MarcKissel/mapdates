library(shiny)
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(shinydashboard)

power <- read_csv(file ="sampledata_mon.csv")
pal3 <- colorFactor(topo.colors(27),
                    domain  = power$species
)

ui <- 

dashboardPage(
  dashboardHeader(title = "hominin fossils"), #title of site
  dashboardSidebar(  #setup the sidebar
    sidebarMenu(
      menuItem("When they were found", tabName = "by_type", icon = icon("wrench")),
      menuItem("background", tabName = "overview", icon = icon("info"))
      
      
    )
  ),
  dashboardBody(    #put info into each sidebar page
    tabItems(
      # Second tab content
      tabItem(tabName = "by_type",
              h2("Search by type"),
              fluidRow(
                
                box(  #put here slider
                  title = "Dates", status = "primary", solidHeader = TRUE,
                  "Select a date range", br(),
                  "",
                  sliderInput("num2", "Number two", value = 1900, min = 1823, max = 2000, step = 1, sep = "", animate = animationOptions(interval = 200, loop = TRUE)),
                  checkboxInput("legend", "Show legend", TRUE)
                ))
                ,
              fluidRow(
                # box(
                #   title = "Map of sites with chosen artifact type. Note that some locations are estimated.", 
                #   status = "warning", solidHeader = TRUE,
                #   dataTableOutput("content")
                #   
                # ),
                box(width = 1000,
                  title = "Map of sites (note some locations estimated)", 
                  solidHeader = TRUE, status = "warning",
                  leafletOutput("map", width = 1000)
                  
                  
                )
              )
              
      ),
      #Overview page Updating with tabboxes
      tabItem(tabName = "overview",
              h2("Overview of project"),
              fluidRow(
                tabBox(title ="General information", width = 12,
                       tabPanel("Project overview",
                                
                                strong("Map of all sites in database. Some site locations estimated based on available data. Please contact us with location updates.")
                                
                       )#close first tab panel
                       ,
                       tabPanel("Funding",
                                strong("Funding for the project:"),
                                p("This project/publication was made possible through the support of a grant from the John Templeton Foundation. The opinions expressed in this site are those of the authors and do not necessarily reflect the views of the John Templeton Foundation."),
                                p("Thanks also to the University of Notre Dame")
                                
                       )#close second TabPanel
                ) # close tabbox
              )#close fluidrow
              
              
              
      )# close overview Tab
      
      
                       )#close second TabPanel
                ) # close tabbox
              )#close fluidrow
              
              
              








server <- function(input, output, session) {
  filteredData <- reactive({
    power %>% filter(year <= input$num2)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(power) %>% addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
  
  observe({
    #pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      addProviderTiles(providers$Stamen.TonerLite) %>% 
      clearShapes() %>%
      addCircles(radius = 10,weight = 20, 
                 color = ~pal3(species), opacity = .7 , fillOpacity = .5 , popup = ~paste("<style> div.leaflet-popup-content {font-size: 15pt;}</style>",
                                                                                          "year:", year, "<br>",
                                                                                          "name:", name, 
                                                                                          "<br>",
                                                                                         "species", species,
                                                                                          "<br>",
                                                                                          "link for more info: coming soon" )
      )
  })
  
  
  observe({
    proxy <- leafletProxy("map", data = power)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      #pal <- colorpal() #skipped
      proxy %>% addLegend(position = "bottomright",
                          pal = pal3, values = ~species
      )
    }
  })  
  
}

shinyApp(ui, server)