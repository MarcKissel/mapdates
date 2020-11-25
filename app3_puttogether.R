#TODO
# have year appear in the map?
#  dates?
#
#
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
        menuItem("When they were found", tabName = "date", icon = icon("wrench")),
        menuItem("background", tabName = "overview", icon = icon("info"))
        
        
      )
    ),
    dashboardBody(    #put info into each sidebar page
      tabItems(
        # Second tab content
        tabItem(tabName = "date",
                h2("Select a date range"),
                tags$style(type = "text/css", ".irs-grid-text { font-size: 12pt; }", "html, body {width:100%;height:100%; font-size: 14pt; }"), #note: font size trick from https://stackoverflow.com/questions/30086881/how-can-i-control-the-size-of-the-numbers-on-my-slider-in-shiny. might have clues for other parts of it
                leafletOutput("map", height = 900),
                absolutePanel(top = 60, right = 10, draggable = TRUE,
                              strong("Hominin finds by when they were found."),
                              br(),
                              strong("A simple attempt to show when"),
                              br(),
                              strong("and where hominin fossils were found"),
                              br(),
                              strong("updates coming soon including links for more info"),
                              br(),
                              "Much of this data was", a("scrapped from wikipedia", href= "https://en.wikipedia.org/wiki/List_of_human_evolution_fossils", target="_blank"),
                              br(),
                              "and some of it needs to be double-checked",
                              br(),
                              sliderInput("num2", "", value = 1900, min = 1823, max = 2000, step = 1, sep = "", animate = animationOptions(interval = 500, loop = TRUE))
                              ,
                              #selectInput("colors", "Color Scheme",
                              #            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                              #),
                              checkboxInput("legend", "Show legend", TRUE)
                )
        ),
        #Overview page Updating with tabboxes
        tabItem(tabName = "overview",
                h2("Overview"),
                fluidRow(
                  tabBox(title ="General information", width = 12,
                         tabPanel("Project overview",
                                  
                                  strong("Some site locations estimated based on available data. Please contact us with location updates.")
                                  
                         )#close first tab panel
                         ,
                         tabPanel("source",
                                  strong("data from:")
                                  
                                  
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