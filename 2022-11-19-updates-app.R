
#TODO
# have year appear in the map?
#  dates?
#    for date
#
library(shiny)
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(shinydashboard)



hominin <- read_csv("2022-11-19-extra-data.csv") # this is for exploring data. Also might be better for map if i add in the stuff i have been fixing? but all it has really is CC data that might be wrong so maybe not...and instead stick with other data set and build from there?
hominin <- hominin %>% select(name:country,date2, Cranial_capacity)
species_list <- hominin %>% distinct(species)



power <- read_csv(file ="2022-11-19-map-data.csv") #this is for map?


pal3 <- colorFactor(topo.colors(27),
                    domain  = power$species
)




#eventually need tofix the above to  jsut one dataset?

ui <- 
  
  dashboardPage(
    dashboardHeader(title = "hominin fossils"), #title of site
    dashboardSidebar(  #setup the sidebar
      sidebarMenu(
        
        menuItem("When they were found", tabName = "date", icon = icon("map-location")),
        menuItem("background", tabName = "overview", icon = icon("info")),
        menuItem("explore the data", tabName = "explore", icon = icon("book-skull")),
        menuItem("look at the data", tabName = "look", icon = icon("skull"))
        
        
      )
    ),
    dashboardBody(    #put info into each sidebar page
      tabItems(
        # Second tab content
        tabItem(tabName = "date",
                h2("Select a date range by moving the slider"),
                #h4("T  a lot of data and linked info"),
                HTML("<h4>This is a work in progress and still missing <b>a lot of</b> data and linked info.</h4>"),
                h4("The locations of many of these sites was estimated and need to be compared to other sources"),
                h4("Note: Much of this data was", a("scrapped from wikipedia", href= "https://en.wikipedia.org/wiki/List_of_human_evolution_fossils", target="_blank")),
                tags$style(".well {background-color:#003399 ;}",   type = "text/css", ".irs-grid-text { font-size: 12pt; }", "html, body {width:100%;height:100%; font-size: 14pt; }" ), #note: font size trick from https://stackoverflow.com/questions/30086881/how-can-i-control-the-size-of-the-numbers-on-my-slider-in-shiny. might have clues for other parts of it
                leafletOutput("map", height = 900),
                
                # strong("A simple attempt to show when"),
                # br(),
                # strong("and where hominin fossils were found"),
                # br(),
                # strong("updates coming soon including links for more info"),
                # br(),
                
                absolutePanel(top = 60, right = 10, draggable = TRUE,
                              
                              
                              sliderInput("num2", "", value = 1830, min = 1823, max = 2021, step = 1, sep = "") #note anim option sets the animation info which can cause lag here
                              #, animate = animationOptions(interval = 1000, loop = FALSE)
                              
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
                                  h4("hello! This is a work in progress attempt to show when different fossils were found "),
                                  strong("Some site locations estimated based on available data. Please contact us with location updates.")
                                  
                         )#close first tab panel
                         ,
                         tabPanel("source",
                                  strong("data from:")
                                  
                                  
                         )#close second TabPanel
                  ) # close tabbox
                )#close fluidrow
                
                
                
        ), # close overview Tab
        tabItem(tabName = "explore",
                h2("explore the data here"),
                fluidRow(
                  box(title = "data table",
                      dataTableOutput("date_table2")
                  )
                )#close fluidrow
                
                
                
        ),# close explore Tab
        tabItem(tabName = "look",
                h2("explore the data interactively here"),
                fluidRow(
                  
                  selectInput("species_choice", "choose hominin", choices = species_list, multiple = T),
                  plotOutput("CC_plot", brush = "plot_brush"),
                  tableOutput("data_cc_scatter")
                  
                  
                )#close fluidrow
                
                
                
                
                
        )#close second TabPanel
      ) # close tabbox
    )#close fluidrow
  ) #close UI










server <- function(input, output, session) {
  filteredData <- reactive({
    power %>% filter(year <= input$num2)
  })
  
  # output$homtable <- renderDataTable(filteredData) 
  
  # output$date_table2 <- renderDataTable({ 
  #   power %>% filter(year <= input$num2)  })
  
  output$date_table2 <- renderDataTable(      #this is for  explore the data...not working with search
    power, options = list(searching = FALSE) )
  
  # output$date_table2 <- renderDataTable(
  #   power,  options = list(pageLength = 5)   )
  
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(power) %>% addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })   #maybe reset the bounds?
  
  #
  #for visual explore data points
  #
  
  output$CC_plot <- renderPlot({
    
    req(input$species_choice)
    
    hominin %>% filter(species %in% input$species_choice) %>% ggplot(aes(x=date2, y = Cranial_capacity, color = species)) + geom_point(aes(size =2))
    
  })
  
  output$data_cc_scatter <- renderTable({
    req(input$species_choice)
    
    brushedPoints(hominin, input$plot_brush) })
  
  
  observe({
    #pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      addProviderTiles(providers$Stamen.TonerLite) %>% 
      clearShapes() %>%
      addCircles(radius = 10,weight = 20, 
                 color = ~pal3(species), opacity = .7 , fillOpacity = .5 , 
                 popup = ~paste("<style> div.leaflet-popup-content {font-size: 15pt;}</style>", #info on the popup goes here
                                                                                          "year:", year, "<br>",
                                                                                          "name:", name, 
                                                                                          "<br>",
                                                                                          "species", species,
                                                                                          "<br>",
                                                                                          
                                                                                          link,
                                                                                          "<br>",
                                                                                          paste0("<img src = ", image, " width = 300>"))
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