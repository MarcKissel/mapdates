#ideas
#color by species  so no longer worry about the pallate
# pal3 is colored via colorbrwer
# pal4 i set
# could play with the other lines to let user do it?

#look into how ot change popup text
#maye playaround with teh tag part type = "text/css"
# way i did it
#found code here https://stackoverflow.com/questions/29365749/how-to-control-popups-width-for-leaflet-features-popup-in-r
# eidte to this "<style> div.leaflet-popup-content {font-size: 20pt;}</style>"



#instad of color by speices could do it by age?
#harder maybe since would need dates but could be interesting




#also maybe redo this layout
#like in Wisdom db
#

#add option for speed of animomato
#add option for type of map?



#needed packages
library(shiny)
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(shinydashboard)
#read in data




power <- read_csv(file ="sampledata_mon.csv")

#power<-a
#power$link <- "<a href = 'https://humanorigins.si.edu/evidence/human-fossils/species/australopithecus-afarensis/'> afarensis </a>"


#pal3 <-  colorFactor(
#  palette = 'Paired',
#  domain = power$species
#)

pal3 <- colorFactor(topo.colors(27),
                    domain  = power$species
)


# shiny from site


ui <- bootstrapPage(title = "map",
                    tags$style(type = "text/css", ".irs-grid-text { font-size: 12pt; }", "html, body {width:100%;height:100%; font-size: 14pt; }"), #note: font size trick from https://stackoverflow.com/questions/30086881/how-can-i-control-the-size-of-the-numbers-on-my-slider-in-shiny. might have clues for other parts of it
                    leafletOutput("map", width = "100%", height = "100%"),
                    absolutePanel(top = 10, right = 10, draggable = TRUE,
                                  strong("Hominin finds by when they were found."),
                                  br(),
                                  "A simple attempt to show when",
                                  br(),
                                  "and where hominin fossils were found",
                                  br(),
                                  "updates coming soon including links for more info",
                                  br(),
                                  "Much of this data was", a("scrapped from wikipedia", href= "https://en.wikipedia.org/wiki/List_of_human_evolution_fossils", target="_blank"),
                                  br(),
                                  "and some of it needs to be double-checked",
                                  br(),
                                  sliderInput("num2", "Number two", value = 1900, min = 1823, max = 2000, step = 1, sep = "", animate = animationOptions(interval = 200, loop = TRUE))
                                  ,
                                  #selectInput("colors", "Color Scheme",
                                  #            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                                  #),
                                  checkboxInput("legend", "Show legend", TRUE)
                    )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    power %>% filter(year <= input$num2)
  })
  
  # # This reactive expression represents the palette function,
  # # which changes as the user makes selections in UI.
  # colorpal <- reactive({
  #     colorNumeric(input$colors, power$year)
  # })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(power) %>% addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
  
  # details on the map
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
  
  # Use a separate observer to recreate the legend as needed.
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