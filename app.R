#ideas
#color by species  so no longer worry about the pallate
# pal3 is colored via colorbrwer
# pal4 i set
# could play with the other lines to let user do it?

#look into how ot change popup text


#needed packages
library(shiny)
library(leaflet)
library(RColorBrewer)
library(tidyverse)
#setup fake data




a <- read_csv(file ="sampledata_mon.csv") %>% filter(!is.na(name))

power<-a
power$link <- "<a href = 'https://humanorigins.si.edu/evidence/human-fossils/species/australopithecus-afarensis/'> afarensis </a>"

pal3 <-  colorFactor(
    palette = 'Dark2',
    domain = a$species
)




# shiny from site


ui <- bootstrapPage(
    tags$style(type = "text/css", ".irs-grid-text { font-size: 20pt; }", "html, body {width:100%;height:100%}"), #note: font size trick from https://stackoverflow.com/questions/30086881/how-can-i-control-the-size-of-the-numbers-on-my-slider-in-shiny. might have clues for other parts of it
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(top = 10, right = 10, draggable = TRUE,
                  "text goes here",
                  br(),
                  "and here",
                  sliderInput("range", "Year range", min(power$year), max(power$year),
                              value = range(power$year), step = 1, sep = "", animate = animationOptions(interval = 300, loop = TRUE)
                  ),
                  #selectInput("colors", "Color Scheme",
                  #            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                  #),
                  checkboxInput("legend", "Show legend", TRUE)
    )
)

server <- function(input, output, session) {
    
    # Reactive expression for the data subsetted to what the user selected
    filteredData <- reactive({
        power[power$year >= input$range[1] & power$year <= input$range[2],]
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
            clearShapes() %>%
            addCircles(radius = 10,weight = 20, 
                       color = ~pal3(species), fillOpacity = 0.7 , popup = ~paste("year:", year, "<br>",
                                                                                 "name:", name, 
                                                                                 "<br>",
                                                                                 "species", species,
                                                                                 "<br>",
                                                                                 "link for more info:", link)
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