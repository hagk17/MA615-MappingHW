#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggmap)
library(maptools)
library(MAP)
library(magrittr)
library(forcats)
library(plyr)
library(leaflet)
library(leaflet.extras)

#################Read the data in#################
data <- read_csv("Vision_Zero_Entry.csv")

################# Initial Cleaning #################

#remove the GlobalId (contains only NA) and comments (qualitative data that I can't do much with), 
#STREETSEGID (all 0) and STATUS (all unassigned)
data %<>% select (-c(GLOBALID, COMMENTS, STREETSEGID, STATUS))


#removes any observations that have images(?) as their values in the REQUESTTYPE variable 
#there's 9693 observations so I feel comfortable deleting those observations 
allVals = unique(data$REQUESTTYPE)
keepVals = allVals[1:21]
data %<>% filter(REQUESTTYPE %in% keepVals)


for(i in seq_along(data$REQUESTTYPE)){
  if(data$REQUESTTYPE[i] == "it’s hard to see / low visibility" | data$REQUESTTYPE[i] == "it’s hard for people to see each other"){
    data$REQUESTTYPE[i] = "low visibility/it's hard to see others"
  }
  
  if(data$REQUESTTYPE[i] == "sidewalks/ramps don't exist or need improvement" | data$REQUESTTYPE[i] == "there are no sidewalks or they need maintenance"){
    data$REQUESTTYPE[i] = "sidewalks/ramps don't exist or need improvement/maintenance"
  }
  
  if(data$REQUESTTYPE[i] == "the roadway surface needs improvement" | data$REQUESTTYPE[i] == "the roadway surface needs maintenance"){
    data$REQUESTTYPE[i] = "the roadway surface needs improvement/maintenance"
  }
  
  if(data$REQUESTTYPE[i] == "people are not given enough time to cross the street" | data$REQUESTTYPE[i] == "there's not enough time to cross the street"){
    data$REQUESTTYPE[i] = "not enough time to cross the street"
  }
  
  if(data$REQUESTTYPE[i] == "the wait for the \"Walk\" signal is too long" | data$REQUESTTYPE[i] == "people have to wait too long for the \"Walk\" signal"){
    data$REQUESTTYPE[i] = "the wait for the walk signal is too long"
  }
}


################# SHINY APP #################
x <- unique(data$REQUESTTYPE)
requestTypeOptions <- c("All of them!", x)

ui <- fluidPage(
   
   titlePanel("Vision Zero Entry Data"),
   
   sidebarLayout(
      sidebarPanel(
        selectInput("option","What type of request would you like to explore?", requestTypeOptions),
        textOutput("some")
      ),
      
      mainPanel(leafletOutput("mapPlot"))
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  summation <- reactive({if(input$option != "All of them!"){value = nrow(data)}})
  
  dataToUse <- reactive({
    if(input$option != "All of them!"){newData <-data %>% filter(REQUESTTYPE == input$option)} 
    else{newData <- data}
    return(newData)
  })
  
  summation <- reactive({
    if(input$option == "All of them!"){return(nrow(data))}
    else{return(nrow(dataToUse()))}
  })
  
  output$some <- renderText({c("Total number: ", summation())})
   
   output$mapPlot <- renderLeaflet({
     leaflet(dataToUse())%>%
       setView(lng = -71.1, lat = 42.32, zoom = 11.5)%>%
       addTiles()%>%
       addCircles(data = dataToUse(), lat = ~ Y, lng = ~ X, weight = 1, 
                  radius = 30, popup = ~as.character(REQUESTTYPE))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

