library(ggmap)
library(maptools)
library(maps)
library(mapproj)
library(shiny)

mapWorld <- map_data("world")
mapOptions <- c("cylindrical","mercator","sinusoidal", "gnomonic")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Let's Play with Maps!"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("imthemap","Pick a coordinate system", mapOptions)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("mapPlot")
      )
   )
)


server <- function(input, output) {
   
  mapOutline <- reactive({
    ggplot(mapWorld, aes(x=long, y=lat, group=group))+
    geom_polygon(fill="white", color="black") 
    })
  
   output$mapPlot <- renderPlot({
     mapOutline()+coord_map(input$imthemap, xlim=c(-180,180), ylim=c(-60, 90))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

