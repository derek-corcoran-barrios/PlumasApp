
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Predictors"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      h3("Predicting occupancy in Plumas National Forest"),
      p("To explore how variables affect occupancy in different bat species in Plumas National Forest, use the sliders bellow."),
      p("Variables interact with each other, a specie that has low occupancy at a certain height and high occupancy at lower altitude might revert this if you change burn intensity"),
      sliderInput(inputId = "Distance.to.water",
                  label = "Distance to water (meters):",
                  min = 0,
                  max = 326,
                  value = 53),
      sliderInput(inputId = "Distance.to.road",
                  label = "Distance to road (meters):",
                  min = 0,
                  max = 2309,
                  value = 245),
      sliderInput(inputId = "Altitude",
                  label = "Altitude (meters):",
                  min = 648,
                  max = 2309,
                  value = 1982),
      sliderInput(inputId = "BC",
                  label = "Burn Intensity",
                  min = 0,
                  max = 5,
                  value = 3)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      downloadButton('downloadPlot', 'Download Plot'),
      dataTableOutput("visFun"),
      downloadButton('downloadData', 'Download Table')
    )
  )
))
