
## Script name: shiny FEISTY
##
##
## Authors: Daniel Ottmann 
## Email: daniel.ottmann.riera@gmail.com
##
## Date created: April 2022
## Last update:  July 2022
##
## ---------------------------
##
## Readme:
##
## This script runs a shiny app for FEISTY
## This R version of FEISTY is build on an earlier version of the model written in MATLAB https://github.com/Dvandenderen/Fish_foodwebs 
##
## ---------------------------


#####################################################################
# Source function:
source("../shiny_FEISTY/FEISTY_app_setup.R") 

#
# Define UI
#
ui <- fluidPage(
  
  # Application title
  titlePanel("FEISTY"),
  
  # Sidebar with on/off switch for species and a slider input for number of bins 
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Functional groups"),
      checkboxInput(inputId = "small_pel",
                    label = "Small pelagics", T),
      checkboxInput(inputId = "meso_pel", 
                    label = "Mesopelagic fish", T),
      checkboxInput(inputId = "large_pel", 
                    label = "Large pelagics", T),
      checkboxInput(inputId = "demersals", 
                    label = "Demersals fish", T),
      checkboxInput(inputId = "squid", 
                    label = "Squid", T),
      checkboxInput(inputId = "bathypelagics", 
                    label = "Bathypelagic fish", T),

      h4("Variables"),
      sliderInput(inputId = "prod",
                  label = "Productivity (1/yr):",
                  min = 0,
                  max = 200,
                  step = 5,
                  value = 50),
      
      sliderInput(inputId = "depth",
                  label = "Depth (m):",
                  min = 0,
                  max = 2500,
                  step = 10,
                  value = 100),
      
      sliderInput(inputId = "nstage",
                  label = "Live stages:",
                  min = 3,
                  max = 9,
                  step = 3,
                  value = 3),
      
      radioButtons("USEdll", 
                   h5("ODE solved by"),
                   choices = list("Fortran dll" = TRUE, "R" = FALSE),
                   selected = TRUE),
      
      h4("Cyte as:"),
      p("van Denderen P. D., Petrik, C. M., Stock, C. A., & Andersen, K. H. 2021. Emergent global biogeography of marine fish food webs. Glob Ecol Biogeogr, 30(9):1882-1834"),
      
    ),
    
    # Show plots
    mainPanel(
      tabsetPanel(tabPanel("Food web", plotOutput(outputId = "plotSimulation",
                                                  height = "400px", width = "700px"), 
                           plotOutput(outputId = "cw", height = "600px", width = "500px")),
                  tabPanel("Diet", plotOutput(outputId = "diet",
                                              height = "700px", width = "700px")))
    )
  )
)

#
# Define server logic
#
server <- function(input, output) {
  
  # Reactive functions:
  shiny_param <- reactive({
    shiny_params(input$depth, input$prod, input$nstage,
                 input$small_pel, input$meso_pel, input$large_pel, input$demersals, input$squid, input$bathypelagics)
  })
 
  
  simulate <- reactive({
    feisty(shiny_param(),USEdll=input$USEdll)
  })
  
  # Get plots:
  # Network:
  output$plotSimulation <- renderPlot({
    plot_network(shiny_param(), simulate())
  })
  
  # Central weight
  output$cw <- renderPlot({
    plotFeistyf(shiny_param(), simulate())
  })
  
  # Diet:
  output$diet <- renderPlot({
    plotdiet(shiny_param(), simulate())
  })
  
}
#
# Run the application 
#
shinyApp(ui = ui, server = server)


#                                          END OF SCRIPT
#############################################################################################################