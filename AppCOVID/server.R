#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shiny.i18n)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  ###################################
  ### Descarga y actualización de datos
  ##################################
  # the progress API.
  output$plot <- renderPlot({
    input$goPlot # Re-run when button is clicked
    
    style <- isolate(input$style)
    
    withProgress(message = 'Creating plot', style = style, value = 0.1, {
      Sys.sleep(0.25)
      
      # Create 0-row data frame which will be used to store data
      dat <- data.frame(x = numeric(0), y = numeric(0))
      
      # withProgress calls can be nested, in which case the nested text appears
      # below, and a second bar is shown.
      withProgress(message = 'Generating data', style = style, detail = "part 0", value = 0, {
        for (i in 1:10) {
          # Each time through the loop, add another row of data. This a stand-in
          # for a long-running computation.
          dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
          
          # Increment the progress bar, and update the detail text.
          incProgress(0.1, detail = paste("part", i))
          
          # Pause for 0.1 seconds to simulate a long computation.
          Sys.sleep(0.1)
        }
      })
      
      # Increment the top-level progress indicator
      incProgress(0.5)
      
      # Another nested progress indicator.
      # When value=NULL, progress text is displayed, but not a progress bar.
      withProgress(message = 'And this also', detail = "This other thing",
                   style = style, value = NULL, {
                     
                     Sys.sleep(0.75)
                   })
      
      # We could also increment the progress indicator like so:
      # incProgress(0.5)
      # but it's also possible to set the progress bar value directly to a
      # specific value:
      setProgress(1)
    })
    
    plot(cars$speed, cars$dist)
  })
  
    
     
})
