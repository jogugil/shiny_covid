#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(ggplot2)
library(shiny)
library(shinydashboard)
source("./helpers/global.R") 
source("./helpers/helpers.R")




# Define server logic required to draw a histogram

compute_data <- function(updateProgress = NULL) {
  # Create 0-row data frame which will be used to store data
  dat <- data.frame(x = numeric(0), y = numeric(0))
  
  for (i in 1:10) {
    Sys.sleep(0.25)
    
    # Compute new row of data
    new_row <- data.frame(x = rnorm(1), y = rnorm(1))
    
    # If we were passed a progress update function, call it
    if (is.function(updateProgress)) {
      text <- paste0("x:", round(new_row$x, 2), " y:", round(new_row$y, 2))
      updateProgress(detail = text)
    }
    
    # Add the new row of data
    dat <- rbind(dat, new_row)
  }
  
  dat
}
server_table <- function (input,output,session) {
  style <- isolate(input$style)
  
  # Create a Progress object
  progress <- shiny::Progress$new(style = style)
  progress$set(message = "Computing data", value = 0)
  # Close the progress when this reactive exits (even if there's an error)
  on.exit(progress$close())
  
  # Create a closure to update progress.
  # Each time this is called:
  # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
  #   distance. If non-NULL, it will set the progress to that value.
  # - It also accepts optional detail text.
  updateProgress <- function(value = NULL, detail = NULL) {
    if (is.null(value)) {
      value <- progress$getValue()
      value <- value + (progress$getMax() - value) / 5
    }
    progress$set(value = value, detail = detail)
  }
  
  # Compute the new data, and pass in the updateProgress function so
  # that it can update the progress indicator.
  compute_data(updateProgress)
}

shinyServer(function(input, output,session) {
  
  
  ###################################
  ### VISUALIZACIÓN DE DATOS#########
  ###################################
  
  
  ###########
  #
  # DATOS GLOBALES
  #
  ###########
  
  ###########
  #
  # DATOS de ESPAÑA
  #
  ###########
  
  ###########
  #
  # DATOS de la COmunidad Valenciana
  #
  ###########
  
  ###########
  #
  # RECOMENDACIONES DE LA OMS (CARGAMOS LOS POSTERS al macenados en local y bajados desde su web)
  #
  ###########
  
 output$tabsBoxRecomendaOMS <- renderUI({
   printApp(" ENTRO EN output$tabsBoxRecomendaOMS")
    images <- load_filesRecomOMS (input, output,session)
    l   <- length(images)
    myTabs <- load_filesRecomOMS ()
    if (!is.null(myTabs))
      do.call(tabBox, args = c(width = 250,title="Recomendaciones de la OMS sobre el COVID 19", myTabs))
    else
      Box(h3("No existen recomendaiones de la OMS en estos momentos."))
    printApp(" SALGO DE output$tabsBoxRecomendaOMS")
  }) 

  ###################################
  ### Descarga y actualización de datos
  ##################################
  
  output$TextDataGlobalUpdate <- renderText({ paste (download_filesCSVOMS (input, output,session),donwload_scrapingWorldometers(input, output,session),sep='\n') })
  # the progress API.
  output$plot <- renderPlot({
    if (DEBUG) print(paste0("Entramos en el progreso plot"))
    
    style <- isolate(input$style)
    data(cars)
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
  
  output$tabsRecomendaOMS <- renderUI({
    images <- donwload_scrapingOMS (input, output,session)
    l   <- length(images)
    myTabs <- lapply(1:l, function(i) {
      label <- paste("R",i," ")
      label <- str_trim(label)
      tabPanel(title = label,h1(images[[i]]$title), fluidRow(tags$img(src =images[[i]]$href)) )
    })
    do.call(tabBox, args = c(width = 250,title="Recomendaciones de la OMS sobre el COVID 19", myTabs))
  
  }) 
  
  # This example uses the Progress object API directly. This is useful because
  # calls an external function to do the computation.
  output$table <- DT::renderDataTable({
    if (DEBUG) print(paste("Dentro de output"))
    server_table (input, output,session)
  })
  output$table1 <- DT::renderDataTable({
    if (DEBUG) print(paste("Dentro de output"))
    server_table (input, output,session)
  })
  output$table2 <- DT::renderDataTable({
    if (DEBUG) print(paste("Dentro de output"))
    server_table (input, output,session)
  })
  output$table3 <- DT::renderDataTable({
    if (DEBUG) print(paste("Dentro de output"))
    server_table (input, output,session)
  })
  output$table4 <- DT::renderDataTable({
    if (DEBUG) print(paste("Dentro de output"))
    server_table (input, output,session)
  })
  output$table5 <- DT::renderDataTable({
    if (DEBUG) print(paste("Dentro de output"))
    server_table (input, output,session)
  })
  output$table6 <- DT::renderDataTable({
    if (DEBUG) print(paste("Dentro de output"))
    server_table (input, output,session)
  })
  #output es una lista que "anota" qué es lo que tiene que mostrar. En este caso
  # le decimos que tieene que hacer un histograma
  output$HistPlot <- renderPlot({
    if (DEBUG) print(paste("Dentro de HistPlot"))
    # faithful es un dataset que viene precargado en R.
    datos  <- data.frame(faithful$waiting)
    # lo que hace es discretizar a los waiting times entre erupción según la cantidad
    # de bins que el usuario elija. inputs es una lista que "anota" lo que le manda 
    # el usuario
    ggplot(datos) +
      geom_histogram(aes(x=faithful.waiting),fill="#75AADB",bins=input$bins) +
      labs(title=isolate({input$titulo}),
           x="Tiempo de espera hasta la próxima erupción (minutos)")
  })
  output$HistPlot2 <- renderPlot({
    if (DEBUG) print(paste("Dentro de HistPlot"))
    # faithful es un dataset que viene precargado en R.
    datos  <- data.frame(faithful$waiting)
    # lo que hace es discretizar a los waiting times entre erupción según la cantidad
    # de bins que el usuario elija. inputs es una lista que "anota" lo que le manda 
    # el usuario
    ggplot(datos) +
      geom_histogram(aes(x=faithful.waiting),fill="#75AADB",bins=input$bins) +
      labs(title=isolate({input$titulo}),
           x="Tiempo de espera hasta la próxima erupción (minutos)")
  })
  output$HistPlot3 <- renderPlot({
    if (DEBUG) print(paste("Dentro de HistPlot"))
    # faithful es un dataset que viene precargado en R.
    datos  <- data.frame(faithful$waiting)
    # lo que hace es discretizar a los waiting times entre erupción según la cantidad
    # de bins que el usuario elija. inputs es una lista que "anota" lo que le manda 
    # el usuario
    ggplot(datos) +
      geom_histogram(aes(x=faithful.waiting),fill="#75AADB",bins=input$bins) +
      labs(title=isolate({input$titulo}),
           x="Tiempo de espera hasta la próxima erupción (minutos)")
  })
  output$HistPlot4 <- renderPlot({
    if (DEBUG) print(paste("Dentro de HistPlot"))
    # faithful es un dataset que viene precargado en R.
    datos  <- data.frame(faithful$waiting)
    # lo que hace es discretizar a los waiting times entre erupción según la cantidad
    # de bins que el usuario elija. inputs es una lista que "anota" lo que le manda 
    # el usuario
    ggplot(datos) +
      geom_histogram(aes(x=faithful.waiting),fill="#75AADB",bins=input$bins) +
      labs(title=isolate({input$titulo}),
           x="Tiempo de espera hasta la próxima erupción (minutos)")
  })
  output$DensPlot <- renderPlot({
    if (DEBUG) print(paste("Dentro de renderplot0"))
    # faithful es un dataset que viene precargado en R.
    datos  <- data.frame(faithful$waiting)
    # lo que hace es discretizar a los waiting times entre erupción según la cantidad
    # de bins que el usuario elija. inputs es una lista que "anota" lo que le manda 
    # el usuario
    ggplot(datos) +
      geom_density(aes(x=faithful.waiting),fill="#75AADB") +
      labs(title=isolate({input$titulo}),
           x="Tiempo de espera hasta la próxima erupción (minutos)")
  })
     
})
