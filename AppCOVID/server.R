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
source("./pages/map.R")
library(shiny)
library(leaflet)
library(RColorBrewer)
library(xts)
library(rgdal)
library(DT)
 
 
  
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
  
  ##########################
  # Internacionalización
  #######################
  observeEvent(input$selected_language, {
    req(input$selected_language)
    update_lang(session, input$selected_language)
    globalUpdate_var()
  })
  #############
  ##
  # Contenido dinámico a traducir
  ###########
  globalUpdate_var <- reactive ({
      load_Data (input, output,session)
     
  })
  output$globalUpdate <- renderText({i18n$t(globalUpdate_var()) })
  
  ###################################
  ### VISUALIZACIÓN DE DATOS#########
  ###################################
  ############
  ## Actualizamos los datos al entrar a la app
  #######
  output$casosCoronaWorld <- renderValueBox({
    req(global_COvidDataWDMeterWorld)
    case <- global_COvidDataWDMeterWorld$TotalCases
     paste(case,i18n$t("Casos Totales"),sep="")
  })
  output$casosFallecidosWorld <- renderValueBox({
    req(global_COvidDataWDMeterWorld)
    case <- global_COvidDataWDMeterWorld$TotalDeaths
    paste(case,i18n$t("Fallecidos Totales"),case,sep="")
  })
  output$casosRecuperadosWorld <- renderValueBox({
    req(global_COvidDataWDMeterWorld)
    case <- global_COvidDataWDMeterWorld$TotalRecovered
    paste(case,i18n$t("Recuperados Totales"),case,sep="")
  })
  
  ###########
  #
  # DATOS GLOBALES
  #
  ###########
  #################################
  ###
  ## MApa Global con la serie de tiempo
  ####################################
  #Creamos el mapa con la serie temporal de evolución de la pandemia en el mundo.
  output$overview_map <- renderLeaflet(map)
  
  
  #Datos de seguimiento de la pandemia
  output$box_keyFigures <- renderUI(box(
    title = paste0("Key Figures (", strftime(input$timeSlider, format = "%d.%m.%Y"), ")"),
    fluidRow(
      column(
        valueBoxOutput("valueBox_confirmed", width = 3),
        valueBoxOutput("valueBox_recovered", width = 3),
        valueBoxOutput("valueBox_deceased", width = 3),
        valueBoxOutput("valueBox_countries", width = 3),
        width = 12,
        style = "margin-left: -20px"
      )
    ),
    div("Last updated: ", strftime(changed_date, format = "%d.%m.%Y - %R %Z")),
    width = 12
  ))
  key_figures <- reactive({
    data           <- sumData(input$timeSlider)
    data_yesterday <- sumData(input$timeSlider - 1)
    
    data_new <- list(
      new_confirmed = (data$confirmed - data_yesterday$confirmed) / data_yesterday$confirmed * 100,
      new_recovered = (data$recovered - data_yesterday$recovered) / data_yesterday$recovered * 100,
      new_deceased  = (data$deceased - data_yesterday$deceased) / data_yesterday$deceased * 100,
      new_countries = data$countries - data_yesterday$countries
    )
    
    keyFigures <- list(
      "confirmed" = HTML(paste(format(data$confirmed, big.mark = " "), sprintf("<h4>(%+.1f %%)</h4>", data_new$new_confirmed))),
      "recovered" = HTML(paste(format(data$recovered, big.mark = " "), sprintf("<h4>(%+.1f %%)</h4>", data_new$new_recovered))),
      "deceased"  = HTML(paste(format(data$deceased, big.mark = " "), sprintf("<h4>(%+.1f %%)</h4>", data_new$new_deceased))),
      "countries" = HTML(paste(format(data$countries, big.mark = " "), "/ 195", sprintf("<h4>(%+d)</h4>", data_new$new_countries)))
    )
    return(keyFigures)
  })
  
  output$valueBox_confirmed <- renderValueBox({
    shinydashboard::valueBox(
      key_figures()$confirmed,
      subtitle = "Confirmed",
      icon     = icon("file-medical"),
      color    = "light-blue",
      width    = NULL
    )
  })
  
  
  output$valueBox_recovered <- renderValueBox({
    shinydashboard::valueBox(
      key_figures()$recovered,
      subtitle = "Estimated Recoveries",
      icon     = icon("heart"),
      color    = "light-blue"
    )
  })
  
  output$valueBox_deceased <- renderValueBox({
    shinydashboard::valueBox(
      key_figures()$deceased,
      subtitle = "Deceased",
      icon     = icon("heartbeat"),
      color    = "light-blue"
    )
  })
  
  output$valueBox_countries <- renderValueBox({
    shinydashboard::valueBox(
      key_figures()$countries,
      subtitle = "Affected Countries",
      icon     = icon("flag"),
      color    = "light-blue"
    )
  })
  
  
  output$summaryTables <- renderUI({
    tabBox(
      tabPanel("Country/Region",
               div(
                 dataTableOutput("summaryDT_country"),
                 style = "margin-top: -10px")
      ),
      tabPanel("Province/State",
               div(
                 dataTableOutput("summaryDT_state"),
                 style = "margin-top: -10px"
               )
      ),
      width = 12
    )
  })
  
  output$summaryDT_country <- renderDataTable(getSummaryDT(data_atDate(current_date), "Country/Region", selectable = TRUE))
  proxy_summaryDT_country  <- dataTableProxy("summaryDT_country")
  output$summaryDT_state   <- renderDataTable(getSummaryDT(data_atDate(current_date), "Province/State", selectable = TRUE))
  proxy_summaryDT_state    <- dataTableProxy("summaryDT_state")
  
  observeEvent(input$timeSlider, {
    data <- data_atDate(input$timeSlider)
    replaceData(proxy_summaryDT_country, summariseData(data, "Country/Region"), rownames = FALSE)
    replaceData(proxy_summaryDT_state, summariseData(data, "Province/State"), rownames = FALSE)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$summaryDT_country_row_last_clicked, {
    selectedRow     <- input$summaryDT_country_row_last_clicked
    selectedCountry <- summariseData(data_atDate(input$timeSlider), "Country/Region")[selectedRow, "Country/Region"]
    location        <- data_evolution %>%
      distinct(`Country/Region`, Lat, Long) %>%
      filter(`Country/Region` == selectedCountry) %>%
      summarise(
        Lat  = mean(Lat),
        Long = mean(Long)
      )
    leafletProxy("overview_map") %>%
      setView(lng = location$Long, lat = location$Lat, zoom = 4)
  })
  
  observeEvent(input$summaryDT_state_row_last_clicked, {
    selectedRow     <- input$summaryDT_state_row_last_clicked
    selectedCountry <- summariseData(data_atDate(input$timeSlider), "Province/State")[selectedRow, "Province/State"]
    location <- data_evolution %>%
      distinct(`Province/State`, Lat, Long) %>%
      filter(`Province/State` == selectedCountry) %>%
      summarise(
        Lat  = mean(Lat),
        Long = mean(Long)
      )
    leafletProxy("overview_map") %>%
      setView(lng = location$Long, lat = location$Lat, zoom = 4)
  })
  
  summariseData <- function(df, groupBy) {
    df %>%
      group_by(!!sym(groupBy)) %>%
      summarise(
        "Confirmed"            = sum(confirmed, na.rm = T),
        "Estimated Recoveries" = sum(recovered, na.rm = T),
        "Deceased"             = sum(deceased, na.rm = T),
        "Active"               = sum(active, na.rm = T)
      ) %>%
      as.data.frame()
  }
  
  getSummaryDT <- function(data, groupBy, selectable = FALSE) {
    datatable(
      na.omit(summariseData(data, groupBy)),
      rownames  = FALSE,
      options   = list(
        order          = list(1, "desc"),
        scrollX        = TRUE,
        scrollY        = "37vh",
        scrollCollapse = T,
        dom            = 'ft',
        paging         = FALSE
      ),
      selection = ifelse(selectable, "single", "none")
    )
  }
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
  # RECOMENDACIONES DE LA OMS (CARGAMOS LOS POSTERS almacenados en local y bajados desde su web)
  #
  ###########
  
 output$tabsBoxRecomendaOMS <- renderUI({
 
    mytabs <- load_filesRecomOMS ()
    if(!is.null(mytabs)) {
      do.call(tabBox, args = c(width = 1024, mytabs))
    } else {
      box(h3("No existen recomendaiones de la OMS en estos momentos."))
    }
  }) 

  ###################################
  ### Descarga y actualización de datos
  ##################################
  ###############
  # Descarga y actualiza los datos internacionales sobre el COVID-19
  ##############
  output$TextDataGlobalUpdate <- renderText({ paste (download_filesCSVOMS (input, output,session),donwload_scrapingWorldometers(input, output,session),sep='\n') })
 
   ###############
  # Descarga y actualiza los datos a Nivel Nacional sobre el COVID-19
  ##############
   
  ###############
  #Descarga las recomendaciones de la OMS sobre el COVID-19
  ###############
  output$tabsRecomendaOMS <- renderText({
    images <- donwload_scrapingOMS (input, output,session)
    l   <- length(images)
    if(l>0)
      paste0("Se ha actualizado las recomendaciones de la OMS sobre el COVID-19!!.","\n")
    else
      paste0("No existen recomendaiones actualmente sobre el COVID-19."," \n")
    
  }) 
  ###########################################################################
 
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
