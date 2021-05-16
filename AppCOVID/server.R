#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(DT)
library(rgdal) 
library(htmltools)
 
library(scales)
library(leaflet)
library(ggplot2)



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
  
  ###########
  #
  # DATOS GLOBALES
  #
  ###########
  #################################
  ###
  ## Mapa Global con la serie de tiempo
  ####################################
  #Creamos el mapa con la serie temporal de evolución de la pandemia en el mundo.
  output$global_map <- renderLeaflet(
    map
  )
  observe({
    data <- data_atDate(input$timeSlider)
  })
  observe({
    req(input$timeSlider, input$global_map_zoom)
    zoomLevel               <- input$global_map_zoom
    data                    <- data_atDate(input$timeSlider) %>% addLabel()
    data$confirmedPerCapita <- data$confirmed / data$population * 100000
    data$activePerCapita    <- data$active / data$population * 100000
    
    leafletProxy("global_map", data = data) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng          = ~Long,
        lat          = ~Lat,
        radius       = ~log(confirmed^(zoomLevel / 2)),
        stroke       = FALSE,
        fillOpacity  = 0.5,
        label        = ~label,
        labelOptions = labelOptions(textsize = 15),
        group        = "Confirmed"
      ) %>%
      addCircleMarkers(
        lng          = ~Long,
        lat          = ~Lat,
        radius       = ~log(confirmedPerCapita^(zoomLevel)),
        stroke       = FALSE,
        color        = "#00b3ff",
        fillOpacity  = 0.5,
        label        = ~label,
        labelOptions = labelOptions(textsize = 15),
        group        = "Confirmed (per capita)"
      ) %>%
      addCircleMarkers(
        lng          = ~Long,
        lat          = ~Lat,
        radius       = ~log(recovered^(zoomLevel)),
        stroke       = FALSE,
        color        = "#005900",
        fillOpacity  = 0.5,
        label        = ~label,
        labelOptions = labelOptions(textsize = 15),
        group = "Estimated Recoveries"
      ) %>%
      addCircleMarkers(
        lng          = ~Long,
        lat          = ~Lat,
        radius       = ~log(deceased^(zoomLevel)),
        stroke       = FALSE,
        color        = "#E7590B",
        fillOpacity  = 0.5,
        label        = ~label,
        labelOptions = labelOptions(textsize = 15),
        group        = "Deceased"
      ) %>%
      addCircleMarkers(
        lng          = ~Long,
        lat          = ~Lat,
        radius       = ~log(active^(zoomLevel / 2)),
        stroke       = FALSE,
        color        = "#f49e19",
        fillOpacity  = 0.5,
        label        = ~label,
        labelOptions = labelOptions(textsize = 15),
        group        = "Active"
      ) %>%
      addCircleMarkers(
        lng          = ~Long,
        lat          = ~Lat,
        radius       = ~log(activePerCapita^(zoomLevel)),
        stroke       = FALSE,
        color        = "#f4d519",
        fillOpacity  = 0.5,
        label        = ~label,
        labelOptions = labelOptions(textsize = 15),
        group        = "Active (per capita)"
      )
  })
  
  #################################
  ###
  #Datos principales de seguimiento de la pandemia
  #################################
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
  
  output$summaryDT_country <- renderDataTable(
          getSummaryDT(data_atDate(last_date), "Country/Region", selectable = TRUE)
  )
  
  proxy_summaryDT_country  <- dataTableProxy("summaryDT_country")
  
  output$summaryDT_state   <- renderDataTable(
    getSummaryDT(data_atDate(last_date), "Province/State", selectable = TRUE))
  
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
    leafletProxy("global_map") %>%
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
    leafletProxy("global_map") %>%
      setView(lng = location$Long, lat = location$Lat, zoom = 4)
  })
  
  
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
    valueBox(
      key_figures()$confirmed,
      subtitle = "Confirmed",
      icon     = icon("file-medical"),
      color    = "light-blue",
      width    = NULL
    )
  })
  
  
  output$valueBox_recovered <- renderValueBox({
    valueBox(
      key_figures()$recovered,
      subtitle = "Estimated Recoveries",
      icon     = icon("heart"),
      color    = "light-blue"
    )
  })
  
  output$valueBox_deceased <- renderValueBox({
    valueBox(
      key_figures()$deceased,
      subtitle = "Deceased",
      icon     = icon("heartbeat"),
      color    = "light-blue"
    )
  })
  
  output$valueBox_countries <- renderValueBox({
    valueBox(
      key_figures()$countries,
      subtitle = "Affected Countries",
      icon     = icon("flag"),
      color    = "light-blue"
    )
  })
  
  output$box_keyFigures <- renderUI({box(
    title = paste0("Key Figures (", strftime(input$timeSlider, format = "%d.%m.%Y"), ")"),
    fluidRow(
      column(
        valueBoxOutput("valueBox_confirmed", width = 6),
        valueBoxOutput("valueBox_recovered", width = 6),
        valueBoxOutput("valueBox_deceased", width = 6),
        valueBoxOutput("valueBox_countries", width = 6),
        width = 12,
        style = "margin-left: -20px"
      )
    ),
    div("Last updated: ", strftime(changed_date, format = "%d.%m.%Y - %R %Z")),
    width = 12
  )})
  
  ###############################
  ###
  # GRÁFICO CON PLOTLY LINETIME
  ##############################
  output$selectize_doublingTime_Country <- renderUI({
    selectizeInput(
      "selectize_doublingTime_Country",
      label    = "Select Countries",
      choices  = unique(data_evolution$`Country/Region`),
      selected = top5_countries,
      multiple = TRUE
    )
  })
  
  output$selectize_doublingTime_Variable <- renderUI({
    selectizeInput(
      "selectize_doublingTime_Variable",
      label    = "Select Variable",
      choices  = list("Confirmed" = "doublingTimeConfirmed", "Deceased" = "doublingTimeDeceased"),
      multiple = FALSE
    )
  })
  output$timeLinePlot <- renderPlotly({
    req(input$selectize_doublingTime_Country, input$selectize_doublingTime_Variable)
    daysGrowthRate <- 7
    data           <- data_evolution %>%
      pivot_wider(id_cols = c(`Province/State`, `Country/Region`, date, Lat, Long), names_from = var, values_from = value) %>%
      filter(if (input$selectize_doublingTime_Variable == "doublingTimeConfirmed") (confirmed >= 100) else (deceased >= 10)) %>%
      filter(if (is.null(input$selectize_doublingTime_Country)) TRUE else `Country/Region` %in% input$selectize_doublingTime_Country) %>%
      group_by(`Country/Region`, date) %>%
      dplyr::select(-recovered, -active) %>%
      summarise(
        confirmed = sum(confirmed, na.rm = T),
        deceased  = sum(deceased, na.rm = T)
      ) %>%
      arrange(date) %>%
      mutate(
        doublingTimeConfirmed = round(log(2) / log(1 + (((confirmed - lag(confirmed, daysGrowthRate)) / lag(confirmed, daysGrowthRate)) / daysGrowthRate)), 1),
        doublingTimeDeceased  = round(log(2) / log(1 + (((deceased - lag(deceased, daysGrowthRate)) / lag(deceased, daysGrowthRate)) / daysGrowthRate)), 1),
      ) %>%
      mutate("daysSince" = row_number()) %>%
      filter(!is.na(doublingTimeConfirmed) | !is.na(doublingTimeDeceased))
    
    p <- plot_ly(data = data, x = ~daysSince, y = data[[input$selectize_doublingTime_Variable]], color = ~`Country/Region`, type = 'scatter', mode = 'lines')
    
    if (input$selectize_doublingTime_Variable == "doublingTimeConfirmed") {
      p <- layout(p,
                  yaxis = list(title = "Doubling time of confirmed cases in days"),
                  xaxis = list(title = "# Days since 100th confirmed case")
      )
    } else {
      p <- layout(p,
                  yaxis = list(title = "Doubling time of deceased cases in days"),
                  xaxis = list(title = "# Days since 10th deceased case")
      )
    }
    
    return(p)
  })
  ##################
  ## DATOS GLOBALES OMS
  #  INDCIDENCIA ACUMULADA 14d
  ########
    output$selectize_cumulate_Variable <- renderUI({
      selectizeInput(
        "selectize_acumulate_Variable",
        label    = "Select Variable",
        choices  = list("Casos" = "casesCumulative", "Fallecimientos" = "deathsCumulative", "Reportados 24h" = "reported24h"),
        multiple = FALSE
      )
    })
    output$selectize_cumulate_type <- renderUI({
      selectizeInput(
        "selectize_acumulate_type",
        label    = "Select Type Transmission",
        choices  = unique(global_COvidDataOMS$transmisionType),
        selected = c("Community transmission","Not applicable","Clusters of cases","Sporadic cases","Pending","No cases"),
        multiple = TRUE
      )
      
    })
    
    dataOMS_var <- reactive ({
      #req(input$selectize_acumulate_type)
      update_dataOMS (input$selectize_acumulate_type)
    }) 
    mapOMS_var <- reactive ({
      req(input$selectize_acumulate_Variable,input$selectize_acumulate_type)
      load_mapOMS (input$selectize_acumulate_Variable,input$selectize_acumulate_type, input$acumulate_map_zoom)
       
    }) 
    output$acumulate_map <- renderLeaflet({
      mapOMS_var()
    })
       
   
    output$summaryDT_OMS <- renderDataTable(
      dataOMS_var() 
    )
    ##################
    ## Datos wordlMeter
    ##################
    output$valueBox_confirmedWDMETER <- renderValueBox({
      valueBox(
        global_COvidDataWDMeterWorld$TotalCases,
        subtitle = "Total Confirmed Worldwide",
        icon     = icon("file-medical"),
        color    = "light-blue",
        width    = NULL
      )
    })
    
    
    output$valueBox_recoveredWDMETER <- renderValueBox({
      valueBox(
        global_COvidDataWDMeterWorld$TotalRecovered,
        subtitle = "Total Recovered Worldwide",
        icon     = icon("heart"),
        color    = "light-blue"
      )
    })
    
    output$valueBox_deceasedWDMETER <- renderValueBox({
      valueBox(
        global_COvidDataWDMeterWorld$TotalDeaths,
        subtitle = "Total Deaths Worldwide",
        icon     = icon("heartbeat"),
        color    = "light-blue"
      )
    })
    
    output$valueBox_countriesWDMETER <- renderValueBox({
      valueBox(
        global_COvidDataWDMeterWorld$ActiveCases,
        subtitle = "Active Cases Worldwide",
        icon     = icon("flag"), 
        color    = "light-blue"
      )
    })
 
    
    output$box_globalDataWorld <- renderUI({box(
      title = paste0("Global Data Worldwide (", strftime(Sys.Date(), format = "%d.%m.%Y"), ")"),
      fluidRow(
        column(
          valueBoxOutput("valueBox_confirmedWDMETER", width = 6),
          valueBoxOutput("valueBox_recoveredWDMETER", width = 6),
          valueBoxOutput("valueBox_deceasedWDMETER", width = 6),
          valueBoxOutput("valueBox_countriesWDMETER", width = 6),
          width = 12,
          style = "margin-left: -20px"
        )
      ),
       width = 12
    )})
    
    output$globalCases_ContinentWDMETER <- renderPlot({
      ggplot(data_CONTINENTWDMETER,aes(x=2,y=prop_cont, fill=continent))+
        geom_bar(stat = "identity", color="white")+
        geom_text(aes(label=percent(prop_cont/100)),
                  position=position_stack(vjust=0.5),color="white",size=6)+
        coord_polar(theta = "y")+
        scale_fill_manual(values=c("salmon","steelblue","orange","gray","yellow","red"))+
        theme_void()+
        labs(title="% Incidence by Region")+
        xlim(0.5,2.5) 
      
    })
    output$globalDeatchs_ContinentWDMETER <- renderPlot({
      ggplot(data_CONTINENTWDMETER,aes(x=2,y=prop_deatchs, fill=continent))+
        geom_bar(stat = "identity", color="white")+
        geom_text(aes(label=percent(prop_deatchs/100)),
                  position=position_stack(vjust=0.5),color="white",size=6)+
        coord_polar(theta = "y")+
        scale_fill_manual(values=c("salmon","steelblue","orange","gray","yellow","red"))+
        theme_void()+
        labs(title="% Deatchs by Region")+
        xlim(0.5,2.5) 
      
    })
    output$globalRecovered_ContinentWDMETER <- renderPlot({
      ggplot(data_CONTINENTWDMETER,aes(x=2,y=prop_Recovered, fill=continent))+
        geom_bar(stat = "identity", color="white")+
        geom_text(aes(label=percent(prop_Recovered/100)),
                  position=position_stack(vjust=0.5),color="white",size=6)+
        coord_polar(theta = "y")+
        scale_fill_manual(values=c("salmon","steelblue","orange","gray","yellow","red"))+
        theme_void()+
        labs(title="% Recovered by Region")+
        xlim(0.5,2.5) 
      
    })
    output$globalActive_ContinentWDMETER <- renderPlot({
      ggplot(data_CONTINENTWDMETER,aes(x=2,y=prop_Active, fill=continent))+
        geom_bar(stat = "identity", color="white")+
        geom_text(aes(label=percent(prop_Active/100)),
                  position=position_stack(vjust=0.5),color="white",size=6)+
        coord_polar(theta = "y")+
        scale_fill_manual(values=c("salmon","steelblue","orange","gray","yellow","red"))+
        theme_void()+
        labs(title="% Tests by Region")+
        xlim(0.5,2.5) 
      
    })
    
  ############
  # DaTOS de ESPAÑA
  #
  ###########
  output$tbl=DT::renderDataTable(casos_cc)
  output$grafico_1<-renderPlot({casos_cc %>%  group_by(fecha) %>%  summarise( sum = sum(num_casos)) %>%
      ggplot() + aes(y=sum,x=fecha) + geom_col()+ xlab("Fecha") + ylab("Casos") +
      ggtitle("Casos en España por fecha") + theme(plot.title = element_text(hjust = 0.5))})
  output$grafico_2<-renderPlot({casos_cc %>%  group_by(ccaa_iso) %>%  summarise( sum = sum(num_casos)) %>%
      ggplot() + aes(y=sum,x=ccaa_iso,fill=ccaa_iso) + geom_col()+ xlab("Comunidades") + ylab("casos") +
      ggtitle("Casos totales por Comunidad") + theme(plot.title = element_text(hjust = 0.5))})
  output$grafico_3<-renderPlot({casos_cc %>%  group_by(fecha) %>%  summarise( sum = sum(num_casos_prueba_pcr)) %>%
      ggplot() + aes(y=sum,x=fecha) + geom_col()+ xlab("Fecha") + ylab("PCR realizadas") +
      ggtitle("Tests PCR realizados en España por fecha") + theme(plot.title = element_text(hjust = 0.5))})
  output$grafico_4<-renderPlot({casos_cc %>%  group_by(ccaa_iso) %>%  summarise( sum = sum(num_casos_prueba_pcr)) %>%
      ggplot() + aes(y=sum,x=ccaa_iso,fill=ccaa_iso) + geom_col()+ xlab("Comunidades") + ylab("PCR realizadas") +
      ggtitle("Test PCR totales realizados por cada comunidad") + theme(plot.title = element_text(hjust = 0.5))})
  
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
  output$dataGlobalUpdate <- renderText({ paste (download_filesCSVOMS (input, output,session),donwload_scrapingWorldometers(input, output,session),sep='\n') })
  observeEvent(input$dataGlobalUpdate, {
    paste (download_filesCSVOMS (input, output,session),donwload_scrapingWorldometers(input, output,session),sep='\n')
  })
  ###############
  # Descarga y actualiza los datos a Nivel Nacional sobre el COVID-19
  ##############
  
  output$dataSPUpdate<- renderText({download_SpdataCC (input, output,session)})
  observeEvent(input$dataSPUpdate, {
    download_SpdataCC (input, output,session)
  })
  ###############
  #Descarga las recomendaciones de la OMS sobre el COVID-19
  ###############
 
  
  output$tabsRecomendaOMS <- renderText({
    images <- donwload_scrapingOMS (input, output,session)
    l   <- length(images)
    if(l>0) {
      
      paste0("Se ha actualizado las recomendaciones de la OMS sobre el COVID-19!!.","\n")
    } else {
      paste0("No existen recomendaiones actualmente sobre el COVID-19."," \n")
    }
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
  
})
