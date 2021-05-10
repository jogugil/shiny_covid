
body_global <-   fluidRow(
       
                    tags$head(
                      tags$style(type = "text/css", "#global_map {height: 50vh !important;}"),
                      tags$style(type = 'text/css', ".slider-animate-button { font-size: 20pt !important; }"),
                      tags$style(type = 'text/css', ".slider-animate-container { text-align: left !important; }"),
                      tags$style(type = "text/css", "@media (max-width: 991px) { .details { display: flex; flex-direction: column; } }"),
                      tags$style(type = "text/css", "@media (max-width: 991px) { .details .map { order: 1; width: 100%; } }"),
                      tags$style(type = "text/css", "@media (max-width: 991px) { .details .summary { order: 3; width: 100%; } }"),
                      tags$style(type = "text/css", "@media (max-width: 991px) { .details .slider { order: 2; width: 100%; } }")
                    ),
                    
                   
                             fluidRow(
                                     uiOutput("box_keyFigures")
                              ),
                              fluidRow(
                                   box(class = "map",
                                         width =12,
                                         style = 'padding:0px;',
                                        leafletOutput("global_map")
                                     )
                                ),
                                fluidRow(
                                    box( 
                                        sliderInput(
                                          "timeSlider",
                                          label      = "Select date",
                                          min        = min(data_evolution$date),
                                          max        = max(data_evolution$date),
                                          value      = max(data_evolution$date),
                                          width      = "100%",
                                          timeFormat = "%d.%m.%Y",
                                          animate    = animationOptions(loop = TRUE)
                                        ),
                                        class = "slider",
                                        width = 12,
                                        style = 'padding-left:15px; padding-right:15px;'
                                    )
                             
                                ),  
                                  fluidRow(       
                                    box(width = 12,
                                           class = "summary",
                                           style = 'padding:0px;',
                                           uiOutput("summaryTables"),
                                           
                                    )
                                ),  
                                    fluidRow(       
                                        box(width = 12,
                                        title = "Evolution of Doubling Times per Country",
                                        plotlyOutput("timeLinePlot"),
                                        fluidRow(
                                          column(
                                            uiOutput("selectize_doublingTime_Country"),
                                            width = 3,
                                          ),
                                          column(
                                            uiOutput("selectize_doublingTime_Variable"),
                                            width = 3,
                                          ),
                                          column(width = 3),
                                          column(
                                            div("Note: The doubling time is calculated based on the growth rate over the last seven days.",
                                                style = "padding-top: 15px;"),
                                            width = 3
                                          )
                                        )
                                      )
                                    )
              
)

page_globalJH <- fluidPage (
    tags$title(i18n$t("Serie Temporal Johns Hopkins University")),
    body_global
  
)