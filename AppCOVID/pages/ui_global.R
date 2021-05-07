body_global <- mainPanel(
  tags$head(
    tags$style(type = "text/css", "#overview_map {height: 50vh !important;}"),
    tags$style(type = 'text/css', ".slider-animate-button { font-size: 20pt !important; }"),
    tags$style(type = 'text/css', ".slider-animate-container { text-align: left !important; }"),
    tags$style(type = "text/css", "@media (max-width: 991px) { .details { display: flex; flex-direction: column; } }"),
    tags$style(type = "text/css", "@media (max-width: 991px) { .details .map { order: 1; width: 100%; } }"),
    tags$style(type = "text/css", "@media (max-width: 991px) { .details .summary { order: 3; width: 100%; } }"),
    tags$style(type = "text/css", "@media (max-width: 991px) { .details .slider { order: 2; width: 100%; } }")
  ),
  fluidRow(
    fluidRow(
      #uiOutput("box_keyFigures")
    ),
    fluidRow(
      column(4,valueBoxOutput("casosCoronaWorld")),    
      column(4,valueBoxOutput("casosFallecidosWorld")), 
      column(4,valueBoxOutput("casosRecuperadosWorld")) , 
    ),fluidRow(
      column(6,box( title = "Casos Activos", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,hr(),div(verbatimTextOutput("textActiveCase")))),    
      column(6,box( title = "Casos Cerrados", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,hr(),div(verbatimTextOutput("textClosedCase"))) ) 
    
    ),
    fluidRow(
      class = "details",
      column(
        box(
          width = 12,
          #leafletOutput("global_map1"),
          verbatimTextOutput("TextDataGlobalUpdate")
        ),
        class = "map",
        width = 8,
        style = 'padding:0px;'
      ),
      column(
        #uiOutput("summaryTables"),
        class = "summary",
        width = 4,
        style = 'padding:0px;'
      ),
      column(
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
    )
  )
)

 

page_global <- fluidPage(
  
  titlePanel(i18n$t("Datos Globales")),
  body_global
)