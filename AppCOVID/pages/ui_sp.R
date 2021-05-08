navbarPage(
  'España - Comunidades',
  tabPanel("Tabla de incidencia de cada comunidad",tabItem(
    fluidRow(
      column(width = 2,
             p("SON LOS DATOS DE ESPAÑA")
      ),
      column(width =2,
             # A static infoBox
             infoBox("New Orders", 10 * 2, icon = icon("world"))
      )
    ), 
    
    column(12,DT::dataTableOutput("tbl")),
    
  )),
  tabPanel("Grafica de casos",tabItem(
    fluidRow(
      column(width = 2,
             p("SON LOS DATOS DE ESPAÑA")
      ),
      column(width =2,
             # A static infoBox
             infoBox("New Orders", 10 * 2, icon = icon("world"))
      )
    ), 
    
    mainPanel(
      plotOutput("grafico_1")
    )
    
  )),
  tabPanel("Casos totales por comunidades",tabItem(
    fluidRow(
      column(width = 2,
             p("SON LOS DATOS DE ESPAÑA")
      ),
      column(width =2,
             # A static infoBox
             infoBox("New Orders", 10 * 2, icon = icon("world"))
      )
    ), 
    
    mainPanel(
      plotOutput("grafico_2")
    )
    
  )),
  tabPanel("PCR realizados en España por fecha",tabItem(
    fluidRow(
      column(width = 2,
             p("SON LOS DATOS DE ESPAÑA")
      ),
      column(width =2,
             # A static infoBox
             infoBox("New Orders", 10 * 2, icon = icon("world"))
      )
    ), 
    
    mainPanel(
      plotOutput("grafico_3")
    )
  )),
  tabPanel("PCR realizados en España por comunidad",tabItem(
    fluidRow(
      column(width = 2,
             p("SON LOS DATOS DE ESPAÑA")
      ),
      column(width =2,
             # A static infoBox
             infoBox("New Orders", 10 * 2, icon = icon("world"))
      )
    ), 
    
    mainPanel(
      plotOutput("grafico_4")
    )
  ))
  
)
