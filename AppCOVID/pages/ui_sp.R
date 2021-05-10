body_sp <- fluidRow(
  navbarPage(
    "C.C - COVID 19",
 
    id = "menuBarSP",  
    tabPanel("Tabla Incidencia", "Tabla de incidencia de cada comunidad",DT::dataTableOutput("tbl")),
    tabPanel("Casos ACtuales Covid", "Grafica de casos",plotOutput("grafico_1")),
    tabPanel("Casos Totales", "Casos totales por comunidades",plotOutput("grafico_2")),
    tabPanel("PCR's Realizadas", "PCR realizados en España por fecha",plotOutput("grafico_3")),
    tabPanel("PCR's por Comunidades", "PCR realizados en España por comunidad",plotOutput("grafico_4"))
  ) 
)

page_sp <- fluidPage(
  
  titlePanel(i18n$t("Datos Nacionales:")),
  body_sp
)
