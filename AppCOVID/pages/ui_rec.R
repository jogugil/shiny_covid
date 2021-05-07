library(shinythemes)
body_reco <- mainPanel (
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
            #Cargamos las recomendaciones de la OMS sobre el COVID.
            #Se cargan los ficheros png descargados de su web 
             
                   uiOutput("tabsBoxRecomendaOMS")
     
  )
)

 
page_reco <- fluidPage(
 
  titlePanel(i18n$t("Recomendaciones OMS. COVID-19")),
  body_reco
)