body_reco <- fluidRow (
    style="overflow-y: auto;",
    #Se cargan los ficheros png descargados de su web 
    uiOutput("tabsBoxRecomendaOMS")
)
page_reco <- fluidPage (
  titlePanel(i18n$t("Recomendaciones OMS. COVID-19")),
  body_reco
)