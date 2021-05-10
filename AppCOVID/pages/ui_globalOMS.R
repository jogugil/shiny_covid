
body_globalOMS <-   fluidRow(
       
                    tags$head(
                      tags$style(type = "text/css", "#acumulate_map {height: 50vh !important;}"),
                      tags$style(type = 'text/css', ".selectcountry { font-size: 20pt !important; text-align: left !important; }"),
                      tags$style(type = 'text/css', ".selectcontinent { font-size: 20pt !important; text-align: left !important; }"),
                    ),
                    fluidRow(
                        box(wclass = "map",
                            width =12,
                            style = 'padding:0px;',
                            title = "Incidencia acumulada (14d) - OMS",
                            leafletOutput("acumulate_map"),
                        )
                    ),
                    fluidRow(
                      column(
                        uiOutput("selectize_cumulate_Variable"),
                        width = 8,
                      ),
                     
                      column(
                        uiOutput("selectize_cumulate_type"),
                        width = 4
                      )
                    ),fluidRow(       
                      column(width = 12,
                          class = "summaryOMS",
                          style = 'padding:0px;',
                          dataTableOutput("summaryDT_OMS") 
                      )
                    )
)

page_globalOMS <- fluidPage (
    tags$title(i18n$t("Incidencia Acumulada (14d) OM")),
    body_globalOMS
  
)