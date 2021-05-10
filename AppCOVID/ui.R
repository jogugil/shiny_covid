#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library (dplyr)
library (plotly)
library (ggplot2)

library (shiny)
library (shinyjs)
library (shiny.i18n)
library (shinyWidgets)
library (shinydashboard)


directorio <- getwd()
cat(file=stdout()," Este es el drectorio:",directorio)

 

#Cargamos el script global.R que contiene las paginas de navegación y las variables globales a usar
source("./helpers/helpers.R" ) 
source("./helpers/global.R" )
source("./pages/map.R" )
 

source("./pages/ui_globalJH.R", local = TRUE)
source("./pages/ui_globalOMS.R", local = TRUE)
source("./pages/ui_globalWDMETER.R", local = TRUE)
source("./pages/ui_sp.R", local = TRUE)
source("./pages/ui_rec.R", local = TRUE)
source("./pages/about.R", local = TRUE)
source("./pages/guideline.R", local = TRUE)
 
header <- dashboardHeader(
  title =  "COVID 19. DV.2020-21",
   titleWidth = 250,
  
  tags$li( 
    fluidRow( 
            shiny.i18n::usei18n(i18n),
            div(style="display: inline-block; font-size: 10px; height=30px;width: 70px;",
                pickerInput(inputId = "selected_language",
                            label = i18n$t('Cambiar idioma'),
                            choices = df$val,
                            choicesOpt = list(content = df$img))
             )
    ),
    class = "dropdown")
)

#SIDE BAR
sidebar <- dashboardSidebar(
  width =250,
  sidebarUserPanel("Creative Data Science",
                   subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
                   image = "logo_company.png"
  ),
  #sidebarSearchForm(label = "Enter a number", "searchText", "searchButton") ,
    sidebarMenu(
        
        id="selectionMenu",style="overflow-y: auto;",
        h3(i18n$t(verbatimTextOutput("globalUpdate"))),
        tags$hr(),
 
        menuItem(
            i18n$t("Seguimiento COVID-19"), 
            tabName = "Options", 
            icon = icon("home"),
            selected=FALSE, 
            menuSubItem( i18n$t("Serie Temporal Johns Hopkins"),selected=FALSE,  tabName ="globalDataJH" ),
            menuSubItem( i18n$t("Incidencia Acumulada (14d) OMS"),selected=FALSE,  tabName ="globalDataOMS" ),
            menuSubItem( i18n$t("Situación Actual wldometer"),selected=FALSE,  tabName ="globalDataWLDMETER" ),
            menuSubItem( i18n$t("España - Comunidades"),selected=FALSE,  tabName ="spanhisData" ),
            menuSubItem( i18n$t("Recomendaciones de la OMS"),selected=FALSE,  tabName ="recomendacionesOMS" )
          ),
        menuItem(
            i18n$t("Actualización de datos"), 
            tabName = "DataUpdate", 
            icon = icon("refresh"),
            selected=FALSE, 
            menuSubItem( i18n$t("Global"),selected=FALSE,tabName ="globalDataUpdate"  ),
            menuSubItem( i18n$t("España - Comunidades"), selected=FALSE,tabName ="spanhisDataUpdate" ),
            menuSubItem( i18n$t("Recomendaciones de la OMS"), selected=FALSE,tabName ="recomendacionesOMSUpdate" )
          ),
        menuItem(
          i18n$t("Información y ayuda de la APP"), 
             icon = icon("question-circle"),
            selected=FALSE,  tabName ="guideline" 
          ),
        menuItem(
          i18n$t("Sobre nosotros"),  
            icon = icon("info-circle"),
            selected=TRUE,  tabName =  "about"        
            )
      )
)

body <- dashboardBody(
 
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
        tags$style(".shiny-progress {top: 30% !important;left: 30% !important;margin-top: -100px !important;margin-left: -250px !important; color: blue;font-size: 20px;font-style: italic;}"),
        tags$head(tags$script(type="text/javascript", src="custom.js"))
        ),
      tabItems(
        tabItem(
            style="overflow-y: auto;",
            tabName = "globalDataJH",
            column(width = 12,page_globalJH)
        ),
        tabItem(
          style="overflow-y: auto;",
          tabName = "globalDataOMS",
          column(width = 12,page_globalOMS)
        ),
        tabItem(
          style="overflow-y: auto;",
          tabName = "globalDataWLDMETER",
          column(width = 12,page_globalWDMETER)
        ),
        tabItem(
          style="overflow-y: auto;",
          tabName = "spanhisData",
          column(width = 12,page_sp)
        ),
        tabItem(
          style="overflow-y: auto;",
          tabName = "recomendacionesOMS",
          column(width = 12,page_reco)
           
        ),
        tabItem( tabName = "globalDataUpdate", 
                 style="overflow-y: auto;",
                 p(i18n$t("Actualizando los datos Globales!!!!!")) ,
                 h3(i18n$t(verbatimTextOutput("dataGlobalUpdate")))
        ),
        tabItem(  tabName = "spanhisDataUpdate",
                  style="overflow-y: auto;",
                  p(i18n$t("Actualización de datos a nivel nacional!!!!")) ,
                  h3(i18n$t(verbatimTextOutput("dataSPUpdate")))
        ), 
        tabItem(
                tabName = "recomendacionesOMSUpdate",
                style="overflow-y: auto;",
                p(i18n$t("Actualizando los datos de recomendaciones de la OMS!!!!!")),
                h3(i18n$t(verbatimTextOutput("tabsRecomendaOMS")))
        ), 
        #GUIDELINE
        tabItem(
            tabName = "guideline",
            style = "overflow-y: auto;", 
            column(width = 12,page_guideles)


        ),
        #about
        tabItem(
          tabName = "about",
          style = "overflow-y: auto;", 
          column(width = 12,page_about)
        )
      )
    )
dashboardPage(
    header, 
    sidebar, 
    body
)
