#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(VIM)
library(shinydashboard)
library(Hmisc)
library(mlbench)
library(mice)
library(shiny.router)
library(shiny.i18n)
library (shinyWidgets)
library(flexdashboard)


directorio <- getwd()
cat(file=stdout()," Este es el drectorio:",directorio)

#Cargamos el script global.R que contiene las paginas de navegación y las variables globales a usar
source("./helpers/global.R")

source("./pages/ui_global.R", local = TRUE)
source("./pages/ui_sp.R", local = TRUE)
source("./pages/ui_rec.R", local = TRUE)

header <- dashboardHeader(
  title =  "COVID 19. DV.2020-21",
  titleWidth = 250,
  
  tags$li( fluidRow( 
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
  
  sidebarUserPanel("Creative Data Science",
                   subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
                   image = "logo_company.png"
  ), 
  #sidebarSearchForm(label = "Enter a number", "searchText", "searchButton") ,
  width = 250,
    sidebarMenu(
        id="selectionMenu",
        div (class="text",h3(i18n$t(verbatimTextOutput("globalUpdate")))),
        tags$hr(),
 
        menuItem(
            i18n$t("Seguimiento COVID-19"), 
            tabName = "Options", 
            icon = icon("home"),
            selected=TRUE, 
            menuSubItem( i18n$t("Global"),selected=TRUE,  tabName ="globalData" ),
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
            selected=FALSE,  tabName =  "about"        
            )
      )
)

body <- dashboardBody(
 
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
        tags$head(tags$script(type="text/javascript", src="custom.js"))
        ),
      tabItems(
        tabItem(
            style="overflow-y: auto;",
            tabName = "globalData",
            #source("./pages/ui_global.R")
            
            
        ),
        tabItem(
          style="overflow-y: auto;",
          tabName = "spanhisData",
          source("./pages/ui_sp.R") 
          
        ) ,tabItem(
          style="overflow-y: auto;",
          tabName = "recomendacionesOMS",
          fluidRow( tabPanel(i18n$t("Recomendaciones de la OMS sobre el COVID 19") ,page_reco, value = "page_reco") ),
        ),
        tabItem( tabName = "globalDataUpdate", 
                 style="overflow-y: auto;",
                 p(i18n$t("Actualizando lo´s datos Globales!!!!!")) ,
                 h3(verbatimTextOutput("TextDataGlobalUpdate"))
        ),
        tabItem(  tabName = "spanhisDataUpdate",
                  style="overflow-y: auto;",
                  p(i18n$t("Actualización de datos a nivel nacional!!!!")) ,
                  DT::dataTableOutput('table1')
        ), 
        tabItem(
                tabName = "recomendacionesOMSUpdate",
                style="overflow-y: auto;",
                p(i18n$t("Actualizando los datos de recomendaciones de la OMS!!!!!")),
                h3(verbatimTextOutput("tabsRecomendaOMS"))
        ), 
        #GUIDELINE
        tabItem(
            tabName = "guideline",
            style = "overflow-y: auto;", 
            source("./pages/guideline.R"),

        ),
        #about
        tabItem(
          tabName = "about",
          style = "overflow-y: auto;", 
          source("./pages/about.R"),
        )
      )
    )
dashboardPage(
    header, 
    sidebar, 
    body
)
