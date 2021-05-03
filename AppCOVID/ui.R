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
 

directorio <- getwd()
cat(file=stdout()," Este es el drectorio:",directorio)
 
 
#Cargamos el script global.R que contiene las paginas de navegación y las variables globales a usar
source("./helpers/global.R")


header <- dashboardHeader(
  title =  "COVID 19. DV.2020-21",
  titleWidth = 250   
   
)


#SIDE BAR
sidebar <- dashboardSidebar(
  width = 250,
    sidebarMenu(
        id="selectionMenu",
        imageOutput("LogoAPP", inline = TRUE),
        tags$hr(),
 
        menuItem(
            "Seguimiento COVID19", 
            tabName = "Options", 
            icon = icon("home"),
            selected=TRUE, 
            menuSubItem( "Global",selected=TRUE,  tabName ="globalData" ),
            menuSubItem( "España - por Couminidades",selected=FALSE,  tabName ="spanhisData" ),
            menuSubItem( "Comunidad Valenciana",selected=FALSE,  tabName ="ValenciaData" ),
            menuSubItem( "Recomendaciones de la OMS",selected=FALSE,  tabName ="recomendacionesOMS" )
          ),
        menuItem(
             "Actualización de datos", 
            tabName = "DataUpdate", 
            icon = icon("refresh"),
            selected=FALSE, 
            menuSubItem( "Global",actionButton('globalDataUpdate', 'Update Global Data')),
            menuSubItem( "España -por Couminidades", tabName ="spanhisDataUpdate" ),
            menuSubItem( "Comunidad Valenciana", tabName ="ValenciaDataUpdate" ),
            menuSubItem( "Recomendaciones de la OMS", tabName ="recomendacionesOMSUpdate" )
          ),
        menuItem(
            "Información y ayuda de la APP", 
             icon = icon("question-circle"),
            selected=FALSE,  tabName ="guideline" 
          ),
        menuItem(
           "Sobre nosotros",  
            icon = icon("info-circle"),
            selected=FALSE,  tabName =  "about"        
            )
      )
)

body <- dashboardBody(
  width = 1024,
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
        tags$head(tags$script(type="text/javascript", src="custom.js"))
        ),
      tabItems(
        tabItem(
            style="overflow-y: auto;",
            tabName = "globalData",
            navbarPage(
              'Datos Globales',
              tabPanel("Resumen"),
              tabPanel("Por país")
            ),
            source("./pages/covidGlobal.R"), 
        ),
        tabItem(
          style="overflow-y: auto;",
          tabName = "spanhisData",
          navbarPage(
            'España - Comunidades',
            tabPanel("Component 1"),
            tabPanel("Component 2"),
            tabPanel("Component 3")
          ),
          source("./pages/covidSP.R"), 
          
        ), tabItem(
          style="overflow-y: auto;",
          tabName = "ValenciaData",
          navbarPage(
            'Comunidad Valenciana',
            tabPanel("Component 1"),
            tabPanel("Component 2"),
            tabPanel("Component 3")
          ),
          source("./pages/covidCV.R"), 
          
        ),tabItem(
          style="overflow-y: auto;",
          tabName = "recomendacionesOMS",
          navbarPage(
            'Recomendaciones',
            tabPanel("Component 1"),
            tabPanel("Component 2"),
            tabPanel("Component 3")
          ),
          source("./pages/covidRec.R"), 
          
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
