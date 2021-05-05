library(shiny)
library(shinyWidgets)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(rvest)
source("./helpers/helpers.R")

DEBUG <- TRUE
PATH_DATA     <-"./data"
STANDBY_TIME  <- 15
PKI_OMS         <- c("Name","Region","casesCumulative","cumulative100000p","reported7d","reported7d100000p",
                       "reported24h","deathsCumulative","deathsCumulative100000p","deathsreported7d",
                       "deathsreported7d100000p","deathsreported24h","transmisionType")
PKI_WD          <- c('Id','World-country-continental','TotalCases','NewCases','TotalDeaths','NewDeaths','TotalRecovered',
                    'NewRecovered','ActiveCases','CritcalCase','TotalCase1M','DeathsCase1M','TotalTests','test1Mpop',
                    'Population','Continent','Caseevery','Deathevery','Testevery','newCase1M','deathsCase1M','ActiveCas1M')
 

#####################################################################
#Descargamos los datos y grabamos en el directorio de datos de la APP
#####################################################################


###
# Datos Internacionales a nivel global de la pagina de la OMS
#  url_CSVOMS <- "https://covid19.who.int/WHO-COVID-19-global-table-data.csv"
# 
###
# Sabemos que el fichero contiene las siguientes columnas que renombraremos para nuestra comodidad en la APP:
#  
# Name (Name): Nombre del país o tipo de datos de las siguientes columnas (Global a nivel mundial| por país)
# WHO Region (Region): Región del país
# Cases - cumulative total (casesCumulative): Total acumulado por país
# Cases - cumulative total per 100000 population (cumulative100000p):
# Cases - newly reported in last 7 days (reported7d):
# Cases - newly reported in last 7 days per 100000 population (reported7d100000p):
# Cases - newly reported in last 24 hours (reported24h):
# Deaths - cumulative total (deathsCumulative):
# Deaths - cumulative total per 100000 population (deathsCumulative100000p):
# Deaths - newly reported in last 7 days (deathsreported7d)::
# Deaths - newly reported in last 7 days per 100000 population (deathsreported7d100000p):
# Deaths - newly reported in last 24 hours (deathsreported24h)
# Transmission Classification (transmisionType)
#
####

download_filesCSVOMS <- function (input, output,session) {
    url_CSVOMS <- "https://covid19.who.int/WHO-COVID-19-global-table-data.csv"
    style <- isolate(input$style)
    
    # Create a Progress object
    progress <- shiny::Progress$new(style = style)
    progress$set(message = "Download OMS data", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
  
    Sys.sleep(0.25)
    t0 <- Sys.time ()
    if (is.function(updateProgress)) {
      text <- paste0("t0:",t0, " t1:", round(STANDBY_TIME))
      updateProgress(detail = text,progress=progress)
    }
 
    filename <- createFilename (path=PATH_DATA,name="globalDataOMS")
    
    if (is.function(updateProgress)) {
      text <- paste0("t0:",difftime(Sys.time(), t0, u = 'secs'), " t1:", round(STANDBY_TIME))
      updateProgress(detail = text,progress=progress)
    }
  
    ini <- Sys.time ()
    data_file <- NULL
    withProgress(message = 'Generating data', style = style, detail = "part 0", value = 0, {
         
        while (is.null (data_file) && 
                standby_donwload (ini,STANDBY_TIME)!=1) {
            data_file <- read.csv(url_CSVOMS)
            
            # Increment the progress bar, and update the detail text.
            incProgress(0.1, detail = paste("part", STANDBY_TIME))
            
            # Pause for 0.1 seconds to simulate a long computation.
            Sys.sleep(0.1)
        }
        # Increment the top-level progress indicator
        incProgress(0.5)
        if(!is.null (data_file)) {
            # Another nested progress indicator.
            # When value=NULL, progress text is displayed, but not a progress bar.
            withProgress(message = 'And this also', detail = "This other thing",
                     style = style, value = NULL, {
                       Sys.sleep(0.75)
                     })
            
            names(data_file) <-  PKI_OMS
            
            withProgress(message = 'And this also', detail = "This other thing",
                              style = style, value = NULL, {
                                Sys.sleep(0.75)
                   write.csv(data_file,filename)
                   if(file.exists(filename))
                     res <- "Los datos han sido actualizados desde la web de la OMS!!!"
                   else
                     res <- "Los datos no han sido actualizados desde la web de la OMS!!!. 
                              Error al guardar los datos en local. Contacte con el administrador."
            })
            setProgress(1)
       
         } else  {
           res <- "Los datos no han sido actualizados desde la web de la OMS!!!.
                        Error al acceder a los datos. Contacte con el administrador."
         }
     
      })
    return (res)
}
##############################################################################################
## Web Scraping web www.worldometers.info - Recomendaciones######
### https://www.worldometers.info/coronavirus/
### Los datos recopilados de esta web son:
#"Country,Other"    (country,world,continental)   
#"TotalCases"       (TotalCases)  
#"NewCases"         (NewCases)             
#"TotalDeaths"      (TotalDeaths)         
#"NewDeaths"        (NewDeaths)          
#"TotalRecovered"   (TotalRecovered)      
#"NewRecovered"     (NewRecovered)        
#"ActiveCases"      (ActiveCases)         
#"Serious,Critical" (CritcalCase)   
#"Tot Cases/1M pop" (TotalCase1M)   
#"Deaths/1M pop"    (DeathsCase1M)  
#"TotalTests"       (TotalTests)          
#"Tests/1M pop"     (test1Mpop)        
#"Population"       (Population)          
#"Continent"         (Continent)  
#"1 Caseevery X ppl"  (Caseevery) 
#"1 Deathevery X ppl" (Deathevery)
#"1 Testevery X ppl"  (Testevery) 
#"New Cases/1M pop"   (newCase1M)    
#"New Deaths/1M pop"  (deathsCase1M) 
#"Active Cases/1M pop" (ActiveCas1M)
##############################################################################################
donwload_scrapingWorldometers <- function (input, output,session) {
  url <- "https://www.worldometers.info/coronavirus/"
  
  # Create a Progress object
  style     <- isolate(input$style)
  progress  <- shiny::Progress$new(style = style)
  progress$set(message = "Extrayendo datos de Worldometers", value = 0)
  # Close the progress when this reactive exits (even if there's an error)
  on.exit(progress$close())
  
  Sys.sleep(0.25)
  t0 <- Sys.time ()
  if (is.function(updateProgress)) {
    text <- paste0("t0:",t0, " t1:", round(STANDBY_TIME))
    updateProgress(detail = text,progress=progress)
  }
  withProgress(message = 'Generating data', style = style, detail = "part 0", value = 0, {
    
    my_table<- url %>% read_html() %>% html_table() %>%.[[1]]
    if (!is.null(my_table) && length(my_table)!=0)  {
      # Increment the progress bar, and update the detail text.
      incProgress(0.1, detail = paste("part", STANDBY_TIME))
      
      # Recopil la tabla que muestra en tiempo real los datos de COVID al recdedor del mundo 
      my_table[]<-lapply(my_table, function(x) (gsub("\\,|\\+", "", (x))))
      
      # Increment the progress bar, and update the detail text.
      incProgress(0.1, detail = paste("part", STANDBY_TIME))
      
      Sys.sleep(0.25)
      t0 <- Sys.time ()
      if (is.function(updateProgress)) {
        text <- paste0("t0:",t0, " t1:", round(STANDBY_TIME))
        updateProgress(detail = text,progress=progress)
      }
      
      
      names(my_table) <- PKI_WD
      # convert all but the first and last column to numeric
      continent <-  my_table[1:6,]
      names(continent)[2] <- 'continent'
      world     <-  my_table[8,]
      names(world)[2] <- 'world'
      contry    <-  my_table[9:230,]  
      names(contry)[2] <- 'contry'
      
      incProgress(0.5)
      if (is.function(updateProgress)) {
        text <- paste0("t0:",t0, " t1:", round(STANDBY_TIME))
        updateProgress(detail = text,progress=progress)
      }
      
      df        <- list(world=world,continent=continent,contry=contry)
      setProgress(1)
      
      withProgress(message = 'Grabando datos en local', detail = "Guardando datos internacionales de worldometers",
                   style = style, value = NULL, {
                     Sys.sleep(0.75)
                     fCont <- createFilename (path=PATH_DATA,name="worldometersContinent")
                     write.csv(continent,fCont)
                     fworld <- createFilename (path=PATH_DATA,name="worldometersWorld")
                     write.csv(world,fworld)
                     fcountry <- createFilename (path=PATH_DATA,name="worldometersCountry")
                     write.csv(contry,fcountry)
                     if(file.exists(fCont) && file.exists(fworld) && file.exists(fcountry))
                       res <- "Los datos han sido actualizados desde la web de la worldometers!!!"
                     else
                       res <- "Los datos no han sido actualizados desde la web de la worldometers!!!. 
                                Error al guardar los datos en local. Contacte con el administrador."
                   })
    } else 
      res <- "Los datos no han sido recopilados del sitio web worldometers. Contacte con el administrador."
    setProgress(1)
    
  })
  res
}

 
###############################################
## Web Scraping web OMS - Recomendaciones######
###
# Recuperamos las recomendaciones que indica la OMS en su página web.
# En un principio usaremos sólo el lenguaje español, con lo que 
# la url donde se encuentra la información es:
# url_recomendationsOMS ='https://www.who.int/es/emergencies/diseases/novel-coronavirus-2019/advice-for-public'
# 
# Si utilizamos también el ingles, la información se recupera en ;
# url_en_recomendationsOMS ='https://www.who.int/emergencies/diseases/novel-coronavirus-2019/advice-for-public'

#
donwload_scrapingOMS <- function (input, output,session) {
  url_recomendationsOMS ='https://www.who.int/es/emergencies/diseases/novel-coronavirus-2019/advice-for-public'
  html <-  url_recomendationsOMS  %>% read_html()  
  #obtenemos sólo los poster resumen de las recomendaciones de la OMS
  images <- html %>% html_nodes(".lazy") %>% lapply(., function (node) {
                                                        df <- NULL
                                                        is_corona <- node %>% html_attr ('data-src')%>%grepl('coronavirus',.) 
                                                        if(is_corona) {
                                                          alt  <- node %>% html_attr ('alt')
                                                          href <- paste ("https://www.who.int",node %>% html_attr ('data-image'),sep='/')
                                                          df <- data.frame (title =alt,href=href)
                                                          
                                                        } 
                                                        df
                                                      }) 
 
  list_images_html (images [!sapply(images, is.null)])
}
