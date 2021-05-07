library(shiny)
library(shinyWidgets)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(rvest)
library ("sf")
library("raster")
library("leaflet")
source("./helpers/helpers.R")

DEBUG <- TRUE
PATH_DATA                 <-"./data"
PATH__RECOMEN_IMG_OMS     <-"./data/RECOM/OMS"
STANDBY_TIME    <- 15
PKI_OMS         <- c("Name","Region","casesCumulative","cumulative100000p","reported7d","reported7d100000p",
                       "reported24h","deathsCumulative","deathsCumulative100000p","deathsreported7d",
                       "deathsreported7d100000p","deathsreported24h","transmisionType")
PKI_WD          <- c('Id','World-country-continental','TotalCases','NewCases','TotalDeaths','NewDeaths','TotalRecovered',
                    'NewRecovered','ActiveCases','CritcalCase','TotalCase1M','DeathsCase1M','TotalTests','test1Mpop',
                    'Population','Continent','Caseevery','Deathevery','Testevery','newCase1M','deathsCase1M','ActiveCas1M')
 
i18n <- Translator$new(translation_json_path = "./translations/translations.json")
i18n$set_translation_language("es")
 
df <- data.frame(
  val = i18n$get_languages()
)

df$img <- lapply (1:length(df$val),function (i) {
                                    img_lang <- paste (str_trim(df$val[i]),"ico",sep=".")
                                    sprintf("<div class='jhr'><img src=\'/img/%s\',width=30px,style=\'vertical-align:middle\' >%s</img></div>",img_lang, df$val[i])
                                })

sp_COvidData            <- data.frame()
cv_COvidData            <- data.frame()
global_COvidDataOMS     <- data.frame()
global_COvidDataWDMeter <- data.frame()

#datos serie temporales. Los tenemos ya descargados para hacer un poco de todo
# Si se desean descargar los csv se encuentran:
#"https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv&filename=time_series_covid19_confirmed_global.csv"

data_confirmed    <- read_csv("./data/time_series_covid19_confirmed_global.csv")
data_deceased     <- read_csv("./data/time_series_covid19_deaths_global.csv")
data_recovered    <- read_csv("./data/time_series_covid19_recovered_global.csv")

#Cargamos los datos de serie temporal por país (Obtenemos la evolución del virus por país)
 
data_confirmed_sub <- data_confirmed %>%
  pivot_longer(names_to = "date", cols = 5:ncol(data_confirmed)) %>%
  group_by(`Province/State`, `Country/Region`, date, Lat, Long) %>%
  summarise("confirmed" = sum(value, na.rm = T))

data_recovered_sub <- data_recovered %>%
  pivot_longer(names_to = "date", cols = 5:ncol(data_recovered)) %>%
  group_by(`Province/State`, `Country/Region`, date, Lat, Long) %>%
  summarise("recovered" = sum(value, na.rm = T))

data_deceased_sub <- data_deceased %>%
  pivot_longer(names_to = "date", cols = 5:ncol(data_deceased)) %>%
  group_by(`Province/State`, `Country/Region`, date, Lat, Long) %>%
  summarise("deceased" = sum(value, na.rm = T))
data_evolution <- data_confirmed_sub %>%
  full_join(data_recovered_sub) %>%
  full_join(data_deceased_sub) %>%
  rbind(data_us) %>%
  ungroup() %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  arrange(date) %>%
  group_by(`Province/State`, `Country/Region`, Lat, Long) %>%
  fill(confirmed, recovered, deceased) %>%
  replace_na(list(deceased = 0, confirmed = 0)) %>%
  mutate(
    recovered_est = lag(confirmed, 14, default = 0) - deceased,
    recovered_est = ifelse(recovered_est > 0, recovered_est, 0),
    recovered     = coalesce(recovered, recovered_est),
    active        = confirmed - recovered - deceased
  ) %>%
  select(-recovered_est) %>%
  pivot_longer(names_to = "var", cols = c(confirmed, recovered, deceased, active)) %>%
  filter(!(is.na(`Province/State`) && `Country/Region` == "US")) %>%
  filter(!(Lat == 0 & Long == 0)) %>%
  ungroup()

# Calculamos nuevos casos
data_evolution <- data_evolution %>%
  group_by(`Province/State`, `Country/Region`) %>%
  mutate(value_new = value - lag(value, 4, default = 0)) %>%
  ungroup()

#Eliminamos los datos que ya no nos sirven
rm(data_confirmed, data_confirmed_sub, data_recovered, data_recovered_sub, data_deceased, data_deceased_sub)

#creamos un dataframe con el numero de población por país


data_evolution <- data_evolution %>%
  left_join(population, by = c("Country/Region" = "country"))
 

data_atDate <- function(inputDate) {
  data_evolution[which(data_evolution$date == inputDate),] %>%
    distinct() %>%
    pivot_wider(id_cols = c("Province/State", "Country/Region", "date", "Lat", "Long", "population"), names_from = var, values_from = value) %>%
    filter(confirmed > 0 |
             recovered > 0 |
             deceased > 0 |
             active > 0)
}

data_latest <- data_atDate(max(data_evolution$date))

top5_countries <- data_evolution %>%
  filter(var == "active", date == current_date) %>%
  group_by(`Country/Region`) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  arrange(desc(value)) %>%
  top_n(5) %>%
  select(`Country/Region`) %>%
  pull()
####################################################################
#  Manipulación de datos globales #################################
###################################################################
##############
#
# Crea y descarga los ficheros CSV de datos  del virus
###############

download_GlobalFiles <- function () {
  url_CSVOMS <- "https://covid19.who.int/WHO-COVID-19-global-table-data.csv"
  url_world  <- "https://www.worldometers.info/coronavirus/"
  url_cvData  <- "https://dadesobertes.gva.es/dataset/385d7d96-693b-4361-a00f-4e72b30a3695/resource/4dc944c0-c70a-474a-9cb6-288d218a93b7/download/covid-19-total-acumulado-de-casos-confirmados-pcr-altas-epidemiologicas-personas-fallecidas-y-da.csv"
  #Descargamos el fichero de la OMS
  filename <- createFilename (path=PATH_DATA,name="globalDataOMS")
  data_file <- read.csv(url_CSVOMS)
  names(data_file) <-  PKI_OMS
  write.csv(data_file,filename)
  
  #Descargamos el fichero de   worldometers con web scraping
  my_table<- url_world %>% read_html() %>% html_table() %>%.[[1]]
  my_table[]<-lapply(my_table, function(x) (gsub("\\,|\\+", "", (x))))
  names(my_table) <- PKI_WD
  
  # convert all but the first and last column to numeric
  continent <-  my_table[1:6,]
  names(continent)[2] <- 'continent'
  world     <-  my_table[8,]
  names(world)[2] <- 'world'
  contry    <-  my_table[9:230,]  
  names(contry)[2] <- 'contry'
  
  fCont <- createFilename (path=PATH_DATA,name="worldometersContinent")
  write.csv(continent,fCont)
  fworld <- createFilename (path=PATH_DATA,name="worldometersWorld")
  write.csv(world,fworld)
  fcountry <- createFilename (path=PATH_DATA,name="worldometersCountry")
  write.csv(contry,fcountry)
  
  #Descargamos datos de la Comunidad Valenciana
  
}

####################
##  Coprobamos si tenemos los ficheros de este día, Si no los tenemos los descargamos y los creamos
#
# globalDataOMS_<DATA>.csv
# 
### Ficheros https://www.worldometers.info/coronavirus/
#
#
# worldometersContinent_<DATA>.csv
# worldometersCountry_<DATA>.csv
# worldometersWorld_<DATA>.csv

###
 
load_Data <- function (input, output,session) {
   nowdate <- Sys.Date ()
   ##PRimero comprobamos si existe el fichero del día actual
   ## Si no existe el fichero los actualizamos
    wldometer_World     <- sprintf("%s/worldometersContinent_%s.csv",PATH_DATA,nowdate)
    wldometer_Country   <- sprintf("%s/worldometersCountry_%s.csv",PATH_DATA,nowdate)
    wldometer_Continent <- sprintf("%s/worldometersWorld_%s.csv",PATH_DATA,nowdate)
    omsGlobal           <- sprintf("%s/globalDataOMS_%s.csv",PATH_DATA,nowdate)
    
    # --123-- FALTA ACTUALIZAR LOS DATOS DE ESPAÑA Y LOS DATOS COMUNIDAD VALENCIANA
    if(!file.exists(omsGlobal)) {
      download_filesCSVOMS (input, output,session)
    } 
    if (!file.exists(wldometer_Continent)) {
      donwload_scrapingWorldometers (input, output,session)
    } 
    return ( "Datos Actualizados")
}
#############################
## Funciones de extracciónd de información del COVID-19 a partir de lso ficheros csv.
############################


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
      
      #  pasamos a valores numéricos
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
      world$Population <- sum(as.numeric(contry$Population), na.rm = TRUE)
      
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
                                                          printApp (node %>% html_attr ('alt') )
                                                          alt  <- node %>% html_attr ('alt')  
                                                          is_ptr <- alt %>% stringr::str_locate (.,"^COVID-19 :")
                                                          printApp (is_ptr)
                                                          label <- alt
                                                          if(!is.na(is_ptr[[2]]))
                                                            label <- str_trim(substring(alt,is_ptr[[2]]+1))
                                                          label <- gsub (" ","_",label)
                                                          printApp (label)
                                                          filename <- paste(PATH__RECOMEN_IMG_OMS,str_trim(label),sep='/')
                                                          filename <- paste(str_trim(filename),'png',sep='.')
                                                          print (filename)
                                                          href <- paste ("https://www.who.int",node %>% html_attr ('data-image'),sep='/')
                                                          print(href)
                                                          df <- data.frame (title =alt,href=href)
                                                          download.file(href, destfile = filename, mod = "wb")
                                                        } 
                                                        df
                                                      }) 
 
  images <- images [!sapply(images, is.null)]
  images
}

########################################################################
## CARGAR FICHEROS PNG DESCARGADOS DE LA web OMS - Recomendaciones######
######################################################################
# Cargamos los nombres de los ficheros y su path del directorio donde se almacena
# en local estos ficheros:
#
# PATH__RECOMEN_IMG_OMS     <-"./data/RECOM/OMS"
# 
#

load_filesRecomOMS<- function (input, output,session) {
  patron <- ".\\w*$" #Eliminamos la extensión del fichero (tendría que ser png, aquí no se comprueba)
  files  <-  list.files(path=PATH__RECOMEN_IMG_OMS,pattern='*.png')
  l   <- length(files)
  mytabs <- NULL
  if (l>0) {
    #Copiamos al directorio cache (www) los ficheros png
    lapply(files, function(x) file.copy(paste (PATH__RECOMEN_IMG_OMS, x , sep = "/"),
                                              paste ("www",x, sep = "/"), recursive = FALSE,  copy.mode = TRUE))
    
    mytabs <- lapply(1:l, function(i) {
      title <- files[i] %>% sub (patron,"",.)%>% str_trim(.)
      label <- paste("R",i," ")
      label <- str_trim(label)
      tabPanel(title = label,width=650,h3(gsub("_"," ",title)), fluidRow(tags$img(src = files [i],width=350)))
    })
    if(is.null(mytabs)) {
      mytabs <- tabPanel(title = 'R 0', fluidRow(h3("No existen recomendaciones de la OMS.")))
    }
  }
  mytabs
}

####################################################
###
### Descarga ficheros serie temporales impacto covid:
#####################################################

download_timeseriesCOVIDGLOBAL () {
  
  url_confirmed <- 
  data_deceased     <- read_csv("data/time_series_covid19_deaths_global.csv")
  data_recovered    <- read_csv("data/time_series_covid19_recovered_global.csv")
 
  
}
