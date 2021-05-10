library(shiny)
library(shinyWidgets)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(rvest)
library (sf)
library(wbstats)

library(leaflet)
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
#Pongo la bandera del idioma en el selectinput, al lado de su código locale.
df$img <- lapply (1:length(df$val),function (i) {
                                    img_lang <- paste (str_trim(df$val[i]),"ico",sep=".")
                                    sprintf("<div class='jhr'><img src=\'/img/%s\',width=30px,style=\'vertical-align:middle\' >%s</img></div>",img_lang, df$val[i])
                                })


#######################################
## FICHEROS DE DATOS
########################################
nowdate <- Sys.Date ()

##PRimero comprobamos si existe el fichero del día actual
## Si no existe el fichero los actualizamos
wldometer_World     <- sprintf("%s/worldometersWorld_%s.csv",PATH_DATA,nowdate)
wldometer_Country   <- sprintf("%s/worldometersCountry_%s.csv",PATH_DATA,nowdate)
wldometer_Continent <- sprintf("%s/worldometersContinent_%s.csv",PATH_DATA,nowdate)
omsGlobal           <- sprintf("%s/globalDataOMS_%s.csv",PATH_DATA,nowdate)
#Ficheros serie temporal covid-19
jhonH_confirmed    <- sprintf("%s/time_series_covid19_confirmed_global.csv",PATH_DATA)
jhonH_deceased     <- sprintf("%s/time_series_covid19_deaths_global.csv",PATH_DATA)
jhonH_recovered    <- sprintf("%s/time_series_covid19_recovered_global.csv",PATH_DATA)

#Datos de España por comunidad
spdata_CC     <- sprintf("%s/casos_diagnostico_ccaa_%s.csv",PATH_DATA,nowdate)
##############
#
# Crea y descarga los ficheros CSV de datos  del virus
###############

#########################
##
# Comprobamos si estan los ficheros de recomendaion 
# si no están los descargamos
#################
is_filesRecomOMS <- function(){
  files  <-  list.files(path=PATH__RECOMEN_IMG_OMS,pattern='*.png')
  l   <- length(files)
 
  mytabs <- NULL
  #Si no estñan los ficheros lso descargamos y kos copiamos en local
  if (l<=0) {
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
        printApp (filename)
        href <- paste ("https://www.who.int",node %>% html_attr ('data-image'),sep='/')
        print(href)
        df <- data.frame (title =alt,href=href)
        download.file(href, destfile = filename, mod = "wb")
        #download.file(href, destfile = "./www", mod = "wb")
      } 
      df
    }) 
    
  }
  
}




update_files <- function () {
  url_CSVOMS <- "https://covid19.who.int/WHO-COVID-19-global-table-data.csv"
  url_world  <- "https://www.worldometers.info/coronavirus/"
  url_sp     <- "https://cnecovid.isciii.es/covid19/resources/casos_diagnostico_ccaa.csv"  
  
  
  #Descargamos el fichero de la OMS y lo guardamos en local
  filename <- createFilename (path=PATH_DATA,name="globalDataOMS")
  if(!file.exists(filename)) {
    data_file <- read.csv(url_CSVOMS)
    names(data_file) <-  PKI_OMS
    write.csv(data_file,filename,row.names = FALSE)
  }
  
   
  if(!file.exists(wldometer_World)) {
  
    #Descargamos el fichero de   worldometers con web scraping y guardamos los tres ficheros en local
    my_table<- url_world %>% read_html() %>% html_table() %>%.[[1]]
    my_table[]<-lapply(my_table, function(x) (gsub("\\,|\\+", "", (x))))
    names(my_table) <- PKI_WD
    my_table <- my_table %>% dplyr::select(-c(Id))
    continent <-  my_table[1:6,]
    names(continent)[1] <- 'continent'
    world     <-  my_table[8,]
    names(world)[1] <- 'world'
    contry    <-  my_table[9:230,]  
    names(contry)[1] <- 'contry'
    
    fCont <- createFilename (path=PATH_DATA,name="worldometersContinent")
    write.csv(continent,fCont,row.names = FALSE)
    fworld <- createFilename (path=PATH_DATA,name="worldometersWorld")
    write.csv(world,fworld,row.names = FALSE)
    fcountry <- createFilename (path=PATH_DATA,name="worldometersCountry")
    write.csv(contry,fcountry,row.names = FALSE)
  }
  
  #Descargamos datos de  España
  if(!file.exists(spdata_CC)) {
    filename <- createFilename (path=PATH_DATA,name="casos_diagnostico_ccaa")
    data_file <- read.csv(url_sp)
    write.csv(data_file,filename,row.names = FALSE)
  }
  
  
   
}

#Compruebo si no están los ficheros, Si no lo estñan los descargo y actualizo cada día
    update_files()
#Ficheros de recomendacion de la OMS
is_filesRecomOMS ()
#Ficheros Datos Actualizados COvid-19
global_COvidDataOMS               <- NULL
global_COvidDataWDMeterWorld      <- NULL
global_COvidDataWDMeterCountry    <- NULL
global_COvidDataWDMeterContinent  <- NULL
#Ficheros serie temporal covid-19
data_confirmed    <- NULL
data_deceased     <- NULL
data_recovered    <- NULL
#Ficheros COvid-19 España
casos_cc          <- NULL

result  <- tryCatch ({    
  global_COvidDataOMS               <- read.csv(omsGlobal)
  global_COvidDataWDMeterWorld      <- read.csv(wldometer_World)
  global_COvidDataWDMeterCountry    <- read.csv(wldometer_Country)
  global_COvidDataWDMeterContinent  <- read.csv(wldometer_Continent)
  
  ###Serie temporal COVID-19
  #datos serie temporales. Los tenemos ya descargados para hacer un poco de todo
  # Si se desean descargar los csv se encuentran:
  #"https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv&filename=time_series_covid19_confirmed_global.csv"
  
  data_confirmed    <- read_csv(jhonH_confirmed)
  data_deceased     <- read_csv(jhonH_deceased)
  data_recovered    <- read_csv(jhonH_recovered)
  
  ##Datos España por comunidades
  casos_cc    <- read.csv(spdata_CC)
}, 
warning = function(w) {
  if(DEBUG) cat('\n\n - This is warning  reading files \n\n')
  printApp(w)
  return (w) 
},
error = function(e) {
  if(DEBUG) cat('\n\n - This is error reading files \n\n')
  printApp(e)
  return (NULL)
},
finally = {if (DEBUG) cat ('\n\n -  Salgo de ::>reading files \n\n')}

)
#Para la manipulación de datos de la OMS
dataOMS <-global_COvidDataOMS %>% 
                          filter(Name != 'Global') %>% 
                          dplyr::select(Name, casesCumulative, deathsCumulative, reported24h, transmisionType)

getValueVar <- function (cod,var) {
  ifelse((cod %in% global_COvidDataOMS$ISO3),
    global_COvidDataOMS [which(global_COvidDataOMS$ISO3 == cod),var], 0)
}

dataOMS_atFilter <- function (typetrasnmision) {
      result  <- tryCatch ({ 
        if (!is.null(typetrasnmision)&& !is.na(typetrasnmision))
          global_COvidDataOMS %>% 
             dplyr::filter (transmisionType %in% typetrasnmision)  
        else
          global_COvidDataOMS 
      }, 
      warning = function(w) {
        if(DEBUG) cat('\n\n - This is warning dataOMS_atFilter \n\n')
        printApp(w)
        return (w) 
      },
      error = function(e) {
        if(DEBUG) cat('\n\n - This is error dataOMS_atFilter \n\n')
        printApp(e)
        return (NULL)
      },
      finally = {if (DEBUG) cat ('\n\n -  Salgo de ::> dataOMS_atFilter\n\n')})
}
update_dataOMS <- function (filterType) {
  dt <- lapply(global_COvidDataOMS$transmisionType,FUN=function(x) ifelse(x %in% filterType,1,0))
  global_COvidDataOMS [which(dt ==1),] %>% dplyr::select(Name,casesCumulative,deathsCumulative,reported24h,transmisionType)
}

##################
# Datos de porporciones por continente de WDMETER.com
#############
data_CONTINENTWDMETER  <- global_COvidDataWDMeterContinent %>% dplyr::select (c(continent,TotalCases,TotalDeaths,TotalRecovered,ActiveCases))
data_CONTINENTWDMETER  <- data_CONTINENTWDMETER %>%  mutate ( prop_cont      =  (TotalCases/sum(TotalCases))*100,
                                                        prop_deatchs   = (TotalDeaths/sum(TotalDeaths))*100,
                                                        prop_Recovered = (TotalRecovered/sum(TotalRecovered))*100,
                                                        prop_Active     = (ActiveCases/sum(ActiveCases))*100
                                                      )
#Cargamos los datos de serie temporal por país (Obtenemos la evolución del virus por país)
result  <- tryCatch ({  
  

    # casos confirmados
    data_confirmed_sub <- data_confirmed %>%
      pivot_longer(names_to = "date", cols = 5:ncol(data_confirmed)) %>%
      group_by(`Province/State`, `Country/Region`, date, Lat, Long) %>%
      summarise("confirmed" = sum(value, na.rm = T))
    # casos recuperados
    data_recovered_sub <- data_recovered %>%
      pivot_longer(names_to = "date", cols = 5:ncol(data_recovered)) %>%
      group_by(`Province/State`, `Country/Region`, date, Lat, Long) %>%
      summarise("recovered" = sum(value, na.rm = T))
    # casos de fallecidos
    data_deceased_sub <- data_deceased %>%
      pivot_longer(names_to = "date", cols = 5:ncol(data_deceased)) %>%
      group_by(`Province/State`, `Country/Region`, date, Lat, Long) %>%
      summarise("deceased" = sum(value, na.rm = T))
    
    #incluimos los tres conjuntos de datos en un solo dataframe
    data_evolution <- data_confirmed_sub %>%
      full_join(data_recovered_sub) %>%
      full_join(data_deceased_sub) %>%
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
      dplyr::select(-recovered_est) %>%
      pivot_longer(names_to = "var", cols = c(confirmed, recovered, deceased, active)) %>%
      filter(!(is.na(`Province/State`) && `Country/Region` == "US")) %>%
      filter(!(Lat == 0 & Long == 0)) %>%
      ungroup()
    
    # Calculating new cases
    data_evolution <- data_evolution %>%
      group_by(`Province/State`, `Country/Region`) %>%
      mutate(value_new = value - lag(value, 4, default = 0)) %>%
      ungroup()
    
     
    #Eliminamos los datos que ya no nos sirven
    rm(data_confirmed, data_confirmed_sub, data_recovered, data_recovered_sub, data_deceased, data_deceased_sub)
    
    ##Obtenemos los datos de la población de cada ciudad/país
    population                                                            <- wb_data(country = "countries_only", indicator = "SP.POP.TOTL", start_date = 2019, end_date = 2021) %>%
                                                                              rename(population = SP.POP.TOTL) %>%
                                                                              dplyr::select(-c(unit,obs_status,footnote,date))
    countryNamesPop                                                       <- c("Brunei Darussalam", "Congo, Dem. Rep.", "Congo, Rep.", "Czech Republic",
                                                                               "Egypt, Arab Rep.", "Iran, Islamic Rep.", "Korea, Rep.", "St. Lucia", "West Bank and Gaza", "Russian Federation",
                                                                               "Slovak Republic", "United States", "St. Vincent and the Grenadines", "Venezuela, RB")
    countryNamesDat                                                       <- c("Brunei", "Congo (Kinshasa)", "Congo (Brazzaville)", "Czechia", "Egypt", "Iran", "Korea, South",
                                                                               "Saint Lucia", "occupied Palestinian territory", "Russia", "Slovakia", "US", "Saint Vincent and the Grenadines", "Venezuela")
    population <- na.omit(population)
    
    population[which(population$country %in% countryNamesPop), "country"] <- countryNamesDat
    
    
    # Data from wikipedia
    noDataCountries <- data.frame(
      country    = c("Cruise Ship", "Guadeloupe", "Guernsey", "Holy See", "Jersey", "Martinique", "Reunion", "Taiwan*"),
      population = c(3700, 395700, 63026, 800, 106800, 376480, 859959, 23780452)
    )
    population      <- bind_rows(population, noDataCountries)
    
    data_evolution <- data_evolution %>%
      left_join(population, by = c("Country/Region" = "country"))
    #Eliminamos los datos que no necesitamos
    rm(population, countryNamesPop, countryNamesDat, noDataCountries)
    
    #Extraemos el conjunto de datos para la fecha especificada por parámetro
    data_atDate <- function(inputDate) {
      tryCatch({
                 
                data_evolution[which(data_evolution$date == inputDate),] %>%
                  distinct() %>%
                  pivot_wider(id_cols = c("Province/State", "Country/Region", "date", "Lat", "Long", "population"), names_from = var, values_from = value) %>%
                  filter(confirmed > 0 |
                           recovered > 0 |
                           deceased > 0 |
                           active > 0)
      }, 
      warning = function(w) {
        if(DEBUG) cat('\n\n - This is warning data_atDate \n\n')
          printApp(w)
          return (w) 
      },
      error = function(e) {
        if(DEBUG) cat('\n\n - This is error data_atDate \n\n')
        printApp(e)
          return (NULL)
      },
      finally = {if (DEBUG) cat ('\n\n -  Salgo de ::> data_atDate\n\n')})
    }
    
    #Extraemos los datos más recientes
    last_date <- max(data_evolution$date)
    data_latest <- data_atDate(last_date)
    changed_date <- last_date
    current_date <- last_date
    #Onteneo,s los primeros cinco países
    top5_countries <- data_evolution %>%
      filter(var == "active") %>%
      group_by(`Country/Region`) %>%
      summarise(value = sum(value, na.rm = T)) %>%
      arrange(desc(value)) %>%
      top_n(5) %>%
      dplyr::select(`Country/Region`) %>%
      pull()
}, 
warning = function(w) {
  if(DEBUG) cat('\n\n - This is warning exploring time series data \n\n')
  printApp(w)
  return (w) 
},
error = function(e) {
  if(DEBUG) cat('\n\n - This is error exploring time series data \n\n')
  printApp(e)
  return (NULL)
},
finally = {if (DEBUG) cat ('\n\n -  Salgo de ::> exploring time series data\n\n')}

)
 
####################################################################
#  Manipulación de datos globales #################################
###################################################################

summariseData <- function(df, groupBy) {
  df %>%
    group_by(!!sym(groupBy)) %>%
    summarise(
      "Confirmed"            = sum(confirmed, na.rm = T),
      "Estimated Recoveries" = sum(recovered, na.rm = T),
      "Deceased"             = sum(deceased, na.rm = T),
      "Active"               = sum(active, na.rm = T)
    ) %>%
    as.data.frame()
}

getSummaryDT <- function(data, groupBy, selectable = FALSE) {
  
  datatable(
    na.omit(summariseData(data, groupBy)),
    rownames  = FALSE,
    options   = list(
      order          = list(1, "desc"),
      scrollX        = TRUE,
      scrollY        = "37vh",
      scrollCollapse = T,
      dom            = 'ft',
      paging         = FALSE
    ),
    selection = ifelse(selectable, "single", "none")
  )
}
sumData <- function(date) {
 
  if (date >= min(data_evolution$date)) {
    data <- data_atDate(date) %>% summarise(
      confirmed = sum(confirmed, na.rm = T),
      recovered = sum(recovered, na.rm = T),
      deceased  = sum(deceased, na.rm = T),
      countries = n_distinct(`Country/Region`)
    )
    return(data)
  }
  return(NULL)
}




####################
##  Comprobamos si tenemos los ficheros de este día, Si no los tenemos los descargamos y los creamos
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
###Aunque no lo haria para una app normal. 
## Para esta app y de forma que sea mas comodo para todos, Cargamos los ficheros de datos
## en memoria.
 

##PRimero comprobamos si existe el fichero del día actual
## Si no existe el fichero los actualizamos
##wldometer_World     <- sprintf("%s/worldometersWorld_%s.csv",PATH_DATA,nowdate)
##wldometer_Country   <- sprintf("%s/worldometersCountry_%s.csv",PATH_DATA,nowdate)
##wldometer_Continent <- sprintf("%s/worldometersContinent_%s.csv",PATH_DATA,nowdate)
##omsGlobal           <- sprintf("%s/globalDataOMS_%s.csv",PATH_DATA,nowdate)
#Ficheros serie temporal covid-19
##jhonH_confirmed    <- sprintf("%s/time_series_covid19_confirmed_global.csv",PATH_DATA)
##jhonH_deceased     <- sprintf("%s/time_series_covid19_deaths_global.csv",PATH_DATA)
##jhonH_recovered    <- sprintf("%s/time_series_covid19_recovered_global.csv",PATH_DATA)

#Datos de España por provincia
##spdata_CC     <- sprintf("%s/casos_diagnostico_ccaa.csv",PATH_DATA)

load_files <- function () {
 
  result  <- tryCatch ({ 
    
    if (is.null(global_COvidDataOMS))
      global_COvidDataOMS               <- read.csv(omsGlobal)
    if (is.null(global_COvidDataWDMeterWorld))
      global_COvidDataWDMeterWorld      <- read.csv(wldometer_World)
    if (is.null(global_COvidDataWDMeterCountry))
      global_COvidDataWDMeterCountry    <- read.csv(wldometer_Country)
    if (is.null(global_COvidDataWDMeterContinent))
      global_COvidDataWDMeterContinent  <- read.csv(wldometer_Continent)
    if(is.null (casos_cc))
      casos_cc <- read.csv (casos_cc)
 
  } ,
    warning = function(w) {
      if (DEBUG) cat('\n\n - This is warning in function load_files: \n\n')
       printApp(w)
      return (h) 
    },
    error = function(e) {
      if (DEBUG) cat('\n\n - This is error in function load_files: \n\n')
      printApp(e)
      return (NULL)
    },
    finally = {if (DEBUG) cat ('\n\n -  Salgo de ::>load_files \n\n')}
    
  )
}
load_Data <- function (input, output,session) {
  
    if(!file.exists(omsGlobal)) {
      download_filesCSVOMS (input, output,session)
      
    } 
    if (!file.exists(wldometer_Continent)) {
      donwload_scrapingWorldometers (input, output,session)
    } 
    if (!file.exists(spdata_CC)) {
      download_SpdataCC (input, output,session)
    } 
  
      
    return ( "Datos Actualizados")
}
 


#####################################################################
#Descargamos los datos y grabamos en el directorio de datos de la APP
#####################################################################
###############
#
# Descargamos Fichero de datos de COVID en España por comunidad  Autonoma
#################
download_SpdataCC <- function (input,output,session) {
  url <- "https://cnecovid.isciii.es/covid19/resources/casos_diagnostico_ccaa.csv"
  
  #req(input$style)
  style <- isolate(input$style)
  
  # Create a Progress object
  progress <- shiny::Progress$new(style = style)
  progress$set(message = "Download Spanish Data ", value = 0)
  # Close the progress when this reactive exits (even if there's an error)
  on.exit(progress$close())
  
  Sys.sleep(0.25)
  t0 <- Sys.time ()
  if (is.function(updateProgress)) {
    text <- paste0("t0:",t0, " t1:", round(STANDBY_TIME))
    updateProgress(detail = text,progress=progress)
  }
  
  filename <- createFilename (path=PATH_DATA,name="casos_diagnostico_ccaa")
  
  if (is.function(updateProgress)) {
    text <- paste0("t0:",difftime(Sys.time(), t0, u = 'secs'), " t1:", round(STANDBY_TIME))
    updateProgress(detail = text,progress=progress)
  }
  
  ini <- Sys.time ()
  data_file <- NULL
  withProgress(message = 'Generating data', style = style, detail = "part 0", value = 0, {
    
    while (is.null (data_file) && 
           standby_donwload (ini,STANDBY_TIME)!=1) {
      data_file <- read.csv(url)
      
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
       withProgress(message = 'And this also', detail = "This other thing",
                   style = style, value = NULL, {
                     Sys.sleep(0.75)
                     write.csv(data_file,filename,row.names = FALSE)
                     if(file.exists(filename))
                       res <- "Los datos han sido actualizados desde la web del Ministerior !!!"
                     else
                       res <- "Los datos no han sido actualizados desde la web del Ministerio!!!. 
                              Error al guardar los datos en local. Contacte con el administrador."
                   })
      setProgress(1)
      
    } else  {
      res <- "Los datos no han sido actualizados desde la web del Ministerio!!!.
                        Error al acceder a los datos. Contacte con el administrador."
    }
    
  })
  return (res)
}

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
    
    
  
    #req(input$style)
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
            #Cambiamos nombre de las columna s para no tener problemas 
            #Trabajamos un poco con lso datos. 
            names(data_file) <-  PKI_OMS
            data_file$transmisionType[which(data_file$Name=='Global')] <- "Not applicable"
            data_file$Region[which(data_file$Name=='Global')] <- "Global"
            
            withProgress(message = 'And this also', detail = "This other thing",
                              style = style, value = NULL, {
                                Sys.sleep(0.75)
                                
                   write.csv(data_file,filename,row.names = FALSE)
                   
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
      my_table <- my_table %>% dplyr::select(-c(Id))
      continent <-  my_table[1:6,]
      names(continent)[1] <- 'continent'
      world     <-  my_table[8,]
      names(world)[1] <- 'world'
      contry    <-  my_table[9:230,]  
      names(contry)[1] <- 'contry'
      
      
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
                     write.csv(continent,fCont,row.names = FALSE)
                     fworld <- createFilename (path=PATH_DATA,name="worldometersWorld")
                     write.csv(world,fworld,row.names = FALSE)
                     fcountry <- createFilename (path=PATH_DATA,name="worldometersCountry")
                     write.csv(contry,fcountry,row.names = FALSE)
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
                                                          printApp(href)
                                                          df <- data.frame (title =alt,href=href)
                                                          download.file(href, destfile = filename, mod = "wb")
                                                          #download.file(href, destfile = "./www", mod = "wb")
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
  printApp("LOAD_FILES")
  printApp(l)
  mytabs <- NULL
  if (l>0) {
    #Copiamos al directorio cache (www)  y al directorio data los ficheros png
    lapply(files, function(x) file.copy(paste (PATH__RECOMEN_IMG_OMS, x , sep = "/"),
                                              paste ("www",x, sep = "/"), recursive = FALSE,  copy.mode = TRUE))
    lapply(files, function(x) file.copy(paste (PATH__RECOMEN_IMG_OMS, x , sep = "/"),
                                        paste ("data",x, sep = "/"), recursive = FALSE,  copy.mode = TRUE))
    
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
