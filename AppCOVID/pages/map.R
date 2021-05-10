library("htmltools")
library(raster)
addLabel <- function(data) {
  data$label <- paste0(
    '<b>', ifelse(is.na(data$`Province/State`), data$`Country/Region`, data$`Province/State`), '</b><br>
    <table style="width:120px;">
    <tr><td>Confirmed:</td><td align="right">', data$confirmed, '</td></tr>
    <tr><td>Deceased:</td><td align="right">', data$deceased, '</td></tr>
    <tr><td>Estimated Recoveries:</td><td align="right">', data$recovered, '</td></tr>
    <tr><td>Active:</td><td align="right">', data$active, '</td></tr>
    </table>'
  )
  data$label <- lapply(data$label, HTML)
  
  return(data)
}


map <- leaflet(addLabel(data_latest)) %>%
  setMaxBounds(-180, -90, 180, 90) %>%
  setView(0, 20, zoom = 2) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
  addProviderTiles(providers$HERE.satelliteDay, group = "Satellite") %>%
  addLayersControl(
    baseGroups    = c("Light", "Satellite"),
    overlayGroups = c("Confirmed", "Confirmed (per capita)", "Estimated Recoveries", "Deceased", "Active", "Active (per capita)")
  ) %>%
  hideGroup("Confirmed (per capita)") %>%
  hideGroup("Estimated Recoveries") %>%
  hideGroup("Deceased") %>%
  hideGroup("Active") %>%
  hideGroup("Active (per capita)") %>%
  addEasyButton(easyButton(
    icon    = "glyphicon glyphicon-globe", title = "Reset zoom",
    onClick = JS("function(btn, map){ map.setView([20, 0], 2); }"))) %>%
  addEasyButton(easyButton(
    icon    = "glyphicon glyphicon-map-marker", title = "Locate Me",
    onClick = JS("function(btn, map){ map.locate({setView: true, maxZoom: 6}); }")))



###############
## MAPA MUNDO DATOS OMS
################

addLabelOMS <- function(data) {
  data$label <- paste0(
    '<b>', ifelse(is.na(data$Region), data$Name, data$Region), '</b><br>
    <table style="width:120px;">
    <tr><td>Confirmed:</td><td align="right">', data$casesCumulative, '</td></tr>
    <tr><td>Deceased:</td><td align="right">', data$deathsCumulative, '</td></tr>
    <tr><td>TRasnmision Type:</td><td align="right">', data$transmisionType, '</td></tr>
    <tr><td>Reported 24h:</td><td align="right">', data$reported24h, '</td></tr>
    </table>'
  )
  data$label <- lapply(data$label, HTML)
  
  return(data)
}


#Creamos paleta colores según el valor de la variable casesCumulative (por defecto)
codes <- ccodes()
#HAy que añadir el codigo ISO3 del pais a los datos de la OMS
getcode <- function (pais) {
  cod <- NA
  if ( pais %in% codes$NAME) 
    cod <- codes$ISO3 [which(codes$NAME == pais)]
  cod
}
global_COvidDataOMS$ISO3 <- lapply(global_COvidDataOMS$Name,getcode)

#añadimos los códigos de lso paises que no coinciden 

ps_nocode <- c("United States of America", "Russian Federation","The United Kingdom","Iran (Islamic Republic of)","Czechia",
"occupied Palestinian territory, including east Jerusalem","Bolivia (Plurinational State of)","Republic of Moldova",
"Venezuela (Bolivarian Republic of)","North Macedonia","Republic of Korea" ,"CÃ´te dâ€™Ivoire","Cabo Verde",
"Syrian Arab Republic","RÃ©union","Eswatini","CuraÃ§ao","Congo","United States Virgin Islands","Viet Nam","Timor-Leste",
"Saint Martin","Bonaire","Lao People's Democratic Republic","Saint BarthÃ©lemy", "United Republic of Tanzania",
"Brunei Darussalam","Northern Mariana Islands (Commonwealth of the)","Falkland Islands (Malvinas)","Holy See",
"Democratic People's Republic of Korea","Micronesia (Federated States of)")
code_nocode <- c("USA","RUS","IRN","CZE","PSE","BOL","MDA","VEN","MKD","KOR","KOS","CIV","CPV","SYR",
                 "REU","SZT","CUW","COG","VIR","VNM","TLS","MAF","BES","LAO","BLM","TZA",
                 "BRN","MNP","FLK","VAT","PRK","FSM")
pais_nocode <- data.frame (Name = ps_nocode,ISO3=code_nocode)

global_COvidDataOMS$ISO3 <-  apply (global_COvidDataOMS,1,FUN = function(reg) { 
                                                                         
                                                                          if(is.na (reg$ISO3) && (reg$Name %in% pais_nocode$Name))
                                                                              pais_nocode$ISO3 [which(pais_nocode$Name==reg$Name)]
                                                                           else
                                                                              reg$ISO3
                                                                            })

path_shpWorld <- "./data/wordlMap/TM_WORLD_BORDERS_SIMPL-0.3.shp"
layer_world    <- st_read(path_shpWorld)
#Cambio la proyección 
layer_world   <- st_transform(layer_world,4326)
 

casesCumulative <- lapply(layer_world$ISO3,getValueVar, var="casesCumulative")
layer_world$casesCumulative <- casesCumulative  %>% unlist()

deathsCumulative <-   lapply(layer_world$ISO3,getValueVar, var="deathsCumulative")
layer_world$deathsCumulative <- deathsCumulative  %>% unlist()

transmisionType <-  lapply(layer_world$ISO3,getValueVar, var="transmisionType")
layer_world$transmisionType <- transmisionType  %>% unlist()

reported24h    <-  lapply(layer_world$ISO3,getValueVar, var="reported24h")
layer_world$reported24h <- reported24h  %>% unlist()
 



#### POnemos el nombre de los paises según código ISO3
#palBin <- colorBin (c('#DAF7A6','#FFC300','#FF5233','#C70039','#900C3F','#581845'), bins = 4, domain= (layer_world$casesCumulative),na.color='transparent')

palBin <- colorBin (c('#DAF7A6','#FFC300','#FF5233','#C70039','#900C3F','#581845'), bins = 10, domain= layer_world$casesCumulative,na.color='transparent')

 


mapOMS  <- leaflet(addLabelOMS (layer_world),options=leafletOptions(minZoom=2,maxZoom = 8)) %>%
                    setMaxBounds(-180, -90, 180, 90) %>%
                    setView(0, 20, zoom = 2) %>%
                    addTiles() %>%
                    addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
                    addProviderTiles(providers$HERE.satelliteDay, group = "Satellite") %>%
                    addLayersControl(
                      baseGroups    = c("Light", "Satellite")                   
                    ) %>% 
                    addEasyButton(easyButton(
                      icon    = "glyphicon glyphicon-globe", title = "Reset zoom",
                      onClick = JS("function(btn, map){ map.setView([20, 0], 2); }"))) %>%
                    addEasyButton(easyButton(
                      icon    = "glyphicon glyphicon-map-marker", title = "Locate Me",
                      onClick = JS("function(btn, map){ map.locate({setView: true, maxZoom: 6}); }"))) %>%
                    addPolygons(color = "#444444" ,
                                weight = 1, 
                                smoothFactor = 0.5,
                                opacity = 1.0,
                                fillOpacity = 0.5,
                                 
                                fillColor = ~palBin (layer_world$casesCumulative),   # Color de llenado
                                data= layer_world,
                                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                    bringToFront = TRUE), #highlight cuando pasas el cursor
                                 
                                labelOptions = labelOptions(direction = "auto") ) %>%
                      addLegend(title = "Cumulative Incidence 14d", pal = palBin, values = layer_world$casesCumulative,
                                position = "bottomright")


 
color_casecumulative <- function (data) {

  
  casesCumulative <- lapply(layer_world$ISO3,FUN=function (cod){ 
                                          ifelse((cod %in% data$ISO3),
                                              data [which(data$ISO3 == cod),"casesCumulative"], 0)
                                        })
  layer_world$casesCumulative <- casesCumulative  %>% unlist()
  
   
  
  palBin <- colorBin (c('#DAF7A6','#FFC300','#FF5233','#C70039','#900C3F','#581845'), bins = 10, domain= layer_world$casesCumulative,na.color='transparent')
  
  
  # Elaboracion del popup:
 
  
  
  mapOMS <- leaflet(addLabelOMS (layer_world)) %>%
                setMaxBounds(-180, -90, 180, 90) %>%
                setView(0, 20, zoom = 2) %>%
                addTiles() %>%
                addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
                addProviderTiles(providers$HERE.satelliteDay, group = "Satellite") %>%
                addLayersControl(
                  baseGroups    = c("Light", "Satellite")                   
                ) %>% 
                addEasyButton(easyButton(
                  icon    = "glyphicon glyphicon-globe", title = "Reset zoom",
                  onClick = JS("function(btn, map){ map.setView([20, 0], 2); }"))) %>%
                addEasyButton(easyButton(
                  icon    = "glyphicon glyphicon-map-marker", title = "Locate Me",
                  onClick = JS("function(btn, map){ map.locate({setView: true, maxZoom: 6}); }"))) %>%
                addPolygons(color = "#444444" ,
                            weight = 1, 
                            smoothFactor = 0.5,
                            opacity = 1.0,
                            fillOpacity = 0.5,
                             
                            fillColor = ~palBin (layer_world$casesCumulative),   # Color de llenado
                            data= layer_world,
                            highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                bringToFront = TRUE), #highlight cuando pasas el cursor
                            
                            labelOptions = labelOptions(direction = "auto") ) %>%
                addLegend(title = "Cumulative Incidence 14d Cases", pal = palBin, values = layer_world$casesCumulative,
                          position = "bottomright")
  
                  
}
 
color_deathsCumulative <- function (data) {

  deathsCumulative <- lapply(layer_world$ISO3,FUN=function (cod){ 
                                         ifelse((cod %in% data$ISO3),
                                               data [which(data$ISO3 == cod),"deathsCumulative"], 0)
                      })
  layer_world$deathsCumulative <- deathsCumulative  %>% unlist()
  
  palBin <- colorBin (c('#DAF7A6','#FFC300','#FF5233','#C70039','#900C3F','#581845'), bins = 10, domain= layer_world$deathsCumulative,na.color='transparent')
  mapOMS  <- leaflet(addLabelOMS (layer_world)) %>%
                        setMaxBounds(-180, -90, 180, 90) %>%
                        setView(0, 20, zoom = 2) %>%
                        addTiles() %>%
                        addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
                        addProviderTiles(providers$HERE.satelliteDay, group = "Satellite") %>%
                        addLayersControl(
                          baseGroups    = c("Light", "Satellite")                   
                        ) %>% 
                        addEasyButton(easyButton(
                          icon    = "glyphicon glyphicon-globe", title = "Reset zoom",
                          onClick = JS("function(btn, map){ map.setView([20, 0], 2); }"))) %>%
                        addEasyButton(easyButton(
                          icon    = "glyphicon glyphicon-map-marker", title = "Locate Me",
                          onClick = JS("function(btn, map){ map.locate({setView: true, maxZoom: 6}); }"))) %>%
                        addPolygons(color = "#444444" ,
                                    weight = 1, 
                                    smoothFactor = 0.5,
                                    opacity = 1.0,
                                    fillOpacity = 0.5,
                            
                                    fillColor = ~palBin (layer_world$deathsCumulative),   # Color de llenado
                                    data= layer_world,
                                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                        bringToFront = TRUE), #highlight cuando pasas el cursor
                                    
                                    labelOptions = labelOptions(direction = "auto") ) %>%
                        addLegend(title = "Cumulative Incidence 14d Deaths", pal = palBin, values = layer_world$deathsCumulative,
                                  position = "bottomright")  
  
}
 color_reported24h <- function (data) {

   
   reported24h <- lapply(layer_world$ISO3,FUN=function (cod){ 
                              ifelse((cod %in% data$ISO3),
                                   data [which(data$ISO3 == cod),"reported24h"], 0)
   })
   layer_world$reported24h <- reported24h  %>% unlist()
   
   palBin <- colorBin (c('#DAF7A6','#FFC300','#FF5233','#C70039','#900C3F','#581845'), bins = 10, domain= layer_world$reported24h,na.color='transparent')
   mapOMS  <- leaflet(addLabelOMS (layer_world)) %>%
                   setMaxBounds(-180, -90, 180, 90) %>%
                   setView(0, 20, zoom = 2) %>%
                   addTiles() %>%
                   addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
                   addProviderTiles(providers$HERE.satelliteDay, group = "Satellite") %>%
                   addLayersControl(
                     baseGroups    = c("Light", "Satellite")                   
                   ) %>% 
                   addEasyButton(easyButton(
                     icon    = "glyphicon glyphicon-globe", title = "Reset zoom",
                     onClick = JS("function(btn, map){ map.setView([20, 0], 2); }"))) %>%
                   addEasyButton(easyButton(
                     icon    = "glyphicon glyphicon-map-marker", title = "Locate Me",
                     onClick = JS("function(btn, map){ map.locate({setView: true, maxZoom: 6}); }"))) %>%
                   addPolygons(color = "#444444" ,
                               weight = 1, 
                               smoothFactor = 0.5,
                               opacity = 1.0,
                               fillOpacity = 0.5,
                                  fillColor = ~palBin (layer_world$reported24h),   # Color de llenado
                               data= layer_world,
                               highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                   bringToFront = TRUE), #highlight cuando pasas el cursor
                               
                               labelOptions = labelOptions(direction = "auto") ) %>%
                   addLegend(title = "Cumulative Reports 24h", pal = palBin, values = layer_world$reported24h,
                             position = "bottomright")
   
  
}
 color_default <- function () {

   
   casesCumulative <- lapply(layer_world$ISO3,getValueVar, var="casesCumulative")
   layer_world$casesCumulative <- casesCumulative  %>% unlist()
   
   deathsCumulative <-   lapply(layer_world$ISO3,getValueVar, var="deathsCumulative")
   layer_world$deathsCumulative <- deathsCumulative  %>% unlist()
   
   transmisionType <-  lapply(layer_world$ISO3,getValueVar, var="transmisionType")
   layer_world$transmisionType <- transmisionType  %>% unlist()
   
   reported24h    <-  lapply(layer_world$ISO3,getValueVar, var="reported24h")
   layer_world$reported24h <- reported24h  %>% unlist()
   
   
   
   
   #### POnemos el nombre de los paises según código ISO3
   #palBin <- colorBin (c('#DAF7A6','#FFC300','#FF5233','#C70039','#900C3F','#581845'), bins = 4, domain= (layer_world$casesCumulative),na.color='transparent')
   
   palBin <- colorBin (c('#DAF7A6','#FFC300','#FF5233','#C70039','#900C3F','#581845'), bins = 10, domain= layer_world$casesCumulative,na.color='transparent')
   
   
   
   
   mapOMS  <- leaflet(addLabelOMS (layer_world)) %>%
                     setMaxBounds(-180, -90, 180, 90) %>%
                     setView(0, 20, zoom = 2) %>%
                     addTiles() %>%
                     addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
                     addProviderTiles(providers$HERE.satelliteDay, group = "Satellite") %>%
                     addLayersControl(
                       baseGroups    = c("Light", "Satellite")                   
                     ) %>% 
                     addEasyButton(easyButton(
                       icon    = "glyphicon glyphicon-globe", title = "Reset zoom",
                       onClick = JS("function(btn, map){ map.setView([20, 0], 2); }"))) %>%
                     addEasyButton(easyButton(
                       icon    = "glyphicon glyphicon-map-marker", title = "Locate Me",
                       onClick = JS("function(btn, map){ map.locate({setView: true, maxZoom: 6}); }"))) %>%
                     addPolygons(color = "#444444" ,
                                 weight = 1, 
                                 smoothFactor = 0.5,
                                 opacity = 1.0,
                                 fillOpacity = 0.5,
                                 fillColor = ~palBin (layer_world$casesCumulative),   # Color de llenado
                                 data= layer_world,
                                 highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                     bringToFront = TRUE), #highlight cuando pasas el cursor
                                 
                                 labelOptions = labelOptions(direction = "auto") ) %>%
                     addLegend(title = "Cumulative Incidence 14d", pal = palBin, values = layer_world$casesCumulative,
                               position = "bottomright")
   
   
 }
 load_mapOMS <- function (variable,type,zoom) {
    
   zoomLevel               <-  zoom
   data                    <- dataOMS_atFilter(type)   
   
   if (variable =="casesCumulative")
     color_casecumulative (data)
   else if(variable =="deathsCumulative")  
     color_deathsCumulative  (data) 
   else if (variable =="reported24h")
     color_reported24h (data)
   else  
     color_default ()
   
 }