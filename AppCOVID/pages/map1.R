library("htmltools")
library (leaflet)

PATH_WORLDMAP <- "./data/wordlMap/TM_WORLD_BORDERS_SIMPL-0.3.shp"
addLabel <- function(data) {
  label <-""
  if (i18n$get_key_translation()=='es') {
         label <- paste0(
                        ' 
                        <table style="width:120px;">
                        <tr><td>Confirmed:</td><td align="right">', data$ActiveCases, '</td></tr>
                        <tr><td>DeceaCasos Actios :</td><td align="right">', data$CritcalCase , '</td></tr>
                        <tr><td>Casos Críticos:</td><td align="right">', data$TotalRecovered, '</td></tr>
                        <tr><td>Población:</td><td align="right">', data$Population , '</td></tr>
                        </table>'
                  )
  } else  {
        label <-  paste0(
                  '<b>', ifelse(is.na(data$`Province/State`), data$`Country/Region`, data$`Province/State`), '</b><br>
                  <table style="width:120px;">
                  <tr><td>Confirmed:</td><td align="right">', data$confirmed, '</td></tr>
                  <tr><td>Deceased:</td><td align="right">', data$deceased, '</td></tr>
                  <tr><td>Estimated Recoveries:</td><td align="right">', data$recovered, '</td></tr>
                  <tr><td>Active:</td><td align="right">', data$active, '</td></tr>
                  </table>'
        )
  }
  data$label <- label
  data$label <- lapply(data$label, HTML)
  
  return(data)
}

capa_world <- st_read(PATH_WORLDMAP)
#Inicializamos un mapa leaflet
mcd1 <- leaflet()
#Limitamos el zoom que se puede realizar en el mapa que vamos a crear
mcd1 <- leaflet(options=leafletOptions(minZoom=3,maxZoom = 16))
mcd1 <- addTiles(mcd1)
#Añadimos un proveedor para la cartografía base
mcd1 <- addProviderTiles(mcd1,provider=providers$Stamen.Toner)
#Centramos el mapa e indicamos el zoom con el que se mostrará en pantalla 
mcd1 <- setView(mcd1,lng=-3.00,lat=39,zoom=6)

 