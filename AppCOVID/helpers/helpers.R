#########################################################################
## Script donde se generan las funciones que utilizaremos en la app
## Estas funciones utilizarán el fichero:
##  "global.R" que contiene las constantes y la carga de datos
########################################################################
########################
##
# Función printApp : Comprueba si el Flag DEBUG esta activo y si lo está escribe en el 
# fichero que se le indica. Por defecto el fichero es stdout (la salida estandar)
# @param : sText -> Cadena a mostrar

#############
printApp <- function (sText,file=stdout()) {
  if(DEBUG)
    print(file=file,sText)
}
############################
## Función Timer que contabiliza si se ha pasado de x segundos
##
## @param in: total : Número de segundos que tiene como tope máximo
## @param in: ini : tiempo inicial
## @return out: res : 0(si aún no ha pasado total seg ; 1 si ya han pasado 'total' seg)
################################
standby_donwload <- function(ini,total = 60) {
  
    seconds <- round(as.double( difftime(Sys.time(), ini, u = 'secs')))
    if (DEBUG) print(seconds)
    if(seconds > total)
      return(1) 
    else
        return(0)
  
}
progress_widget <- function (){
# Create a Progress object
progress <- shiny::Progress$new(style = style)
progress$set(message = "Computing data", value = 0)
# Close the progress when this reactive exits (even if there's an error)
on.exit(progress$close())

}
# Create a closure to update progress.
# Each time this is called:
# - If `value` is NULL, it will move the progress bar 1/5 of the remaining
#   distance. If non-NULL, it will set the progress to that value.
# - It also accepts optional detail text.
updateProgress <- function(value = NULL, detail = NULL,progress=NULL) {
  if (DEBUG) print(progress$getValue())
  if (is.null(value)) {
    value <- progress$getValue()
    value <- value + (progress$getMax() - value) / 5
  }
  progress$set(value = value, detail = detail)
}
# #################
# Función para crear el nomre del fichero para su descarga y almacen en disco local
#@param path
#@param name
#@param ext
#  
createFilename <- function (path = '.7', name = 'file',ext= 'csv') {
  date <- Sys.Date()
  filename  <- paste (name,date,sep="_")
  filename  <- paste (filename ,ext,sep=".")
  filename  <- paste (path,filename  ,sep="/")
  filename
}
#################
# función que corrige texto
###############
clean.text = function(x) {
  # tolower
  x = toupper(x)
  # remover signos de puntuaión
  x = gsub("[[:punct:]]", "", x)
  # remover numeros
  x = gsub("[[:digit:]]", "", x)
  # remover tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remover espacios en blanco al comienzo
  x = gsub("^ ", "", x)
  # remover espacios vacios al final
  x = gsub(" $", "", x)
  return(x)
}
################
# función transformar un número en carácter a numérico 
###############
numberchange <- function (x){
  
  # eliminar el “.” que se utiliza como separador de miles
  x <- gsub("\\.", "",x)
  # la”," se reemplaza por el “.”.
  x <- gsub(",", ".",x)
  x <- as.numeric(x)
  
  
}

################
# función Crea una lista en un bloque html de titulo e imagen a partir de una lista 
# donde cada elemento de la lista tiene dos parámetros:
# title: titulo de la imagen
# href: link donde se encuentra la imagen
###############
elem_table_list_images <- function (elem) {
  str_html__elem_Cap <- "<tr> <td style=\'width: 100%;\'>"
  str_html_elem_mid <- "</td></tr><tr><td style=\'width: 100%;\'><img src="
  str_html_elem_end <- " alt=\'\'/></td></tr>"
  
  html  <- paste (str_html__elem_Cap,elem['title'])
  html  <- paste (html,str_html_elem_mid)
  html  <- paste (html,elem['href'])
  html  <- paste (html,str_html_elem_end)  
  html
}                                     
list_images_html <- function (list_src){
  str_div_cap <-"<div class=\'list-recomen\'>"
 
  
  str_html_table_cap <- "<table style=\'border-collapse: collapse; width: 37.1547%;\' border=\'0\'><tbody> "
  
  str_html_table_end <- "</tbody></table></div>" 
  
  #Recorremos la lista de imágenes 
  images <- lapply(list_src, elem_table_list_images)
   
  #Creamos el bloque html 
  html  <- paste (str_div_cap,str_html_table_cap)
  html  <- paste (html,images)
  html  <- paste (html,str_html_table_end)
  html  <- paste (html,str_html_elem_end)  
  html
  
}

###################
##  Gauge Witget
##
##################
myGauge <- function(id, label, value) { 
  tagList( tags$label( `for` = id, label ), 
           tags$meter( id = id, value = value ) 
  ) 
}


