#########################################################################
## Script donde se generan las funciones que utilizaremos en la app
## Estas funciones utilizarán el fichero:
##  "global.R" que contiene las constantes y la carga de datos
########################################################################

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
#  
#  
createFilename <- function (path = '.7', name = 'file',ext= 'csv') {
  date <- Sys.Date()
  filename  <- paste (name,date,sep="_")
  filename  <- paste (filename ,ext,sep=".")
  filename  <- paste (path,filename  ,sep="/")
  filename
}
