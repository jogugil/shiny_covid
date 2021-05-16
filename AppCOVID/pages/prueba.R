library (sf) 
library(rvest)
library(leaflet)
library(ggplot2)
library(wbstats)
library (stringr)
library (tidyverse)

url_recomendationsOMS ='https://www.who.int/es/emergencies/diseases/novel-coronavirus-2019/advice-for-public'
PATH_DATA                 <-"./appcovid/data"
PATH__RECOMEN_IMG_OMS     <-"./appcovid/data/RECOM/OMS"
PATH__RECOMEN_IMG_OMS_Backup     <-"./appcovid/data/RECOM/OMS/old"

move_Files <- function (origin_path,dest_path) {
  files  <-  list.files(path=origin_path, include.dirs = FALSE,all.files = FALSE,pattern = "\\.[a-z]*$")
  if (length(files) >0) {
    lapply (files,FUN=function (x) {
      file.copy(paste (origin_path, x , sep = "/"),
                paste (dest_path,x, sep = "/"), recursive = FALSE,  copy.mode = TRUE)
 
    })
  }
}
 
dowmload_fileRecoOMS <- function (node) {
  df <- NULL
  is_corona <- node %>%  html_node(xpath='contains(@data-src,"coronavirus")') 
  if(is_corona) {
    printApp (node %>% html_attr ('alt') )
    patern <- "\\/[a-z\\-\\.0-9]*\\.[a-z]*\\?"
    name <- node %>%html_attr("data-image") %>% str_extract(.,patern) %>% str_sub(.,start = 2,end=str_length(.)-1)
 
    label  <- node %>% html_attr ('alt')  
    is_ptr <- label %>% stringr::str_locate (.,":")
    if(!is.na(is_ptr[[2]]))
      label <- str_trim(substring(label,is_ptr[[2]]+1))

    filename <- paste(PATH__RECOMEN_IMG_OMS,str_trim(name),sep='/')
     
   
    printApp (filename)
    
    href <- paste ("https://www.who.int",node %>% html_attr ('data-image'),sep='/')
    
    printApp(href)
    printApp(filename)
    
    df <- data.frame (title =label,href=href)
    download.file(href, destfile = filename, mod = "wb")
    
    #download.file(href, destfile = "./www", mod = "wb")
  } 
  df
}
move_Files (PATH__RECOMEN_IMG_OMS,PATH__RECOMEN_IMG_OMS_Backup)
html <-  url_recomendationsOMS  %>% read_html(  encoding = "utf8")   
#obtenemos sólo los poster resumen de las recomendaciones de la OMS
images <- html %>% html_nodes(".lazy") %>% lapply(., function (node) {
  dowmload_fileRecoOMS (node)
}) 

images <- images [!sapply(images, is.null)]
images
 