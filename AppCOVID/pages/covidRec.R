tabItem(  fluidRow( column(width = 2, )),
          #Cargamos las recomendaciones de la OMS sobre el COVID.
          #Se cargan los ficheros png descargados de su web 
          column(width = 12,
             uiOutput("tabsBoxRecomendaOMS")
          )
               
          
)
