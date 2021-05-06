tabItem(
  fluidRow(
    column(width = 2,
           p("SON LOS DATOS DE ESPAÑARECOMENDACIONES DE LA OMS")
    )),
       fluidRow(
          column(width = 4,
             #Cargamos las recomendaciones de la OMS sobre el COVID.
             #Se cargan los ficheros png descargados de su web 
             box(fluidRow(uiOutput("tabsBoxRecomendaOMS")))
          )  
      )
   
)
