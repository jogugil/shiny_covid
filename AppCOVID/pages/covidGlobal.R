tabItem(
  fluidRow( column(width = 2, )),
    tabsetPanel(
           tabPanel("Resumen",  
                sidebarLayout(
                  sidebarPanel( 
                                         
                                               # A static infoBox
                    fluidRow(   infoBox("Casos", 10 * 2, icon = icon("credit-card"))),
                                               
                                        
                                         
                                               # A static infoBox
                                fluidRow(    valueBox(10 * 2, "New Orders", icon = icon("credit-card"))),
                                               
                                         
                                       
                                               # A static infoBox
                                             fluidRow( infoBox("Recuperados", 10 * 2, icon = icon("world"))),
                                               
                                         
                                   
                      ),
                      mainPanel( 
                          fluidRow(
                                    column(width=4,
                                            box(title = "Casos Abiertos", status = "primary", solidHeader = TRUE,
                                                collapsible = TRUE)  
                                    ),
                                    column(width=4,
                                            box(title = "Casos Cerrados", status = "primary", solidHeader = TRUE,
                                                collapsible = TRUE,)    
                                    ),
                            )
                      )
                )
           ), 
           tabPanel("Por País",  
           )
    )  
 )
