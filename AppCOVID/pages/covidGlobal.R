tabItem(
  fluidRow( column(width = 2, )),
    tabsetPanel(
           tabPanel("Resumen",  
                    
                    fluidRow(
                      flexdashboard::gaugeOutput("casosCoronaWorld"),    
                      flexdashboard::gaugeOutput("casosFallecidosWorld") , 
                      flexdashboard::gaugeOutput("casosRecuperadosWorld") , 
                    ),
                    fluidRow(),
                    fluidRow(
                      column(
                        width = 6,
                         
                      ),
                      column(
                        width = 6,
                         
                      )
                    )
                    
               ), 
           tabPanel("Por País",  
           )
    )  
 )
