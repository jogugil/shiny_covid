tabItem(
       fluidRow(
        column(width = 2,
          # A static infoBox
          infoBox("New Orders", 10 * 2, icon = icon("credit-card"))
        ) 
         
       ), 
       
       fluidRow (
             #LOAD AND DESCRIBE DATA
             box(title = "Data Loading", width = NULL, collapsible = TRUE, class = "mainBox",
                  
                 box(
                   title = "Histogram4f", status = "primary", solidHeader = TRUE,
                   collapsible = TRUE,
                   
                 ),
             ), box(
               dataTableOutput('table6')),
      )
   
)
