tabItem(
       fluidRow(
        column(width = 2,
          p("SON LOS DATOS DE ESPAÑA")
        ),
        column(width =2,
               # A static infoBox
               infoBox("New Orders", 10 * 2, icon = icon("world"))
        )
       ), 
       
      column(width = 4,
             #LOAD AND DESCRIBE DATA
             box(title = "Data Loading", width = NULL, collapsible = TRUE, class = "mainBox",
                 fileInput('datafile1', 'Choose First CSV file', accept=c('text/csv', 'text/comma-separated-values, text/plain')),
                 fileInput('datafile2', 'Choose Second CSV file', accept=c('text/csv', 'text/comma-separated-values, text/plain')),
                 tags$hr(),
                 box(
                   title = "Histogram", status = "primary", solidHeader = TRUE,
                   collapsible = TRUE,
                   
                 ),
             ) 
      )
   
)