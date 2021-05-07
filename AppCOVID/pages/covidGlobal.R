tabItem(
  fluidRow( column(width = 2, )),
    tabsetPanel(
           tabPanel("Resumen",  
                sidebarLayout(
                  sidebarPanel( 
                    
                  ),
                  mainPanel( 
                    tags$head(tags$style(HTML('.info-box {min-height: 45px;} .info-box-icon 
{height: 45px; line-height: 45px;} .info-box-content {padding-top: 0px; 
padding-bottom: 0px;} 
                          '))),
                    fluidRow(
                      column(1,
                             selectInput("Position", "", 
                                         c("User_Analyses","User_Activity_Analyses"),selected = "Median", width = 
                                           "400"),
                             conditionalPanel(
                               condition = "input.Position == 'User_Analyses'",
                               selectInput("stats", "", c("Time","Cases"),selected = "Median", width = 
                                             "400"))),
                      tags$br(),
                      column(10,
                             infoBox("User1", paste0(10), icon = icon("credit-card"), width = "3"),
                             infoBox("User2",paste0(10), icon = icon("credit-card"), width = 
                                       "3"),
                             
                             infoBox("User3",paste0(10), icon = icon("credit-card"), width = 
                                       "3"),
                             infoBox("User4",paste0(16), icon = icon("credit-card"), width = 
                                       "3")),
                      
                      
                      column(10,
                             conditionalPanel(
                               condition = "input.Position == 'User_Analyses'",
                               
                               box(title = "Plot1", status = "primary",height = "537" ,solidHeader = T,
                                   plotOutput("case_hist",height = "466")),
                               box(title = "Plot2", status = "primary",height = "537" ,solidHeader = T,
                                   plotOutput("trace_hist",height = "466"))
                               
                               
                             ),
                             conditionalPanel(
                               condition = "input.Position == 'User_Activity_Analyses'",
                               box(title = "Plot3",status = "primary",solidHeader = T,height = "537",width = "6",
                                   plotOutput("sankey_plot")),
                               box(title = "Plot4",status = "primary",solidHeader = T,height = "537",width = "6",
                                   plotOutput("sankey_table"))
                               
                             )
                        )
                )
           ))), 
           tabPanel("Por País",  
           )
    )  
 )
