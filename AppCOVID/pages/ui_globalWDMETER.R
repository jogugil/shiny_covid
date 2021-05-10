
body_globalWDM <-   fluidRow(
       
                    fluidRow(
                          uiOutput("box_globalDataWorld")
                    ),
                     
                   tabBox(width=12,      
     
                     tabPanel("Total Incidence", "Incidence Cases", 
                              plotOutput("globalCases_ContinentWDMETER"),
                              
                            ),
                     tabPanel("Total Deatchs", "Deatchs Cases of Worldwide",
                              plotOutput("globalDeatchs_ContinentWDMETER"),
                              width = 6,
                            ), 
                     tabPanel("Total Recovered", "Total Recovered of Worldwide",
                                  plotOutput("globalRecovered_ContinentWDMETER"),
                                  width = 6,
                                ),
                     tabPanel("Total Active Case", "Active Case of Worldwide",
                                  plotOutput("globalActive_ContinentWDMETER"),
                                  width = 6,
                                )
       
                            )
                    
              
)

page_globalWDMETER <- fluidPage (
    tags$title(i18n$t("situación Actual wldometer")),
    body_globalWDM
  
)