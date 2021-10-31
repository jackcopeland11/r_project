library(shinydashboard)

emissions <- fread("./co2_emissions.csv", header=T)
population <- fread("./population.csv", header=T)
gdp <- fread("./gdp.csv", header=T)
renew_kwh <- fread("./total_renewable_kwh.csv" , header = T) # <- Total energy (KwH) that comes from renewable sources


dashboardPage(
  dashboardHeader(title = 'Carbon Emissions'),
  dashboardSidebar(),
  dashboardBody(
    tabsetPanel(tabPanel("Problem (CO2 Emissions)", ## Beginning of "All Countries" tab
        tabsetPanel(
          tabPanel("Bar Chart" ,
                   fluidRow(align = "center" , 
                            titlePanel("World Trends"),
                            plotOutput("emission_bar_chart", width= "90%")),
                   fluidRow(align = "center" , style = "padding-top:100px;",
                            column(6,plotOutput("population_bar_chart")),
                            column(6,  plotOutput("gdp_bar_chart"))
          )),
          
          tabPanel("Distribution of Emissions" ,
                fluidRow(align = "center" , sliderInput("year",
                                "Year",
                                min = 1960,
                                max = 2018,
                                value = 1960),
                    plotOutput("emission_by_year", width = "90%")),
                fluidRow(align = 'center', style = "padding-top:100px;",
                         plotOutput("emission_by_quintile", width = "90%")),
                fluidRow(align = 'center' , style = "padding-top:100px;",
                         plotOutput("quintile_percent", width = "90%"))
                ),
                
                tabPanel("Scatter by Country",
                         fluidRow(align = "center",
                           selectizeInput('country',
                                          'Country',
                                          choices = unique(emissions$country_name)),
                           plotOutput("scatter_plot", width = "90%")
                         ),
                         fluidRow(align = 'center', style = "padding-top:100px;",
                                  plotOutput('pop_scatter', width = "90%")),
                      fluidRow(align = 'center', style = "padding-top:100px;",
                               plotOutput('gdp_scat_plot', width = "90%")))
          
                        )),
        ## Ending all countries tab
  tabPanel("Soltuions (Renewable Energy)",
        tabsetPanel( 
          tabPanel('Renewable Energy',
           fluidRow(align = "center" , 
                    titlePanel("Total Renewable Energy"),
                    plotOutput("renew_energy_kwh", width= "90%"),
            ),
           fluidRow(align = 'center', style = 'padding-top:100px;',
                    plotOutput("focused_renew_energy", width = "90%"))
           ))
    
    )
  )))
