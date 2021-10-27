library(shinydashboard)

emissions <- fread("./co2_emissions.csv", header=T)

dashboardPage(
  dashboardHeader(title = 'Carbon Emissions'),
  dashboardSidebar(),
  dashboardBody(
    tabsetPanel(tabPanel("Histogram" ,
                fluidRow(align = "center" , sliderInput("year",
                                "Year",
                                min = 1960,
                                max = 2018,
                                value = 1960),
                    plotOutput("emission_by_year"))
                
                ),
                tabPanel("Bar Chart" ,
                         fluidRow(align = "center" , 
                          plotOutput("emission_bar_chart"))  ),
                tabPanel("Scatter by Country",
                         fluidRow(align = "center",
                           selectizeInput('country',
                                          'Country',
                                          choices = unique(emissions$country_name)),
                           plotOutput("scatter_plot")
                         ))
                )
  ))  