library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)

emissions <- fread("./co2_emissions.csv", header=T)

shinyServer(function(input, output){
  
  output$emission_by_year <- renderPlot({
    x = emissions %>%
      pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'emission') %>%
      pivot_longer(cols = country_name, values_to = 'country') %>%
      filter(year == input$year)
      
    g = ggplot(data = x , aes(x = emission))
    g+ geom_histogram()
    })
  
  output$emission_bar_chart <- renderPlot({
    x = emissions %>%
      pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'emission') %>%
      pivot_longer(cols = country_name, values_to = 'country') 
    
    g = ggplot(data = x, aes(x = year))
    g+ geom_bar(aes(y = emission),stat = 'identity') + 
      scale_y_discrete(name = 'CO2 Emissions (kt)')+ 
      scale_x_discrete(name = "Year", limits = c('1960','1965', '1970',
                                                 '1975', '1980','1985' ,'1990',
                                                 '1995', '2000','2005','2010','2018'))

    
  })
  
  output$scatter_plot <- renderPlot({
    x = emissions %>%
      pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'emission') %>%
      pivot_longer(cols = country_name, values_to = 'country') %>%
      filter(country == input$country)
    scatter = ggplot(data = x , aes(x = year, y = emission))
    scatter + geom_point() 
    
  })
  
  
  
  
})