library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)

emissions <- fread("./co2_emissions.csv", header=T)
population <- fread("./population.csv" , header = T)
gdp <- fread("./gdp.csv", header=T)


shinyServer(function(input, output){
  
##Emission Basic Graphs ------------------
  
  output$emission_by_year <- renderPlot({
    x = emissions %>%
      pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'emission') %>%
      pivot_longer(cols = country_name, values_to = 'country') %>%
      filter(year == input$year)
      
    g = ggplot(data = x , aes(x = emission))
    g+ geom_histogram(bins =  10)
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
                                                 '1995', '2000','2005','2010','2018'))+
      ggtitle("World Carbon Emission Output (Kt) by Year") + theme(plot.title = element_text(hjust = 0.5))
    

    
  })
  
  output$scatter_plot <- renderPlot({
    x = emissions %>%
      pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'emission') %>%
      pivot_longer(cols = country_name, values_to = 'country') %>%
      filter(country == input$country)
    scatter = ggplot(data = x , aes(x = year, y = emission))
    scatter + geom_point() 
    
  })

  
  ##Population Basic Graphs ------------------
  output$population_bar_chart <- renderPlot({
    pop = population %>%
      pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'population') 
    pop[,6] = sapply(pop[,6],as.numeric)
    
    d = ggplot(data = pop, aes(x = year))
    d+ geom_bar(aes(y = population),stat = 'identity') + 
      scale_y_discrete(name = 'Population')+ 
      scale_x_discrete(name = "Year", limits = c('1960','1965', '1970',
                                                 '1975', '1980','1985' ,'1990',
                                                 '1995', '2000','2005','2010','2018'))+
      ggtitle("World Population by Year") + theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  ## GDP Basic Graphs =-----------------------------------------------------
  
  output$gdp_bar_chart <- renderPlot({
    gdp2 = gdp %>% 
      pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'gdp') 
    
    gdp_bar_chart = ggplot(data = gdp2, aes(x = year))
    gdp_bar_chart + geom_bar(aes(y = gdp),stat = 'identity') + 
      scale_y_discrete(name = 'GDP ($)')+ 
      scale_x_discrete(name = "Year", limits = c('1960','1965', '1970',
                                                 '1975', '1980','1985' ,'1990',
                                                 '1995', '2000','2005','2010','2018')) +
      ggtitle("World GDP ($) by Year") + theme(plot.title = element_text(hjust = 0.5))
  })
  
  
})