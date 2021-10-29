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
    g+ geom_histogram(bins =  10, fill = '#B14932')
    })

  
  output$emission_bar_chart <- renderPlot({
    x = emissions %>%
      pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'emission') %>%
      pivot_longer(cols = country_name, values_to = 'country') 
    
    g = ggplot(data = x, aes(x = year))
    g+ geom_bar(aes(y = emission),stat = 'identity', fill='#B14932') + 
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
    scatter + geom_point() + ggtitle("Emission Output Over Time")+ theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  ## Emission by quintile -------------------
  
  output$emission_by_quintile <- renderPlot({
    
    e = emissions
    e$quintile <- ntile(e$"2018" , 5)
    quints = e %>%
      pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'emission') %>%
      arrange("2018")
    
    quints_grouped = select(quints, -1:-4)
    
    
    emis_line = ggplot(data = quints_grouped, aes(x = year, y = emission, fill = quintile))
    emis_line + geom_bar(stat = 'identity', position = 'dodge') +
      scale_y_discrete(name = 'CO2 Emissions (kt)')+
      scale_x_discrete(name = "Year", limits = c('1960','1965', '1970',
                                                 '1975', '1980','1985' ,'1990',
                                                 '1995', '2000','2005','2010','2018'))
    
  })

  
  ##Population Basic Graphs ------------------
  output$population_bar_chart <- renderPlot({
    pop = population %>%
      pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'population') 
    pop[,6] = sapply(pop[,6],as.numeric)
    
    d = ggplot(data = pop, aes(x = year))
    d+ geom_bar(aes(y = population),stat = 'identity', fill = '#171B8E') + 
      scale_y_discrete(name = 'Population')+ 
      scale_x_discrete(name = "Year", limits = c('1960','1965', '1970',
                                                 '1975', '1980','1985' ,'1990',
                                                 '1995', '2000','2005','2010','2018'))+
      ggtitle("World Population by Year") + theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  output$pop_scatter <- renderPlot({
  scatter_pop = population %>%
    pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'population') %>%
    pivot_longer(cols = country_name, values_to = 'country') %>%
    filter(country == input$country)
  
  scatter_pop[,5] = sapply(scatter_pop[,5],as.numeric)
  scatter = ggplot(data = scatter_pop , aes(x = year, y = population))
  scatter + geom_point() + ggtitle("Population Over Time")+ theme(plot.title = element_text(hjust = 0.5))
  })
  ## GDP Basic Graphs =-----------------------------------------------------
  
  output$gdp_bar_chart <- renderPlot({
    gdp2 = gdp %>% 
      pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'gdp') 
    
    gdp_bar_chart = ggplot(data = gdp2, aes(x = year))
    gdp_bar_chart + geom_bar(aes(y = gdp),stat = 'identity', fill = '#3CA606') + 
      scale_y_discrete(name = 'GDP ($)')+ 
      scale_x_discrete(name = "Year", limits = c('1960','1965', '1970',
                                                 '1975', '1980','1985' ,'1990',
                                                 '1995', '2000','2005','2010','2018')) +
      ggtitle("World GDP ($) by Year") + theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$gdp_scat_plot <- renderPlot({
    x = gdp %>%
      pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'gdp') %>%
      pivot_longer(cols = country_name, values_to = 'country') %>%
      filter(country == input$country)
    scatter = ggplot(data = x , aes(x = year, y = gdp))
    scatter + geom_point() + ggtitle("GDP Over Time")+ theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  
})