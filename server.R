library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)

renew_kwh <- fread("./total_renewable_kwh.csv" , header = T) # <- Total energy (KwH) that comes from renewable sources
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
    g + geom_histogram(bins = 10, fill = "#B14932") + scale_y_continuous(name = 'Cou')+
      scale_x_continuous(name = 'Carbon Emissions (Kt)')

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
      ggtitle("World Carbon Emission Output (Kt) by Year") + theme(plot.title = element_text(hjust = 0.5)) + scale_y_continuous()
    

    
  })
  
  output$scatter_plot <- renderPlot({
    x = emissions %>%
      pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'emission') %>%
      pivot_longer(cols = country_name, values_to = 'country') %>%
      filter(country == input$country)
    scatter = ggplot(data = x , aes(x = year, y = emission))
    scatter + geom_point(color = "#B14932") + ggtitle("Emission Output Over Time")+ theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  ## Emission by quintile -------------------
  
  output$emission_by_quintile <- renderPlot({
    
    e = emissions
    e$quintile <- ntile(e$"2018" , 10)
    e = select(e, -1:-4)
    
    quints = e %>% 
      pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'emission') %>%
      arrange("2018") %>% filter(year == input$year)

    
    emis_line = ggplot(data = na.omit(quints), aes(x = year, y = emission, fill = as.factor(quintile)))
    emis_line + geom_bar(stat = 'identity', position = 'dodge') +
      scale_y_discrete(name = 'CO2 Emissions (kt)')+
      scale_x_discrete(name = "Year") + coord_flip()+ scale_y_continuous(name = 'CO2 Emissions (Kt)')
    
  })
  
    output$quintile_percent <- renderPlot({
    e = emissions
    e$quintile <- ntile(e$"2018" , 10)
    
    e = e %>% 
      pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'emission')
    total_emissions = sum(na.omit(emissions$"2018"))
    
    v = e %>%
      group_by(quintile, year) %>% 
      summarise(percent_of_emission = (sum(emission)/total_emissions)*100)%>%
      filter(year == input$year)

    g23 = ggplot(data = na.omit(v), aes(x = as.factor(quintile), y= percent_of_emission, fill = as.factor(quintile)))
    g23 + geom_bar(stat = 'identity') + xlab('Segment') + ylab('Percent of Emission')
    })
    
    
  
  output$emission_by_country <- renderPlot({
    
    e = emissions
    e$quintile <- ntile(e$"2018" , 10)
    e = select(e, -1:-4)
    
    quints = e %>% 
      pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'emission') %>%
      arrange("2018") %>% filter(qunitile == 10)%>% filter(year == input$year)
    
    
    emis_line = ggplot(data = na.omit(quints), aes(x = country_name, y = emission))
    emis_line + geom_bar(stat = 'identity', position = 'dodge') +
      scale_y_discrete(name = 'CO2 Emissions (kt)')+
      scale_x_discrete(name = "Country") + coord_flip()
    
  })

  
  ##Population Basic Graphs ------------------
  
  output$population_bar_chart <- renderPlot({
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
      ggtitle("World GDP ($) by Year") + theme(plot.title = element_text(hjust = 0.5))+ scale_y_continuous()
  })
  
  output$gdp_scat_plot <- renderPlot({
    x = gdp %>%
      pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'gdp') %>%
      pivot_longer(cols = country_name, values_to = 'country') %>%
      filter(country == input$country)
    scatter = ggplot(data = x , aes(x = year, y = gdp))
    scatter + geom_point(color = '#3CA606') + ggtitle("GDP Over Time")+ theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  
    pop = population %>%
      pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'population') 
    pop[,6] = sapply(pop[,6],as.numeric)
    
    d = ggplot(data = pop, aes(x = year))
    d+ geom_bar(aes(y = population),stat = 'identity', fill = '#171B8E') + 
      scale_y_discrete(name = 'Population')+ 
      scale_x_discrete(name = "Year", limits = c('1960','1965', '1970',
                                                 '1975', '1980','1985' ,'1990',
                                                 '1995', '2000','2005','2010','2018'))+
      ggtitle("World Population by Year") + theme(plot.title = element_text(hjust = 0.5))+ scale_y_continuous()
    
  })
  
  output$pop_scatter <- renderPlot({
  scatter_pop = population %>%
    pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'population') %>%
    pivot_longer(cols = country_name, values_to = 'country') %>%
    filter(country == input$country)
  
  scatter_pop[,5] = sapply(scatter_pop[,5],as.numeric)
  scatter = ggplot(data = scatter_pop , aes(x = year, y = population))
  scatter + geom_point(color = '#171B8E') + ggtitle("Population Over Time")+ theme(plot.title = element_text(hjust = 0.5))
  })
  ## Renewable Energy Graphs -----------------
  
  output$renew_energy_kwh <- renderPlot({
    ren = renew_kwh %>%
      pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'kwh_renewable') %>%
      pivot_longer(cols = country_name, values_to = 'country') 
    
    renew_bar_chart = ggplot(data = ren, aes(x = year, y =kwh_renewable ))
    renew_bar_chart + geom_bar(stat = 'identity', fill = '#3A761C') +
      scale_x_discrete(name = "Year", limits = c('1990',
                                                 '1995', '2000','2005','2010','2015'))+
      ggtitle("Renewable Energy Output")+ theme(plot.title = element_text(hjust = 0.5)) +
      ylab("Renewable Energy (KwH)")
  })
  
  
  output$focused_renew_energy <- renderPlot({
    e = emissions
    e$quintile <- ntile(e$"2015" , 10)
    e <- e %>%
      select(country_code, "2015", quintile)
    e = e %>% rename(emissions = "2015")
    renew_for_merge = renew_kwh
    rnm = renew_for_merge %>%
      select(country_name, country_code, "2015")
    rnm = rnm %>% rename(renewable_energy = "2015" )
    
    renew_and_emission = merge(e, rnm, by = 'country_code')
    
    gg = ggplot(data = na.omit(renew_and_emission), aes(x = as.factor(quintile), y = renewable_energy, fill = as.factor(quintile)))
    gg + geom_bar(stat = 'identity') + ylab("Renewable Energy (KwH)")
    
  })
  
  
  ##### Change in emissions by quintile data ----------------
  
  output$all_changes_by_quintile <- renderPlot({
    
    m = emissions
    m$quintile <- ntile(m$"2015" , 10)
    first_table = m %>%
      pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'emission') %>%
      pivot_longer(cols = country_name, values_to = 'country') %>%
      filter(year == input$start_year)
    second_table = emissions %>%
      pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'emission') %>%
      pivot_longer(cols = country_name, values_to = 'country') %>%
      filter(year == input$end_year)
    
    total = merge(first_table, second_table, by = "country")
    
    all_changes = total %>% 
      select(country, quintile, emission.x, emission.y)
    
    
    all_changes = all_changes %>%
      mutate(change_in_co2 = emission.y - emission.x) %>% 
      select(country, change_in_co2, quintile)
    
    all_changes = all_changes %>%
      group_by(quintile) %>%
      summarise(change_in_co2 = sum(change_in_co2))
    
    ## Change in CO2 emissions by quintile 
    
    change = ggplot(data = na.omit(all_changes), aes(x = as.factor(quintile), y = change_in_co2, fill = as.factor(quintile)))
    change + geom_bar(stat = 'identity', na.rm = T) + ylab("Change in CO2 Emission (Kt)")+ 
      xlab('Segment')
    
    
  })
  
  output$best_ten <- renderPlot({
    
    m = emissions
    m$quintile <- ntile(m$"2015" , 10)
    first_table = m %>%
      pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'emission') %>%
      pivot_longer(cols = country_name, values_to = 'country') %>%
      filter(year == input$start_year)
    second_table = emissions %>%
      pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'emission') %>%
      pivot_longer(cols = country_name, values_to = 'country') %>%
      filter(year == input$end_year)
    
    total = merge(first_table, second_table, by = "country")
    
    all_changes = total %>% 
      select(country, quintile, emission.x, emission.y)
    
    
    all_changes = all_changes %>%
      mutate(change_in_co2 = emission.y - emission.x) %>% 
      select(country, change_in_co2, quintile) %>% arrange(change_in_co2) %>%
      slice(1:10)
    
    final = all_changes %>%
      group_by(quintile, country) %>%
      summarise(change_in_co2 = sum(change_in_co2)) %>%
      filter(change_in_co2 < 0 ) 
    
    ## Change in CO2 emissions by quintile 
    
    
    change = ggplot(data = na.omit(all_changes), aes(x = country, y = change_in_co2, fill = as.factor(quintile)))
    change + geom_bar(stat = 'identity', na.rm = T) +xlab("Country") + ylab("Change in Emissions (Kt)")
    
    
  })
  
  output$worst_ten <- renderPlot({
    
  m = emissions
  m$quintile <- ntile(m$"2015" , 10)
  first_table = m %>%
    pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'emission') %>%
    pivot_longer(cols = country_name, values_to = 'country') %>%
    filter(year == input$start_year)
  second_table = emissions %>%
    pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'emission') %>%
    pivot_longer(cols = country_name, values_to = 'country') %>%
    filter(year == input$end_year)
  
  total = merge(first_table, second_table, by = "country")
  
  all_changes = total %>% 
    select(country, quintile, emission.x, emission.y)
  
  
  all_changes = all_changes %>%
    mutate(change_in_co2 = emission.y - emission.x) %>% 
    select(country, change_in_co2, quintile) %>% arrange(desc(change_in_co2)) %>%
    slice(1:10)
  
  final = all_changes %>%
    group_by(quintile, country) %>%
    summarise(change_in_co2 = sum(change_in_co2)) %>%
    filter(change_in_co2 > 0 ) 
  
  ## Change in CO2 emissions by quintile 
  
  
  change = ggplot(data = na.omit(all_changes), aes(x = country, y = change_in_co2, fill = as.factor(quintile)))
  change + geom_bar(stat = 'identity', na.rm = T) + 
    xlab("Country") + ylab("Change in Emissions (Kt)")
  
  
  
})
  
  output$cluster_good <- renderPlot({
    emissions_cluster = emissions %>%
      select(country_name, "2018","1998") %>% 
      mutate(change_in_co2 = emissions$"2018"- emissions$"1998") %>%
      select(country_name, "2018", change_in_co2 ) %>%
      rename(recent_co2 = "2018")
    
    # Prepare GDP data for join
    gdp_cluster = gdp %>%
      select(country_name, "2018","1998") %>% 
      mutate(change_in_gdp = gdp$"2018"- gdp$"1998") %>%
      select(country_name, "2018", change_in_gdp )%>%
      rename(recent_gdp = "2018")
    
    # Prepare population data for join
    pop_cluster = pop %>%
      select(country_name, "2018","1998") %>% 
      mutate(change_in_pop = pop$"2018"- pop$"1998") %>%
      select(country_name, "2018", change_in_pop)%>%
      rename(recent_pop = "2018")
    
    ### merge all data into one table
    
    first_merge = merge(emissions_cluster, gdp_cluster, by = 'country_name')
    final_cluster = merge(first_merge, pop_cluster, by = 'country_name')
    
    
    final_cluster$recent_pop = as.numeric(final_cluster$recent_pop)
    final_cluster$change_in_pop = as.numeric(final_cluster$change_in_pop)
    
    ## Clean for clustering
    for_cluster = final_cluster
    #for_cluster <- for_cluster[!is.infinite(rowSums(for_cluster)),]\
    for_cluster = na.omit(for_cluster) ## Remove NAs
    for_cluster[,2:7] <- for_cluster[,2:7][is.finite(rowSums(for_cluster[,2:7])),]
    
    
    
    for_cluster <- data.frame(for_cluster, scale(for_cluster[,2:7])) # standardize variables
    for_cluster = for_cluster %>%
      select(1, 8:13)
    
    
    fit <- kmeans(for_cluster[,2:7], 5) # 5 cluster solution
    for_cluster <- data.frame(for_cluster, fit$cluster)
    
    for_cluster = for_cluster %>%
      select(country_name, fit.cluster)
    
    data_and_cluster = merge(final_cluster, for_cluster, by = 'country_name')
    
    
    example_countries = data_and_cluster %>%
      filter(fit.cluster ==input$Cluster) %>%
      filter(change_in_co2 < 0)
    
    graph = ggplot(data = example_countries, aes(x = country_name, y = change_in_co2))
    graph + geom_bar(stat = 'identity', fill = "#B14932")
  })

  output$cluster_bad <- renderPlot({
    emissions_cluster = emissions %>%
      select(country_name, "2018","1998") %>% 
      mutate(change_in_co2 = emissions$"2018"- emissions$"1998") %>%
      select(country_name, "2018", change_in_co2 ) %>%
      rename(recent_co2 = "2018")
    
    # Prepare GDP data for join
    gdp_cluster = gdp %>%
      select(country_name, "2018","1998") %>% 
      mutate(change_in_gdp = gdp$"2018"- gdp$"1998") %>%
      select(country_name, "2018", change_in_gdp )%>%
      rename(recent_gdp = "2018")
    
    # Prepare population data for join
    pop_cluster = pop %>%
      select(country_name, "2018","1998") %>% 
      mutate(change_in_pop = pop$"2018"- pop$"1998") %>%
      select(country_name, "2018", change_in_pop)%>%
      rename(recent_pop = "2018")
    
    ### merge all data into one table
    
    first_merge = merge(emissions_cluster, gdp_cluster, by = 'country_name')
    final_cluster = merge(first_merge, pop_cluster, by = 'country_name')
    
    
    final_cluster$recent_pop = as.numeric(final_cluster$recent_pop)
    final_cluster$change_in_pop = as.numeric(final_cluster$change_in_pop)
    
    ## Clean for clustering
    for_cluster = final_cluster
    #for_cluster <- for_cluster[!is.infinite(rowSums(for_cluster)),]\
    for_cluster = na.omit(for_cluster) ## Remove NAs
    for_cluster[,2:7] <- for_cluster[,2:7][is.finite(rowSums(for_cluster[,2:7])),]
    
    
    
    for_cluster <- data.frame(for_cluster, scale(for_cluster[,2:7])) # standardize variables
    for_cluster = for_cluster %>%
      select(1, 8:13)
    
    
    fit <- kmeans(for_cluster[,2:7], 5) # 5 cluster solution
    for_cluster <- data.frame(for_cluster, fit$cluster)
    
    for_cluster = for_cluster %>%
      select(country_name, fit.cluster)
    
    data_and_cluster = merge(final_cluster, for_cluster, by = 'country_name')
    
    
    example_countries = data_and_cluster %>%
      filter(fit.cluster ==input$Cluster) %>%
      filter(change_in_co2 > 0)
    
    graph = ggplot(data = example_countries, aes(x = country_name, y = change_in_co2))
    graph + geom_bar(stat = 'identity' ,fill = "#B14932")
  })
  
  
})