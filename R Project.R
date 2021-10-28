library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)
emissions <- read.csv(file = "./co2_emissions.csv")


emissions <- fread("./co2_emissions.csv", header=T)



View(emissions)
x = emissions %>%
    pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'emission') %>%
    pivot_longer(cols = country_name, values_to = 'country') 


g = ggplot(data = x, aes(x = year))
g+ geom_bar(aes(y = emission),stat = 'identity') + 
  scale_y_discrete(name = 'CO2 Emissions (kt)')+ 
  scale_x_discrete(name = "Year", limits = c('1960','1965', '1970',
                                             '1975', '1980','1985' ,'1990',
                                             '1995', '2000','2005','2010','2018'))


histogram_1970 = emissions %>%
  pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'emission') %>%
  pivot_longer(cols = country_name, values_to = 'country') %>%
  filter(year == '1970')

histogram_2018 = emissions %>%
  pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'emission') %>%
  pivot_longer(cols = country_name, values_to = 'country') %>%
  filter(year == '2018')

View(histogram_1970)

hist = ggplot(data = histogram_1970, aes(emission))
hist+geom_histogram()

hist2 = ggplot(data = histogram_2018, aes(emission))
hist2+geom_histogram()

emissions$quartile = ntile(emissions$"2018", 4)

y = emissions %>%
  pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'emission') %>%
  pivot_longer(cols = country_name, values_to = 'country') %>%

  
z = emissions %>%
  group_by(quartile)


population <- fread("./population.csv", header=T)
View(population)

pop = population %>%
  pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'population') 
pop[,6] = sapply(pop[,6],as.numeric)

d = ggplot(data = pop, aes(x = year))
d+ geom_bar(aes(y = population),stat = 'identity') + 
  scale_y_discrete(name = 'CO2 Emissions (kt)')+ 
  scale_x_discrete(name = "Year", limits = c('1960','1965', '1970',
                                             '1975', '1980','1985' ,'1990',
                                             '1995', '2000','2005','2010','2018'))


gdp <- fread("./gdp.csv", header=T)

gdp2 = gdp %>% 
  pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'gdp') 

gdp_bar_chart = ggplot(data = gdp2, aes(x = year))
gdp_bar_chart + geom_bar(aes(y = gdp),stat = 'identity') + 
  scale_y_discrete(name = 'CO2 Emissions (kt)')+ 
  scale_x_discrete(name = "Year", limits = c('1960','1965', '1970',
                                             '1975', '1980','1985' ,'1990',
                                             '1995', '2000','2005','2010','2018'))

e = emissions
View(e)

e$quintile <- ntile(e$"2018" , 5)

quints = e %>%
  pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'emission') 

quints_grouped = select(quints, -1:-4)



emis_line = ggplot(data = quints_grouped, aes(x = year, y = emission, group = quintile, color = quintile))
emis_line + geom_point(lm= 'smooth') +
  scale_x_discrete(name = "Year", limits = c('1960','1965', '1970',
                                             '1975', '1980','1985' ,'1990',
                                             '1995', '2000','2005','2010','2018'))

                                             