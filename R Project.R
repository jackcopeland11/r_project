library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)
emissions <- read.csv(file = "./co2_emissions.csv")
emissions <- fread("./co2_emissions.csv", header=T)

####Prepare and plot Emissions data ---------------------------------------------------------

x = emissions %>%
    pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'emission') %>%
    pivot_longer(cols = country_name, values_to = 'country') 


g = ggplot(data = x, aes(x = year))
g+ geom_bar(aes(y = emission),stat = 'identity') + 
  scale_y_discrete(name = 'CO2 Emissions (kt)')+ 
  scale_x_discrete(name = "Year", limits = c('1960','1965', '1970',
                                             '1975', '1980','1985' ,'1990',
                                             '1995', '2000','2005','2010','2018'))



###Test a specific year of emissions data to make sure it will filter properly with user input---------------------
histogram_1970 = emissions %>%
  pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'emission') %>%
  pivot_longer(cols = country_name, values_to = 'country') %>%
  filter(year == '1970')

histogram_2018 = emissions %>%
  pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'emission') %>%
  pivot_longer(cols = country_name, values_to = 'country') %>%
  filter(year == '2018')

hist = ggplot(data = histogram_1970, aes(emission))
hist+geom_histogram()

hist2 = ggplot(data = histogram_2018, aes(emission))
hist2+geom_histogram()



##### Break up emissions data into quartiles and prepare to graph ----------------------

emissions$quartile = ntile(emissions$"2018", 4)

y = emissions %>%
  pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'emission') %>%
  pivot_longer(cols = country_name, values_to = 'country') %>%


  
  
### prepare population data for EDA --------------------------------
population <- fread("./population.csv", header=T)

pop = population %>%
  pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'population') 
pop[,6] = sapply(pop[,6],as.numeric)

d = ggplot(data = pop, aes(x = year))
d+ geom_bar(aes(y = population),stat = 'identity') + 
  scale_y_discrete(name = 'CO2 Emissions (kt)')+ 
  scale_x_discrete(name = "Year", limits = c('1960','1965', '1970',
                                             '1975', '1980','1985' ,'1990',
                                             '1995', '2000','2005','2010','2018'))


### prepare gdp data for EDA --------------------------------

gdp <- fread("./gdp.csv", header=T)

gdp2 = gdp %>% 
  pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'gdp') 

gdp_bar_chart = ggplot(data = gdp2, aes(x = year))
gdp_bar_chart + geom_bar(aes(y = gdp),stat = 'identity') + 
  scale_y_discrete(name = 'CO2 Emissions (kt)')+ 
  scale_x_discrete(name = "Year", limits = c('1960','1965', '1970',
                                             '1975', '1980','1985' ,'1990',
                                             '1995', '2000','2005','2010','2018'))



#### Create quartile information and graph ------------------- 
e = emissions
View(e)

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

# Create percentage of emission by grouped quartile 
v = quints_grouped %>%
  group_by(quintile) %>%
  mutate(percent_of_emission = emission/sum(emission))
                                             



## Build Population Scatter Plots ---------------------------------------------------------------------------------
population <- fread("./population.csv" , header = T)
scatter_pop = population %>%
  pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'population') %>%
  pivot_longer(cols = country_name, values_to = 'country') %>%
  filter(country == "United States")

scatter_pop[,5] = sapply(scatter_pop[,5],as.numeric)
scatter = ggplot(data = scatter_pop , aes(x = year, y = population))
scatter + geom_point() + ggtitle("Population Over Time")+ theme(plot.title = element_text(hjust = 0.5))

