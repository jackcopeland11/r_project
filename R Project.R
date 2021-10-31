library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)
emissions <- fread("./co2_emissions.csv", header=T)
renewable <- fread("./renewable_energy.csv" , header = T) # <- % of total energy that comes from renewable sources
renew_kwh <- fread("./total_renewable_kwh.csv" , header = T) # <- Total energy (KwH) that comes from renewable sources
gdp <- fread('./gdp.csv', header = T)
pop <- fread('./population.csv', header=T)


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
View(e)
e = select(e, -1:-4)

quints = e %>% 
  pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'emission') %>%
  arrange("2018")
View(quints)


emis_line = ggplot(data = na.omit(quints), aes(x = year, y = emission, fill = as.factor(quintile)))
emis_line + geom_bar(stat = 'identity', position = 'dodge') +
  scale_y_discrete(name = 'CO2 Emissions (kt)')+
  scale_x_discrete(name = "Year",limits = c('1960','1965', '1970',
                                                      '1975', '1980','1985' ,'1990',
                                                      '1995', '2000','2005','2010','2018')) + coord_flip()


e = emissions
e$quintile <- ntile(e$"2018" , 5)
View(e)

quints = e %>% 
  pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'emission') %>%
  filter(quintile == 5)%>% filter(year == "2012")



emis_line = ggplot(data = na.omit(quints), aes(x = country_name, y = emission))
emis_line + geom_bar(stat = 'identity', position = 'dodge') +
  scale_y_discrete(name = 'CO2 Emissions (kt)')+
  scale_x_discrete(name = "Country") + coord_flip()






# Create percentage of emission by grouped quartile ------------

e = emissions
e$quintile <- ntile(e$"2018" , 5)

e = e %>% 
  pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'emission') 

total_emissions = sum(na.omit(emissions$"2018"))

v = e %>%
  group_by(quintile, year) %>% 
  summarise(percent_of_emission = (sum(emission)/total_emissions)*100) %>% 
  filter(year == "2018") 
                                            

percent_by_quintile= ggplot(data = na.omit(v), aes(x = as.factor(quintile), y = percent_of_emission))
percent_by_quintile + geom_bar(stat = 'identity', position = 'dodge')
  





###Renewable energy -------

ren = renew_kwh %>%
  pivot_longer(cols = (starts_with('19') | starts_with('20')), names_to = 'year' , values_to = 'kwh_renewable') %>%
  pivot_longer(cols = country_name, values_to = 'country') 

renew_bar_chart = ggplot(data = ren, aes(x = year, y =kwh_renewable ))
renew_bar_chart + geom_bar(stat = 'identity') +
  scale_x_discrete(name = "Year", limits = c('1990',
                                             '1995', '2000','2005','2010','2015'))

#### Join emissions and renewable energy dataset to see which segments are using
### the most renewable energy -----------------


e = emissions
e$quintile <- ntile(e$"2015" , 10)
e <- e %>%
  select(country_code, "2015", quintile)
e = e %>% rename(emissions = "2015")
View(e)
renew_for_merge = renew_kwh
rnm = renew_for_merge %>%
  select(country_name, country_code, "2015")
rnm = rnm %>% rename(renewable_energy = "2015" )

renew_and_emission = merge(e, rnm, by = 'country_code')

gg = ggplot(data = na.omit(renew_and_emission), aes(x = as.factor(quintile), y = renewable_energy, fill = as.factor(quintile)))
gg + geom_bar(stat = 'identity')

## Create "Successful country" metrics and charts -------------

m = emissions
m$quintile <- ntile(m$"2015" , 10)

largest_changes_good = m %>%
    mutate(change_in_co2 = m$"2015" - m$"2005") %>% 
  select(country_name, change_in_co2, quintile) %>% 
  filter(change_in_co2 < 0 ) %>%
  arrange(change_in_co2)

largest_change_bad = m %>%
  mutate(change_in_co2 = m$"2015" - m$"2005") %>% 
  select(country_name, change_in_co2, quintile) %>% 
  filter(change_in_co2>0 ) %>%
  arrange(desc(change_in_co2))

all_changes = m %>%
  mutate(change_in_co2 = m$"2015" - m$"2005") %>% 
  select(country_name, change_in_co2, quintile)

all_changes = all_changes %>%
  group_by(quintile) %>%
  summarise(change_in_co2 = sum(change_in_co2))

## Change in CO2 emissions by quintile 


change = ggplot(data = na.omit(all_changes), aes(x = as.factor(quintile), y = change_in_co2, fill = as.factor(quintile)))
change + geom_bar(stat = 'identity', na.rm = T)



#### Attempt at cluster analysis ------------


#Prepare emissions data for join
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
for_cluster = na.omit(for_cluster) ## Remove NAs
for_cluster <- data.frame(for_cluster, scale(for_cluster[,2:7])) # standardize variables
for_cluster = for_cluster %>%
  select(1, 8:13)


fit <- kmeans(for_cluster, 5) # 5 cluster solution
aggregate(for_cluster, by = list(fit$cluster), FUN= mean)

for_cluster <- data.frame(for_cluster, fit$cluster)
for_cluster = for_cluster %>%
  select(country_name, fit.cluster)

data_and_cluster = merge(final_cluster, for_cluster, by = 'country_name')

example_countries = data_and_cluster %>%
  filter(fit.cluster ==5) #%>%
  filter(change_in_co2 < 0)

graph = ggplot(data = example_countries, aes(x = country_name, y = change_in_co2))
graph + geom_bar(stat = 'identity')





###------------------------------------



#### Cluster Analysis ----------------------
#Prepare emissions data for join
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
for_cluster = na.omit(for_cluster) ## Remove NAs
for_cluster <- data.frame(for_cluster, scale(for_cluster[,2:7])) # standardize variables
for_cluster = for_cluster %>%
  select(1, 8:13)


fit <- kmeans(for_cluster, 5) # 5 cluster solution
aggregate(for_cluster, by = list(fit$cluster), FUN= mean)

for_cluster <- data.frame(for_cluster, fit$cluster)
for_cluster = for_cluster %>%
  select(country_name, fit.cluster)

data_and_cluster = merge(final_cluster, for_cluster, by = 'country_name')

example_countries = data_and_cluster %>%
  filter(fit.cluster == 2) %>%
  filter(change_in_co2 < 0)

graph = ggplot(data = example_countries, aes(x = country_name, y = change_in_co2))
graph + geom_bar(stat = 'identity')

























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
  filter(fit.cluster == 2) %>%
  filter(change_in_co2 < 0)

graph = ggplot(data = example_countries, aes(x = country_name, y = change_in_co2))
graph + geom_bar(stat = 'identity')


