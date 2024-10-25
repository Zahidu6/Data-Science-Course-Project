library(tidyverse)
library(maps)
library(viridis)
library(countrycode)
library(dplyr)
library(plotly)

world_map <- map_data("world")
world_map

getwd()
pf <- read.csv("wdi.csv")

pf$region <- as.factor(pf$region)
pf$gdpPercap <- as.factor(pf$gdpPercap)
pf$iso3c <- countrycode(pf$country, "country.name", "iso3c")
world_map$iso3c <- countrycode(world_map$region, "country.name", "iso3c")


gdp <- pf%>%
  filter(year %in% 2018)
leftjoin_data <- left_join(world_map, gdp, by="iso3c")
ggplot(leftjoin_data, aes(x= long, y= lat, group= group, 
                          fill= gdpPercap, text= country))+
  geom_polygon()+
  theme_void()+
  theme(legend.position = "none")+
  

?left_join

pf
WDIsearch("gdp per capita")
gdppc <- pf(indicator="itnet_pop")

gdppc <- gdppc %>%
  filter(year == 2019) %>% # Keep data for 2015 and for both sex
  select(country, NY.GDP.PCAP.CD) %>% # Select the two columns of interest
  rename(gdppc = NY.GDP.PCAP.CD) %>% # Rename columns
  mutate(iso3c = countrycode(country, "country.name", "iso3c"),
         lgdppc = log(gdppc))
world_map <- map_data("world")
world_map$iso3c <- countrycode(world_map$region, "country.name", "iso3c")
gdppc.map <- left_join(world_map, gdppc, by = "iso3c")
ggplot(gdppc.map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = lgdppc ), color = "white")+
  scale_fill_viridis_c() +
  theme_void() +
  labs(title = "Levels of Economic Development Around the World (2019)",
       subtitle = "Using GDP per capita (current US$)",
       fill = "GDP \nPer Capita \n(Logged)")
