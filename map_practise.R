library(tidyverse)
library(maps) # map package
install.packages("viridis")
library(viridis)
world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white") +
  theme_void()

library(countrycode)
data(codelist)
asian_cs <- codelist %>%
  filter(continent == "Asia") %>%
  mutate(cname = country.name.en) %>%
  select(cname)
asian_cs %>% slice(1:10)

asia_map <- map_data("world", region = asian_cs$cname)

ggplot(asia_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white") +
  theme_void()


region.lab.data <- world_map %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))
region.lab.data




ggplot(world_map, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = region))+
  #geom_text(aes(label = region), data = region.lab.data, size = 1)+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")


library(dplyr)
library(plotly)
# Rorder data + Add a new column with tooltip text
data <- data %>%
  arrange("pop") %>%
  mutate( name=factor(name, unique(name))) %>%
  mutate( mytext=paste(
    "City: ", name, "\n",
    "Population: ", pop, sep=""))

?arrange

p <- data %>%
  ggplot() +
  geom_polygon(data = ROK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=data, aes(x=long, y=lat, size=pop, color=pop, text=mytext)) +
  scale_color_viridis(trans="log") +
  theme_void() + coord_map() +
  labs(title = "Cities in South Korea")

p <- ggplotly(p, tooltip="text")
p

getwd()
setwd()
inr <- read.csv("NRI_2020.csv")
inr


ggplot(inr, aes(x = long, y = lat)) +
  geom_polygon(aes(fill = nri_rank))+
  #geom_text(aes(label = region), data = region.lab.data, size = 1)+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")

fac_nir <- factor(inr)
fac_nir

is.numeric("nir")
is.factor("nir")
f <- numeric(inr)





library("WDI")
WDIsearch("gdp per capita")
gdppc <- WDI(country="all", indicator="NY.GDP.PCAP.CD")


