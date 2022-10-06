# clear the working environment
rm(list=ls())

# set up the working directory
setwd("F:/BIGDATA_R/Learning/WHR")
getwd()
# prepare packages
pacman::p_load("tidyverse", "hrbrthemes", "ggplot2", "tidyverse", "patchwork", "plotly", "viridis",
               "lubridate", "zoo", "shiny", "shinydashboard","forecast", "fpp2")

library(shiny)
library(ggthemes)
library(plotly)
whr <- read.csv("WorldHappinessReport.csv")

CVars_xindex <- c('GDP_per_capital','Social_support_Family','Health_Expectancy','Freedom','Perceptions_of_corruption','Generosity')
CVars_yindex <- c('Happiness_Score','GDP_per_capital','Social_support_Family','Health_Expectancy','Freedom','Perceptions_of_corruption','Generosity')
CVars_year <- sort(unique(whr$Year), decreasing = T)
CVars_country <- unique(whr$Country)
CVars_region <- unique(whr$region)

world = map_data("world") %>%
  filter(! long > 180)
colnames(world)[ colnames(world) == "region" ] = 'Country'

worldSubset <- merge(world, whr, by = "Country")


plot1 <- worldSubset %>%
  ggplot(mapping = aes( x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Happiness_Score )) +
  scale_fill_distiller(palette ="RdBu", direction = -1) + # or direction=1
  labs(x="Longitude",y="Latitude")

plot2 <- worldSubset %>%
  ggplot(mapping = aes( x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = GDP_per_capital )) +
  scale_fill_distiller(palette ="RdBu", direction = -1) + # or direction=1
  labs(x="Longitude",y="Latitude")

plot3 <- worldSubset %>%
  ggplot(mapping = aes( x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Social_support_Family )) +
  scale_fill_distiller(palette ="RdBu", direction = -1) + # or direction=1
  labs(x="Longitude",y="Latitude")

plot4 <- worldSubset %>%
  ggplot(mapping = aes( x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Health_Expectancy )) +
  scale_fill_distiller(palette ="RdBu", direction = -1) + # or direction=1
  labs(x="Longitude",y="Latitude")

plot5 <- worldSubset %>%
  ggplot(mapping = aes( x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Freedom )) +
  scale_fill_distiller(palette ="RdBu", direction = -1) + # or direction=1
  labs(x="Longitude",y="Latitude")

plot6 <- worldSubset %>%
  ggplot(mapping = aes( x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Perceptions_of_corruption )) +
  scale_fill_distiller(palette ="RdBu", direction = -1) + # or direction=1
  labs(x="Longitude",y="Latitude")

plot7 <- worldSubset %>%
  ggplot(mapping = aes( x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Generosity )) +
  scale_fill_distiller(palette ="RdBu", direction = -1) + # or direction=1
  labs(x="Longitude",y="Latitude")

library(plotly)
plot1 <- ggplotly(plot1)
plot2 <- ggplotly(plot2)
plot <- subplot(plot1, plot2, nrows = 2)%>%
  layout(showlegend = FALSE, coloraxis=list(colorscale='RdBu'),
         plot_bgcolor='#e5ecf6',
         xaxis = list(
           zerolinecolor = '#ffff',
           zerolinewidth = 2,
           gridcolor = 'ffff'),
         yaxis = list(
           zerolinecolor = '#ffff',
           zerolinewidth = 2,
           gridcolor = 'ffff'))
plot


install.packages("cowplot")
library(cowplot)
plot_grid(plot1,plot2,plot3,ncol = 3)
library(patchwork)
(plot1 | plot2)/(plot3 | plot4)
(plot5 | plot6 | plot7)
