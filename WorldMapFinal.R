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
library(xlsx)
# Downloan the data
whr <- read.xlsx("World_data.xls",sheetIndex = 1)
# Describe
library(psych)
psych::describe.by(whr,whr$year)

whr <- whr %>%
  filter(year==2020)

# Set the geo
g <- list(
  scope = 'world',
  showframe = FALSE,
  showcoastlines = TRUE,
  lakecolor = toRGB('white'),
  domain=list(column=1,row=1,x=list(0,1),y=list(0.1,0.8))
)

# Use plot_ly for graphic
p1 <- plot_ly()
p1 <- p1 %>% add_trace(
  type="choropleth",
  locations=whr$Country.name,
  locationmode = 'country names',
  z=whr$Life.Ladder,
  colorscale="RdBu",
  marker=list(line=list(width=0))
)
p1 <- p1 %>% layout(
  geo=g
)

p1

p2 <- plot_ly()
p2 <- p2 %>% add_trace(
  type="choropleth",
  locations=whr$Country,
  locationmode = 'country names',
  z=whr$GDP_per_capital,
  colorscale="RdBu",
  marker=list(line=list(width=0))
)
p2 <- p2 %>% layout(
  geo=g
)

p3 <- plot_ly()
p3 <- p3 %>% add_trace(
  type="choropleth",
  locations=whr$Country,
  locationmode = 'country names',
  z=whr$Social_support_Family,
  colorscale="RdBu",
  marker=list(line=list(width=0))
)
p3 <- p3 %>% layout(
  geo=g
)

p4 <- plot_ly()
p4 <- p4 %>% add_trace(
  type="choropleth",
  locations=whr$Country,
  locationmode = 'country names',
  z=whr$Health_Expectancy,
  colorscale="RdBu",
  marker=list(line=list(width=0))
)
p4 <- p4 %>%
  layout(
    geo=g
  )

p5 <- plot_ly()
p5 <- p5 %>% add_trace(
  type="choropleth",
  locations=whr$Country,
  locationmode = 'country names',
  z=whr$Freedom,
  colorscale="RdBu",
  marker=list(line=list(width=0))
)
p5 <- p5 %>%
  layout(
    geo=g
  )

p6 <- plot_ly()
p6 <- p6 %>% add_trace(
  type="choropleth",
  locations=whr$Country,
  locationmode = 'country names',
  z=whr$Perceptions_of_corruption,
  colorscale="RdBu",
  marker=list(line=list(width=0))
)
p6 <- p6 %>%
  layout(
    geo=g
  )

p7 <- plot_ly()
p7 <- p7 %>% add_trace(
  type="choropleth",
  locations=whr$Country,
  locationmode = 'country names',
  z=whr$Generosity,
  colorscale="RdBu",
  marker=list(line=list(width=0))
)
p7 <- p7 %>%
  layout(
    geo=g
  )

# Mapping indicators across world
# Here,change i and p1
i=7
fig <- p7 %>% colorbar(y=0.75)
fig <- fig %>% layout(
  font=list(family="Times New Roman")
)
# Title
fig <- fig %>% layout(annotations =
                        list(x = 0, y = 1, text = paste("<b>Mapping",CVars_yindex[i],"across world<b>"),
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='left', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=30, color="black",family="Times New Roman"))
)
# Subtitle
fig <- fig %>% layout(annotations =
                        list(x = 0, y = 0.94, text = paste(CVars_yindex[i],"world map base on Different Countries in 2021"),
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='left', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=25, color="black",family="Times New Roman"))
)
# Caption
fig <- fig %>% layout(annotations =
                        list(x = 1, y = 0.05, text = "Data from World Happiness Report 2015-2022",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=18, color="black",family="Times New Roman"))
)
fig <- fig %>% layout(annotations =
                        list(x = 1, y = 0, text = "Plot by @Wyatt624",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=18, color="black",family="Times New Roman"))
)

# save
setwd("C:/Users/wujia/Desktop/R_pre/World Map")
htmlwidgets::saveWidget(as_widget(fig), paste("World Map",CVars_yindex[i],"2021.html"))

