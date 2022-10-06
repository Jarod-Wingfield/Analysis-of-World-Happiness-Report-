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
library(ggpubr)
whr <- read.csv("WorldHappinessReport.csv")

# whr %>%
#   filter(Year=='2015') %>%
#   ggplot(aes(x = GDP_per_capital, y = Happiness_Score)) +
#   geom_point()

CVars_xindex <- c('GDP_per_capital','Social_support_Family','Health_Expectancy','Freedom','Perceptions_of_corruption','Generosity')
CVars_yindex <- c('Happiness_Score','GDP_per_capital','Social_support_Family','Health_Expectancy','Freedom','Perceptions_of_corruption','Generosity')
CVars_year <- sort(unique(whr$Year), decreasing = T)
CVars_country <- unique(whr$Country)
CVars_region <- unique(whr$region)

# Different year scatter plot of two index.
# 气泡图
ui1 <- fluidPage(
  fluidRow(
    titlePanel('World Happiness Report'),
    box(selectInput(inputId = 'year_select',
                    label = 'Select the year',
                    choices = CVars_year)),
    box(selectInput(inputId = 'xindex_select',
                    label = 'Select the x Index',
                    choices = CVars_xindex)),
    box(selectInput(inputId = 'yindex_select',
                    label = 'Select the y Index',
                    choices = CVars_yindex))
  ),
  fluidRow(
    box(width = 9, status = 'info', title = 'World Happiness Report'),
    plotlyOutput('h_plot', height = 400)
  )
)

sever1 <- function(input, output){
  output$h_plot <- renderPlotly({
    whr %>%
      filter(Year == input$year_select) %>%
      ggplot(aes(x = .data[[input$xindex_select]], y =.data[[input$yindex_select]],
                 size=Happiness_Rank,color=region)) +
      geom_point(alpha = 0.6)
  })
}

shinyApp(ui1, sever1)


# Different Region country's year-Index.
# 折线图
ui2 <- fluidPage(
  fluidRow(
    titlePanel('World Happiness Report'),
    box(selectInput(inputId = 'region_select',
                label = 'Select the region',
                choices = CVars_region)),
    box(selectInput(inputId = 'yindex_select',
                label = 'Select the y Index',
                choices = CVars_yindex))
  ),
  fluidRow(
    box(width = 9, status = 'info', title = 'World Happiness Report'),
    plotlyOutput('h_plot', height = 400)
    )
)

sever2 <- function(input, output){
  output$h_plot <- renderPlotly({
    whr %>%
      filter(region == input$region_select) %>%
      ggplot(aes(x = Year, y =.data[[input$yindex_select]],color=Country)) +
      geom_line(alpha = 0.6)
  })
}

shinyApp(ui2, sever2)



world = map_data("world") %>%
  filter(! long > 180)
colnames(world)[ colnames(world) == "region" ] = 'Country'

worldSubset <- merge(world, whr, by = "Country")

# for (i in seq(1, 7)){
#   plot <- paste0('plot', i)
#   p <- worldSubset %>%
#     ggplot(mapping = aes( x = long, y = lat, group = group)) +
#     coord_fixed(1.3) +
#     geom_polygon(aes(fill = as.numeric(unlist(worldSubset[CVars_yindex[i]])))) +
#     scale_fill_distiller(palette ="RdBu", direction = -1) + # or direction=1
#     ggtitle("World Happiness Report")
#   assign(plot, p)
# }


# World Map
# Notice: Wait for some time.
ui3 <- fluidPage(
    fluidRow(
      titlePanel('WHR'),
      box(selectInput(inputId = 'yindex_select',
                  label = 'Select the y Index',
                  choices = CVars_yindex))
    ),
    fluidRow(
      box(width = 9, status = 'info'),
      plotlyOutput('h_plot', height = 400),
    )
  )

sever3 <- function(input, output){
  output$h_plot <- renderPlotly({
    worldSubset %>%
      ggplot(mapping = aes(x = long, y = lat, group = group)) +
      coord_fixed(1.3) +
      geom_polygon(aes(fill = .data[[input$yindex_select]])) +
      scale_fill_distiller(palette ="RdBu", direction = -1) + # or direction=1
      ggtitle("World Happiness Report")
  })
}

shinyApp(ui3, sever3)



