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
# install.packages("reshape2")
library(reshape2)
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



# Without year (Total)
ui2 <- fluidPage(
  fluidRow(
    titlePanel('World Happiness Report'),
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

sever2 <- function(input, output){
  output$h_plot <- renderPlotly({
    whr %>%
      ggplot(aes(x = .data[[input$xindex_select]], y =.data[[input$yindex_select]],
                 size=Happiness_Rank,color=region)) +
      geom_point(alpha = 0.6)
  })
}

shinyApp(ui2, sever2)
