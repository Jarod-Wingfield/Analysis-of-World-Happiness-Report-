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

CVars_xindex <- c('GDP_per_capital','Social_support_Family','Health_Expectancy','Freedom','Perceptions_of_corruption','Generosity')
CVars_yindex <- c('Happiness_Score','GDP_per_capital','Social_support_Family','Health_Expectancy','Freedom','Perceptions_of_corruption','Generosity')
CVars_year <- sort(unique(whr$Year), decreasing = T)
CVars_country <- unique(whr$Country)
CVars_region <- unique(whr$region)

whr<- whr%>%filter(Year==2021)

fig1 <- plot_ly(x = whr$Country, y = whr$GDP_per_capital, type = 'bar')


# Happiness in countries with extreme economic contribution levels
p1 <- plot_ly(data = whr,
              x = ~reorder(Country,Happiness_Score),
              y = ~Happiness_Score,
              type = "bar",
              showlegend=FALSE,
              marker=list(color=~Happiness_Score,showscale=FALSE))
m <- whr[with(whr,order(-GDP_per_capital)),]
m <- m[1:10,]
n <- whr[with(whr,order(GDP_per_capital)),]
n <- n[1:10,]
# annotations
p1 <- p1 %>% add_annotations( x = m$Country,
                              y = m$Happiness_Score,
                              text = c('1','2','3','4','5','6','7','8','9','10'),
                              xref = "x",
                              yref = "y",
                              showarrow = c(TRUE, TRUE, TRUE)) %>%
  add_annotations(x = n$Country,
                y = n$Happiness_Score,
                text = c("-1","-2","-3","-4","-5","-6",'-7','-8','-9','-10'),
                xref = "x",
                yref = "y",
                showarrow = c(TRUE, TRUE, TRUE))

p2 <- plot_ly(data=whr,
  x = ~Country, y ="GDP_per_capital",
  z = t(data.matrix(whr$GDP_per_capital)),type = "heatmap",colorbar=list(y=0.5)
)
p2

fig <- subplot(p1,p2,nrows=2,shareX=TRUE,shareY=TRUE)
fig <- fig %>% layout(
  font=list(family="Times New Roman"),
  yaxis = list(domain = c(0.15,0.8)),
  yaxis2 = list(domain = c(0,0.1))
)
# Title
fig <- fig %>% layout(annotations =
                        list(x = 0, y = 1, text = "<b>Happiness in countries with extreme economic contribution levels<b>",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='left', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=25, color="black",family="Times New Roman"))
)
# Subtitle
fig <- fig %>% layout(annotations =
                        list(x = 0, y = 0.94, text = "Histogram of Happiness Score and Heat Map of GDP_per_capital in Different Countries in 2021",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='left', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=18, color="black",family="Times New Roman"))
)
# Caption
fig <- fig %>% layout(annotations =
                        list(x = 1, y = -0.3, text = "Data from World Happiness Report 2015-2022",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=13, color="black",family="Times New Roman"))
                      )
fig <- fig %>% layout(annotations =
                        list(x = 1, y = -0.33, text = "Plot by @Wyatt624",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=13, color="black",family="Times New Roman"))
)
fig

# Happiness in countries with extreme Social_support_Family contribution levels
p1 <- plot_ly(data = whr,
              x = ~reorder(Country,Happiness_Score),
              y = ~Happiness_Score,
              type = "bar",
              showlegend=FALSE,
              marker=list(color=~Happiness_Score,showscale=FALSE))
m <- whr[with(whr,order(-Social_support_Family)),]
m <- m[1:10,]
n <- whr[with(whr,order(Social_support_Family)),]
n <- n[1:10,]
# annotations
p1 <- p1 %>% add_annotations( x = m$Country,
                              y = m$Happiness_Score,
                              text = c('1','2','3','4','5','6','7','8','9','10'),
                              xref = "x",
                              yref = "y",
                              showarrow = c(TRUE, TRUE, TRUE)) %>%
  add_annotations(x = n$Country,
                  y = n$Happiness_Score,
                  text = c("-1","-2","-3","-4","-5","-6",'-7','-8','-9','-10'),
                  xref = "x",
                  yref = "y",
                  showarrow = c(TRUE, TRUE, TRUE))

p2 <- plot_ly(data=whr,
              x = ~Country, y ="Social_support_Family",
              z = t(data.matrix(whr$Social_support_Family)),type = "heatmap",colorbar=list(y=0.5)
)

fig <- subplot(p1,p2,nrows=2,shareX=TRUE,shareY=TRUE)
fig <- fig %>% layout(
  font=list(family="Times New Roman"),
  yaxis = list(domain = c(0.15,0.8)),
  yaxis2 = list(domain = c(0,0.1))
)
# Title
fig <- fig %>% layout(annotations =
                        list(x = 0, y = 1, text = "<b>Happiness in countries with extreme Social_support_Family contribution levels<b>",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='left', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=25, color="black",family="Times New Roman"))
)
# Subtitle
fig <- fig %>% layout(annotations =
                        list(x = 0, y = 0.94, text = "Histogram of Happiness Score and Heat Map of Social_support_Family in Different Countries in 2021",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='left', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=18, color="black",family="Times New Roman"))
)
# Caption
fig <- fig %>% layout(annotations =
                        list(x = 1, y = -0.3, text = "Data from World Happiness Report 2015-2022",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=13, color="black",family="Times New Roman"))
)
fig <- fig %>% layout(annotations =
                        list(x = 1, y = -0.33, text = "Plot by @Wyatt624",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=13, color="black",family="Times New Roman"))
)
fig



# Happiness in countries with extreme Health_expectancy contribution levels
p1 <- plot_ly(data = whr,
              x = ~reorder(Country,Happiness_Score),
              y = ~Happiness_Score,
              type = "bar",
              showlegend=FALSE,
              marker=list(color=~Happiness_Score,showscale=FALSE))
m <- whr[with(whr,order(-Health_Expectancy)),]
m <- m[1:10,]
n <- whr[with(whr,order(Health_Expectancy)),]
n <- n[1:10,]
# annotations
p1 <- p1 %>% add_annotations( x = m$Country,
                              y = m$Happiness_Score,
                              text = c('1','2','3','4','5','6','7','8','9','10'),
                              xref = "x",
                              yref = "y",
                              showarrow = c(TRUE, TRUE, TRUE)) %>%
  add_annotations(x = n$Country,
                  y = n$Happiness_Score,
                  text = c("-1","-2","-3","-4","-5","-6",'-7','-8','-9','-10'),
                  xref = "x",
                  yref = "y",
                  showarrow = c(TRUE, TRUE, TRUE))

p2 <- plot_ly(data=whr,
              x = ~Country, y ="Health_Expectancy",
              z = t(data.matrix(whr$Health_Expectancy)),type = "heatmap",colorbar=list(y=0.5)
)

fig <- subplot(p1,p2,nrows=2,shareX=TRUE,shareY=TRUE)
fig <- fig %>% layout(
  font=list(family="Times New Roman"),
  yaxis = list(domain = c(0.15,0.8)),
  yaxis2 = list(domain = c(0,0.1))
)
# Title
fig <- fig %>% layout(annotations =
                        list(x = 0, y = 1, text = "<b>Happiness in countries with extreme Health_expectancy contribution levels<b>",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='left', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=25, color="black",family="Times New Roman"))
)
# Subtitle
fig <- fig %>% layout(annotations =
                        list(x = 0, y = 0.94, text = "Histogram of Happiness Score and Heat Map of Health_expectancy in Different Countries in 2021",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='left', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=18, color="black",family="Times New Roman"))
)
# Caption
fig <- fig %>% layout(annotations =
                        list(x = 1, y = -0.3, text = "Data from World Happiness Report 2015-2022",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=13, color="black",family="Times New Roman"))
)
fig <- fig %>% layout(annotations =
                        list(x = 1, y = -0.33, text = "Plot by @Wyatt624",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=13, color="black",family="Times New Roman"))
)
fig


# Happiness in countries with extreme Freedom contribution levels
p1 <- plot_ly(data = whr,
              x = ~reorder(Country,Happiness_Score),
              y = ~Happiness_Score,
              type = "bar",
              showlegend=FALSE,
              marker=list(color=~Happiness_Score,showscale=FALSE))
m <- whr[with(whr,order(-Freedom)),]
m <- m[1:10,]
n <- whr[with(whr,order(Freedom)),]
n <- n[1:10,]
# annotations
p1 <- p1 %>% add_annotations( x = m$Country,
                              y = m$Happiness_Score,
                              text = c('1','2','3','4','5','6','7','8','9','10'),
                              xref = "x",
                              yref = "y",
                              showarrow = c(TRUE, TRUE, TRUE)) %>%
  add_annotations(x = n$Country,
                  y = n$Happiness_Score,
                  text = c("-1","-2","-3","-4","-5","-6",'-7','-8','-9','-10'),
                  xref = "x",
                  yref = "y",
                  showarrow = c(TRUE, TRUE, TRUE))

p2 <- plot_ly(data=whr,
              x = ~Country, y ="Freedom",
              z = t(data.matrix(whr$Freedom)),type = "heatmap",colorbar=list(y=0.5)
)

fig <- subplot(p1,p2,nrows=2,shareX=TRUE,shareY=TRUE)
fig <- fig %>% layout(
  font=list(family="Times New Roman"),
  yaxis = list(domain = c(0.15,0.8)),
  yaxis2 = list(domain = c(0,0.1))
)
# Title
fig <- fig %>% layout(annotations =
                        list(x = 0, y = 1, text = "<b>Happiness in countries with extreme Freedom contribution levels<b>",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='left', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=25, color="black",family="Times New Roman"))
)
# Subtitle
fig <- fig %>% layout(annotations =
                        list(x = 0, y = 0.94, text = "Histogram of Happiness Score and Heat Map of Freedom in Different Countries in 2021",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='left', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=18, color="black",family="Times New Roman"))
)
# Caption
fig <- fig %>% layout(annotations =
                        list(x = 1, y = -0.3, text = "Data from World Happiness Report 2015-2022",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=13, color="black",family="Times New Roman"))
)
fig <- fig %>% layout(annotations =
                        list(x = 1, y = -0.33, text = "Plot by @Wyatt624",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=13, color="black",family="Times New Roman"))
)
fig


# Happiness in countries with extreme Perceptions_of_corruption contribution levels
p1 <- plot_ly(data = whr,
              x = ~reorder(Country,Happiness_Score),
              y = ~Happiness_Score,
              type = "bar",
              showlegend=FALSE,
              marker=list(color=~Happiness_Score,showscale=FALSE))
m <- whr[with(whr,order(-Perceptions_of_corruption)),]
m <- m[1:10,]
n <- whr[with(whr,order(Perceptions_of_corruption)),]
n <- n[1:10,]
# annotations
p1 <- p1 %>% add_annotations( x = m$Country,
                              y = m$Happiness_Score,
                              text = c('1','2','3','4','5','6','7','8','9','10'),
                              xref = "x",
                              yref = "y",
                              showarrow = c(TRUE, TRUE, TRUE)) %>%
  add_annotations(x = n$Country,
                  y = n$Happiness_Score,
                  text = c("-1","-2","-3","-4","-5","-6",'-7','-8','-9','-10'),
                  xref = "x",
                  yref = "y",
                  showarrow = c(TRUE, TRUE, TRUE))

p2 <- plot_ly(data=whr,
              x = ~Country, y ="Perceptions_of_corruption",
              z = t(data.matrix(whr$Perceptions_of_corruption)),type = "heatmap",colorbar=list(y=0.5)
)

fig <- subplot(p1,p2,nrows=2,shareX=TRUE,shareY=TRUE)
fig <- fig %>% layout(
  font=list(family="Times New Roman"),
  yaxis = list(domain = c(0.15,0.8)),
  yaxis2 = list(domain = c(0,0.1))
)
# Title
fig <- fig %>% layout(annotations =
                        list(x = 0, y = 1, text = "<b>Happiness in countries with extreme Perceptions_of_corruption contribution levels<b>",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='left', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=25, color="black",family="Times New Roman"))
)
# Subtitle
fig <- fig %>% layout(annotations =
                        list(x = 0, y = 0.94, text = "Histogram of Happiness Score and Heat Map of Perceptions_of_corruption in Different Countries in 2021",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='left', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=18, color="black",family="Times New Roman"))
)
# Caption
fig <- fig %>% layout(annotations =
                        list(x = 1, y = -0.3, text = "Data from World Happiness Report 2015-2022",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=13, color="black",family="Times New Roman"))
)
fig <- fig %>% layout(annotations =
                        list(x = 1, y = -0.33, text = "Plot by @Wyatt624",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=13, color="black",family="Times New Roman"))
)
fig



# Happiness in countries with extreme Generosity contribution levels
p1 <- plot_ly(data = whr,
              x = ~reorder(Country,Happiness_Score),
              y = ~Happiness_Score,
              type = "bar",
              showlegend=FALSE,
              marker=list(color=~Happiness_Score,showscale=FALSE))
m <- whr[with(whr,order(-Generosity)),]
m <- m[1:10,]
n <- whr[with(whr,order(Generosity)),]
n <- n[1:10,]
# annotations
p1 <- p1 %>% add_annotations( x = m$Country,
                              y = m$Happiness_Score,
                              text = c('1','2','3','4','5','6','7','8','9','10'),
                              xref = "x",
                              yref = "y",
                              showarrow = c(TRUE, TRUE, TRUE)) %>%
  add_annotations(x = n$Country,
                  y = n$Happiness_Score,
                  text = c("-1","-2","-3","-4","-5","-6",'-7','-8','-9','-10'),
                  xref = "x",
                  yref = "y",
                  showarrow = c(TRUE, TRUE, TRUE))

p2 <- plot_ly(data=whr,
              x = ~Country, y ="Generosity",
              z = t(data.matrix(whr$Generosity)),type = "heatmap",colorbar=list(y=0.5)
)

fig <- subplot(p1,p2,nrows=2,shareX=TRUE,shareY=TRUE)
fig <- fig %>% layout(
  font=list(family="Times New Roman"),
  yaxis = list(domain = c(0.15,0.8)),
  yaxis2 = list(domain = c(0,0.1))
)
# Title
fig <- fig %>% layout(annotations =
                        list(x = 0, y = 1, text = "<b>Happiness in countries with extreme Generosity contribution levels<b>",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='left', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=25, color="black",family="Times New Roman"))
)
# Subtitle
fig <- fig %>% layout(annotations =
                        list(x = 0, y = 0.94, text = "Histogram of Happiness Score and Heat Map of Generosity in Different Countries in 2021",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='left', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=18, color="black",family="Times New Roman"))
)
# Caption
fig <- fig %>% layout(annotations =
                        list(x = 1, y = -0.3, text = "Data from World Happiness Report 2015-2022",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=13, color="black",family="Times New Roman"))
)
fig <- fig %>% layout(annotations =
                        list(x = 1, y = -0.33, text = "Plot by @Wyatt624",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=13, color="black",family="Times New Roman"))
)
fig
