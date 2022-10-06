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

CVars_xindex <- c('GDP_per_capital','Social_support_Family','Health_Expectancy','Freedom','Perceptions_of_corruption','Generosity',"Residual")
CVars_yindex <- c('Happiness_Score','GDP_per_capital','Social_support_Family','Health_Expectancy','Freedom','Perceptions_of_corruption','Generosity',"Residual")
CVars_year <- sort(unique(whr$Year), decreasing = T)
CVars_country <- unique(whr$Country)
CVars_region <- unique(whr$region)


h_year=group_by(whr, Year) %>%
  select(-c('Country','region','Happiness_Rank','Happiness_Score')) %>%
  summarize_each(funs(mean))

h <- melt(h_year,id="Year")

# World situation
# Time trend of different indicators from 2014 to 2021
fig <- plot_ly(h, x = ~Year, y = ~value, color = ~variable) %>%
  add_lines()%>%
  layout(
    font=list(family="Time News Roman"),
    yaxis=list(domain=c(0.1,0.9)),
    legend=list(y=0.5),
    plot_bgcolor='#e5ecf6'
  )
# Title
fig <- fig %>% layout(annotations =
                       list(x = 0, y = 1, text = "<b>Time trend of Seven Indicators from 2014 to 2021<b>",
                            showarrow = F, xref='paper', yref='paper',
                            xanchor='left', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=30, color="black",family="Times New Roman"))
)
# Caption
fig <- fig %>% layout(annotations =
                        list(x = 1, y = 0.025, text = "Data from World Happiness Report 2015-2022",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=15, color="black",family="Times New Roman"))
)
fig <- fig %>% layout(annotations =
                        list(x = 1, y = 0, text = "Plot by @Wyatt624",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=15, color="black",family="Times New Roman"))
)
fig

# Time trend of different indicators from 2014 to 2021
fig <- plot_ly(h, x = ~Year, y = ~value, color = ~variable, type = 'bar')
fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack') %>%
  layout(
    font=list(family="Time News Roman"),
    yaxis=list(domain=c(0.1,0.9)),
    legend=list(y=0.5,font=list(size=20)),
    plot_bgcolor='#e5ecf6'
  )
fig
# Title
fig <- fig %>% layout(annotations =
                        list(x = 0, y = 1, text = "<b>Stacked Bar Chart of Seven Indicators From 2014 to 2021<b>",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='left', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=30, color="black",family="Times New Roman"))
)
# Caption
fig <- fig %>% layout(annotations =
                        list(x = 1, y = -0.01, text = "Data from World Happiness Report 2015-2022",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=20, color="black",family="Times New Roman"))
)
fig <- fig %>% layout(annotations =
                        list(x = 1, y = -0.04, text = "Plot by @Wyatt624",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=20, color="black",family="Times New Roman"))
)
fig















for (i in seq(1,8)) {
  h <- paste0('h', i)
  l <- whr%>%
    select('Year','region',CVars_yindex[i])%>%
    group_by(Year, region)%>%
    summarize_each(funs(mean))
  assign(h, l)
}
H1 <- melt(h1,id=c("region","Year"))
H2 <- melt(h2,id=c("region","Year"))
H3 <- melt(h3,id=c("region","Year"))
H4 <- melt(h4,id=c("region","Year"))
H5 <- melt(h5,id=c("region","Year"))
H6 <- melt(h6,id=c("region","Year"))
H7 <- melt(h7,id=c("region","Year"))
H8 <- melt(h8,id=c("region","Year"))

f1 <- plot_ly(H1, x = ~Year, y = ~value ,color = ~region)
f1 <- f1 %>% add_lines()%>%layout(title='Time trend of Happiness_Score from 2014 to 2021')
f2 <- plot_ly(H2, x = ~Year, y = ~value ,color = ~region)
f2 <- f2 %>% add_lines()%>%layout(title='Time trend of GDP_per_capital from 2014 to 2021')
f3 <- plot_ly(H3, x = ~Year, y = ~value ,color = ~region)
f3 <- f3 %>% add_lines()%>%layout(title='Time trend of Social_spport_Family from 2014 to 2021')
f4 <- plot_ly(H4, x = ~Year, y = ~value ,color = ~region)
f4 <- f4 %>% add_lines()%>%layout(title='Time trend of Health_Expectancy from 2014 to 2021')
f5 <- plot_ly(H5, x = ~Year, y = ~value ,color = ~region)
f5 <- f5 %>% add_lines()%>%layout(title='Time trend of Freedom from 2014 to 2021')
f6 <- plot_ly(H6, x = ~Year, y = ~value ,color = ~region)
f6 <- f6 %>% add_lines()%>%layout(title='Time trend of Perceptions_of_corruption from 2014 to 2021')
f7 <- plot_ly(H7, x = ~Year, y = ~value ,color = ~region)
f7 <- f7 %>% add_lines()%>%layout(title='Time trend of Generosity from 2014 to 2021')
f8 <- plot_ly(H8, x = ~Year, y = ~value ,color = ~region)
f8 <- f8 %>% add_lines()%>%layout(title='Time trend of Residual from 2014 to 2021')
# Time trend of Seven indicators from 2014 to 2021
# Group by Region
f1 <- plot_ly(H1, x = ~Year, y = ~value ,color = ~region)%>%
  add_lines()%>%
  layout(
  font=list(family="Time News Roman"),
  yaxis=list(domain=c(0.1,0.9)),
  legend=list(y=0.5),
  plot_bgcolor='#e5ecf6'
  )
# Title
fig <- f1 %>% layout(annotations =
                        list(x = 0, y = 1, text = "<b>Time trend of Happiness_Score from 2014 to 2021<b>",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='left', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=30, color="black",family="Times New Roman"))
)
# Caption
fig <- fig %>% layout(annotations =
                        list(x = 1, y = 0.025, text = "Data from World Happiness Report 2015-2022",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=15, color="black",family="Times New Roman"))
)
fig <- fig %>% layout(annotations =
                        list(x = 1, y = 0, text = "Plot by @Wyatt624",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=15, color="black",family="Times New Roman"))
)
fig

f2 <- plot_ly(H2, x = ~Year, y = ~value ,color = ~region)%>%
  add_lines()%>%
  layout(
    font=list(family="Time News Roman"),
    yaxis=list(domain=c(0.1,0.9)),
    legend=list(y=0.5),
    plot_bgcolor='#e5ecf6'
  )
# Title
fig <- f2 %>% layout(annotations =
                       list(x = 0, y = 1, text = "<b>Time trend of GDP_per_capital from 2014 to 2021<b>",
                            showarrow = F, xref='paper', yref='paper',
                            xanchor='left', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=30, color="black",family="Times New Roman"))
)
# Caption
fig <- fig %>% layout(annotations =
                        list(x = 1, y = 0.025, text = "Data from World Happiness Report 2015-2022",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=15, color="black",family="Times New Roman"))
)
fig <- fig %>% layout(annotations =
                        list(x = 1, y = 0, text = "Plot by @Wyatt624",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=15, color="black",family="Times New Roman"))
)
fig

f3 <- plot_ly(H3, x = ~Year, y = ~value ,color = ~region)%>%
  add_lines()%>%
  layout(
    font=list(family="Time News Roman"),
    yaxis=list(domain=c(0.1,0.9)),
    legend=list(y=0.5),
    plot_bgcolor='#e5ecf6'
  )
# Title
fig <- f3 %>% layout(annotations =
                       list(x = 0, y = 1, text = "<b>Time trend of Social_support_Family from 2014 to 2021<b>",
                            showarrow = F, xref='paper', yref='paper',
                            xanchor='left', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=30, color="black",family="Times New Roman"))
)
# Caption
fig <- fig %>% layout(annotations =
                        list(x = 1, y = 0.025, text = "Data from World Happiness Report 2015-2022",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=15, color="black",family="Times New Roman"))
)
fig <- fig %>% layout(annotations =
                        list(x = 1, y = 0, text = "Plot by @Wyatt624",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=15, color="black",family="Times New Roman"))
)
fig

f4 <- plot_ly(H4, x = ~Year, y = ~value ,color = ~region)%>%
  add_lines()%>%
  layout(
    font=list(family="Time News Roman"),
    yaxis=list(domain=c(0.1,0.9)),
    legend=list(y=0.5),
    plot_bgcolor='#e5ecf6'
  )
# Title
fig <- f4 %>% layout(annotations =
                       list(x = 0, y = 1, text = "<b>Time trend of Health_Expectancy from 2014 to 2021<b>",
                            showarrow = F, xref='paper', yref='paper',
                            xanchor='left', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=30, color="black",family="Times New Roman"))
)
# Caption
fig <- fig %>% layout(annotations =
                        list(x = 1, y = 0.025, text = "Data from World Happiness Report 2015-2022",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=15, color="black",family="Times New Roman"))
)
fig <- fig %>% layout(annotations =
                        list(x = 1, y = 0, text = "Plot by @Wyatt624",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=15, color="black",family="Times New Roman"))
)
fig

f5 <- plot_ly(H5, x = ~Year, y = ~value ,color = ~region)%>%
  add_lines()%>%
  layout(
    font=list(family="Time News Roman"),
    yaxis=list(domain=c(0.1,0.9)),
    legend=list(y=0.5),
    plot_bgcolor='#e5ecf6'
  )
# Title
fig <- f5 %>% layout(annotations =
                       list(x = 0, y = 1, text = "<b>Time trend of Freedom from 2014 to 2021<b>",
                            showarrow = F, xref='paper', yref='paper',
                            xanchor='left', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=30, color="black",family="Times New Roman"))
)
# Caption
fig <- fig %>% layout(annotations =
                        list(x = 1, y = 0.025, text = "Data from World Happiness Report 2015-2022",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=15, color="black",family="Times New Roman"))
)
fig <- fig %>% layout(annotations =
                        list(x = 1, y = 0, text = "Plot by @Wyatt624",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=15, color="black",family="Times New Roman"))
)
fig


f6 <- plot_ly(H6, x = ~Year, y = ~value ,color = ~region)%>%
  add_lines()%>%
  layout(
    font=list(family="Time News Roman"),
    yaxis=list(domain=c(0.1,0.9)),
    legend=list(y=0.5),
    plot_bgcolor='#e5ecf6'
  )
# Title
fig <- f6 %>% layout(annotations =
                       list(x = 0, y = 1, text = "<b>Time trend of Perceptions_of_corruption from 2014 to 2021<b>",
                            showarrow = F, xref='paper', yref='paper',
                            xanchor='left', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=30, color="black",family="Times New Roman"))
)
# Caption
fig <- fig %>% layout(annotations =
                        list(x = 1, y = 0.025, text = "Data from World Happiness Report 2015-2022",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=15, color="black",family="Times New Roman"))
)
fig <- fig %>% layout(annotations =
                        list(x = 1, y = 0, text = "Plot by @Wyatt624",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=15, color="black",family="Times New Roman"))
)
fig

f7 <- plot_ly(H7, x = ~Year, y = ~value ,color = ~region)%>%
  add_lines()%>%
  layout(
    font=list(family="Time News Roman"),
    yaxis=list(domain=c(0.1,0.9)),
    legend=list(y=0.5),
    plot_bgcolor='#e5ecf6'
  )
# Title
fig <- f7 %>% layout(annotations =
                       list(x = 0, y = 1, text = "<b>Time trend of Generosity from 2014 to 2021<b>",
                            showarrow = F, xref='paper', yref='paper',
                            xanchor='left', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=30, color="black",family="Times New Roman"))
)
# Caption
fig <- fig %>% layout(annotations =
                        list(x = 1, y = 0.025, text = "Data from World Happiness Report 2015-2022",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=15, color="black",family="Times New Roman"))
)
fig <- fig %>% layout(annotations =
                        list(x = 1, y = 0, text = "Plot by @Wyatt624",
                             showarrow = F, xref='paper', yref='paper',
                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                             font=list(size=15, color="black",family="Times New Roman"))
)
fig


# World Situation 2014-2021
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
