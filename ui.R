# https://rstudio.github.io/shinydashboard/examples.html
p.list=c('shinydashboard','shinyFiles','shiny','raster','dplyr')#,'jpeg')
# p.list=c('shiny','leaflet','dplyr','tidyr','tidyverse','shinydashboard','ggmap','inlmisc','leaflet.extras','plotly','lattice','plot3D','rgl','raster','mapview','sp','leafpop','ggplot2')
i.list=p.list[-which(p.list==intersect(p.list,installed.packages()))]
if(length(i.list)>0){install.packages(i.list)}

library(shinydashboard)
library(shinyFiles)
library(shiny)
library(raster)
library(dplyr)
# setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# library(jpeg)
masterdf=read.csv('./masterdf.csv')
c.data=unique(c(masterdf$player2,masterdf$player3,masterdf$player1))
#### Source Functions #####



#### Default Params ####
skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
  skin <- "blue"



#### UI: sidebar ####
sidebar <- dashboardSidebar(
  sidebarSearchForm(label = "Search...", "searchText", "searchButton"),
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    
    #textInput('main','Graph Title','','100%','Your title here'),
    radioButtons("metric", "Metric", choices=c('Percent' = 'pct', 'Count' ='count'), selected = "pct" ),
    selectizeInput("contestant", "Contestant", c('All',c.data),selected = 'All',multiple=TRUE),
    
    selectInput("order", 'Daily Double Sequence',multiple=T,
                                 choices= c( 'First' =1,'Second'=2, 'Third'=3),selected=c(1,2,3)),#),
    
    dateRangeInput('dateRange2', 
                  label = 'Episode Dates',#paste('Date range input 2: range is limited,',
                                # 'dd/mm/yy, language: fr, week starts on day 1 (Monday),',
                                # 'separator is "-", start view is year'),
                  start = min(masterdf$date),#Sys.Date() - 3, 
                  end = max(masterdf$date),#Sys.Date() + 3,
                  min =min(masterdf$date),# Sys.Date() - 10, 
                  max = max(masterdf$date),#Sys.Date() + 10,s
                  separator = " to ", format = "mm/dd/yy",startview = 'year', 
                  language = 'en', weekstart = 1)
  )
)
#### UI: Body ####
body <- dashboardBody(
  fluidPage(
    height=3000,
    
    tabItems(
      tabItem("dashboard",
              fluidRow(
                box(
                  title = "Daily Double Heat Map",
                  status = "primary",
                  plotOutput("plot1", height = 350),
                  height = 400,width = 10
                ),
                box(
                  title = "Dollar Value Heat Map",
                  status = "primary",
                  plotOutput("plot2", height = 350),
                  height = 400,width = 5
                ),
                box(
                  title = "Category Heat Map",
                  status = "primary",
                  plotOutput("plot3", height = 350),
                  height = 400,width = 5
                )# ),
                # box(title='text',
                #     textOutput('selected_var'),
                #     height = 350,width = 5)
              # box(
              #   title = "Plot Array",
              #   status = "primary",
              #   plotOutput("plot1", height = 200),
              #   plotOutput("plot2", height = 200),
              #   plotOutput("plot3", height = 200),
              #   height = 800,width = 15
              # )
              
      )
    )
  )
)

)
#### UI: Header ####

header <- dashboardHeader(
  titleWidth=300,
  title = "Daily Double Heat Map"#,
  # messages,
  # notifications,
  # tasks
)



## INPUTS 
# colorchoice
# plotxvar
# plotyvar

#### UI Define ####
ui <- dashboardPage(header, sidebar, body, skin = 'blue')
