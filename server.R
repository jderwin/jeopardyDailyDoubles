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
#### server ####
server <- function(input, output) {
  # output=list()
  library(shinydashboard)
  library(shiny)
  library(raster)
  library(dplyr)
  
  all.indata=reactive({ # allows data to be accessed reactively w. conductor
    masterdf=read.csv('./masterdf.csv')
    
    d2=masterdf
    dateRange2=input$dateRange2
    metric=input$metric
    contestant=input$contestant
    order=input$order
    
    # input=list()
    # input$dateRange2=c("1983-09-18","2025-04-18")
    # input$metric='pct'
    # input$contestant='All'
    # input$order=c('first','second','third')
    
    if(contestant == 'All'){
      d2==d2
    }else{
      if(length(contestant)>1){
        d2=d2[which(rowSums(d2[,c('player1','player2','player3')] %in% contestant)>0),]
      }else{
        d2=d2[which(rowSums(d2[,c('player1','player2','player3')] == contestant)>0),]
      }
    }
    if(length(which(d2$date>=dateRange2[1]&d2$date<=dateRange2[2]))>0){
      d2=d2[which(d2$date>=dateRange2[1]&d2$date<=dateRange2[2]),]
    }
    if(length(match(input$order, c('first','second','third')))<3){
      d2=d2[which(d2$order %in% order),]
    }
    all.indata=list('d2'=d2,'metric'=metric)
    
    
    return(all.indata)
  })
  
  
  
  
  
  
  
  output$plot3 <- renderPlot({ 
    d2=all.indata()$d2
    metric=all.indata()$metric
    d2.summary=d2 %>% count(row,col)
    d2.col.summary=aggregate(d2.summary$n,FUN='sum',by=list(d2.summary$col))
    d2.col.summary$pct=round(100*d2.col.summary$x/sum(d2.col.summary$x),2)
    d2.col.summary$m=d2.col.summary$pct
    if(metric=='pct'){d2.col.summary$m=d2.col.summary$pct}else{d2.col.summary$m=d2.col.summary$x}
    r.col=matrix(ncol=6,nrow=1,data=d2.col.summary$m)
    r2.col=raster(r.col)
    extent(r2.col) <- c(0, 1, 0, 0.2)
    
    plot(r2.col,axes=FALSE, box=FALSE)
    text(r2.col,digits=2)
    text(seq(0.08,0.94,0.17),rep(0.25,6),paste0('C',1:6))
  })
  output$plot2 <- renderPlot({ 
    d2=all.indata()$d2
    metric=all.indata()$metric
    d2.summary=d2 %>% count(row,col)
    d2.row.summary=aggregate(d2.summary$n,FUN='sum',by=list(d2.summary$row))
    d2.row.summary$pct=round(100*d2.row.summary$x/sum(d2.row.summary$x),2)
    d2.row.summary$m=d2.row.summary$pct
    if(metric=='pct'){d2.row.summary$m=d2.row.summary$pct}else{d2.row.summary$m=d2.row.summary$x}
    r.row=matrix(ncol=1,nrow=5,data=d2.row.summary$m)
    r2.row=raster(r.row)
    extent(r2.row) <- c(0, 0.2, 0, 1)
    
    plot(r2.row,axes=FALSE, box=FALSE)
    text(r2.row,digits=2)
    text(rep(-0.08, 5),seq(0.08,0.88,0.2),paste0('$',seq(1000,200,-200)))
  })
  output$plot1 <- renderPlot({ 
    d2=all.indata()$d2
    metric=all.indata()$metric
    d2.summary=d2 %>% count(row,col)
    r=matrix(ncol=6,nrow=5,data=0,
             dimnames = list(seq(200,1000,200),
                             c("C.1", "C.2", "C.3", "C.4","C.5","C.6")))#represents Jeopardy! board
    d2.summary$pct=round(100*d2.summary$n/sum(d2.summary$n),2)
    d2.summary$m=d2.summary$pct
    if(metric=='pct'){d2.summary$m=d2.summary$pct}else{d2.summary$m=d2.summary$n}
    
    r[1,d2.summary$col[which(d2.summary$row==1)]]=d2.summary$m[which(d2.summary$row==1)]
    r[2,d2.summary$col[which(d2.summary$row==2)]]=d2.summary$m[which(d2.summary$row==2)]
    r[3,d2.summary$col[which(d2.summary$row==3)]]=d2.summary$m[which(d2.summary$row==3)]
    r[4,d2.summary$col[which(d2.summary$row==4)]]=d2.summary$m[which(d2.summary$row==4)]
    r[5,d2.summary$col[which(d2.summary$row==5)]]=d2.summary$m[which(d2.summary$row==5)]
    r2=raster(r)
    extent(r2) <- c(0, 1, 0, 1)
    plot(r2,axes=FALSE, box=FALSE)
    text(r2,digits=2)
    text(seq(0.08,0.94,0.17),rep(1.05,6),paste0('C',1:6), xpd=NA)
    text(rep(-0.08, 5),seq(0.08,0.88,0.2),paste0('$',seq(1000,200,-200)))
  })
  
  
}

# shinyApp(ui, server)


