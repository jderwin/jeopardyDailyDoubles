# https://j-archive.com/showgame.php?game_id=9173
# https://www.reddit.com/r/Jeopardy/comments/eu8dfc/how_to_get_data_from_jarchive_for_codingresearch/
# https://github.com/k5cents/whatr/tree/master/data
gc()
rm(list=ls())
install.packages("remotes")
library(remotes)
library(pbapply)
library(dplyr)
library(raster)
remotes::install_github("kiernann/whatr") 
#estimated ~1 hour 10 min
d2=do.call('rbind',pblapply(X = 1:9315,cl=cl,FUN=function(i){
  tryCatch({
    # cat(i)
    # cat(" ")
    #i=
    mydate=as.character(whatr_airdate(game=i)$date)
    d=whatr_doubles(game=i)
    d=data.frame('row'=d$row,'col'=d$col,'date'=rep(mydate,length(d$row)),
                 'day'=rep(strsplit(mydate,'-')[[1]][3],length(d$row)),
                 'month'=rep(strsplit(mydate,'-')[[1]][2],length(d$row)),
                 'year'=rep(strsplit(mydate,'-')[[1]][1],length(d$row)),
                 'game'=rep(i,length(d$row)),'order'=1:length(d$row))
    return(d)
  },error = function(cond) { cat(i)})
  }
))
save(d2,file = './dailyDoubles.RData')
load('./dailyDoubles.RData')
p2=do.call('rbind',pblapply(X = 1:9315,FUN=function(i){
  tryCatch({
    # cat(i)
    # cat(" ")
    #i=
    d=whatr_players(game = i)
    d=data.frame('first1'=d$first[1],'last1'=d$last[1],
                 'first2'=d$first[2],'last2'=d$last[2],
                 'first3'=d$first[3],'last3'=d$last[3],
                 'game'=i)
    return(d)
  },error = function(cond) { cat(i)})
}
))
save(p2,file = './players.RData')
load('./players.RData')
p2$player1=paste0(p2$last1,'_',p2$first1)
p2$player2=paste0(p2$last2,'_',p2$first2)
p2$player3=paste0(p2$last3,'_',p2$first3)
p2=p2[,7:10]
masterdf=merge(x=d2,y=p2,by='game',all.x=T,all.y=F)
write.csv(masterdf,file='./masterdf.csv',row.names=F)
save(masterdf,file='./masterdf.RData')
load('./masterdf.RData')
masterdf=read.csv('./masterdf.csv')
d2=masterdf[which(rowSums(masterdf=='Holzhauer_James')>0),]
d2.summary=d2 %>% count(row,col)
r=matrix(ncol=6,nrow=5,data=0,
         dimnames = list(seq(200,1000,200),
                         c("C.1", "C.2", "C.3", "C.4","C.5","C.6")))#represents Jeopardy! board
d2.summary$pct=round(100*d2.summary$n/sum(d2.summary$n),2)
d2.row.summary=aggregate(d2.summary$n,FUN='sum',by=list(d2.summary$row))
d2.row.summary$pct=round(100*d2.row.summary$x/sum(d2.row.summary$x),2)
d2.col.summary=aggregate(d2.summary$n,FUN='sum',by=list(d2.summary$col))
d2.col.summary$pct=round(100*d2.col.summary$x/sum(d2.col.summary$x),2)

r[1,d2.summary$col[which(d2.summary$row==1)]]=d2.summary$pct[which(d2.summary$row==1)]
r[2,d2.summary$col[which(d2.summary$row==2)]]=d2.summary$pct[which(d2.summary$row==2)]
r[3,d2.summary$col[which(d2.summary$row==3)]]=d2.summary$pct[which(d2.summary$row==3)]
r[4,d2.summary$col[which(d2.summary$row==4)]]=d2.summary$pct[which(d2.summary$row==4)]
r[5,d2.summary$col[which(d2.summary$row==5)]]=d2.summary$pct[which(d2.summary$row==5)]
r.row=matrix(ncol=1,nrow=5,data=d2.row.summary$pct)
r.col=matrix(ncol=6,nrow=1,data=d2.col.summary$pct)

r2=raster(r)
extent(r2) <- c(0, 1, 0, 1)
r2.row=raster(r.row)
extent(r2.row) <- c(0, 0.2, 0, 1)
r2.col=raster(r.col)
extent(r2.col) <- c(0, 1, 0, 0.2)
par(mfrow=c(1,1))
plot(r2.col,axes=FALSE, box=FALSE)
text(r2.col,digits=2)
text(seq(0.08,0.94,0.17),rep(0.25,6),paste0('C',1:6))
plot(r2.row,axes=FALSE, box=FALSE)
text(r2.row,digits=2)
text(rep(-0.08, 5),seq(0.08,0.88,0.2),paste0('$',seq(1000,200,-200)))
plot(r2,axes=FALSE, box=FALSE)
text(r2,digits=2)
text(seq(0.08,0.94,0.17),rep(1.05,6),paste0('C',1:6), xpd=NA)
text(rep(-0.08, 5),seq(0.08,0.88,0.2),paste0('$',seq(1000,200,-200)))

