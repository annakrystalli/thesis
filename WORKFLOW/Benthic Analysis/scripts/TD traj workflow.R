

rm(list=ls())

range01 <- function(x){(x-min(x))/(max(x)-min(x))}
range01all <- function(x){(x-xr[1])/(xr[2]-xr[1])}


source("analyseTrees.R")
load(file="FD analysis table_all.RData")
load(file="TD analysis table_all.RData")

#The following function permutes majority of possible trait sequences to maximise 
#the total number of species trait records in the analysis as these vary according to the sequence 
#in which traits are ordered. No need to re-run as I have saved the order vector, just load object below:
#trait.select<-maxCompl(FD.table, var.table)
#save(trait.select, file="trait.order.RData")
load(file="trait.order.RData")

#Order columns in FD.table
data<-data.frame(t(FD.table[,trait.select]))
colnames(data)<-FD.table$AphiaID_acc

cat.type<-vector("list",1)
names(cat.type)<-"asymm"
cat.type[[1]]<-colnames(data)

d.m<-daisy(data, metric="gower", type=cat.type)
tree<-hclust(d.m, method="average")

apply(data, FUN=unique, 2)





load(file="RESULTS.RData") 



analyses<-c("both", "var", "spp")
trials<-data.frame(var=rep(res.both$var, 2), spp=rep(res.both$n,2))


for(i in 1:3){

  
  an.ID<-analyses[i]
  
  load(file=paste("SD RESULTS ", an.ID, ".RData", sep=""))            
  
  res<-get(paste("res.",an.ID, sep=""))
  names(res)[names(res)=="n"]<-"spp"
  
  plotTraj(res, SD, an.ID, pt=12, trials=trials)


}
  
  
 par(mfrow=c(3,1))

for(i in 1:3){
  
  an.ID<-analyses[i]
  
  load(file=paste("SD RESULTS ", an.ID, ".RData", sep=""))            
  
  res<-get(paste("res.",an.ID, sep=""))
  
  f.dat<-NULL
  b.dat<-NULL
  
  for (i in 1:length(SD)){
    
    b.dat<-data.frame(rbind(b.dat,data.frame(SD[[i]], id = names(res$TREES)[i])))
    }
  
  boxplot( Ddevsd ~ factor(id), data=b.dat )
  abline(h= -2, lty=2)
  abline(h= 0, lty=1, lwd=0.5)
  
}






b.dat[is.nan(b.dat$Ddevsd), "Ddevsd"]<-0

aovm<-aov(Ddevsd ~ id, data=b.dat)
print(model.tables(aovm,"means"),digits=3)

summary(aovm)
names(res$TREES)
  
tree<-res$TREES[[i]]$tree

par(mar=c(1.5,5,4,0.5),mfrow=c(1,1))
plot(rev(as.dendrogram(tree, hang= -5)),horiz=T,
     xlim=c(0,1),
     main=paste("t = ", res$var[i],", s = ", res$n[i],  sep=""))
abline(v=max(tree$height), lty=2)
