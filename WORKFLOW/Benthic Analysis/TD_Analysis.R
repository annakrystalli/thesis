rm(list=ls())

source("analyseTrees.R")
load(file="FD analysis table_all.RData")

compl<-NULL
for(i in 1:(length(FD.table)-5)){
  compl<-c(compl,sum(complete.cases(FD.table[6:(length(FD.table)-i+1)])))}
compl<-rev(compl)



an.ID<-"both"
min.spp<-15
int<-rev(unique(compl[compl>=min.spp]))
seq.int<-seq(min.spp,198, l=6)
#plot.int<-sapply(seq.int, FUN=function(x){x<-min(int[which(int>=x)])})
plot.int<-c(17, 46, 58, 74, 179, 198)

#both.lims<-findPlotLims(res.both)
res.both<-analyseTrees(FD.table, int, plot.int=plot.int, an.ID=an.ID, metric="gower", type="asymm", select.spp=NULL, 
                       vars=NULL, var.names=var.names, xlim=c(0,1), ylim=c(0,100))

load(file=paste("plots/",an.ID," TD vs #spp data.RData"))

for(i in 1:length(C.SIZES)){
plot(unlist(TDs[[i]])~C.SIZES[[i]]$cluster.size, main=names(C.SIZES[i]), pch=21, col=grey(level=0.2,alpha=0.3),
bg=grey(level=0.2,alpha=0.3), xlab="cluster size", ylab="cluster TD")}


an.ID<-"var"
min.spp<-58
int<-rev(unique(compl[compl>=min.spp]))  
#seq.int<-seq(min.spp,198, l=6)
plot.int<-c(58, 67, 74, 164, 179, 198)

#plot.int<-sapply(seq.int, FUN=function(x){x<-min(int[which(int>=x)])})

#res.var<-analyseTrees(FD.table, int, plot.int=plot.int, an.ID=an.ID, metric="gower", type="asymm", select.spp=58, 
#                      vars=NULL, var.names=var.names)

#var.lims<-findPlotLims(res.var)
res.var<-analyseTrees(FD.table, int, plot.int=plot.int, an.ID=an.ID, metric="gower", type="asymm", select.spp=58, 
                      vars=NULL, var.names=var.names, xlim=c(0,1), ylim=c(0,100))

load(file=paste("plots/",an.ID," TD vs #spp data.RData"))
     
     for(i in 1:length(C.SIZES)){
       plot(unlist(TDs[[i]])~C.SIZES[[i]]$cluster.size, main=names(C.SIZES[i]), pch=21, col=grey(level=0.2,alpha=0.3),
            bg=grey(level=0.2,alpha=0.3), xlab="cluster size", ylab="cluster TD")}     
     
     

an.ID<-"spp"
#int<-rep(61,8)
#res.spp<-analyseTrees(FD.table, int, plot.int=c(87, 76  ,57  ,46  ,36 , 23), an.ID=an.ID, metric="gower", type="asymm", 
#                      select.spp=c(87,80, 76  ,57  ,46  ,36 , 23), 
#                     vars=NULL, var.names=var.names)
#spp.lims<-findPlotLims(res.spp)
an.ID<-"spp"
int<-rep(67,7)
min.spp<-rev(unique(compl[compl>=min.spp]))
res.spp<-analyseTrees(FD.table, int, plot.int=c(67,  61,  58,  47,  17,  15), an.ID=an.ID, metric="gower", type="asymm", 
                      select.spp=c(67,  61,  58,  47,  46,  17,  15), 
                      vars=NULL, var.names=var.names, xlim=c(0,1), ylim=c(0,100))


cor.lims<-findCorLims(res.both, res.spp, res.var)
cor.both<-AnalysisPlots(res=res.both, an.ID="both", ylim3=cor.lims$ylim3, ylim4=cor.lims$ylim4)
cor.var<-AnalysisPlots(res=res.var, an.ID="var", ylim3=cor.lims$ylim3, ylim4=cor.lims$ylim4)
cor.spp<-AnalysisPlots(res=res.spp, an.ID="spp", ylim3=cor.lims$ylim3, ylim4=cor.lims$ylim4)



plot(res.both$TD~res.both$FD)
plot(res.var$TD~res.var$FD)
plot(res.spp$TD~res.spp$FD)

plot(res.both$max.h~res.both$n.all)
plot(res.both$max.h~res.both$var.n.all)
plot(res.spp$max.h~res.spp$n.all)
plot(res.var$max.h~res.var$var.n.all)


plot(res.both$prop.100~res.both$n.all)
plot(res.both$prop.100~res.both$var.n.all)
plot(res.spp$prop.100~res.spp$n.all)
plot(res.var$prop.100~res.var$var.n.all)


plot(res.both$FD~res.both$n.all)
plot(res.both$FD~res.both$var.n.all)
plot(res.var$FD~res.var$var.n.all)
plot(res.spp$FD~res.spp$n.all)

plot(res.both$TD~res.both$n.all, ylim=c(0,100))
plot(res.both$TD~res.both$var.n.all,ylim=c(0,100))
plot(res.var$TD~res.var$var.n.all,ylim=c(0,100))
plot(res.spp$TD~res.spp$n.all, ylim=c(0,100) )


load(file="TD analysis table_all.RData")
check<-lapply(res.both$C.IDs[[1]]$cluster.ids, FUN=unlist)[res.both$TDs[[1]]$clust.td==100]

for(i in 1:length(check)){
print(taxo.table[row.names(taxo.table) %in% check[[i]],])}


try(taxa2dist(clust.taxa, varstep = F), silent=T)
if(class(clust.dist)=="try-error"){clust.td[[i]]<-100}else{