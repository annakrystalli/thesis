rm(list=ls()[!(ls() %in% c('an.IDCV','varnames', 'names.spp', 'cv.no','run'))])

#names.spp<-load(file="~/Documents/TRAINING DATA/spp.names")

plot.vars<-setdiff(varnames, c("time"))

#load(file="~/Documents/TRAINING DATA/spp.names")
#names.spp<-"centrot"


source('~/Documents/WORKFLOW/randomForest/Autocorrelation diagnostics functions.R', chdir = TRUE)

residXcorr(method="pearson", model="rf_i", data=" mod", varnames, plot.vars, 
           names.spp, an.IDCV)
residXcorr(method="spearman", model="rf_i", data=" mod", varnames, plot.vars, 
           names.spp, an.IDCV)

residXcorr(method="pearson", model="rf_b", data=" mod", names.spp, varnames)
residXcorr(method="spearman", model="rf_b", data=" mod", names.spp, varnames)


spatialAutoc(model="rf_i", names.spp, an.IDCV)

plotResidMaps(model="rf_i", t.res=c(1, 2 ,3), names.spp, an.IDCV)






  
require(randomForest)
set.seed(1)

selectX<-function(X, varnames){X[,which(names(X) %in% varnames)]}

varnames<-c("year", "time",
            "month",  "T.m", "Sed.m", 
            "Sedf.m",  "Ch.m", "fdens.m", 
            "fdist.m",  "fside.m",  "fdens.c.m", 
            "fdist.c.m",  "fside.c.m", 
            "bath", "NAO", "wNAO")

load(file=paste("/Users/annakrystalli/documents/TRAINING DATA/y mod training data.RData", sep=""))
load("~/Documents/TRAINING DATA/normalized (min.max) x training data.RData")

norm.x.m$time[norm.x.m$time>=0.25 & norm.x.m$time<0.75]<-"day"
norm.x.m$time[norm.x.m$time!="day"]<-"night"
norm.x.m$time<-as.factor(norm.x.m$time)


all.samples<-paste(norm.x.m$tow,norm.x.m$sample.no., sep="-")

x.dat<-data.frame(spl.id=sort(all.samples), norm.x.m[order(all.samples),])
y.m<-y.m[order(all.samples),]
all.samples<-sort(all.samples)


names.spp<-c("tem", "centrot", "cfin", "chel", "metrilu")


rm(norm.x.m)


unNormalise<-function(x, var.id){
  
  load(file="~/Documents/SATELLITE/Associated data/r files/min.max.ind pre processing file.RData")
  
  for(var in var.id){
    x.v<-x[,var]
    min<-as.numeric(min.max.tab$min[min.max.tab$var==var])
    max<-as.numeric(min.max.tab$max[min.max.tab$var==var])
    x[,var]<-min+(x.v*(max-min))}
  
  return(x)}

cor.x.spp.AC<-vector( "list", 5)
names(cor.x.spp.AC)<-names.spp

for(spp in names.spp){
load(paste("~/Documents/TRAINING DATA/Models/randomForest/model assess1/forests/",spp,"/rf_i.Rdata", sep=""))
load(paste("~/Documents/TRAINING DATA/Models/randomForest/model assess1/forests/",spp,"/results.Rdata", sep=""))



y.er<-res[[1]][[1]]$AC$y.er
pr<-res[[1]][[1]]$AC$pr

x.cor.sum<-matrix(0, nrow=length(varnames), ncol=2, 
                  dimnames=list(varnames,
                                c("rho", "p-value")))


for(i in 1:length(varnames)){
x.cor<-cor.test(abs(y.er-pr), as.numeric(x.dat[rf.dat$test.indAC, 
            varnames[i]]), method="spearman")

x.cor.sum[i, 1]<-x.cor$estimate
x.cor.sum[i, 2]<-x.cor$p.value}

cor.x.spp.AC[[spp]]<-round(x.cor.sum, 2)}



cor.x.spp.OC<-vector( "list", 5)
names(cor.x.spp.OC)<-names.spp

for(spp in names.spp){
  load(paste("~/Documents/TRAINING DATA/Models/randomForest/model assess1/forests/",spp,"/rf_i.Rdata", sep=""))
  load(paste("~/Documents/TRAINING DATA/Models/randomForest/model assess1/forests/",spp,"/results.Rdata", sep=""))
  
  
  
  y.er<-res[[2]][[1]]$OC$y.er
  pr<-res[[2]][[1]]$OC$pr
  
  x.cor.sum<-matrix(0, nrow=length(varnames), ncol=2, 
                    dimnames=list(varnames,
                                  c("rho", "p-value")))
  
  
  for(i in 1:length(varnames)){
    x.cor<-cor.test(abs(y.er-pr), as.numeric(x.dat[rf.dat$test.ind, 
                                                   varnames[i]]), method="spearman")
    
    x.cor.sum[i, 1]<-x.cor$estimate
    x.cor.sum[i, 2]<-x.cor$p.value}
  
  cor.x.spp.OC[[spp]]<-round(x.cor.sum, 2)}

run.lab=c("rf", "rf_i", "rf_c", "rf_b") 



plotPRmatrix<-function(an.ID, spp, runs, run.lab){

 
load(paste("~/Documents/TRAINING DATA/Models/randomForest/", an.ID,
           "forests/", spp, "/results.Rdata", sep=""))

for (i in runs){
  
y.er<-res[[i]][[1]]$AC$y.er
pr<-res[[i]][[1]]$AC$pr

png(file = paste("~/Documents/TRAINING DATA/Models/randomForest/", an.ID, "forests/",spp,"/",spp,
                 " ",run.lab[i]," RP matix.png",
                 sep=""),
    width=150*8.3, height=150*11.7, pointsize=30)

par(mfrow=c(2,1), mar=c(4, 4, 3,0), oma=c(0.5,0.5,0.5,0.5))

#Produce table of recall
tabR<-(table(pr, y.er))/rowSums((table(pr, y.er)))
    img<-image(tabR,col=rev(gray((0:1000)/1000)), axes = FALSE, xlab="predicted",
               ylab="observed", main=paste(spp, run.lab[i],"recall"))
        box("plot")
        axis(1, at = seq(0, length(dimnames(tabR)[1][[1]]), 
                         l = length(dimnames(tabR)[1][[1]]))/length(dimnames(tabR)[1][[1]]), 
             labels=dimnames(tabR)[1][[1]])
        axis(2, at = seq(0, length(dimnames(tabR)[2][[1]]), 
                         l = length(dimnames(tabR)[2][[1]]))/length(dimnames(tabR)[2][[1]]), 
             labels=dimnames(tabR)[2][[1]])
      abline(v=seq(0.5, length(dimnames(tabR)[1][[1]])-1.5, 
                   l = length(dimnames(tabR)[1][[1]])-1)/(length(dimnames(tabR)[1][[1]])-1), 
             col="white", lwd=3)

#Produce table of precision
tabP<-t(t(table(pr, y.er))/colSums((table(pr, y.er))))
    img<-image(tabP,col=rev(gray((0:1000)/1000)), axes = FALSE, xlab="predicted",
               ylab="observed", main=paste(spp, run.lab[i],"precision"))
        box("plot")
        axis(1, at = seq(0, length(dimnames(tabP)[1][[1]]), 
                         l = length(dimnames(tabP)[1][[1]]))/length(dimnames(tabP)[1][[1]]), 
             labels=dimnames(tabP)[1][[1]])
        axis(2, at = seq(0, length(dimnames(tabP)[2][[1]]), 
                         l = length(dimnames(tabP)[2][[1]]))/length(dimnames(tabP)[2][[1]]), 
             labels=dimnames(tabP)[2][[1]])
        abline(h=seq(0.5, length(dimnames(tabR)[2][[1]])-1.5, 
                     l = length(dimnames(tabR)[2][[1]])-1)/(length(dimnames(tabR)[2][[1]])-1), 
               col="white", lwd=3)
dev.off()

}
}



