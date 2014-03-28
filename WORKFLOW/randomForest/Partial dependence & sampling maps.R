rm(list=ls())

load("~/Documents/TRAINING DATA/normalized (min.max) x training data.RData")
output.folder<-"~/Documents/TRAINING DATA/sampling maps/"

dir.create(path=output.folder)

#_______________________________________________________________________________________________________
unNormalise<-function(x, var.id){
  
  load(file="~/Documents/SATELLITE/Associated data/r files/min.max.ind pre processing file.RData")
  
  for(var in var.id){
    x.v<-x[,var]
    min<-as.numeric(min.max.tab$min[min.max.tab$var==var])
    max<-as.numeric(min.max.tab$max[min.max.tab$var==var])
    x[,var]<-min+(x.v*(max-min))}
  
  return(x)}

#_______________________________________________________________________________________________________


selectX<-function(X, varnames){X[,which(names(X) %in% varnames)]}
#____________________________________________


v3<-c("year", 
      "month",  "T.m", "Sed.m", 
      "Sedf.m",  "Ch.m", "fdens.m", 
      "fdist.m",  "fside.m",  "fdens.c.m", 
      "fdist.c.m",  "fside.c.m", 
      "bath", "NAO", "wNAO")


sampleMap<-function(x=x.oc){
  require(maps)
  
  x<-unNormalise(x, var.id=c("lat", "lon", "year", "month"))
    
  
  for(year in sort(unique(x$year))){
      
    x.y<-x[x$year==year,]
  
    
  
  png(file = paste(output.folder, year, ".png", sep=""), 
      width=150*8.3, height=150*11.7, pointsize=30)
  
  par(mfrow=c(4,3), oma=c(0,0,4,0))
  
    for(month in 1:12){

      if(month %in% unique(x.y$month)){
        
      map("world", xlim=c(-4,11), ylim=c(51,61), fill=TRUE, 
          mar = c(0.1, 0.1, 0.1, 0.1))
  
    points(x=x.y$lon[x.y$month==month], 
         y=x.y$lat[x.y$month==month], pch=20, cex=0.5)
  
    text(x=3, y=60, labels=month)}else{frame()}}
    mtext(year,side=3,line=-0.2,cex=1.5,outer=TRUE)
  
  dev.off()
}
#___________________________________________________________________________________________________________
  
  varnames<-v3
  
  x<-selectX(x, varnames)
  
  
  
spp="chel"
  an.ID="No Geo strata/"  
  
partDependPlots<-function(spp, an.ID, varnames){
  
  require(randomForest)
  
  selectX<-function(X, varnames){X[,which(names(X) %in% varnames)]}
  
  dir.create(path=paste("~/Documents/TRAINING DATA/Models/randomForest/", 
                        an.ID, "partial dependence plots/", sep=""))
  dir.create(path=paste("~/Documents/TRAINING DATA/Models/randomForest/", 
                        an.ID, "partial dependence plots/",spp,"/", sep=""))
  
  
  pdPlotOC(spp, an.ID, varnames)
  pdPlotAC(spp, an.ID, varnames)  

  }
 
pdPlotOC<-function(spp, an.ID, varnames){
  load(file="~/Documents/SATELLITE/Associated data/r files/min.max.ind pre processing file.RData")
  
  load(paste("~/Documents/TRAINING DATA/Models/randomForest/", an.ID, 
             "forests/",spp,"/OCforest.Rdata", sep=""))
  load(paste("~/Documents/TRAINING DATA/Models/randomForest/"
             ,spp,"_train_data.Rdata", sep=""))
  
  x.oc<-selectX(x.oc, varnames)
  
  imp <- importance(rforOC)
  impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
  
  png(file = paste("~/Documents/TRAINING DATA/Models/randomForest/", 
                   an.ID, "partial dependence plots/",spp,"/OC pd plot.png", sep=""), 
      width=150*8.3, height=150*11.7, pointsize=30)
  
  par(mfrow=c(5,3), oma=c(0,0,4,0))
  for (i in seq_along(impvar)){
    
    pp<-partialPlot(x=rforOC, pred.data=x.oc, x.var=impvar[i], which.class="1", plot=FALSE)
    
    min<-as.numeric(min.max.tab$min[min.max.tab$var==impvar[i]])
    max<-as.numeric(min.max.tab$max[min.max.tab$var==impvar[i]])
    pp$x<-min+(pp$x*(max-min))
    
    save(pp,file=paste("~/Documents/TRAINING DATA/Models/randomForest/", 
                  an.ID, "partial dependence plots/",spp,"/",
                  impvar[i]," OC pd.Rdata", sep=""))
    
    plot(pp$y~pp$x, type="l" ,xlab=impvar[i], ylab="partial dependence", lwd="2")}
  
    mtext(spp,side=3,line=-0.2,cex=1.5,outer=TRUE)
    
    
    dev.off()}  

  
pdPlotAC<-function(spp, an.ID, varnames){
  load(file="~/Documents/SATELLITE/Associated data/r files/min.max.ind pre processing file.RData")
  
  load(paste("~/Documents/TRAINING DATA/Models/randomForest/", an.ID, "forests/",spp,"/ACforest.Rdata", sep=""))
  load(paste("~/Documents/TRAINING DATA/Models/randomForest/",spp,"_train_data.Rdata", sep=""))
  
  x<-selectX(x, varnames)
  
  imp <- importance(rforAC)
  impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
  cols=grey((20-seq_along(levels(y)))/22)
  
  png(file = paste("~/Documents/TRAINING DATA/Models/randomForest/", 
                   an.ID, "partial dependence plots/",spp,"/AC pd plot.png", sep=""), 
      width=150*8.3, height=150*11.7, pointsize=30)
  
  par(mfrow=c(5,3), oma=c(0,0,4,0))
  for (i in seq_along(impvar)) {
    
    
    
    for (j in seq_along(levels(y))){
      
    
      
    pp<-partialPlot(x=rforAC, pred.data=x, x.var=impvar[i], which.class=levels(y)[j],
                    plot=FALSE)
    
    min<-as.numeric(min.max.tab$min[min.max.tab$var==impvar[i]])
    max<-as.numeric(min.max.tab$max[min.max.tab$var==impvar[i]])
    pp$x<-min+(pp$x*(max-min))
    
    save(pp,file=paste("~/Documents/TRAINING DATA/Models/randomForest/", 
               an.ID, "partial dependence plots/",spp,"/",
                  impvar[i],levels(y)[j]," AC pd.Rdata", sep=""))
    
    if(j==1){plot(pp$y~pp$x, type="l" ,xlab=impvar[i], ylab="partial dependence", 
         lwd="2", lty=j, col=cols[j])}else{
           points(pp$y~pp$x, type="l", 
                  lwd="2", lty=j, 
                  col=cols[j])}}
    legend("topright", col=cols, lty=seq_along(levels(y)), lwd="2", legend=levels(y))}
    
    mtext(spp,side=3,line=-0.2,cex=1.5,outer=TRUE)
    
    
    dev.off()}
  
  
  
  
  
  
  
  
#____________________________________________________________________________  
  spp="chel"
  an.ID="No Geo strata/"  
  
  v3<-c("year", 
        "month",  "T.m", "Sed.m", 
        "Sedf.m",  "Ch.m", "fdens.m", 
        "fdist.m",  "fside.m",  "fdens.c.m", 
        "fdist.c.m",  "fside.c.m", 
        "bath", "NAO", "wNAO")
  
  
  
  
  
  plotPP<-function(spp, type)
    
    
    if(type=="AC")
      
  
  load(paste("~/Documents/TRAINING DATA/Models/randomForest/", an.ID, "forests/",spp,"/ACforest.Rdata", sep=""))
  load(paste("~/Documents/TRAINING DATA/Models/randomForest/",spp,"_train_data.Rdata", sep=""))
  

  
  imp <- importance(rforAC)
  impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
  cols=grey((20-seq_along(levels(y)))/22)
  
  png(file = paste("~/Documents/TRAINING DATA/Models/randomForest/", 
                   an.ID, "partial dependence plots/",spp,"/AC pd plot.png", sep=""), 
      width=150*8.3, height=150*11.7, pointsize=20)
  
  par(mfrow=c(5,3), oma=c(0,0,4,0))
  for (i in seq_along(impvar)) {
    
    
    
    for (j in seq_along(levels(y))){
      
      
      load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/", 
                         an.ID, "partial dependence plots/",spp,"/",
                         impvar[i],levels(y)[j]," AC pd.Rdata", sep=""))
      if(j==1){pp.comp<-vector("list")}
      pp.comp[[j]]<-pp}
      
      rgy<-range(sapply(pp.comp, function(x){as.numeric(x[[2]])}))
      rgy<-rgy+(c(-0.025*diff(rgy),0.025*diff(rgy)))
      
      rgx<-range(sapply(pp.comp, function(x){as.numeric(x[[1]])}))
      rgx<-rgx+(c(-0*diff(rgx),0.25*diff(rgx)))
     
    for (j in seq_along(levels(y))){
      if(j==1){plot(pp.comp[[j]]$y~pp.comp[[j]]$x, type="l" ,
                    xlab=impvar[i], ylab="partial dependence", 
                    lwd="2", lty=j, col=cols[j], xlim=rgx, ylim=rgy)}else{
                      points(pp.comp[[j]]$y~pp.comp[[j]]$x, type="l", 
                             lwd="2", lty=j, 
                             col=cols[j])}}
    
    legend("topright", col=cols, lty=seq_along(levels(y)), lwd="2", 
           legend=levels(y), cex=0.6, bty="n")}
  
  mtext(spp,side=3,line=-0.2,cex=1.5,outer=TRUE)
  
  
  dev.off()