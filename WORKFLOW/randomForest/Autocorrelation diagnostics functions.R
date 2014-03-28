unNormalise<-function(x, var.id){
  
  load(file="~/Documents/SATELLITE/Associated data/r files/min.max.ind pre processing file.RData")
  
  for(var in var.id){
    x.v<-x[,var]
    min<-as.numeric(min.max.tab$min[min.max.tab$var==var])
    max<-as.numeric(min.max.tab$max[min.max.tab$var==var])
    x[,var]<-min+(x.v*(max-min))}
  
  return(x)}



residXcorr<-function(method, model, data=" mod", varnames, plot.vars, names.spp, an.IDCV){

varnames.un<-c("year", "time",
            "month",  "T.m", "Sed.m", 
            "Sedf.m",  "Ch.m", "fdens.m", 
            "fdist.m",  "fside.m",  "fdens.c.m", 
            "fdist.c.m",  "fside.c.m", 
            "bath", "NAO", "wNAO")

load(file=paste("/Users/annakrystalli/documents/TRAINING DATA/y", data, " training data.RData", sep=""))
load("~/Documents/TRAINING DATA/normalized (min.max) x training data.RData")

norm.x.m$time[norm.x.m$time>=0.25 & norm.x.m$time<0.75]<-"day"
norm.x.m$time[norm.x.m$time!="day"]<-"night"
norm.x.m$time<-as.factor(norm.x.m$time)


all.samples<-paste(norm.x.m$tow,norm.x.m$sample.no., sep="-")

x.dat<-data.frame(spl.id=sort(all.samples), norm.x.m[order(all.samples),])
x.dat<-unNormalise(x.dat, varnames.un[varnames.un!="time"])




rm(norm.x.m)


cor.x.spp.AC<-vector( "list", length(names.spp))
names(cor.x.spp.AC)<-names.spp

if(model=="rf_i"){j<-1}else{j<-2}

for(spp in names.spp){
  
  
  y.er<-NULL
  pr<-NULL
  x.ind<-NULL

  for(i in 1:cv.no){
    
    load(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.IDCV, i,"/forests/",spp,"/",model,".Rdata", sep=""))
    load(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.IDCV, i,"/forests/",spp,"/results.Rdata", sep=""))
    
    
      y.er<-c(y.er, res[[model]][["rf.p"]]$AC$y.er)
      pr<-c(pr, res[[model]][["rf.p"]]$AC$pr)
      x.ind<-c(x.ind, rf.dat$test.indAC)}
  
  
  
  x.cor.sum<-matrix(0, nrow=length(varnames), ncol=4, 
                    dimnames=list(varnames,
                                  c("rho", "p-value", "r-sq", "p-value")))
  

  an.ID<-paste(an.IDCV,1, "/", sep="")
  png(file = paste("~/Documents/TRAINING DATA/Models/randomForest/", an.ID, "plots/",spp," AC var vs residuals.png",
                   sep=""),
      width=150*8.3, height=150*11.7, pointsize=30)
  
  par(mfrow=c(5,3), mar=c(5, 4, 0,0), oma=c(0.5,0.5,4,0.5))
  
  for(i in 1:length(varnames)){
    x.cor<-cor.test(pr-y.er, as.numeric(x.dat[x.ind, 
                                                   varnames[i]]), 
                    method=method)
    
    x.cor.sum[i, 1]<-x.cor$estimate
    x.cor.sum[i, 2]<-x.cor$p.value
    
    lm<-lm((pr-y.er)~ as.numeric(x.dat[x.ind, 
                                 varnames[i]]))
    x.cor.sum[i, 3]<-summary(lm)$r.squared
    x.cor.sum[i, 4]<-summary(lm)$ coefficients[2,4]
 
    if(varnames[i] %in% plot.vars){smoothScatter(y=pr-y.er, x=as.numeric(x.dat[x.ind,varnames[i]]),
                  xlab=varnames[i], ylab="residuals")}  

  }
  

  cor.x.spp.AC[[spp]]<-round(x.cor.sum, 2)
  mtext(spp,side=3,line=-0.3,cex=1.2,outer=TRUE)
  dev.off()}

#-------------------------------


cor.x.spp.OC<-vector( "list", length(names.spp))
names(cor.x.spp.OC)<-names.spp

for(spp in names.spp){
  
  y.er<-NULL
  pr<-NULL
  x.ind<-NULL
  
  for(i in 1:cv.no){
    
    load(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.IDCV, i,"/forests/",spp,"/",model,".Rdata", sep=""))
    load(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.IDCV, i,"/forests/",spp,"/results.Rdata", sep=""))
    
    
    y.er<-c(y.er, res[[model]][["rf.p"]]$OC$y.er)
    pr<-c(pr, res[[model]][["rf.p"]]$OC$pr)
    x.ind<-c(x.ind, rf.dat$test.ind)}
  
  
  x.cor.sum<-matrix(0, nrow=length(varnames), ncol=4, 
                    dimnames=list(varnames,
                                  c("rho", "p-value", "r-sq", "p-value")))
  
  
  for(i in 1:length(varnames)){
    x.cor<-cor.test(pr-y.er, as.numeric(x.dat[x.ind, 
                                                   varnames[i]]), 
                    method=method)
    
    x.cor.sum[i, 1]<-x.cor$estimate
    x.cor.sum[i, 2]<-x.cor$p.value
 
    lm<-lm((pr-y.er)~ as.numeric(x.dat[x.ind, 
                                       varnames[i]]))
    x.cor.sum[i, 3]<-summary(lm)$r.squared
    x.cor.sum[i, 4]<-summary(lm)$ coefficients[2,4]
    
  }
  
 
  cor.x.spp.OC[[spp]]<-round(x.cor.sum, 2)}

save(cor.x.spp.OC, cor.x.spp.AC, 
     file=paste("~/Documents/TRAINING DATA/Models/randomForest/model assess NC CV1/forests/X corr ",
                model, method,".Rdata", sep=""))}

printXcorrReport<-function(method="spearman", model, an.IDCV){
  
  
  load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",an.IDCV,"1/forests/X corr ", 
                  model,method,".Rdata", sep=""))
  
  print("Occupancy Models")
  print(cor.x.spp.OC)
  
  print("Abundance Models")
  print(cor.x.spp.AC)}


moranCalc<-function(x.dat, y.er, pr, x.ind, an.IDCV){
  
  
  geo.cood<-x.dat[x.ind,2:3]
  
  timepoints<-unNormalise(x.dat[x.ind,c("month", "year")],c("month", "year"))
  t.id<-unique(timepoints)
  mat<-matrix(NA, dim(t.id)[1], 4)
  
  
  for(i in 1:dim(t.id)[1]){
    geo.coo<-as.matrix(unNormalise(geo.cood[timepoints$month==t.id[i,1] & 
                                              timepoints$year==t.id[i,2],], 
                                   c("lon", "lat")))
    
    
    resid<-(pr-y.er)[timepoints$month==t.id[i,1] & 
                       timepoints$year==t.id[i,2]]
    
    
    geo.w<-1/distm(geo.coo, fun=distHaversine)
    geo.w[which(is.infinite(geo.w), arr.ind=T)]<-0
    
    MI<-try(Moran.I(resid, geo.w), silent=T)
    if(class(MI)=="try-error"){next}else{
    mat[i,]<-c(MI$observed,
               MI$expected,
               MI$sd,
               MI$p.value)}}
  
  colnames(mat)<-c("observed","expected", "sd", "p.value")
  mat<-cbind(t.id, mat)
  
  return(mat)}

spatialAutoc<-function(model, names.spp, an.IDCV){
  
  #Preliminaries
  load("~/Documents/TRAINING DATA/normalized (min.max) x training data.RData")
  
  norm.x.m$time[norm.x.m$time>=0.25 & norm.x.m$time<0.75]<-"day"
  norm.x.m$time[norm.x.m$time!="day"]<-"night"
  norm.x.m$time<-as.factor(norm.x.m$time)
  
  
  all.samples<-paste(norm.x.m$tow,norm.x.m$sample.no., sep="-")
  x.dat<-data.frame(spl.id=sort(all.samples), norm.x.m[order(all.samples),])
  all.samples<-sort(all.samples)
  
  
  rm(norm.x.m)
  
  require(geosphere)
  require(ape)
  
  if(model=="rf_i"){j<-1}else{j<-2}
  
  moransOC.spp<-vector("list", length(names.spp))
  names(moransOC.spp)<-names.spp
  moransAC.spp<-vector("list", length(names.spp))
  names(moransAC.spp)<-names.spp
  
  for(spp in names.spp){
    
    
    y.erOC<-NULL
    prOC<-NULL
    x.indOC<-NULL
    
    y.erAC<-NULL
    prAC<-NULL
    x.indAC<-NULL
    
    for(i in 1:cv.no){
      
      load(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.IDCV, i,"/forests/",spp,"/",model,".Rdata", sep=""))
      load(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.IDCV, i,"/forests/",spp,"/results.Rdata", sep=""))
      
      
      y.erAC<-c(y.erAC, res[[model]][["rf.p"]]$AC$y.er)
      prAC<-c(prAC, res[[model]][["rf.p"]]$AC$pr)
      x.indAC<-c(x.indAC, rf.dat$test.indAC)
      
      y.erOC<-c(y.erOC, res[[model]][["rf.p"]]$OC$y.er)
      prOC<-c(prOC, res[[model]][["rf.p"]]$OC$pr)
      x.indOC<-c(x.indOC, rf.dat$test.ind)
    }
    
    
    moransOC.spp[[spp]]<-moranCalc(x.dat, y.er=y.erOC, pr=prOC, x.ind=x.indOC)
    moransAC.spp[[spp]]<-moranCalc(x.dat, y.er=y.erAC, pr=prAC, x.ind=x.indAC)}
  
  save(moransOC.spp, moransAC.spp, file=paste("~/Documents/TRAINING DATA/Models/randomForest/model assess NC CV1/forests/Spatial Autocorr ",
                                         model, ".Rdata", sep=""))}


#t.res = 1<-plot each timepoint, 2<-plot monthly composites, 3<-plot all on single plot
plotResidMaps<-function(model="rf_i", t.res=c(1,2,3), names.spp, an.IDCV){

  #Preliminaries
  dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.IDCV, 1,"/resid maps/", sep=""))
  load("~/Documents/TRAINING DATA/normalized (min.max) x training data.RData")

norm.x.m$time[norm.x.m$time>=0.25 & norm.x.m$time<0.75]<-"day"
norm.x.m$time[norm.x.m$time!="day"]<-"night"
norm.x.m$time<-as.factor(norm.x.m$time)


all.samples<-paste(norm.x.m$tow,norm.x.m$sample.no., sep="-")
x.dat<-data.frame(spl.id=sort(all.samples), norm.x.m[order(all.samples),])
all.samples<-sort(all.samples)


rm(norm.x.m)

require(maps)

if(model=="rf_i"){j<-1}else{j<-2}


for(spp in names.spp){
  
  dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.IDCV, 1
                   ,"/resid maps/",spp,"/", sep=""))
  
  y.erOC<-NULL
  prOC<-NULL
  x.indOC<-NULL
  
  y.erAC<-NULL
  prAC<-NULL
  x.indAC<-NULL
  
  for(i in 1:cv.no){
    
    load(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.IDCV, i,"/forests/",spp,"/",model,".Rdata", sep=""))
    load(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.IDCV, i,"/forests/",spp,"/results.Rdata", sep=""))
    
    
    y.erAC<-c(y.erAC, res[[model]][["rf.p"]]$AC$y.er)
    prAC<-c(prAC, res[[model]][["rf.p"]]$AC$pr)
    x.indAC<-c(x.indAC, rf.dat$test.indAC)
    
    y.erOC<-c(y.erOC, res[[model]][["rf.p"]]$OC$y.er)
    prOC<-c(prOC, res[[model]][["rf.p"]]$OC$pr)
    x.indOC<-c(x.indOC, rf.dat$test.ind)
  }
  
  if(1 %in% t.res){
  plotAllMaps(x.dat=x.dat, y.er=y.erAC, pr=prAC, x.ind=x.indAC, AC=T, spp=spp, an.IDCV)
  plotAllMaps(x.dat, y.er=y.erOC, pr=prOC, x.ind=x.indOC, AC=F, spp=spp, an.IDCV)}
  
  if(2 %in% t.res){
    plotmMaps(x.dat=x.dat, y.er=y.erAC, pr=prAC, x.ind=x.indAC, AC=T, spp=spp, an.IDCV)
    plotmMaps(x.dat, y.er=y.erOC, pr=prOC, x.ind=x.indOC, AC=F, spp=spp, an.IDCV)}
  
  if(3 %in% t.res){
    plotMap(x.dat=x.dat, y.er=y.erAC, pr=prAC, x.ind=x.indAC, AC=T, spp=spp, an.IDCV)
    plotMap(x.dat, y.er=y.erOC, pr=prOC, x.ind=x.indOC, AC=F, spp=spp, an.IDCV)}
}
}

  
  
plotAllMaps<-function(x.dat, y.er, pr, x.ind, AC=T, spp=spp, an.IDCV){
      
        require(maps)
        
        geo.cood<-x.dat[x.ind,2:3]
        
        timepoints<-unNormalise(x.dat[x.ind,c("month", "year")],c("month", "year"))
        t.id<-unique(timepoints)
        
        geo.coo<-as.matrix(unNormalise(geo.cood, 
                                       c("lon", "lat")))
        
        resid<-(pr-y.er)
        
        
        blues<-colorRampPalette(c("dodgerblue4",  "cadetblue1"))(4)
        reds<-colorRampPalette(c("lightpink",  "darkred"))(4)
        
        my.palette<-c(blues,"gray53",reds)[match(resid, -4:4)]
        
        for(year in sort(unique(t.id$year))){
         
          if(AC){
          png(file = paste("~/Documents/TRAINING DATA/Models/randomForest/",an.IDCV, 1
                           ,"/resid maps/",spp,"/AC resid map", year , sep=""), 
              width=150*8.3, height=150*11.7, pointsize=30)}else{
                png(file = paste("~/Documents/TRAINING DATA/Models/randomForest/",an.IDCV, 1
                                 ,"/resid maps/",spp,"/OC resid map", year , sep=""), 
                    width=150*8.3, height=150*11.7, pointsize=30)  
              }
          
          par(mfrow=c(4,3), oma=c(0,0,4,0))
          
          for(month in 1:12){
            
            if(month %in% unique(t.id$month)){
              
              map("world", xlim=c(-4,11), ylim=c(51,61), fill=TRUE, 
                  mar = c(0.1, 0.1, 0.1, 0.1))
              
              cols<-my.palette[timepoints$month==month & 
                                                 timepoints$year==year]
              
              points(x=geo.coo[timepoints$month==month & 
                                  timepoints$year==year, "lon"], 
                     y=geo.coo[timepoints$month==month & 
                                 timepoints$year==year, "lat"], 
                     pch=20, cex=0.5, col=cols)
              
              text(x=3, y=60, labels=month)}else{frame()}}
          mtext(year,side=3,line=-0.2,cex=1.5,outer=TRUE)
          
          dev.off()
        }
  }
       

plotmMaps<-function(x.dat, y.er, pr, x.ind, AC=T, spp=spp, an.IDCV){
  
  require(maps)
  
  geo.cood<-x.dat[x.ind,2:3]
  
  timepoints<-unNormalise(x.dat[x.ind,c("month", "year")],c("month", "year"))
  t.id<-unique(timepoints)
  
  geo.coo<-as.matrix(unNormalise(geo.cood, 
                                 c("lon", "lat")))
  
  resid<-(pr-y.er)
  
  
  blues<-colorRampPalette(c("dodgerblue4",  "cadetblue1"))(4)
  reds<-colorRampPalette(c("lightpink",  "darkred"))(4)
  
  my.palette<-c(blues,"gray53",reds)[match(resid, -4:4)]
  

    
    if(AC){
      png(file = paste("~/Documents/TRAINING DATA/Models/randomForest/",an.IDCV, 1
                       ,"/resid maps/",spp,"AC monthly resid map",sep=""), 
          width=150*8.3, height=150*11.7, pointsize=30)}else{
            png(file = paste("~/Documents/TRAINING DATA/Models/randomForest/",an.IDCV, 1
                             ,"/resid maps/",spp,"OC monthly resid map",sep=""), 
                width=150*8.3, height=150*11.7, pointsize=30)  
          }
    
    par(mfrow=c(4,3), oma=c(0,0,4,0))
    
    for(month in 1:12){
      
      if(month %in% unique(t.id$month)){
        
        map("world", xlim=c(-4,11), ylim=c(51,61), fill=TRUE, 
            mar = c(0.1, 0.1, 0.1, 0.1))
        
        cols<-my.palette[timepoints$month==month]
        
        points(x=jitter(geo.coo[timepoints$month==month, "lon"]), 
               y=jitter(geo.coo[timepoints$month==month, "lat"]), 
               pch=20, cex=0.5, col=cols)
        
        text(x=3, y=60, labels=month)}else{frame()}}
   
    
    dev.off()
  
}


plotMap<-function(x.dat, y.er, pr, x.ind, AC=T, spp=spp, an.IDCV){
  
  require(maps)
  
  geo.cood<-x.dat[x.ind,2:3]
  
  timepoints<-unNormalise(x.dat[x.ind,c("month", "year")],c("month", "year"))
  t.id<-unique(timepoints)
  
  geo.coo<-as.matrix(unNormalise(geo.cood, 
                                 c("lon", "lat")))
  
  resid<-(pr-y.er)
  
  
  blues<-colorRampPalette(c("dodgerblue4",  "cadetblue1"))(4)
  reds<-colorRampPalette(c("lightpink",  "darkred"))(4)
  
  my.palette<-c(blues,"gray53",reds)[match(resid, -4:4)]
  
  
  
  if(AC){
    png(file = paste("~/Documents/TRAINING DATA/Models/randomForest/",an.IDCV, 1
                     ,"/resid maps/",spp,"AC overall resid map",sep=""), 
        width=150*8.3, height=150*11.7, pointsize=30)}else{
          png(file = paste("~/Documents/TRAINING DATA/Models/randomForest/",an.IDCV, 1
                           ,"/resid maps/",spp,"OC overall resid map",sep=""), 
              width=150*8.3, height=150*11.7, pointsize=14)  
        }
  
  par(mfrow=c(1,1), oma=c(1,1,1,1))
  


      
      map("world", xlim=c(-4,11), ylim=c(51,61), fill=TRUE, 
          mar = c(5, 4, 1, 1))
      
      cols<-my.palette
      
      points(x=jitter(geo.coo[, "lon"]), 
             y=jitter(geo.coo[, "lat"]), 
             pch=20, cex=0.5, col=cols)
      
      dev.off()
  
}
