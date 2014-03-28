sampleTows<-function(split=4/5){
  
  
  load(file=paste("/Users/annakrystalli/documents/TRAINING DATA/y training data.RData", sep=""))
  load("~/Documents/TRAINING DATA/normalized (min.max) x training data.RData")
  
  tow.n<-length(unique(norm.x.m$tow))
  
  s<-sample(unique(norm.x.m$tow), size=tow.n, replace=FALSE)
  
  s.l<-rep(NA,times=length(tow.n))
  for (i in 1:tow.n){
    s.l[i]<-sum(norm.x.m$tow==s[i])}
  
  s.sum<-rep(NA,times=length(tow.n))
  for (i in 1:tow.n){
    s.sum[i]<-sum(s.l[1:i])}
  
  tr.tows<-s[1:(min(which(s.sum>=(split)*dim(norm.x.m)[[1]], arr.ind=TRUE))-1)]
  ts.tows<-s[min(which(s.sum>=(split)*dim(norm.x.m)[[1]], arr.ind=TRUE)):tow.n]
  
  train.dat.m<-which(norm.x.m$tow%in%tr.tows, arr.ind=TRUE)
  test.dat.m<-which(norm.x.m$tow%in%ts.tows, arr.ind=TRUE)
  
  save(train.dat.m, test.dat.m, 
       file="~/Documents/TRAINING DATA/Dataset tow splits monthly.RData")
  
  
  names.spp<-names(y.m)
  names.spp[3]<-"cfin"
  names.spp[4]<-"chel"
  names(y.m)<-names.spp
  
  
  
  
  
  
  for(spp.id in 1:5){
    
    spp<-names.spp[spp.id]
    y.oc<-y.m[train.dat.m, spp.id]
    y.oc[y.oc>=1]<-1
    y.toc<-y.m[test.dat.m, spp.id]
    y.toc[y.toc>=1]<-1
    
    x.oc<-norm.x.m[train.dat.m,]
    x.toc<-norm.x.m[test.dat.m, ]
    
    
    y<-y.m[train.dat.m, spp.id]
    y<-y[y.oc>=1]
    
    x<-norm.x.m[train.dat.m,]
    x<-x[y.oc>=1,]
    
    y.t<-y.m[test.dat.m, spp.id]
    y.t<-y.t[y.toc>=1]
    
    x.t<-norm.x.m[test.dat.m, ]
    x.t<-x.t[y.toc>=1,]
    
    
    y.t<-as.factor(y.t) 
    y.toc<-as.factor(y.toc) 
    y.oc<-as.factor(y.oc)
    y<-as.factor(y)
    
    
    
    
    save(y, y.oc, y.t, y.toc, x, x.oc, x.t, x.toc,
         file=paste("~/Documents/TRAINING DATA/Models/randomForest/",spp, "_train_tow_data.Rdata", sep=""))  
    
    
  }
}

#______________________________________________________________________________________________________
sampCombineForest<-function(x, y, m=30, trees=200, varnames=v2){
  
  #_______
  require(doBy)
  selectX<-function(X, varnames){X[,which(names(X) %in% varnames)]}
  #____________________________________________
  
  smps<-NULL
  samp.ids<-paste(x$tow,x$sample.no., sep="-")
  ss<-length(unique(samp.ids))
  n=length(y)
  
  fmla <- as.formula(paste("y ~ ", paste(varnames ,collapse= " + ", sep="")))
  
  for(i in 1:m){
    
    smp<-sampleBy(~samp.ids, frac=ss/n, data=cbind(x, y, samp.ids))
    smps<-rbind(smps, smp)
    
    if(i==1){ print(i) 
              
              fmla <- as.formula(paste("y ~ ", paste(varnames ,collapse= " + ", sep="")))          
              
              rforSMP<-randomForest(formula=fmla, data=smp, ntree=trees, sampsize=ceiling(.632*nrow(smp)),
                                    replace=TRUE, classwt=NULL, keep.forest=TRUE)}else{
                                      print(i)  
                                      
                                      fmla <- as.formula(paste("y ~ ", paste(varnames ,collapse= " + ", sep="")))
                                      
                                      rforSMP.n<-randomForest(formula=fmla, data=smp, ntree=trees,
                                                              replace=TRUE, classwt=NULL, keep.forest=TRUE,
                                                              sampsize = ceiling(.632*nrow(smp)))
                                      
                                      rforSMP<-combine(rforSMP, rforSMP.n)}}
  
  preds<-predict(rforSMP, newdata=selectX(smps, varnames))
  print(table(smps$y, preds))
  
  return(rforSMP)
  
}







v2<-c("lon", "lat",  "year",
      "month",  "T.m", "Sed.m", 
      "Sedf.m",  "Ch.m", "fdens.m", 
      "fdist.m",  "fside.m",  "fdens.c.m", 
      "fdist.c.m",  "fside.c.m", 
      "bath", "NAO", "wNAO")

v1<-c("lon", "lat",  "year", "day",
      "month",  "T.m", "Sed.m", 
      "Sedf.m",  "Ch.m", "fdens.m", 
      "fdist.m",  "fside.m",  "fdens.c.m", 
      "fdist.c.m",  "fside.c.m", 
      "bath", "NAO", "wNAO")
#_________________________________________________________________________________________________________________________

selectX<-function(X, varnames){X[,which(names(X) %in% varnames)]}
#____________________________________________

plotACimp<-function(rforAC, spp=spp, plot=TRUE){
  
  if(plot==FALSE){dev.off()
                  png(file = paste("~/Documents/TRAINING DATA/Models/randomForest/forests/",
                                   spp, "/ACimp.png", sep=""), 
                      width=650, height=600, pointsize=12)}
  
  imp.ac<-importance(rforAC)
  var.names.ac<-rownames(imp.ac)
  
  FUN.c<-colorRampPalette(c( "thistle1",  "darkorchid3"), space="Lab")
  cols<-FUN.c(n=max(imp.ac))[imp.ac]
  
  
  par(las=2, col.axis="darkslategray", 
      col.lab="darkslategray", 
      col.main="darkslategray",
      font.axis=2, family="Helvetica",
      font.main=4, oma=c(1,1,1.2,1), mar=c(3,4.2,1,2)) 
  barplot(imp.ac[order(imp.ac)], names.arg=var.names.ac[order(imp.ac)], horiz=TRUE, 
          col=cols[order(imp.ac)], cex.axis=0.8, cex.names=0.9, border="darkorchid4",
          main=paste(spp, "AC rfor variable importance"))
  
  if(plot==FALSE){dev.off()}}
#____________________________________________


plotOCimp<-function(rforOC, spp=spp, plot=TRUE){
  
  if(plot==FALSE){dev.off()
                  png(file = paste("~/Documents/TRAINING DATA/Models/randomForest/forests/",
                                   spp, "/OCimp.png", sep=""), 
                      width=650, height=600, pointsize=12)}
  
  imp.oc<-importance(rforOC)
  var.names.oc<-rownames(imp.oc)
  
  FUN.c<-colorRampPalette(c( "honeydew",  "aquamarine4"), space="Lab")
  cols<-FUN.c(n=max(imp.oc))[imp.oc]
  
  
  par(las=2, col.axis="darkslategray", 
      col.lab="darkslategray", 
      col.main="darkslategray",
      font.axis=2, family="Helvetica",
      font.main=4, oma=c(1,1,1.2,1), mar=c(3,4.2,1,2)) 
  barplot(imp.oc[order(imp.oc)], names.arg=var.names.oc[order(imp.oc)], horiz=TRUE, 
          col=cols[order(imp.oc)], cex.axis=0.8, cex.names=0.9, border="darkslategray4",
          main=paste(spp, "OC rfor variable importance"))
  
  if(plot==FALSE){dev.off()}}
#_________________________________________________________________________________________________________________________


sampleSamples<-function(split=9/10){
  
  
  load(file=paste("/Users/annakrystalli/documents/TRAINING DATA/y training data.RData", sep=""))
  load("~/Documents/TRAINING DATA/normalized (min.max) x training data.RData")
  
  norm.x.m$time[norm.x.m$time>=0.25 & norm.x.m$time<0.75]<-"day"
  norm.x.m$time[norm.x.m$time!="day"]<-"night"
  norm.x.m$time<-as.factor(norm.x.m$time)

  
  names.spp<-names(y.m)
  names.spp[3]<-"cfin"
  names.spp[4]<-"chel"
  names(y.m)<-names.spp
  
  for(spp.id in 1:5){
  spp<-names.spp[spp.id]
  
  all.samples<-paste(norm.x.m$tow,norm.x.m$sample.no., sep="-")
  n<-length(all.samples)
  
  samples<-unique(all.samples)
  tow.n<-length(samples)
  
  y.samp<-y.m[c(1,which(all.samples[-n]!=all.samples[-1], arr.ind=TRUE)+1),spp.id]
  
  strat.dat<-data.frame(samples=samples, y.samp=y.samp)
  strat.dat<-strat.dat[order(strat.dat$y.samp),]
  size<-as.integer(table(y.samp)*split)
  
 if(any(size==0)){
  sp.name<-as.numeric(
    names(which(table(y.samp)==1)))
  size<-size[-(sp.name+1)]
  
  sp.samp<-as.character(strat.dat$samples[strat.dat$y.samp==sp.name])
  sp.n<-length(sp.samp)
  
  sp.strat.dat<-data.frame(samples=all.samples[which(all.samples%in%sp.samp, arr.ind=TRUE)],
                           pixels=which(all.samples%in%sp.samp, arr.ind=TRUE))
 
  tr.sps<-sp.strat.dat$pixel[strata(sp.strat.dat, 
                                                 "samples", 
                                                 size=as.integer(table(sp.strat.dat$samples)*split), 
                                                 method="srswor")[,2]]
  
  strat.dat<-subset(strat.dat, samples %in% setdiff(samples, sp.samp ))
  print(c(spp, sp.name, sp.samp))}
  
  
  
  tr.tows<-as.character(strat.dat$samples[strata(strat.dat, 
                                        "y.samp", 
                                        size=size, 
         method="srswor")[,2]])
  
  .env<-environment()
  if(exists("tr.sps", envir=.env)){tr.tows<-c(tr.tows,tr.sps)}
  
  ts.tows<-setdiff(samples, tr.tows)
  
  train.dat.m<-which(all.samples%in%tr.tows, arr.ind=TRUE)
  
  .env<-environment()
  if(exists("tr.sps", envir=.env)){train.dat.m<-c(train.dat.m,tr.sps)}
  
  test.dat.m<-setdiff(1:n, train.dat.m)


 
  
  save(train.dat.m, test.dat.m, 
       file=paste("~/Documents/TRAINING DATA/Dataset "
                  ,spp," samp splits monthly.RData", sep=""))
  
  

    

    y.oc<-y.m[train.dat.m, spp.id]
    y.oc[y.oc>=1]<-1
    y.toc<-y.m[test.dat.m, spp.id]
    y.toc[y.toc>=1]<-1
    
    x.oc<-norm.x.m[train.dat.m,]
    x.toc<-norm.x.m[test.dat.m, ]
    
    
    y<-y.m[train.dat.m, spp.id]
    y<-y[y.oc>=1]
    
    x<-norm.x.m[train.dat.m,]
    x<-x[y.oc>=1,]
    
    y.t<-y.m[test.dat.m, spp.id]
    y.t<-y.t[y.toc>=1]
    
    x.t<-norm.x.m[test.dat.m, ]
    x.t<-x.t[y.toc>=1,]
    
    
    y.t<-as.factor(y.t) 
    y.toc<-as.factor(y.toc) 
    y.oc<-as.factor(y.oc)
    y<-as.factor(y)
    
    
    
    
    save(y, y.oc, y.t, y.toc, x, x.oc, x.t, x.toc,
         file=paste("~/Documents/TRAINING DATA/Models/randomForest/train_",
                    spp,"_data.Rdata", sep=""))  
    
  if(exists("tr.sps", envir=.env)){rm(tr.sps)}
  }
}


randomForest(x=x.oc, y=y.oc, xtest=x.toc, ytest=y.toc,
             ntree=ntree,
             replace=TRUE, classwt=NULL, 
             strata=y.oc,
             sampsize = as.vector(c(table(y.oc)[1]*.632, 
                                    table(y.oc)[2]*.632)))