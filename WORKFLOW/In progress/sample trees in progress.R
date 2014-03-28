rm(list=ls())
mhvb


  require(PresenceAbsence)
  set.seed(1)
  
  selectX<-function(X, varnames){X[,which(names(X) %in% varnames)]}
  
  varnames<-c("year", "time",
              "month",  "T.m", "Sed.m", 
              "Sedf.m",  "Ch.m", "fdens.m", 
              "fdist.m",  "fside.m",  "fdens.c.m", 
              "fdist.c.m",  "fside.c.m", 
              "bath", "NAO", "wNAO")
  
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
    n.samples<-length(samples)
    test.samples<-sample(samples, size=0.2*n.samples)
    
    
    train.samples<-setdiff(all.samples,test.samples)
    test.ind<-which(all.samples %in% test.samples, arr.ind=T)
    
    train.ind<-which(all.samples %in% train.samples, arr.ind=T)
    train.samples.full<-all.samples[which(all.samples %in% train.samples)]
    train.n<-length(train.ind)  
    n<-length(all.samples)

    split<-split(train.ind, train.samples.full)
    
    
    
    
    
    repRfor<-function(x, spp.id, varnames, ntree){
      
      require(randomForest)
      selectX<-function(X, varnames){X[,which(names(X) %in% varnames)]}
      x.oc<-selectX(norm.x.m[x,], varnames)
      
      y.oc<-y.m[x,spp.id]
      y<-y.oc[y.oc>=1]
      y.oc[y.oc>=1]<-1
      
      x<-x.oc[y.oc>=1,]
      
      y.oc<-as.factor(y.oc)
      y<-as.factor(y)

      
      rforOC<-randomForest(x=x.oc, y=y.oc, keep.forest=TRUE,
                           ntree=ntree,
                           replace=TRUE, classwt=NULL, 
                           strata=y.oc,
                           sampsize = as.vector(table(y.oc)*.632))
      
      return(rforOC)
      
      
    }
    
    spp.id=5
    spp<-names.spp[spp.id]
    
    splits<-apply(replicate(200,unlist(lapply(split, sample, size=1, replace=F)), 
                            simplify="list"), 2, list)
    
    splits<-lapply(splits, unlist, 2)
    
    
    rforOCs<-lapply(splits, repRfor, spp.id=spp.id, varnames=varnames, ntree=1)
    rforOC.com<-do.call(combine, rforOCs)
    
    save(rforOC.com, file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                            an.ID,"forests/",
                            spp, "/OC.com forest.Rdata", sep=""))
    
    
    pr<-as.numeric(predict(rforOC.com, 
                           newdata=selectX(norm.x.m[test.ind,], varnames),
                           type="response"))-1
    
    probs<-predict(rforOC.com, 
                           newdata=selectX(norm.x.m[test.ind,], varnames),
                           type="prob")
    
    
    y.er<-y.m[test.ind,spp.id]
    y.er[y.er>=1]<-1
    
    pr.adj<-pr
    pr.adj[probs[,2]>=0.2631579]<-1
    pr.adj[probs[,2]<0.2631579]<-0
    
    print(sum(pr!=y.er)/length(pr))
    
    require(verification)
    
    roc.plot.calculate(DATA=data.frame(id=1, y.er=y.er, pr=probs))
    roc.area(y.er, probs[,2])
    
    table(pr, y.er)
    table(pr.adj, y.er)
    print(sum(pr.adj!=y.er)/length(pr))
    
    optimal.thresholds(DATA=data.frame(id=1, y.er=y.er, pr=probs),
                       threshold=20,
                       which.model=1,
                       opt.methods=1:12)
    
    cohen.kappa(x=cbind(y.er, pr))
    cohen.kappa(x=cbind(y.er, pr.adj))
    
    varImpPlot(rforOC.com)
    