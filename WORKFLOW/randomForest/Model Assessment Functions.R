



  require(PresenceAbsence)
  require(randomForest)
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
  

  all.samples<-paste(norm.x.m$tow,norm.x.m$sample.no., sep="-")
  
  x.dat<-data.frame(spl.id=sort(all.samples), norm.x.m[order(all.samples),])
  y.m<-y.m[order(all.samples),]
  all.samples<-sort(all.samples)
  
  
  names.spp<-names(y.m)
  names.spp[3]<-"cfin"
  names.spp[4]<-"chel"
  names(y.m)<-names.spp
  
  y.moc<-y.m
  y.moc[y.m!=0]<-1
  
  rm(norm.x.m)
  
rftune<-function(all.samples, y.m, y.moc, x.dat, 
                 varnames, test.size, spl="independent"){
  
            require(randomForest)
          
            selectX<-function(X, varnames){X[,which(names(X) %in% varnames)]}
            
            rftunes<-vector("list", 10)
            
            for(spp.id in 1:5){
              set.seed(1)
              if(spl=="dependent"){
              rfc.dat<-testTrainSplit(all.samples, y.m, test.size=test.size, 
                                      spp.id, spl="dependent")}else{
                                        rfc.dat<-testTrainSplit(all.samples, y.m, 
                                                       test.size=test.size,
                                                       spp.id, spl="independent")}
              
              if(spl=="combined"){tune.s<-sampleSamples(rfc.dat$train.ind,
                                    rfc.dat$train.samples.full, 
                                    y=y.m[,spp.id], all.samples=all.samples)}
              if(spl %in% c("dependent", "independent")){tune.s<-rfc.dat$train.ind}
              if(spl=="balanced"){tune.s<-sampleBalancedCats(rfc.dat$train.ind, 
                                                             rfc.dat$train.samples.full,
                                                             all.samples, 
                                                             y=y.m, type="oc", 
                                                             spp.id)}
              
              rftunes[[spp.id]]<-tuneRF(selectX(x.dat[tune.s,], varnames),
                                        as.factor(y.moc[tune.s, spp.id]), 
                                        mtryStart=4, ntreeTry=100, stepFactor=2, 
                                        improve=0.05, trace=TRUE, plot=F)  
              
              if(spl=="combined"){tune.sAC<-sampleSamples(rfc.dat$train.indAC, 
                                      rfc.dat$train.samples.fullAC,
                                        y=y.m[,spp.id], all.samples=all.samples)}
              if(spl %in% c("dependent", "independent")){tune.sAC<-rfc.dat$train.indAC}
              if(spl=="balanced"){tune.sAC<-sampleBalancedCats(train.ind=rfc.dat$train.indAC, 
                                                             train.samples.full=rfc.dat$train.samples.fullAC,
                                                             all.samples, 
                                                             y=y.m, type="ac", 
                                                             spp.id)}
              
              rftunes[[spp.id+5]]<-tuneRF(selectX(x.dat[tune.sAC,], varnames), 
                                          as.factor(y.m[tune.sAC, spp.id]), 
                                          mtryStart=4, ntreeTry=100, stepFactor=2, 
                                          improve=0.05, trace=TRUE, plot=F)} 
            
            rftune<-lapply(rftunes, function(x){x[which(x[,2]==min(x)),1]})
            rftune<-matrix(rftune, 5, 2, byrow=F, 
                           dimnames=list(names.spp, c("oc", "ac")))
            
            return(rftune)}
   


    growForest<-function(x=x, y=y, ntree=200,
                         varnames=varnames, set.seed=TRUE, an.ID, spp=spp, type="oc",
                         mtry=mtry){
      
      dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID, 
                       sep=""), 
                 showWarnings = F)
      dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID, 
                       "forest plots/",sep=""), 
                 showWarnings = F)
      dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID, 
                       "forests/",sep=""), 
                 showWarnings = F)
      
      dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID, 
                       "forests/",spp,sep=""), 
                 showWarnings = F)
      
      require(randomForest)
      
      x<-selectX(x, varnames)
      
    
      if(type=="oc"){y[y>=1]<-1}
      
      y<-as.factor(y)
      
      if(set.seed==TRUE){set.seed(seed=1)}
      
      return(assign(paste("rf", type, sep=""),randomForest(x=x, y=y, keep.forest=TRUE,
                           ntree=ntree,
                           replace=TRUE, classwt=NULL, 
                           strata=y,
                           sampsize = as.vector(table(y)*.632),
                                                           mtry=mtry)))
    }
    

    
    
    
    
    testTrainSplit<-function(all.samples, y.m, test.size, spp.id, spl){
            n<-length(all.samples)
            
            samples<-sort(unique(all.samples))
            n.samples<-length(samples)
            
        if(spl=="dependent"){
            test.ind<-sample(1:n, size=test.size*n)
            test.samples<-sort(samples[which(samples %in% all.samples[test.ind])])
            
            train.ind<-sort(setdiff(1:n,test.ind))    
            train.samples<-sort(samples[which(samples %in% all.samples[train.ind])])
            train.samples.full<-sort(all.samples[train.ind])
            dup.spls=NULL
            samp.means<-unlist(lapply(split(y.m[, spp.id], all.samples), mean))}else{samp.means<-unlist(
              lapply(split(y.m[, spp.id], all.samples), mean)) 
                               size<-table(samp.means)
            
            if(any(size==1)){cat<-as.numeric(names(size)[which(size==1)])
                            dup.spls<-names(samp.means)[which(samp.means==cat)]
                            samples<-setdiff(samples, dup.spls)
                             samp.means<-samp.means[samp.means!=cat]
                             n.samples<-length(samples)}else{dup.spls=NULL}
            
            test.samples<-sort(unlist(lapply(split(samples,samp.means),
                                      FUN=function(x, test.size){
                                        sample(x, size=test.size*length(x))},
                                      test.size=test.size)))
                               
            train.samples<-sort(setdiff(all.samples,test.samples))
            
            if(any(size==1)){test.samples<-sort(c(test.samples,dup.spls))
                             train.samples<-sort(c(train.samples,dup.spls))
                             samples<-c(samples, dup.spls)
                             samp.means<-unlist(lapply(split(y.m[, spp.id], all.samples), mean))}
            
            
            test.ind<-which(all.samples %in% test.samples, arr.ind=T)
            
            train.ind<-sort(which(all.samples %in% train.samples, arr.ind=T))
            train.samples.full<-sort(all.samples[which(all.samples %in% train.samples)])}
            
            train.n<-length(train.ind)  
            test.n<-length(test.ind)
            trainAC.n<-sum(y.m[train.ind, spp.id]!=0)
            testAC.n<-sum(y.m[test.ind, spp.id]!=0)
            
            train.indAC<-sort(train.ind[train.ind %in% which(all.samples %in% names(which(samp.means!=0)))])
            test.indAC<-sort(test.ind[test.ind %in% which(all.samples %in% names(which(samp.means!=0)))])
            train.samples.fullAC<-sort(all.samples[train.indAC])
            
            return(list(n=n,
                        samples=samples,
                        n.samples=n.samples,
                        test.samples=test.samples,
                        test.ind=test.ind,
                        test.n=test.n,
                        test.indAC=test.indAC,
                        testAC.n=testAC.n,
                        train.samples=train.samples,
                        train.ind=train.ind,
                        train.n=train.n,
                        train.indAC=train.indAC,
                        trainAC.n=trainAC.n,
                        train.samples.full=train.samples.full,
                        train.samples.fullAC=train.samples.fullAC,
                        dup.spls=dup.spls))}

    sampleSamples<-function(train.ind, train.samples.full, y=y.m[,spp.id], all.samples){
      
          split<-split(train.ind, train.samples.full)
          s<-unlist(lapply(split, sample, size=1, replace=T))
          diff<-names(s[which(s %in% setdiff(s, train.ind))])
          if(length(diff)!=0){s[which(s %in% setdiff(s, train.ind))]<-which(all.samples %in% diff)}
          return(s)
          
    }
    
    sampleBalancedCats<-function(train.ind, train.samples.full,
                                 all.samples, y=y.m[,spp.id], type, spp.id, 
                                 min.cat=10){
    
            if(type=="oc"){y<-y[,spp.id]
                          y[y>=1]<-1}else{y<-y[,spp.id]}
          
          train.samples<-unique(train.samples.full)    
          
          cat.split<-unlist(lapply(split(y[train.ind], train.samples.full), mean))
          
          spl.sz<-min(table(cat.split))  
          if(spl.sz>=min.cat){
            
            spl.cats<-as.numeric(names(table(cat.split)[(table(cat.split)>=min.cat)]))
   
            spls<-unlist(lapply(split(train.samples[cat.split %in% spl.cats],cat.split[cat.split %in% spl.cats]), 
                                sample, size=spl.sz))}else{
             spl.cats<-as.numeric(names(table(cat.split)[(table(cat.split)>=min.cat)]))
             spls<-unlist(lapply(split(train.samples[cat.split %in% spl.cats],cat.split[cat.split %in% spl.cats]), 
                                sample, size=min.cat))
             spls<-sort(c(spls,train.samples[cat.split %in% setdiff(cat.split,spl.cats)]))
                          }
            
          s<-unlist(lapply(split(which(all.samples %in% spls), 
                       all.samples[all.samples %in% spls]), sample, size=1, replace=T))
            diff<-names(s[which(s %in% setdiff(s, train.ind))])
            if(length(diff)!=0){s[which(s %in% setdiff(s, train.ind))]<-which(all.samples %in% diff)}
    
        return(s)}
    
    repRfor<-function(s, data=x.dat, y , spp.id, varnames, ntree, type,
                      train.ind, mtry){
      
            require(randomForest)
            selectX<-function(X, varnames){X[,which(names(X) %in% varnames)]}
            
            if(type=="oc"){y<-y[s,spp.id]
                          y[y>=1]<-1
                          y<-as.factor(y)}else{
            y<-factor(y[s,spp.id], 
                      levels=sort(unique(y[train.ind,spp.id])))}
            
            x<-selectX(data[s,], varnames)
      
          rfor<-randomForest(x=x, y=y, keep.forest=TRUE,
                           ntree=ntree,
                           replace=TRUE, classwt=NULL, 
                           strata=y,
                           sampsize = as.vector(table(y)*0.632),
                           mtry=mtry)
      
      return(rfor)
      
      
    }
    
GrowRFs<-function(x.dat, y.m,varnames, test.size=0.2, model){
            
            set.seed(1)
  #....DEPENDENT TEST DATA.....   
            
 if(model=="D"|| model=="ALL"){  rf.dat<-testTrainSplit(all.samples, y.m, test.size=test.size,
                               spp.id, spl="dependent")
            
            #...Standard RF..... 
                rfOC<-growForest(x=x.dat[rf.dat$train.ind,], y=y.m[rf.dat$train.ind, spp.id], ntree=200,
                                 varnames=varnames, set.seed=TRUE, an.ID, spp=spp, type="oc",
                                 mtry=tune[spp.id,1][[1]])
                
                rfAC<-growForest(x=x.dat[rf.dat$train.indAC,], y=y.m[rf.dat$train.indAC, spp.id], ntree=200,
                                 varnames=varnames, set.seed=TRUE, an.ID, spp=spp, type="ac",
                                 mtry=tune[spp.id,2][[1]])
                
                
                save(rf.dat, rfOC,rfAC, tune, 
                     file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                                an.ID,"forests/",
                                spp, "/rf.Rdata", sep=""))}
                
        
    #....INDEPENDENT TEST DATA.....
            set.seed(1)
            
 if(model!="D"){rf.dat<-testTrainSplit(all.samples, y.m, test.size=test.size,
                                   spp.id, spl="independent")}

 if(model=="I"|| model=="ALL"){            
          #...Standard RF..... 
            set.seed(10)
            rfOC<-growForest(x=x.dat[rf.dat$train.ind,], y=y.m[rf.dat$train.ind, spp.id], ntree=200,
                             varnames=varnames, set.seed=TRUE, an.ID, spp=spp, type="oc",
                             mtry=tune.i[spp.id,1][[1]])
            
            set.seed(100)
            rfAC<-growForest(x=x.dat[rf.dat$train.indAC,], y=y.m[rf.dat$train.indAC, spp.id], ntree=200,
                             varnames=varnames, set.seed=TRUE, an.ID, spp=spp, type="ac",
                             mtry=tune.i[spp.id,2][[1]])
            
            
            save(rf.dat, rfOC,rfAC, tune.i, 
                 file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                            an.ID,"forests/",
                            spp, "/rf_i.Rdata", sep=""))}

if(model=="C"|| model=="ALL"){  
      #....Combined RF.....     
            set.seed(10)
            split.oc<-lapply(apply(replicate(500, sampleSamples(rf.dat$train.ind,rf.dat$train.samples.full,
                                                         y=y.m[,spp.id], all.samples), 
                                                   simplify="list"), 2, list), unlist)
            
            set.seed(100)
                  rfOC.c<-lapply(split.oc, repRfor, data=x.dat, y=y.m,
                                 spp.id=spp.id, varnames=varnames, ntree=1, type="oc", 
                                 train.ind=rf.dat$train.ind, mtry=tune.c[spp.id,1][[1]])
                  
                  rfOC.c<-do.call(combine, rfOC.c)
            
            set.seed(1000)        
                  split.ac<-lapply(apply(replicate(500, sampleSamples(rf.dat$train.indAC,rf.dat$train.samples.fullAC,
                                                         y=y.m[,spp.id], all.samples), 
                                                   simplify="list"), 2, list), unlist)
            
            set.seed(10000)      
                  rfAC.c<-lapply(split.ac, repRfor, data=x.dat, y=y.m,
                                 spp.id=spp.id, varnames=varnames, ntree=1, type="ac", 
                                 train.ind=rf.dat$train.indAC,mtry=tune.c[spp.id,2][[1]])
                  
                  rfAC.c<-do.call(combine, rfAC.c)
                  
              save(rf.dat, rfOC.c,rfAC.c, tune.c,split.oc, split.ac, 
                   file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                              an.ID,"forests/",
                              spp, "/rf_c.Rdata", sep=""))}
        
        
        
if(model=="B"|| model=="ALL"){          
        #....Balanced RF.....
            set.seed(10)
                    split.oc<-lapply(apply(replicate(500,sampleBalancedCats(rf.dat$train.ind, 
                                                                            rf.dat$train.samples.full,
                                                                            all.samples, 
                                                                            y=y.m, type="oc", spp.id), 
                                                     simplify="list"), 2, list), unlist)
          
            set.seed(100)       
                    rfOC.b<-lapply(split.oc, repRfor, data=x.dat, y=y.m,
                                   spp.id=spp.id, varnames=varnames, ntree=1, type="oc", 
                                   train.ind=rf.dat$train.ind, tune.b[spp.id,1][[1]])
                    
                    rfOC.b<-do.call(combine, rfOC.b)
          
            set.seed(10)
                    split.ac<-lapply(apply(replicate(500,sampleBalancedCats(rf.dat$train.indAC, 
                                                                            rf.dat$train.samples.fullAC,
                                                                            all.samples, 
                                                                            y=y.m, type="ac", spp.id), 
                                                                  simplify="list"), 2, list), unlist)
          
            set.seed(100)        
                    rfAC.b<-lapply(split.ac, repRfor, data=x.dat, y=y.m,
                                   spp.id=spp.id, varnames=varnames, ntree=1, type="ac", 
                                   train.ind=rf.dat$train.indAC, tune.b[spp.id,2][[1]])
                    
                    rfAC.b<-do.call(combine, rfAC.b)
                        
                 save(rf.dat, rfOC.b,rfAC.b, tune.b, split.oc, split.ac,
                 file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                            an.ID,"forests/",
                            spp, "/rf_b.Rdata", sep=""))}
            
if(model=="ALL"){return(list(rfOC, rfAC, rfOC.c, rfAC.c,rfOC.b, rfAC.b))}}
  
  multi.AUC<-function(y=y.er, probs=probs){
    
    nR = length(y)
    nC = 1
    nY = table(y)
    uL = as.factor(rownames(nY))
    nL = length(nY)
    
    L = matrix(rep(uL, each = nR), nR, nL)
    
    per = combs(1:nL, 2)
    nP = nrow(per)
    Auc = matrix(0.5, nP, nC)
    rownames(Auc) = paste(uL[per[, 1]], " vs. ", uL[per[, 2]], 
                          sep = "")
    
    
    idxL = vector(mode = "list", length = nL)
    for (i in 1:nL) idxL[[i]] = which(y == uL[i])
    
    for (i in 1:nP) {
      c1 = per[i, 1]
      c2 = per[i, 2]
      n1 = nY[c1]
      n2 = nY[c2]
      X<-probs[,c1]
      if (n1 > 0 & n2 > 0) {
        r = rank(c(X[idxL[[c1]]], X[idxL[[c2]]]))
        Auc[i, 1] = (sum(r[1:n1]) - n1 * (n1 + 1)/2)/(n1 * 
                                                        n2)
      }
    }
    return(list(class.AUC=Auc, AUC=mean(Auc)))
    
  }
  
  performPr<-function(y.m, y.moc, rf, x.dat, rf.dat, type, evaluate, spp.id){
    
    #.....train set validation
    
    if(evaluate=="train"){
      if(type=="oc"){y.er<-y.moc[rf.dat$train.ind,spp.id]
                     pr<-as.numeric(predict(rf, 
                                            newdata=selectX(x.dat[rf.dat$train.ind,], varnames),
                                            type="response"))-1
                     
                     probs<-predict(rf, 
                                    newdata=selectX(x.dat[rf.dat$train.ind,], varnames),
                                    type="prob")}else{
                                      y.er<-y.m[rf.dat$train.indAC,spp.id]
                                      
                                      pr<-as.numeric(predict(rf, 
                                                             newdata=selectX(x.dat[rf.dat$train.indAC,], varnames),
                                                             type="response"))
                                      probs<-predict(rf, 
                                                     newdata=selectX(x.dat[rf.dat$train.indAC,], varnames),
                                                     type="prob")
                                      y.er<-y.er[y.er!=0]
                                    }}
    
    
    
    #.....test set validation
    
    
    if(evaluate=="test"){
      if(type=="oc"){y.er<-y.moc[rf.dat$test.ind,spp.id]
                     pr<-as.numeric(predict(rf, 
                                            newdata=selectX(x.dat[rf.dat$test.ind,], varnames),
                                            type="response"))-1
                     
                     probs<-predict(rf, 
                                    newdata=selectX(x.dat[rf.dat$test.ind,], varnames),
                                    type="prob")}else{
                                      y.er<-y.m[rf.dat$test.indAC,spp.id]
                                      pr<-as.numeric(predict(rf, 
                                                             newdata=selectX(x.dat[rf.dat$test.indAC,], varnames),
                                                             type="response"))
                                      probs<-predict(rf, 
                                                     newdata=selectX(x.dat[rf.dat$test.indAC,], varnames),
                                                     type="prob") 
                                    }}
    
    return(list(y.er=y.er, pr=pr, probs=probs))
  }
  
  measurePerformance<-function(y, pr, probs, type, rf){
    
    require(nnet)
    require(PerfMeas)
    require(verification)
    require(caret)
    require(psych)
    require(randomForest)
    require(PresenceAbsence)
    require(caTools)
    
    if(type=="oc"){
      
      
      AUC<-list(A=roc.area(y, probs[,2])$A, 
                p=roc.area(y, probs[,2])$p.value)
      
      thresholds<-optimal.thresholds(DATA=data.frame(id=1, y=y, pr=probs[,2]),
                                     threshold=20,
                                     which.model=1,
                                     opt.methods=1:12)
      tab.stats<-table.stats(as.numeric(y), as.numeric(pr),silent = FALSE)
      hists<-list(y=hist(y, breaks=2), pr=hist(pr, breaks=2))}
    
    
    Acc<-confusionMatrix(pr, y)
    F<-F.measure.single.over.classes(class.ind(y), class.ind(pr))
    
    k<-cohen.kappa(x=cbind(y, pr))
    
    if(type=="ac"){
      mean.Acc<-colMeans(Acc$byClass)
      TSS<-list(class=Acc$byClass[,1] + Acc$byClass[,2] - 1, mean=mean.Acc[1] + mean.Acc[2] - 1)
      mAUC<-multi.AUC(y, probs)
      hists<-list(y=hist(as.numeric(y), breaks=0:max(as.numeric(y))), 
                  pr=hist(as.numeric(pr), breaks=0:max(as.numeric(y))))}
    
    err.dat<-data.frame(err.id=which(pr!=y), err=abs(as.numeric(pr)-as.numeric(y))[which(pr!=y)])
    var.order<-(length(importance(rf))+1)-(rank(importance(rf)))
    var.imp<-data.frame(var=rownames(importance(rf))[var.order],MeanDecreaseGini=rev(sort(importance(rf))))
    
    
    if(type=="oc"){return(list(Acc=Acc,  AUC=AUC, F=F, k=k, tab.stats=tab.stats, thresh=thresholds,
                               hists=hists, err.dat=err.dat,var.order=var.order, var.imp=var.imp))}else{
                                 return(list(Acc=Acc, mean.Acc=mean.Acc, AUC=mAUC, F=F, k=k, TSS=TSS, 
                                             hists=hists, err.dat=err.dat,var.order=var.order, var.imp=var.imp))
                               }
  } 
  
  processRun<-function(run, y.m, y.moc, x.dat, an.ID, spp.id, names.spp){
    
    print(run)
    
    spp<-names.spp[spp.id]
    
    load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                    an.ID,"forests/",
                    spp, "/", run, ".Rdata", sep=""))
    
    rfOC<-get(ls()[grep("rfOC", ls())])
    rfAC<-get(ls()[grep("rfAC", ls())])
    
    rf.p<-list(trOC=performPr(y.m, y.moc, rf=rfOC, x.dat, rf.dat, type="oc", evaluate="train", spp.id),
               OC=performPr(y.m, y.moc, rf=rfOC, x.dat, rf.dat, type="oc", evaluate="test", spp.id),
               trAC=performPr(y.m, y.moc, rf=rfAC, x.dat, rf.dat, type="ac", evaluate="train", spp.id),
               AC=performPr(y.m, y.moc, rf=rfAC, x.dat, rf.dat, type="ac", evaluate="test", spp.id))
    
    
    
    rf.res<-list(res.trOC=measurePerformance(y=rf.p$trOC$y.er, pr=rf.p$trOC$pr, probs=rf.p$trOC$probs, 
                                             type="oc", rf=rfOC),
                 res.OC=measurePerformance(y=rf.p$OC$y.er, pr=rf.p$OC$pr, probs=rf.p$OC$probs, 
                                           type="oc", rf=rfOC),
                 res.trAC=measurePerformance(y=factor(rf.p$trAC$y.er, levels=(sort(unique(rf.p$trAC$y.er)))),
                                             pr=factor(rf.p$trAC$pr, levels=(sort(unique(rf.p$trAC$y.er)))),
                                             probs=rf.p$trAC$probs, 
                                             type="ac", rf=rfAC),
                 res.AC=measurePerformance(y=factor(rf.p$AC$y.er, levels=(sort(unique(rf.p$AC$y.er)))),
                                           pr=factor(rf.p$AC$pr, levels=(sort(unique(rf.p$AC$y.er)))),
                                           probs=rf.p$AC$probs, 
                                           type="ac", rf=rfAC))
    
    return(list(rf.p=rf.p, rf.res=rf.res))}
  
  
  extractOCres<-function(mod.out){
    v<-vector(length=14)
    
    options(scipen=999)
    
    v[1]<-round(mod.out$Acc$overall[1],3)
    v[2]<-round(mod.out$Acc$overall[7],3)
    v[3]<-round(mod.out$Acc$ byClass[1],3)
    v[4]<-round(mod.out$Acc$ byClass[2],3)
    v[5]<-round(mod.out$Acc$ byClass[5],3)
    v[6]<-round(mod.out$Acc$ byClass[7],3)
    v[7]<-mod.out$AUC$A
    v[8]<-mod.out$AUC$p
    v[9]<-mod.out$k$kappa
    v[10]<-mod.out$k$weighted.kappa
    v[11]<-mod.out$ F$ average[4]
    v[12]<-mod.out$ F$ average[2]
    v[13]<-mod.out$ F$ average[3]
    v[14]<-mod.out$tab.stats$TS 
    
    return(v)}  
  
  extractACres<-function(mod.out){
    v<-vector(length=13)
    
    options(scipen=999)
    
    v[1]<-round(mod.out$Acc$overall[1],3)
    v[2]<-round(mod.out$Acc$overall[7],3)
    v[3]<-round(mean(mod.out$Acc$ byClass[,1]),3)
    v[4]<-round(mean(mod.out$Acc$ byClass[,2]),3)
    v[5]<-round(mean(mod.out$Acc$ byClass[,5]),3)
    v[6]<-round(mean(mod.out$Acc$ byClass[,7]),3)
    v[7]<-mod.out$AUC$AUC
    v[8]<-mod.out$k$kappa
    v[9]<-mod.out$k$weighted.kappa
    v[10]<-mod.out$ F$ average[4]
    v[11]<-mod.out$ F$ average[2]
    v[12]<-mod.out$ F$ average[3]
    v[13]<-mod.out$TSS$mean 
    
    return(v)}    
  
  
  
  compileMeasures<-function(res=res){
    measures.oc<-c("Accuracy", "Acc p-value", "Sensitivity", "Specificity", "Prevalence", "Detection Prevalence", "AUC",
                   "AUC p-value", "kappa", "weighted kappa", "F score", "Precision", "Recall", "TSS")
    
    measures.ac<-c("Accuracy", "Acc p-value", "Sensitivity", "Specificity", "Prevalence", "Detection Prevalence", "AUC",
                   "kappa", "weighted kappa", "F score", "Precision", "Recall", "TSS")
    
    res.oc<-matrix(0,14,8, dimnames=list(measures.oc, 
                                         c("RF.tr","RF.t","RF_I.tr",  "RF_I.t", "RF_C.tr", "RF_C.t","RF_B.tr", 
                                           "RF_B.t")))
    ind<-c(0, 1, 2, 3)
    for(i in 1:4){  
      v<-extractOCres(mod.out=res[[i]]$rf.res$res.trOC)
      res.oc[,i+ind[i]]<-v 
      v<-extractOCres(mod.out=res[[i]]$rf.res$res.OC)
      res.oc[,i+1+ind[i]]<-v} 
    
    res.oc<-round(res.oc, digits=3)  
  
    
    res.ac<-matrix(0,13,8, dimnames=list(measures.ac, 
                                         c("RF.tr","RF.t","RF_I.tr",  "RF_I.t", "RF_C.tr", "RF_C.t","RF_B.tr", 
                                           "RF_B.t")))
    ind<-c(0, 1, 2, 3)
    for(i in 1:4){  
      v<-extractACres(mod.out=res[[i]]$rf.res$res.trAC)
      res.ac[,i+ind[i]]<-v 
      v<-extractACres(mod.out=res[[i]]$rf.res$res.AC)
      res.ac[,i+1+ind[i]]<-v} 
    
    res.ac<-round(res.ac, digits=3)   
    
    
    return(list(OC=res.oc, AC=res.ac))}  
  
  compileResults<-function(names.spp, y.m=y.m, y.moc=y.moc, x.dat=x.dat, 
                           an.ID=an.ID){
    
    results<-vector("list", 5)
    names(results)<-names.spp
    
    for(spp.id in c(1:5)){
      spp<-names.spp[spp.id]
      
      run=list("rf", "rf_i", "rf_c", "rf_b")
      res<-lapply(run, FUN=processRun, y.m=y.m, y.moc=y.moc, x.dat=x.dat, 
                  an.ID=an.ID, spp.id=spp.id, names.spp=names.spp)
      
      save(res, 
           file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                      an.ID,"forests/",spp,"/results.Rdata", sep=""))
      
      results[[spp.id]]<-compileMeasures(res=res)
      
      print(spp)
      print(results[[spp.id]])}
    
    
    save(results, 
         file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                    an.ID,"forests/result summary.Rdata", sep=""))
    
  }
  
  compileVarImp<-function(an.ID){
    load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                    an.ID,"forests/vars for varimp.Rdata", sep=""))
    
    load(file="~/Documents/TRAINING DATA/spp.names")
    
    var.res<-vector("list", 5)
    names(var.res)<-names.spp
    
    for(spp.id in 1:5){
      spp<-names.spp[spp.id]
      
      load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                      an.ID,"forests/",spp,"/results.Rdata", sep=""))
      
      OC<-matrix(0, 16, 4, dimnames=list(variables, c("RF","RF_I","RF_C","RF_B")))
      
      for(i in 1:4){
        OC[,i]<-res[[i]][[2]][[1]]$ var.order}
        OC<-OC[order(OC[,3]),]
      
      
      AC<-matrix(0, 16, 4, dimnames=list(variables, c("RF","RF_I","RF_C","RF_B")))
      
      for(i in 1:4){
        AC[,i]<-res[[i]][[2]][[3]]$ var.order}
        AC<-AC[order(AC[,3]),]
      
      var.res[[spp.id]]<-list(OC=OC, AC=AC)}
    
    
    print(var.res)
    
    save(var.res, 
         file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                    an.ID,"forests/variable importance.Rdata", sep=""))} 
#----------------------------------------------------------------------------
    
