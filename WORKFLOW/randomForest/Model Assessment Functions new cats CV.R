



require(PresenceAbsence)
require(randomForest)
set.seed(1)

selectX<-function(X, varnames){X[,which(names(X) %in% varnames)]}

varnames.un<-c("year", "time",
            "month",  "T.m", "Sed.m", 
            "Sedf.m",  "Ch.m", "fdens.m", 
            "fdist.m",  "fside.m",  "fdens.c.m", 
            "fdist.c.m",  "fside.c.m", 
            "bath", "NAO", "wNAO")

varnames<-c("year", "time",
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
y.m<-y.m[order(all.samples),]
all.samples<-sort(all.samples)


names.spp<-names(y.m)
names.spp[3]<-"cfin"
names.spp[4]<-"chel"
names(y.m)<-names.spp

y.moc<-y.m
y.moc[y.m!=0]<-1

rm(norm.x.m)

dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.IDCV, "/",
                 sep=""), 
           showWarnings = F)
dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.IDCV, "/",
                 "forest plots/",sep=""), 
           showWarnings = F)
dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.IDCV, "/",
                 "forests/",sep=""), 
           showWarnings = F)


rftune<-function(all.samples, y.m, y.moc, x.dat, 
                 varnames, test.size, spl="independent", cv.no, i,
                 names.spp=names.spp, varnames.un){
  
  require(randomForest)
  
  selectX<-function(X, varnames){X[,which(names(X) %in% varnames)]}
  
  rftunes<-vector("list", 10)
  
  for(spp.id in 1:length(names.spp)){
    set.seed(1)
    if(spl=="dependent"){
      rfc.dat<-testTrainSplit(all.samples, y.m, spp.id, cv.no, i)}else{
                                rfc.dat<-testTrainSplit(all.samples, y.m, spp.id, cv.no, i)}
    
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
    
    rftunes[[spp.id+length(names.spp)]]<-tuneRF(selectX(x.dat[tune.sAC,], varnames), 
                                as.factor(y.m[tune.sAC, spp.id]), 
                                mtryStart=4, ntreeTry=100, stepFactor=2, 
                                improve=0.05, trace=TRUE, plot=F)} 
  
  rftune<-lapply(rftunes, function(x){x[which(x[,2]==min(x)),1]})
  rftune<-matrix(rftune, length(names.spp), 2, byrow=F, 
                 dimnames=list(names.spp, c("oc", "ac")))
  
  return(rftune)}



growForest<-function(x=x, y=y, ntree=200,
                     varnames=varnames, set.seed=TRUE, an.IDCV, spp=spp, type="oc",
                     mtry=mtry){
  
  
  
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






testTrainSplit<-function(all.samples, y.m, spp.id, cv.no=cv.no, i=i){
  
  n<-length(all.samples)
  
  
  samples<-sort(unique(all.samples))
  n.samples<-length(samples)
  
  samp.means<-unlist(
    lapply(split(y.m[, spp.id], all.samples), mean)) 

 
 set.seed(34)
  

 cv.group<-sample(rep(1:cv.no, length.out=n.samples), n.samples, replace=T)
                                                                               
                                                                             
 test.samples<-samples[cv.group==i]
                                                                             
  train.samples<-sort(setdiff(samples,test.samples))
                                                                             
  test.ind<-which(all.samples %in% test.samples, arr.ind=T)
                                                                             
  train.ind<-sort(which(all.samples %in% train.samples, arr.ind=T))
  train.samples.full<-sort(all.samples[which(all.samples %in% train.samples)])
  
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
              train.samples.fullAC=train.samples.fullAC))}


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
                 y[y>=1]<-1
                 train.samples<-unique(train.samples.full)    
                 
                 cat.split<-unlist(lapply(split(y[train.ind], train.samples.full), mean))}else{
                   
                   y<-y[,spp.id]
                   train.samples<-unique(train.samples.full)    
                   
                   cat.split<-unlist(lapply(split(y[train.ind], train.samples.full), mean))}
  

  
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
  if(type=="oc"){
  diff<-names(s[which(s %in% setdiff(s, train.ind))])
  if(length(diff)!=0){s[which(s %in% setdiff(s, train.ind))]<-which(all.samples %in% diff)}}else{
    diff<-names(s[which(s %in% setdiff(s, train.ind))])
    if(length(diff)!=0){s[which(s %in% setdiff(s, train.ind))]<-which(all.samples %in% diff)}  
  }
  
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

GrowRFs<-function(x.dat, y.m, spp, cv.no, 
                  tune=NULL, tune.i=NULL, tune.b=NULL, tune.c=NULL,
                  spp.id, all.samples, ntree, ntree.b,
                  varnames, model, i=i, an.ID, min.cat=10){
  
  dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID, 
                   "forests/",spp,sep=""), 
             showWarnings = F)
  
  set.seed(1)
  #....DEPENDENT TEST DATA.....REDUNDANT DO NOT USE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
  
  if(model=="rf"|| model=="ALL"){  rf.dat<-testTrainSplit(all.samples, y.m, cv.no=cv.no,
                                                         spp.id=spp.id, i=i)
                                  
                                  #...Standard RF..... 
                                  rfOC<-growForest(x=x.dat[rf.dat$train.ind,], y=y.m[rf.dat$train.ind, spp.id], ntree=ntree,
                                                   varnames=varnames, set.seed=TRUE, an.ID, spp=spp, type="oc",
                                                   mtry=tune[spp.id,1][[1]])
                                  
                                  rfAC<-growForest(x=x.dat[rf.dat$train.indAC,], y=y.m[rf.dat$train.indAC, spp.id], ntree=ntree,
                                                   varnames=varnames, set.seed=TRUE, an.ID, spp=spp, type="ac",
                                                   mtry=tune[spp.id,2][[1]])
                                  
                                  
                                  save(rf.dat, rfOC,rfAC, tune, 
                                       file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                                                  an.ID,"forests/",
                                                  spp, "/rf.Rdata", sep=""))}
  
  
  #....INDEPENDENT TEST DATA.....
  set.seed(1)
  
  if(model!="rf"){rf.dat<-testTrainSplit(all.samples, y.m, cv.no=cv.no,
                                        spp.id=spp.id, i=i)}
  
  if(model=="rf_i"|| model=="ALL"){            
    #...Standard RF..... 
    set.seed(10)
    rfOC<-growForest(x=x.dat[rf.dat$train.ind,], y=y.m[rf.dat$train.ind, spp.id], ntree=ntree,
                     varnames=varnames, set.seed=TRUE, an.ID, spp=spp, type="oc",
                     mtry=tune.i[spp.id,1][[1]])
    
    set.seed(100)
    rfAC<-growForest(x=x.dat[rf.dat$train.indAC,], y=y.m[rf.dat$train.indAC, spp.id], ntree=ntree,
                     varnames=varnames, set.seed=TRUE, an.ID, spp=spp, type="ac",
                     mtry=tune.i[spp.id,2][[1]])
    
    
    save(rf.dat, rfOC,rfAC, tune.i, 
         file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                    an.ID,"forests/",
                    spp, "/rf_i.Rdata", sep=""))}
  
  if(model=="rf_c"|| model=="ALL"){  
    #....Combined RF.....     
    set.seed(10)
    split.oc<-lapply(apply(replicate(ntree.b, sampleSamples(rf.dat$train.ind,rf.dat$train.samples.full,
                                                        y=y.m[,spp.id], all.samples), 
                                     simplify="list"), 2, list), unlist)
    
    set.seed(100)
    rfOC.c<-lapply(split.oc, repRfor, data=x.dat, y=y.m,
                   spp.id=spp.id, varnames=varnames, ntree=1, type="oc", 
                   train.ind=rf.dat$train.ind, mtry=tune.c[spp.id,1][[1]])
    
    rfOC.c<-do.call(combine, rfOC.c)
    
    set.seed(1000)        
    split.ac<-lapply(apply(replicate(ntree.b, sampleSamples(rf.dat$train.indAC,rf.dat$train.samples.fullAC,
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
  
  
  
  if(model=="rf_b"|| model=="ALL"){          
    #....Balanced RF.....
    set.seed(10)
    split.oc<-lapply(apply(replicate(ntree.b,sampleBalancedCats(rf.dat$train.ind, 
                                                            rf.dat$train.samples.full,
                                                            all.samples, 
                                                            y=y.m, type="oc", spp.id, 
                                                                min.cat=min.cat), 
                                     simplify="list"), 2, list), unlist)
    
    set.seed(100)       
    rfOC.b<-lapply(split.oc, repRfor, data=x.dat, y=y.m,
                   spp.id=spp.id, varnames=varnames, ntree=1, type="oc", 
                   train.ind=rf.dat$train.ind, tune.b[spp.id,1][[1]])
    
    rfOC.b<-do.call(combine, rfOC.b)
    
    set.seed(10)
    split.ac<-lapply(apply(replicate(ntree.b,sampleBalancedCats(rf.dat$train.indAC, 
                                                            rf.dat$train.samples.fullAC,
                                                            all.samples, 
                                                            y=y.m, type="ac", spp.id, 
                                                                min.cat=min.cat), 
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

mTSS<-function(tab=Acc$table){
  
  A=(1/sum(tab))*sum(diag(tab))
  
  B=(1/sum(tab)^2)*sum(colSums(tab)*rowSums(tab))
  
  C=(1/sum(tab)^2)*sum(colSums(tab)^2)
  
  (A-B)/(1-C)}

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

measurePerformance<-function(y, pr, probs, type, rf, thresh=NULL, an.IDCV, spp){
  
  if(is.null(thresh)){}else{
    load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                    an.IDCV,"1/forests/thresholds.Rdata", sep=""))}
  
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
    
    if(is.null(thresh)){
    thresholds<-optimal.thresholds(DATA=data.frame(id=1, y=y, pr=probs[,2]),
                                   threshold=20,
                                   which.model=1,
                                   opt.methods=1:12)}else{
     thresholds<-NULL
     pr<-as.numeric(probs[,2]>=THRESH.spp[[spp]][thresh])                                
                                   }
    tab.stats<-table.stats(as.numeric(y), as.numeric(pr),silent = FALSE)
    hists<-list(y=hist(y, breaks=2), pr=hist(pr, breaks=2))
    Acc<-confusionMatrix(factor(pr, levels=0:1), factor(y, levels=0:1), positive="1")}
  
  

  F<-F.measure.single.over.classes(class.ind(y), class.ind(pr))
  
  k<-cohen.kappa(x=cbind(y, pr))
  
  if(type=="ac"){
    Acc<-confusionMatrix(pr, y)
    mean.Acc<-colMeans(Acc$byClass)
    TSS<-mTSS(Acc$table)
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

processRun<-function(run, y.m, y.moc, x.dat, an.ID, spp.id, names.spp, an.IDCV, 
                     thresh=NULL, i){
  
  print(run)
  
  
  spp<-names.spp[spp.id]
  
  load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                  an.ID,"forests/",
                  spp, "/", run, ".Rdata", sep=""))
  
  rfOC<-get(ls()[grep("rfOC", ls())])
  rfAC<-get(ls()[grep("rfAC", ls())])
  
  if(is.null(thresh)){

  
  rf.p<-list(trOC=performPr(y.m, y.moc, rf=rfOC, x.dat, rf.dat, type="oc", evaluate="train", spp.id),
             OC=performPr(y.m, y.moc, rf=rfOC, x.dat, rf.dat, type="oc", evaluate="test", spp.id),
             trAC=performPr(y.m, y.moc, rf=rfAC, x.dat, rf.dat, type="ac", evaluate="train", spp.id),
             AC=performPr(y.m, y.moc, rf=rfAC, x.dat, rf.dat, type="ac", evaluate="test", spp.id))}else{
               an.ID<-paste(an.IDCV,i, "/", sep="") 
          
          load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                          an.ID,"forests/",spp,"/results.Rdata", sep=""))
               
          rf.p<-res[[run]]$rf.p}
  
  
  rf.res<-list(res.trOC=measurePerformance(y=rf.p$trOC$y.er, pr=rf.p$trOC$pr, probs=rf.p$trOC$probs, 
                                           type="oc", rf=rfOC, thresh=thresh, an.IDCV, spp),
               res.OC=measurePerformance(y=rf.p$OC$y.er, pr=rf.p$OC$pr, probs=rf.p$OC$probs, 
                                         type="oc", rf=rfOC, thresh=thresh, an.IDCV, spp),
               res.trAC=measurePerformance(y=factor(rf.p$trAC$y.er, levels=(sort(unique(rf.p$trAC$y.er)))),
                                           pr=factor(rf.p$trAC$pr, levels=(sort(unique(rf.p$trAC$y.er)))),
                                           probs=rf.p$trAC$probs, 
                                           type="ac", rf=rfAC, thresh=thresh, an.IDCV, spp),
               res.AC=measurePerformance(y=factor(rf.p$AC$y.er, levels=(sort(unique(rf.p$AC$y.er)))),
                                         pr=factor(rf.p$AC$pr, levels=(sort(unique(rf.p$AC$y.er)))),
                                         probs=rf.p$AC$probs, 
                                         type="ac", rf=rfAC, thresh=thresh, an.IDCV, spp))
  
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
  v[14]<-mod.out$tab.stats$KSS 
  
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
  v[13]<-mod.out$TSS 
  
  return(v)}    



compileMeasures<-function(res=res, run){
  measures.oc<-c("Accuracy", "Acc p-value", "Sensitivity", "Specificity", "Prevalence", "Detection Prevalence", "AUC",
                 "AUC p-value", "kappa", "weighted kappa", "F score", "Precision", "Recall", "TSS")
  
  measures.ac<-c("Accuracy", "Acc p-value", "Sensitivity", "Specificity", "Prevalence", "Detection Prevalence", "AUC",
                 "kappa", "weighted kappa", "F score", "Precision", "Recall", "TSS")
  
  res.oc<-matrix(0,14,length(run)*2, dimnames=list(measures.oc, 
                                       paste(rep(run, each=2), c("tr",  "t"))))
  ind<-c(0, 1, 2, 3)
  for(i in 1:length(run)){  
    v<-extractOCres(mod.out=res[[i]]$rf.res$res.trOC)
    res.oc[,i+ind[i]]<-v 
    v<-extractOCres(mod.out=res[[i]]$rf.res$res.OC)
    res.oc[,i+1+ind[i]]<-v} 
  
  res.oc<-round(res.oc, digits=3)  
  
  
  res.ac<-matrix(0,13,length(run)*2, dimnames=list(measures.ac, 
                                      paste(rep(run, each=2), c("tr",  "t"))))
  ind<-c(0, 1, 2, 3)
  for(i in 1:length(run)){  
    v<-extractACres(mod.out=res[[i]]$rf.res$res.trAC)
    res.ac[,i+ind[i]]<-v 
    v<-extractACres(mod.out=res[[i]]$rf.res$res.AC)
    res.ac[,i+1+ind[i]]<-v} 
  
  res.ac<-round(res.ac, digits=3)   
  
  
  return(list(OC=res.oc, AC=res.ac))}  

compileResults<-function(names.spp, y.m=y.m, y.moc=y.moc, x.dat=x.dat, 
                         an.ID=an.ID, run, an.IDCV, thresh=thresh, i=i){
  
  require(e1071)
  
  results<-vector("list", length(names.spp))
  names(results)<-names.spp
  
  for(spp.id in c(1:length(names.spp))){
    spp<-names.spp[spp.id]
    
    run=as.list(run)
    res<-lapply(run, FUN=processRun, y.m=y.m, y.moc=y.moc, x.dat=x.dat, 
                an.ID=an.ID, spp.id=spp.id, names.spp=names.spp, an.IDCV, thresh=thresh,i=i)
    
    names(res)<-run
    
    if(is.null(thresh)){save(res, 
                              file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                                         an.ID,"forests/",spp,
                                         "/results.Rdata", sep=""))}else{
    dir.create(path=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                          an.ID,"forests/",spp,
                          "/",thresh,  sep=""), showWarnings = F)
    save(res, 
         file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                    an.ID,"forests/",spp,"/",thresh,
                    "/results.Rdata", sep=""))}
    
    
    results[[spp.id]]<-compileMeasures(res=res, run=run)
    
    print(spp)
    print(results[[spp.id]])}
  
  
  save(results, 
       file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                  an.ID,"forests/",thresh,"result summary.Rdata", sep=""))
  
}

compileVarImp<-function(an.ID, names.spp=names.spp){
  load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                  an.ID,"forests/vars for varimp.Rdata", sep=""))
  
  
  var.res<-vector("list", length(names.spp))
  names(var.res)<-names.spp
  
  for(spp.id in 1:length(names.spp)){
    spp<-names.spp[spp.id]
    
    load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                    an.ID,"forests/",spp,"/results.Rdata", sep=""))
    
    OC<-matrix(0, 16, 1, dimnames=list(variables, c("RF_M")))
    
    for(i in 1){
      OC[,i]<-res[[i]][[2]][[1]]$ var.order}
    OC<-OC[order(OC[,3]),]
    
    
    AC<-matrix(0, 16, 1, dimnames=list(variables, c("RF_M")))
    
    for(i in 1){
      AC[,i]<-res[[i]][[2]][[3]]$ var.order}
    AC<-AC[order(AC[,3]),]
    
    var.res[[spp.id]]<-list(OC=OC, AC=AC)}
  
  
  print(var.res)
  
  save(var.res, 
       file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                  an.ID,"forests/variable importance.Rdata", sep=""))} 
#----------------------------------------------------------------------------

compileCVResults<-function(names.spp, y.m=y.m, y.moc=y.moc, x.dat=x.dat, 
                          cv.no, run=run, an.IDCV, thresh){
  
#Compile results for each CV
  
for (i in 1:cv.no){ 
  
  an.ID<-paste(an.IDCV,i, "/", sep="") 
  
  compileResults(names.spp, y.m=y.m, y.moc=y.moc, x.dat=x.dat, 
                 an.ID=an.ID, run=run, an.IDCV, thresh, i=i)}


#Compile all results into list 
compile<-vector("list", 5)

for (i in 1:cv.no){ 
  
  an.ID<-paste(an.IDCV,i, "/", sep="") 
  
  load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                  an.ID,"forests/",thresh,"result summary.Rdata", sep=""))
  
  compile[[i]]<-results}


#Compile means and variance of CV metrics
results.mean<-vector("list",length(names.spp))
names(results.mean)<-names.spp

results.var<-vector("list",length(names.spp))
names(results.var)<-names.spp

dimnames.oc<-dimnames(compile[[1]][[1]][[1]])
dimnames.ac<-dimnames(compile[[1]][[1]][[2]])
dim.oc<-dim(compile[[1]][[1]][[1]])
dim.ac<-dim(compile[[1]][[1]][[2]])

for(j in 1:length(names.spp)){
  OC.comp<-NULL
  AC.comp<-NULL
  
  for(i in 1:cv.no){
    
    OC.comp<-cbind(OC.comp, as.vector(compile[[i]][[j]][[1]]))
    AC.comp<-cbind(AC.comp, as.vector(compile[[i]][[j]][[2]]))
    
  }
  results.mean[[j]]$OC<-matrix(rowMeans(OC.comp), nrow=dim.oc[1], ncol=dim.oc[2], 
                               dimnames=dimnames.oc)
  results.var[[j]]$OC<-matrix(apply(OC.comp,1,var), nrow=dim.oc[1], ncol=dim.oc[2], 
                              dimnames=dimnames.oc)
  
  results.mean[[j]]$AC<-matrix(rowMeans(AC.comp), nrow=dim.ac[1], ncol=dim.ac[2], 
                               dimnames=dimnames.ac)
  results.var[[j]]$AC<-matrix(apply(AC.comp,1,var), nrow=dim.ac[1], ncol=dim.ac[2], 
                              dimnames=dimnames.ac)
}


an.ID<-paste(an.IDCV,1, "/", sep="") 

save(results.mean, results.var, file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                                           an.ID,"forests/",thresh,"result overall summary.Rdata", sep=""))}

tabulateCVResults<-function(thresh, an.IDCV){
  

  
  load(paste("~/Documents/TRAINING DATA/Models/randomForest/",
             an.IDCV,"1/forests/",thresh,"result overall summary.Rdata", sep=""))
  
  OC.table<-NULL
  AC.table<-NULL
  
  for(i in 1:length(names(results.mean))){
    
    OC.table<-cbind(OC.table, results.mean[[i]][[1]][c("Accuracy", "AUC", "kappa", "TSS", "Sensitivity", "Specificity", "Prevalence"), 
                                         "rf_i t"])
    AC.table<-cbind(AC.table, results.mean[[i]][[2]][c("Accuracy", "AUC", "kappa", "weighted kappa", "TSS", "Sensitivity", "Specificity"), 
                                                     "rf_i t"])
  }

  colnames(OC.table)<-names(results.mean)
  colnames(AC.table)<-names(results.mean)
 
  write.csv(OC.table, paste("~/Documents/TRAINING DATA/Models/randomForest/",
                            an.IDCV,"1/forests/",thresh,"OC results table.csv", sep=""))
  write.csv(AC.table, paste("~/Documents/TRAINING DATA/Models/randomForest/",
                            an.IDCV,"1/forests/",thresh,"AC results table.csv", sep=""))
  
}


compileHistsNCCV<-function(model, pt=12, names.spp, thresh, an.IDCV){
  
  load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                  an.IDCV,"1/forests/thresholds.Rdata", sep=""))
  
  naOC<-list(NULL,NULL, NULL,NULL, 0:1)
  naAC<-list(NULL,NULL, NULL,NULL, 1:5)
  
  xlabs<-list(NULL,NULL, NULL,NULL, "Abundance Category")
  
  for(spp.id in 1:length(names.spp)){
    if(spp.id==1){

        an.ID<-paste(an.IDCV,spp.id, "/", sep="")
        dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",
                         an.ID,"plots/", sep=""), showWarnings = F)
        png(filename = paste("~/Documents/TRAINING DATA/Models/randomForest/",
                                         an.ID,"plots/",thresh,model," hist plots.png", sep=""), 
                  pointsize=pt, units = "in", res=200, width=8.13, 
            height=length(names.spp)/5*11.7)
        
        par(mfrow=c(length(names.spp),2), mar=c(0.1, 4, 0.1, 0.5), 
            oma=c(0.3, 0.2, 2.5, 0.2))}
    
    spp<-names.spp[spp.id]

    
    yOC<-NULL
    probs<-NULL
    
    
    yAC<-NULL
    prAC<-NULL
    
    for (i in 1:cv.no){ 
      
      an.ID<-paste(an.IDCV,i, "/", sep="") 
      
      load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                      an.ID,"forests/",spp,"/results.Rdata", sep=""))
      
      probs<-c(probs,res[[model]]$rf.p$OC$probs[,2])
      yOC<-c(yOC,res[[model]]$rf.p$OC$y.er)
      
      prAC<-c(prAC,res[[model]]$rf.p$AC$pr) 
      yAC<-c(yAC,res[[model]]$rf.p$AC$y.er)}
    
    yAC<-factor(yAC, levels=1:5)
    prAC<-factor(prAC, levels=1:5)
    
    prOC<-as.numeric(probs>=THRESH.spp[[spp]][thresh])
    
    mOC<-cbind(y=hist(as.numeric(yOC), breaks=2, plot=F)$counts, 
               pr=hist(as.numeric(prOC), breaks=2, plot=F)$counts)
    
    mAC<-cbind(y=hist(as.numeric(yAC), breaks=0:max(levels(yAC)), plot=F)$counts, 
              pr=hist(as.numeric(prAC), breaks=0:max(levels(yAC)), plot=F)$counts)
    
    
    if(spp.id==1){par(mar=c(4, 4, 0.5, 0.5))}
 
    barplot(t(mOC), beside=T, names.arg=naOC[[spp.id]], xlab=xlabs[[spp.id]], 
            cex.lab=1.4,cex.main=1.4, 
            ylab=spp, col=c("darkslategray4", "darkslategray3"))
    
    
    barplot(t(mAC), beside=T, names.arg=naAC[[spp.id]], xlab=xlabs[[spp.id]], 
            cex.lab=1.4,cex.main=1.4, 
            col=c("darkslategray4", "darkslategray3"))
    if(spp.id==1){legend("topright", legend=c("y", "pr"), fill=c("darkslategray4", "darkslategray3"), cex=1.2)}
    
    
}
  
  mtext(paste("y vs pr frequency distribution", model), side=3, 1, outer=T)

dev.off()}
  

compileBoxplotsNCCV<-function(model, pt=12, names.spp, thresh="PredPrev=Obs", an.IDCV){

  load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                              an.IDCV,"1/forests/thresholds.Rdata", sep=""))
  
for(spp.id in 1:length(names.spp)){
  
  if(spp.id==1){
    
    an.ID<-paste(an.IDCV,spp.id, "/", sep="")
    
    dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",
                     an.ID,"plots/", sep=""), showWarnings = F)
    
    png(filename = paste("~/Documents/TRAINING DATA/Models/randomForest/",
                         an.ID,"plots/",thresh,model," box plots.png", sep=""), 
        pointsize=pt, units = "in", res=200, width=8.13, height=length(names.spp)/5*11.7)
    
    par(mfrow=c(length(names.spp),2), mar=c(4, 4, 0.1, 0.5), oma=c(0.3, 0.2, 2.5, 0.2))}
  
  spp<-names.spp[spp.id]
  
  yOC<-NULL
  probs<-NULL

  
  yAC<-NULL
  prAC<-NULL
  
for (i in 1:cv.no){ 
  
  an.ID<-paste(an.IDCV,i, "/", sep="") 
  
  load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                  an.ID,"forests/",spp,"/results.Rdata", sep=""))
  
  probs<-c(probs,res[[model]]$rf.p$OC$probs[,2])
  yOC<-c(yOC,res[[model]]$rf.p$OC$y.er)
  
  prAC<-c(prAC,res[[model]]$rf.p$AC$pr) 
  yAC<-c(yAC,res[[model]]$rf.p$AC$y.er)}
  
  prOC<-as.numeric(probs>=THRESH.spp[[spp]][thresh])
  
  boxplot(prOC ~ as.factor(yOC), ylab=spp, xlim=c(0.5,3), ylim=c(0,1))
  boxplot(prAC ~ as.factor(yAC), ylab=spp, xlim=c(0,6), ylim=c(0,6))}

dev.off()
}


extractThresholds<-function(an.IDCV, model, names.spp, cv.no){

  THRESH.spp<-vector("list", length(names.spp))
  names(THRESH.spp)<-names.spp
  
  for(spp in names.spp){
   
    for(i in 1:cv.no){
   
      an.ID<-paste(an.IDCV,i, "/", sep="") 
      
      load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                      an.ID,"forests/",spp,"/results.Rdata", sep=""))
      if(i == 1){
      thresh<-res[[model]]$rf.res$res.OC$thresh}else{
      thresh<-cbind(thresh, res[[model]]$rf.res$res.OC$thresh[,2]) 
      }}
    
    rownames(thresh)<-thresh[,1]
    thresh<-rowMeans(thresh[,2:(cv.no+1)])
    
    THRESH.spp[[spp]]<-thresh}
    
    an.ID<-paste(an.IDCV,1, "/", sep="") 
    
    save(THRESH.spp, file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                                an.ID,"forests/thresholds.Rdata", sep=""))
    }
    

