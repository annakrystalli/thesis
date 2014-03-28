require(nnet)

load(file=paste(input.folder,"X_ac_data.RData", sep=""))
load(file=paste(input.folder,"y_ac_",spp,"_data.RData", sep=""))

v1<-c("lon", "lat",  "year", 
      "month",  "T.m",  "Tf.m",  "Sed.m", 
      "Sedf.m",  "Chf.m", "Ch.m",
      "fdist.m",  "fside.m",
      "fdist.c.m",  "fside.c.m", 
      "bath", "NAO", "wNAO")

v2<-c("lon", "lat",  "year", 
      "month",  "T.m", "Sed.m", 
      "Sedf.m",  "Ch.m", "fdens.m", 
      "fdist.m",  "fside.m",  "fdens.c.m", 
      "fdist.c.m",  "fside.c.m", 
      "bath", "NAO", "wNAO")
#_________________________________________________________________________________________________________________________

selectX<-function(X, varnames){X[,which(names(X) %in% varnames)]}

#_________________________________________________________________________________________________________________________

metricFy<-function(y, hx, K){
  n<-length(y)  
  
  v.table<-table(y, hx)
      r<-dim(v.table)[1]
      c<-dim(v.table)[2]
        m0<-matrix(0,K,K, dimnames=list(as.character(1:K), as.character(1:K)))
        m0[cbind(rep(1:dim(v.table)[1], times=dim(v.table)[2]), 
                 rep(as.numeric(dimnames(v.table)[[2]]), each=dim(v.table)[1]))]<-v.table
        v.table<-m0

  A<-sum(diag(v.table))/n
  
  R.table<-t(t(v.table)/colSums(v.table))
  R.table[which(is.nan(R.table))]<-0
  
  P.table<-v.table/rowSums(v.table)
  P.table[which(is.nan(P.table))]<-0

  P<-sum(diag(P.table))/r
  R<-sum(diag(R.table))/c
  F1<-2*((P*R)/(P+R))
   
  return(cbind(A,P,R,F1))
}

#_________________________________________________________________________________________________________________________
trainACNN<-function(x, y, x.val, y.val, size, decay, maxit,  
                  varnames, weights, K){
  #prep y
  n<-length(y)
  n.val<-length(y.val)
  y.cat<-y
  
  y<-class.ind(y)
  add<-setdiff(as.character(1:K),dimnames(y)[[2]])
  y<-cbind(y,matrix(0, nrow=length(y[,1]), ncol=length(add), dimnames=list(NULL,add)))
  
  y.val<-class.ind(y.val)
  add<-setdiff(as.character(1:K),dimnames(y.val)[[2]])
  y.val<-cbind(y.val,matrix(0, nrow=length(y.val[,1]), ncol=length(add), dimnames=list(NULL,add)))
  
  
  #nnet formula
  name<-varnames
  fmla <- as.formula(paste("y~ ", paste("x$",name ,collapse= "+", sep="")))
  #
  
nnet.mod<-nnet(fmla,  size=size, maxit=maxit, decay=decay, weights=weights)
                    
                    #predict for validation
                    x<-selectX(x.val, name)
                    hx.vp<-predict(nnet.mod, newdata=x, type="raw")
                    hx.v<-max.col(hx.vp)

  hx<-max.col(nnet.mod$fitted.values)
  
  return(list(nnet=nnet.mod, hx=hx, hx.v=hx.v, hx.vp=hx.vp))

}

 

#_________________________________________________________________________________________________________________________
adaboostAC<-function(X, y, x.val, y.val, size=20, decay=0.1, maxit=50, 
                     varnames, T, detail.metrics=TRUE, K){
  
  y.neg<-y
  y.neg[y.neg==0]<--1
  
  y.neg.val<-y.val
  y.neg.val[y.neg.val==0]<--1
  
  n<-dim(X)[1]
  n.v<-length(y.val)
  
  hxt<-array(0, dim=c(T, n, K))
  hxt.p<-array(0, dim=c(T, n, K))
  
  hxt.v<-array(0, dim=c(T, n.v, K))
  hxt.vp<-array(0, dim=c(T, n.v, K))
  
  if(detail.metrics==TRUE){
    metrics.c<-matrix(0, nrow=T, ncol=4)
    metrics.cp<-matrix(0, nrow=T, ncol=4)
    metrics.cv<-matrix(0, nrow=T, ncol=4)
    metrics.cvp<-matrix(0, nrow=T, ncol=4)}              
  
  
  epsilon<-rep(0, times=T)
  D<-matrix(0, nrow=n, ncol=T)
  a<-rep(0, times=T)
  
  D[,1]<-1/n
  adaboost.oc.nnets<-vector("list", T)
  
  for(t in 1:T){
    #Train nnet with D[t] weights
    nnet.out<-trainACNN(x=X,y=y, x.val=x.val, y.val=y.val, 
                      size=size, decay=decay/n,  
                      maxit=maxit, 
                      varnames=varnames, weights=D[,t], K=K)
    
    #Compile hx[t]s for training & validation set {-1,1}
    hx<-class.ind(nnet.out$hx)
    add<-setdiff(as.character(1:K),dimnames(hx)[[2]])
    hx<-cbind(hx,matrix(0, nrow=length(hx[,1]), ncol=length(add), dimnames=list(NULL,add)))
    hx<-hx[,order(as.numeric(dimnames(hx)[[2]]))]
    
    hx.v<-class.ind(nnet.out$hx.v)
    add<-setdiff(as.character(1:K),dimnames(hx.v)[[2]])
    hx.v<-cbind(hx.v,matrix(0, nrow=length(hx.v[,1]), ncol=length(add), dimnames=list(NULL,add)))
    hx.v<-hx.v[,order(as.numeric(dimnames(hx.v)[[2]]))]
    
    
    #Misclassification (I {0,1}) & correct classification (Ii {-1,1}) indicator vectors  
    I<-y!= nnet.out$hx

    
    #Calculate nn[t] error
    epsilon[t]<-sum(D[,t]*I)
    if((1-epsilon[t])<=1/K){next}
    
    #Calculate nn[t] weight
    a[t]<-log((1-epsilon[t])/epsilon[t])+log(K-1)
    
    
    
    #Update weights
    if(t!=T){D[,t+1]<-(D[,t]*exp(-a[t]*I))/sum(D[,t]*exp(-a[t]*I))}
    
    #Compile hx[t] probabilities for training & validation set {0,1} & weight by a
    hxt[t,,]<-hx*a[t]
    hxt.v[t,,]<-hx.v*a[t]   
    
    hxt.p[t,,]<-nnet.out$nnet$fitted.values*a[t]
    hxt.vp[t,,]<-nnet.out$hx.vp*a[t]
    
    #Compile cumulative metrics at iter[t]
    if(detail.metrics==TRUE){
      metrics.c[t,]<-metricFy(y=y, hx=max.col(colSums(hxt)), K=K)
      metrics.cp[t,]<-metricFy(y=y, hx=max.col(colSums(hxt.p)), K=K)
      metrics.cv[t,]<-metricFy(y=y.val, hx=max.col(colSums(hxt.v)), K=K)
      metrics.cvp[t,]<-metricFy(y=y.val, hx=max.col(colSums(hxt.vp)), K=K)
      colnames(metrics.c)<-c("A", "P", "R", "F1")
      colnames(metrics.cp)<-c("A", "P", "R", "F1")
      colnames(metrics.cv)<-c("A", "P", "R", "F1")
      colnames(metrics.cvp)<-c("A", "P", "R", "F1")}
    
    #compile nnet[t]
    adaboost.oc.nnets[[t]]<-nnet.out$nnet
    
    #On final iteration compile overall ensemble predictions & metrics
    if(t==T){
      Hx<-max.col(colSums(hxt))
      Hx.v<-max.col(colSums(hxt.v))
      
      Hx.p<-max.col(colSums(hxt.p))
      Hx.vp<-max.col(colSums(hxt.vp))
      
      ABmetrics<-rbind(metricFy(y=y.neg.val, hx=Hx.v, K=K), metricFy(y=y.val, hx=Hx.vp, K=K))
      rownames(ABmetrics)<-c("hxv", "hxvp")
      
      if(length(grep("fdens", varnames))>0){
        file.ID<-paste(spp, "v2",size, signif(decay, digits=2), sep="_")}else{
          file.ID<-paste(spp, "v1",size, signif(decay, digits=2), sep="_")  
        }
      
      #save nnets
      save(adaboost.ac.nnets,  file=paste(output.folder,"models/",file.ID, "_AB_ac_nnets.RData", sep=""))
      
      #save metrics
      if(detail.metrics==TRUE){
        save(Hx, Hx.v, Hx.p, Hx.vp, ABmetrics, metrics.cvp, metrics.cv, metrics.c, metrics.cp,
             file=paste(output.folder,file.ID, "_AB_ac_outputs.RData", sep=""))}else{
               save(Hx, Hx.v, Hx.p, Hx.vp, ABmetrics,  
                    file=paste(output.folder,file.ID, "_AB_ac_outputs.RData", sep=""))      
             }
      
    }
    
    
    
  }
  
  print(ABmetrics)
}



#_________________________________________________________________________________________________________________________
ensembleOC<-function(X, y, x.val, y.val, size, decay=0.1, maxit=100, 
                     varnames, T, detail.metrics=TRUE){
  
  y[y>=1]<-1
  y.val[y.val>=1]<-1
  
  y.neg<-y
  y.neg[y.neg==0]<--1
  
  y.neg.val<-y.val
  y.neg.val[y.neg.val==0]<--1
  
  n<-dim(X)[1]
  n.v<-length(y.val)
  
  hxt<-matrix(0, nrow=n, ncol=T)
  hxta.p<-array(0, dim=c(T, n, 2))
  hxt.p<-array(0, dim=c(T, n, 2))
  
  hxt.v<-matrix(0, nrow=n.v, ncol=T)
  hxta.vp<-array(0, dim=c(T, n.v, 2))
  hxt.vp<-array(0, dim=c(T, n.v, 2))
  
  #matrix to compile performance of individual networks
  metrics.t<-matrix(0, nrow=T, ncol=4)
  colnames(metrics.t)<-c("A", "P", "R", "F1")
  
  if(detail.metrics==TRUE){
    metrics.c<-matrix(0, nrow=T, ncol=4)
    metrics.cv<-matrix(0, nrow=T, ncol=4)
    
    metrics.cp<-matrix(0, nrow=T, ncol=4)
    metrics.cvp<-matrix(0, nrow=T, ncol=4)
    
    metrics.ca<-matrix(0, nrow=T, ncol=4)
    metrics.cva<-matrix(0, nrow=T, ncol=4)
    
    metrics.cpa<-matrix(0, nrow=T, ncol=4)
    metrics.cvpa<-matrix(0, nrow=T, ncol=4)
    
    
    colnames(metrics.c)<-c("A", "P", "R", "F1")
    colnames(metrics.cp)<-c("A", "P", "R", "F1")
    colnames(metrics.cv)<-c("A", "P", "R", "F1")
    colnames(metrics.cvp)<-c("A", "P", "R", "F1")
    colnames(metrics.ca)<-c("A", "P", "R", "F1")
    colnames(metrics.cpa)<-c("A", "P", "R", "F1")
    colnames(metrics.cva)<-c("A", "P", "R", "F1")
    colnames(metrics.cvpa)<-c("A", "P", "R", "F1")}              
  
  
  epsilon<-rep(0, times=T)
  a<-rep(0, times=T)
  
  ensemble.oc.nnets<-vector("list", T)
  
  for(t in 1:T){
    nnet.out<-trainNN(x=X,y=y, x.val=x.val, y.val=y.val, 
                      size=size, decay=decay,  
                      maxit=maxit, 
                      varnames=varnames, weights=NULL)
    
    hxt[,t]<-nnet.out$hx
    hxt[nnet.out$hx<=0,t]<--1
    hxt.v[,t]<-nnet.out$hx.v
    hxt.v[nnet.out$hx.v<=0,t]<--1
    
    
    
    I<-y!= nnet.out$hx
    Ii<-rep(1, times=length(I))
    Ii[I==1]<--1
    
    epsilon[t]<-sum((1/n)*I)
    if((1-epsilon[t])<=1/2){next}
    
    a[t]<-log((1-epsilon[t])/epsilon[t])
    
    
    hxta.p[t,,]<-nnet.out$nnet$fitted.values*a[t]
    hxta.vp[t,,]<-nnet.out$hx.vp*a[t]
    hxt.p[t,,]<-nnet.out$nnet$fitted.values
    hxt.vp[t,,]<-nnet.out$hx.vp
    
    metrics.t[t,]<-metricFy(y=y.neg.val, hx=hxt.v[,t])
    
    if(detail.metrics==TRUE){
      metrics.c[t,]<-metricFy(y=y.neg, hx=sign(rowSums(hxt)))
      metrics.cv[t,]<-metricFy(y=y.neg.val, hx=sign(rowSums(hxt.v)))
      metrics.cp[t,]<-metricFy(y=y, hx=max.col(colSums(hxt.p))-1)
      metrics.cvp[t,]<-metricFy(y=y.val, hx=max.col(colSums(hxt.vp))-1)
      metrics.ca[t,]<-metricFy(y=y.neg, hx=sign(hxt%*%a))
      metrics.cpa[t,]<-metricFy(y=y, hx=max.col(colSums(hxta.p))-1)
      metrics.cva[t,]<-metricFy(y=y.neg.val, hx=sign(hxt.v%*%a))
      metrics.cvpa[t,]<-metricFy(y=y.val, hx=max.col(colSums(hxta.vp))-1)
    }
    
    ensemble.oc.nnets[[t]]<-nnet.out$nnet
    
    if(t==T){
      Hx<-sign(rowSums(hxt))
      Hx.v<-sign(rowSums(hxt.v))
      Hxa<-sign(hxt%*%a)
      Hxa.v<-sign(hxt.v%*%a)
      
      Hxa.p<-max.col(colSums(hxta.p))-1
      Hxa.vp<-max.col(colSums(hxta.vp))-1
      Hx.p<-max.col(colSums(hxt.p))-1
      Hx.vp<-max.col(colSums(hxt.vp))-1
      
      ESmetrics<-rbind(metricFy(y=y.neg.val, hx=Hx.v), 
                       metricFy(y=y.val, hx=Hx.vp),
                       metricFy(y=y.neg.val, hx=Hxa.v), 
                       metricFy(y=y.val, hx=Hxa.vp))
      rownames(ESmetrics)<-c("hxv", "hxvp", "hxva", "hxvpa")
      
      if(length(grep("fdens", varnames))>0){
        file.ID<-paste(spp, "v2",size, signif(decay, digits=2), sep="_")}else{
          file.ID<-paste(spp, "v1",size, signif(decay, digits=2), sep="_")  
        }
      
      save(ensemble.oc.nnets,  file=paste(output.folder,"models/",file.ID, "_ES_oc_nnets.RData", sep=""))
      
      if(detail.metrics==TRUE){
        save(Hx, Hx.v, Hx.p, Hx.vp, Hxa, Hxa.v, Hxa.p, Hxa.vp, 
             ESmetrics,
             metrics.t,
             metrics.c,
             metrics.cv,
             metrics.cp,
             metrics.cvp,
             metrics.ca,
             metrics.cpa,
             metrics.cva,
             metrics.cvpa, 
             file=paste(output.folder,file.ID, "_ES_oc_outputs.RData", sep=""))}else{
               save(Hx, Hx.v, Hx.p, Hx.vp, Hxa, Hxa.v, Hxa.p, Hxa.vp, ESmetrics, metrics.t,  
                    file=paste(output.folder,file.ID, "_ES_oc_outputs.RData", sep=""))      
             }
      
    }
    
    
    
  }
  
  print(ESmetrics) 
}





#_________________________________________________________________________________________________________________________
adacost2OC<-function(X, y, x.val, y.val, size, decay=0.1, maxit=100, 
                     varnames, T, C, detail.metrics=TRUE){
  
  y[y>=1]<-1
  y.val[y.val>=1]<-1
  
  y.neg<-y
  y.neg[y.neg==0]<--1
  
  y.neg.val<-y.val
  y.neg.val[y.neg.val==0]<--1
  
  n<-dim(X)[1]
  n.v<-length(y.val)
  
  hxt<-matrix(0, nrow=n, ncol=T)
  hxt.p<-array(0, dim=c(T, n, 2))
  
  hxt.v<-matrix(0, nrow=n.v, ncol=T)
  hxt.vp<-array(0, dim=c(T, n.v, 2))
  
  if(detail.metrics==TRUE){
    metrics.c<-matrix(0, nrow=T, ncol=4)
    metrics.cp<-matrix(0, nrow=T, ncol=4)
    metrics.cv<-matrix(0, nrow=T, ncol=4)
    metrics.cvp<-matrix(0, nrow=T, ncol=4)}              
  
  
  epsilon<-rep(0, times=T)
  D<-matrix(0, nrow=n, ncol=T)
  a<-rep(0, times=T)
  
  D[,1]<-1/n
  adacost.oc.nnets<-vector("list", T)
  
  for(t in 1:T){
    #Train nnet with D[t] weights
    nnet.out<-trainNN(x=X,y=y, x.val=x.val, y.val=y.val, 
                      size=size, decay=decay,  
                      maxit=maxit, 
                      varnames=varnames, weights=D[,t])
    
    #Compile hx[t]s for training & validation set {-1,1}
    hxt[,t]<-nnet.out$hx
    hxt[nnet.out$hx<=0,t]<--1
    hxt.v[,t]<-nnet.out$hx.v
    hxt.v[nnet.out$hx.v<=0,t]<--1
    
    
    #Misclassification (I {0,1}) & correct classification (Ii {-1,1}) indicator vectors  
    I<-y!= nnet.out$hx
    Ii<-rep(1, times=length(I))
    Ii[I==1]<--1
    
    #set up cost vector
    Iic<-y
    Iic[y==0]<-C
    II<-y==nnet.out$hx
    
    
    
    
    if(sum(II*D[,t]*Iic)<sum(I*D[,t]*Iic)){next}
    
    
    #Calculate nn[t] weight
    a[t]<-(1/2)*log(sum(II*D[,t]*Iic)/sum(I*D[,t]*Iic))
    
    
    
    #Update weights
    if(t!=T){D[,t+1]<-(Iic*D[,t]*exp(-a[t]*Ii))/sum(Iic*D[,t]*exp(-a[t]*Ii))}
    
    
    
    #Compile hx[t] probabilities for training & validation set {0,1} & weight by a
    hxt.p[t,,]<-nnet.out$nnet$fitted.values*a[t]
    hxt.vp[t,,]<-nnet.out$hx.vp*a[t]
    
    #Compile cumulative metrics at iter[t]
    if(detail.metrics==TRUE){
      metrics.c<-metricFy(y=y.neg, hx=sign(hxt%*%a))
      metrics.cp<-metricFy(y=y, hx=max.col(colSums(hxt.p))-1)
      metrics.cv[t,]<-metricFy(y=y.neg.val, hx=sign(hxt.v%*%a))
      metrics.cvp[t,]<-metricFy(y=y.val, hx=max.col(colSums(hxt.vp))-1)
      colnames(metrics.c)<-c("A", "P", "R", "F1")
      colnames(metrics.cp)<-c("A", "P", "R", "F1")
      colnames(metrics.cv)<-c("A", "P", "R", "F1")
      colnames(metrics.cvp)<-c("A", "P", "R", "F1")}
    
    #compile nnet[t]
    adacost.oc.nnets[[t]]<-nnet.out$nnet
    
    #On final iteration compile overall ensemble predictions & metrics
    if(t==T){
      Hx<-sign(hxt%*%a)
      Hx.v<-sign(hxt.v%*%a)
      
      Hx.p<-max.col(colSums(hxt.p))-1
      Hx.vp<-max.col(colSums(hxt.vp))-1
      
      ACmetrics<-rbind(metricFy(y=y.neg.val, hx=Hx.v), metricFy(y=y.val, hx=Hx.vp))
      rownames(ACmetrics)<-c("hxv", "hxvp")
      
      if(length(grep("fdens", varnames))>0){
        file.ID<-paste(spp, "v2",size, signif(decay, digits=2), sep="_")}else{
          file.ID<-paste(spp, "v1",size, signif(decay, digits=2), sep="_")  
        }
      
      #save nnets
      save(adacost.oc.nnets,  file=paste(output.folder,"models/",file.ID, "_AC_oc_nnets.RData", sep=""))
      
      #save metrics
      if(detail.metrics==TRUE){
        save(Hx, Hx.v, Hx.p, Hx.vp, ACmetrics, metrics.cvp, metrics.cv, metrics.c, metrics.cp,
             file=paste(output.folder,file.ID, "_AC_oc_outputs.RData", sep=""))}else{
               save(Hx, Hx.v, Hx.p, Hx.vp, ACmetrics,  
                    file=paste(output.folder,file.ID, "_AC_oc_outputs.RData", sep=""))      
             }
      
      
    }
    
    
    
  }
  
  print(ACmetrics)  
}
#____________________________________________________________________________
adacost2OC.test<-function(X, y, x.val, y.val, size, decay=0.1, maxit=100, 
                          varnames, T.test, C.vec){
  
  y[y>=1]<-1
  y.val[y.val>=1]<-1
  
  y.neg<-y
  y.neg[y.neg==0]<--1
  
  y.neg.val<-y.val
  y.neg.val[y.neg.val==0]<--1
  
  n<-dim(X)[1]
  n.v<-length(y.val)
  
  
  C2.test.metrics<-array(0, dim=c(2, 4, length(C.vec)))
  
  for(i in 1:length(C.vec)){
    
    C<-C.vec[i]
    
    hxt<-matrix(0, nrow=n, ncol=T.test)
    hxt.p<-array(0, dim=c(T.test, n, 2))
    
    hxt.v<-matrix(0, nrow=n.v, ncol=T.test)
    hxt.vp<-array(0, dim=c(T.test, n.v, 2))  
    
    epsilon<-rep(0, times=T.test)
    D<-matrix(0, nrow=n, ncol=T.test)
    a<-rep(0, times=T.test)
    
    D[,1]<-1/n
    
    
    for(t in 1:T.test){
      #Train nnet with D[t] weights
      nnet.out<-trainNN(x=X,y=y, x.val=x.val, y.val=y.val, 
                        size=size, decay=decay,  
                        maxit=maxit, 
                        varnames=varnames, weights=D[,t])
      
      #Compile hx[t]s for training & validation set {-1,1}
      hxt[,t]<-nnet.out$hx
      hxt[nnet.out$hx<=0,t]<--1
      hxt.v[,t]<-nnet.out$hx.v
      hxt.v[nnet.out$hx.v<=0,t]<--1
      
      
      #Misclassification (I {0,1}) & correct classification (Ii {-1,1}) indicator vectors  
      I<-y!= nnet.out$hx
      II<-1-I
      Ii<-rep(1, times=length(I))
      Ii[I==1]<--1
      
      #set up cost vector
      Iic<-y
      Iic[y==0]<-C
      
      if(sum(II*D[,t]*Iic)<sum(I*D[,t]*Iic)){next}
      
      
      #Calculate nn[t] weight
      a[t]<-(1/2)*log(sum(II*D[,t]*Iic)/sum(I*D[,t]*Iic))
      
      
      
      #Update weights
      if(t!=T.test){D[,t+1]<-(Iic*D[,t]*exp(-a[t]*Ii))/sum(Iic*D[,t]*exp(-a[t]*Ii))}
      
      #Compile hx[t] probabilities for training & validation set {0,1} & weight by a
      hxt.vp[t,,]<-nnet.out$hx.vp*a[t]
      
      
      #On final iteration compile overall ensemble predictions & metrics
      if(t==T.test){
        Hx.v<-sign(hxt.v%*%a)
        if(length(unique(Hx.v))>1){C2.test.metrics[1,,i]<-metricFy(y=y.neg.val, hx=Hx.v)}
        Hx.vp<-max.col(colSums(hxt.vp))-1
        if(length(unique(Hx.vp))>1){C2.test.metrics[2,,i]<-metricFy(y=y.val, hx=Hx.vp)}
        
      }
      
    }
    
    if(i==length(C.vec)){
      
      if(length(grep("fdens", varnames))>0){
        file.ID<-paste(spp, "v2",size, signif(decay, digits=2), sep="_")}else{
          file.ID<-paste(spp, "v1",size, signif(decay, digits=2), sep="_")  
        }
      #save metrics
      save(C2.test.metrics, 
           file=paste(output.folder,file.ID,"_C2_cost_setup_test_outputs.RData", sep=""))      
      #set C  
      
      C<-C.vec[which(C2.test.metrics[,4,]==max(C2.test.metrics[,4,]), arr.ind=TRUE)[2]]
      save(C, 
           file=paste(output.folder,file.ID,"_C_outputs.RData", sep=""))      
    }
    
    
  }
  
  print(C)
  
}


