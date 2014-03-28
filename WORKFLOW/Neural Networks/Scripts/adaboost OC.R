#Set up
rm(list = ls())

require(nnet)

#Load ave
ave<-read.csv("~/Documents/CPR/DATA/RAW DATA/Accepted values.csv")


load(file="/Users/annakrystalli/Documents/TRAINING DATA/normalized (min.max) x training data.RData")
load(file="/Users/annakrystalli/Documents/TRAINING DATA/y training data.RData")


load(file="~/Documents/TRAINING DATA/Dataset splits monthly.RData")
load(file="~/Documents/TRAINING DATA/Dataset splits weekly.RData")

load(file=paste("/Users/annakrystalli/Documents/SATELLITE/"
                ,"monthly","/PROCESSED/normalised/","2000-6-",".RData", sep=""))

X.pred<-X.pred[-which((X.pred$lat<=0.45) & (X.pred$lon<=0.133333)),]

n.m<-length(y.m[,1])
n.w<-length(y.w[,1])

selectX<-function(X, varnames){X[,which(names(X) %in% varnames)]}
#........MONTHLY
#Value is misclassification error ie needs to be minimised
#...1)tem  2)centrot 3)calfin 4)calhel 5)metrilu

allvar<-c("lon", "lat", "overlap", "time", "year", 
          "month", "tem", "centrot", "calfin", "calhel", 
          "metrilu", "day", "week", "T.m", "T.w", "Tf.m", "Tf.w", "Sed.m", "Sed.w", 
          "Sedf.m", "Sedf.w", "Chf.m", "Chf.w", "Ch.m", "Ch.w", "fdens.m", 
          "fdens.w", "fdist.m", "fdist.w", "fside.m", "fside.w", "fdens.c.m", 
          "fdens.c.w", "fdist.c.m", "fdist.c.w", "fside.c.m", "fside.c.w", 
          "bath", "NAO", "wNAO")

#.......Monthly varnames............................................................
varnames.m<-c("lon", "lat", "time", "year", 
              "month",  "T.m",  "Tf.m",  "Sed.m", 
              "Sedf.m",  "Chf.m", "Ch.m", "fdens.m", 
              "fdist.m",  "fside.m",  "fdens.c.m", 
              "fdist.c.m",  "fside.c.m", 
              "bath", "NAO", "wNAO")

varnames.w<-c("lon", "lat", "time", "year", 
              "month", "week", "T.w",  "Tf.w",  "Sed.w", 
              "Ch.w",  "fdens.w",  "fdist.w",  "fside.w", 
              "bath", "NAO", "wNAO")

#.......varnames WITHOUT TIME...........................................................
varnames.m<-c("lon", "lat",  "year", 
              "month",  "T.m",  "Tf.m",  "Sed.m", 
              "Sedf.m",  "Chf.m", "Ch.m", "fdens.m", 
              "fdist.m",  "fside.m",  "fdens.c.m", 
              "fdist.c.m",  "fside.c.m", 
              "bath", "NAO", "wNAO")

varnames.w<-c("lon", "lat",  "year", 
              "month", "week", "T.w",  "Tf.w",  "Sed.w", 
              "Ch.w",  "fdens.w",  "fdist.w",  "fside.w", 
              "bath", "NAO", "wNAO")
#......SPlit DATASET........................................................  

#...MONTHLY...SET SPECIES

y.o.m<-y.m$calhel

y.o.tr.m<-y.o.m[train.dat.m]
y.a.tr.m<-y.o.tr.m[y.o.tr.m>=1]

y.o.v.m<-y.o.m[val.dat.m]
y.a.v.m<-y.o.v.m[y.o.v.m>=1]

y.o.ts.m<-y.o.m[test.dat.m]
y.a.ts.m<-y.o.ts.m[y.o.ts.m>=1]


X.o.m<-selectX(norm.x.m, varnames.m)

X.o.tr.m<-X.o.m[train.dat.m,]
X.a.tr.m<-X.o.tr.m[y.o.tr.m>=1,]   

X.o.v.m<-X.o.m[val.dat.m,]
X.a.v.m<-X.o.v.m[y.o.v.m>=1,]                   

X.o.ts.m<-X.o.m[test.dat.m,]
X.a.ts.m<-X.o.ts.m[y.o.ts.m>=1,]                    

#...WEEKLY..................

y.o.w<-y.m$calhel

y.o.tr.w<-y.o.w[train.dat.w]
y.a.tr.w<-y.o.tr.w[y.o.tr.w>=1]

y.o.v.w<-y.o.w[val.dat.w]
y.a.v.w<-y.o.v.w[y.o.v.w>=1]

y.o.ts.w<-y.o.w[test.dat.w]
y.a.ts.w<-y.o.ts.w[y.o.ts.w>=1]


X.o.w<-selectX(norm.x.w, varnames.w)

X.o.tr.w<-X.o.w[train.dat.w,]
X.a.tr.w<-X.o.tr.w[y.o.tr.w>=1,]   

X.o.v.w<-X.o.w[val.dat.w,]
X.a.v.w<-X.o.v.w[y.o.v.w>=1,]                   

X.o.ts.w<-X.o.w[test.dat.w,]
X.a.ts.w<-X.o.ts.w[y.o.ts.w>=1,]


#............................................................................
#...TRAIN NN FUNCTION w/ diagnostics

trainADANN<-function(x, y, x.val, y.val, size, decay, maxit,  
                  varnames, weights){
  
  require(nnet)
  
  #prelims
  n<-length(y)
  n.val<-length(y.val)
  
  #prep y
  y<-class.ind(y)
  y.val<-class.ind(y.val)
  
  #nnet formula
  name<-varnames
  fmla <- as.formula(paste("y~ ", paste("x$",name ,collapse= "+", sep="")))
  
  
  
  
  #TRAIN NNET
  nnet.mod<-nnet(fmla,  size=size, maxit=maxit, decay=decay, weights=weights)
  

  
  #predict
  x<-selectX(x.val, name)
  pr.nnet<-predict(nnet.mod, newdata=x, type="raw")
  

    hx<-max.col(nnet.mod$fitted.values)
    tr.table<-table(y[,2], hx) 
    tr.value<-1-(sum(abs(y[,2]- (hx)))/n)
  
    
    v.table<-table(y.val[,2], max.col(pr.nnet)-1) 
    v.value<-1-(sum(abs(y.val[,2]- (max.col(pr.nnet)-1)))/n.val)

    if(dim(v.table)[2]>=2){
    TP<-v.table[2,2]
    FP<-v.table[1,2]
    TN<-v.table[1,1]
    FN<-v.table[2,1]
    
    P<-TP/(TP+FP)
    R<-TP/(TP+FN)
    
    F1<-2*((P*R)/(P+R))
    
    print(F1)
    
    return(list(nnet=nnet.mod, hx=hx, tr.table=tr.table, 
                tr.value=tr.value, v.table=v.table,v.value=v.value, 
                R=R, P=P,F1=F1))
    }else{return(list(nnet=nnet.mod, hx=hx, tr.table=tr.table, 
                      tr.value=tr.value, v.table=v.table,v.value=v.value))}
      
    }


adaboostNN<-function(X, y, x.val, y.val, size, decay=0.1, maxit=2500, 
                             varnames,file.ID=NULL, T, K){
  
        y[y>=1]<-1
        y.val[y.val>=1]<-1
        
        n<- dim(X)[1]
        hxt<-matrix(0, nrow=n, ncol=T)
        epsilon<-rep(0, times=T)
        D<-matrix(0, nrow=n, ncol=T)
        a<-rep(0, times=T)
          
        D[,1]<-1/n
        adaNNET.outs<-vector("list", T)
        
            for(t in 1:T){
              nnet.out<-trainADANN(x=X,y=y, x.val=x.val, y.val=y.val, 
                                   size=size, decay=decay,  
                                   maxit=maxit, 
                                   varnames=varnames, weights=D[,t])
              
              hxt[,t]<-nnet.out$hx
              I<-y!= nnet.out$hx
              Ii<-rep(1, times=length(I))
              Ii[I==1]<--1
              
              epsilon[t]<-sum(D[,t]*I)
              if((1-epsilon[t])<=1/K){next}
              
              a[t]<-log((1-epsilon[t])/epsilon[t])+log(K-1)
              
              Hx<-sign(hxt%*%a)
              
              if(t!=T){D[,t+1]<-(D[,t]*exp(-a[t]*Ii))/sum(D[,t]*exp(-a[t]*Ii))}
              
              adaNNET.outs[[t]]<-nnet.out
            }
        
        save(adaNNET.outs, D, file=paste("~/Documents/TRAINING DATA/Models/",file.ID, "latest adaboost models ANN Model.RData", sep=""))
        
        return(list(adaNNET.outs=adaNNET.outs, D=D))
        }
        




ADA.chel.oc.m<-adaboostNN(X=X.o.tr.m, y=y.o.tr.m, x.val=X.o.v.m, 
                       y.val=y.o.v.m, maxit=5000000,
                      size=30, decay=0.0000001,
                       varnames=varnames.m, file.ID="ADABOOSTtest", T=1)



