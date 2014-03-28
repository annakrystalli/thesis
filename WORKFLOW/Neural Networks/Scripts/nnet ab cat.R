#Set up
rm(list = ls())

require(nnet)

load(file="/Users/annakrystalli/Documents/TRAINING DATA/normalized (min.max) x training data.RData")
load(file="/Users/annakrystalli/Documents/TRAINING DATA/y training data.RData")


load(file="~/Documents/TRAINING DATA/Dataset splits monthly.RData")
load(file="~/Documents/TRAINING DATA/Dataset splits weekly.RData")

load(file=paste("/Users/annakrystalli/Documents/SATELLITE/"
                ,"monthly","/PROCESSED/normalised/","2000-6-",".RData", sep=""))

X.pred<-X.pred[-which((X.pred$lat<=0.45) & (X.pred$lon<=0.133333)),]



pred.oc<-max.col(mod$preds)-1


n.m<-length(y.m[,1])
n.w<-length(y.w[,1])

selectX<-function(X, varnames){X[,which(names(X) %in% varnames)]}

polyFeature<-function(X, d){
    n<-dim(X)[2]
    m<-dim(X)[1]
    
    mat<-matrix(NA, m, n*d)
    for(i in 1:n){
      for(k in 1:d){
        mat[,(i+k-1)]<-X[,i]^k
        
      }
    }
    
    Xpoly<-as.data.frame(mat)
    names(Xpoly)<-paste(rep(names(X), each=d), 1:d, sep=".")
    
    return(Xpoly)
}

X.a.tr.m.p2<-polyFeature(X.a.tr.m, 2)
X.a.v.m.p2<-polyFeature(X.a.v.m, 2)
X.pred.p2<-polyFeature(X.pred, 2)
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

var1<-c("lon", "lat", "year", 
               "month",  "T.m",  "Tf.m",  "Sed.m", 
               "Sedf.m",  "Chf.m", "Ch.m", 
               "fdist.m",  "fside.m", 
               "fdist.c.m",  "fside.c.m", 
               "bath", "NAO", "wNAO")

var2<-c("lon", "lat", "year", 
               "month",  "T.m",  "Sed.m", 
               "Sedf.m",  "Ch.m", "fdens.m", 
               "fdist.m",  "fside.m",  "fdens.c.m", 
               "fdist.c.m",  "fside.c.m", 
               "bath", "NAO", "wNAO")



varnames.w<-c("lon", "lat",  "year", 
              "month", "week", "T.w",  "Tf.w",  "Sed.w", 
              "Ch.w",  "fdens.w",  "fdist.w",  "fside.w", 
              "bath", "NAO", "wNAO")
#......SPlit DATASET........................................................  

#...MONTHLY...ABUNDANCE CATEGORY SET SPECIES

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
chel.ac.m.multim<-trainACNN(x=X.a.tr.m, y=y.a.tr.m, x.val=X.a.v.m, 
                         y.val=y.a.v.m, x.pred=X.pred, maxit=500000,
                        size=30, decay=0.1, pred.oc=pred.oc,
                         varnames=varnames.m1, weights=NULL, model="nnet")


#..............................................................................
#LOOP
n<-10
cost.rep.nnet.v1.0.1<-data.frame(J=rep(0,times=n))

for(i in 1:n){
  
  mod<-trainACNN(x=X.a.tr.m, y=y.a.tr.m, x.val=X.a.v.m, 
                 y.val=y.a.v.m, x.pred=X.pred, maxit=500000,
                 size=30, decay=0.1, pred.oc=pred.oc,
                 varnames=var1, weights=NULL, model="nnet",
                 file.ID=paste(model,decay,size,"var1",i), plot=FALSE)
  
  cost.rep.nnet.v1.0.1[i,1]<-mod$nnet$value
}







#...TRAINING FUNCTION.............................................................
trainACNN<-function(x,y, x.val, y.val, x.pred, pred.oc, size,
                  decay=0.1,  maxit=300, varnames, weights, 
                    model, plot=TRUE, file.ID=NULL){
  
  require(nnet)
  #prep y
  n<-length(y)
  n.val<-length(y.val)
  y.cat<-y
  
  y<-class.ind(y)
  add<-setdiff(as.character(1:12),dimnames(y)[[2]])
  y<-cbind(y,matrix(0, nrow=length(y[,1]), ncol=length(add), dimnames=list(NULL,add)))
  
  y.val<-class.ind(y.val)
  add<-setdiff(as.character(1:12),dimnames(y.val)[[2]])
  y.val<-cbind(y.val,matrix(0, nrow=length(y.val[,1]), ncol=length(add), dimnames=list(NULL,add)))
  
  
  #nnet formula
  name<-varnames
  fmla <- as.formula(paste("y~ ", paste("x$",name ,collapse= "+", sep="")))
  #
  
  if(model=="nnet"){nnet.mod<-nnet(fmla,  size=size, maxit=maxit, decay=decay, weights=weights)
                    #predict for validation
                    x<-selectX(x.val, name)
                    pr<-predict(nnet.mod, newdata=x, type="raw")
                    pr.nnet<-max.col(pr)}
  if(model=="multinom"){nnet.mod<-multinom(fmla, weights=weights, maxit=maxit)
                        #predict for validation
                        x<-selectX(x.val, name)
                        pr.nnet<-as.numeric(predict(nnet.mod, newdata=x, 
                                                    type="class"))
                        pr<-predict(nnet.mod, newdata=x, 
                                                    type="probs")}
  
  hx<-max.col(nnet.mod$fitted.values)
  
  save(nnet.mod, file=paste("~/Documents/TRAINING DATA/Models/AC/", file.ID, "AC.RData", sep="")
  )  

     
      
      table<-table(y.cat, hx)  
  
      value<-sum(table[cbind(as.numeric(dimnames(table)[[2]]),1:dim(table)[2])])/n
      
      v.table<-table(max.col(y.val), pr.nnet) 
      v.value<-sum(diag(v.table))/n.val
      
      devs<-abs(max.col(y.val)-pr.nnet)  
      dev.table<-table(max.col(y.val),devs) 
    
      R.table<-t(t(v.table)/colSums(v.table))
      R.table[which(is.nan(R.table))]<-0
  
      P.table<-v.table/rowSums(v.table)
      
        r<-dim(v.table)[1]
        c<-dim(v.table)[2]
        
      R.cat<-R.table[cbind(as.numeric(dimnames(R.table)[[2]]),1:c)]
      P.cat<-P.table[cbind(as.numeric(dimnames(P.table)[[2]]),1:c)]
  

      
      P<-sum(P.table[cbind(as.numeric(dimnames(P.table)[[2]]),1:c)])/r
  

      R<-sum(R.table[cbind(as.numeric(dimnames(R.table)[[2]]),1:c)])/c
  
  
      F1<-2*((P*R)/(P+R))
      
      x<-x.pred
  if(model=="nnet"){ preds<-predict(nnet.mod, newdata=x, type="raw")
                     preds<-max.col(preds)}
  if(model=="multinom"){preds<-as.numeric(predict(nnet.mod, newdata=x, type="class"))}
  
 
  
  
       if(plot==FALSE){
         png(file = paste("~/Documents/TRAINING DATA/Models/plots/AC/",file.ID, 
                          "AC diagnostics.png", sep=""), 
         width=750, height=1000, pointsize=19)
       }
  
  layout(matrix(c(1,1,1,2,2,2,3,3,4,4,5,5,6,6,6,7,7,7),3,6, byrow=TRUE))     
  par(las=1, mar=c(2, 2, 2, 0.4) + 0.1, col.axis="darkslategray", 
      family="Helvetica", font.axis=2 , font.lab=2, oma=c(0.2,0,0.2,0))
  

  par(mgp=c(0.7,0.1,0), cex.lab=0.8,cex.axis=0.6)    
                image(z=t(P.table[dim(P.table)[1]:1,]), 
                      x=1:(c+1), y=1:(r+1), 
                      main="predicted AC", cex.main=0.8,
                      xlab="Precision matrix for each actual AC", ylab="Actual AC",
                      col=grey.colors(n=100000, start=0, end=1),
                      axes=FALSE)
                axis(3, at=seq(1.5,c+0.5 , length.out=c), 
                     labels=rev(dimnames(P.table)[[2]]), tick=FALSE)
                axis(2, at=seq(1.5,r+0.5 , length.out=r), 
                     labels=rev(dimnames(P.table)[[1]]), tick=FALSE)
           
                abline(h=(1:r), col="white", lwd=3)
                abline(v=(1:(c+1)), col="grey")
      
     
  par(mgp=c(0.7,0.1,0), cex.lab=0.8,cex.axis=0.6)        
              image(z=t(R.table[dim(R.table)[1]:1,]), 
                    x=1:(dim(R.table)[2]+1), y=1:(dim(R.table)[1]+1), 
                    main="predicted AC", cex.main=0.8,
                    xlab="Recall matrix for each predicted AC", ylab="Actual AC",
                    col=grey.colors(n=100000, start=0, end=1),
                    axes=FALSE)
        axis(3, at=seq(1.5,c+0.5 , length.out=c), 
             labels=dimnames(R.table)[[2]], tick=FALSE)
        axis(2, at=seq(1.5,r+0.5 , length.out=r), 
             labels=rev(dimnames(R.table)[[1]]), tick=FALSE)
                            
                            abline(h=(1:r), col="grey" )
                            abline(v=(1:(c+1)), col="white", lwd=3)

  
  
  par(mgp=c(0.7,1,0), cex.lab=0.8,cex.axis=0.8)
  barplot(P.cat, col="seagreen4", border="white", lwd=2, axes=TRUE,
          names.arg=dimnames(P.table)[[2]],
          main="% yK correctly classified", cex.main=0.8)
        box("plot", lwd=1.5)
        abline(h=c(0.2,0.4,0.6,0.8), col="grey") 
      
  par(mgp=c(0.7,1,0), cex.lab=0.8,cex.axis=0.8)
      barplot(R.cat, col="seagreen4", border="white", lwd=2, 
              names.arg=dimnames(R.table)[[2]],
              main="% h(x)K correctly predicted", cex.main=0.8,
              axes=TRUE)
            box("plot", lwd=1.5)
            abline(h=c(0.2,0.4,0.6,0.8), col="grey")

  
  par(mgp=c(0.7,0.2,0), cex.lab=0.8,cex.axis=0.6) 
  image(dev.table, col=gray(100:0/100), axes=FALSE,
        xlab="h(x) AC", ylab="h(x) AC deviation", 
        main="Absolute class deviation", cex.main=0.8)
      box("plot", lwd=1.5)
      axis(1, at=seq(0,1, length.out=dim(dev.table)[1]) , 
           labels=dimnames(dev.table)[[1]], tick=FALSE)
      axis(2, at=seq(0,1, length.out=dim(dev.table)[2]), 
           labels=dimnames(dev.table)[[2]], tick=FALSE)
      text(0.3,c(0.97,0.85),adj=c(1,0), 
           labels=c(paste("mean=", signif(mean(devs), digits=2), sep=""),
                    paste("F1=", signif(F1,digits=2), sep="")), 
           cex=0.7)
      
  
  
  par(mgp=c(1.1,0.4,0), cex.lab=0.8,cex.axis=0.6, tcl=-0.2)   
  plot(jitter(pr%*%1:12)~jitter(max.col(y.val)), 
       cex=0.3, xlab= "y val", ylab= "p(h(x) val) %*% AC" ,
       xlim=c(1,12), ylim=c(1,12))
    abline(0,1, lty=2)  
  text(c(1, 4),12,adj=c(0,1), 
       labels=c(expression(paste("Spearmans ",rho," = ", sep="")) ,
                signif(cor(pr%*%1:12,max.col(y.val), 
                           method="spearman"), digits=2)), 
       cex=0.7)
  
  
  par(mgp=c(1.1,0.4,0), cex.lab=0.8,cex.axis=0.6, tcl=-0.2)   
  plot(jitter(pr.nnet)~jitter(max.col(y.val)), 
       cex=0.3, xlab= "y val", ylab= "h(x) val" ,
       xlim=c(1,12), ylim=c(1,12))
  abline(0,1, lty=2)  
  text(c(1, 4),12,adj=c(0,1), 
       labels=c(expression(paste("Spearmans ",rho," = ", sep="")) ,
                    signif(cor(pr.nnet,max.col(y.val), 
                               method="spearman"), digits=2)), 
       cex=0.7)
  
  
  if(plot==FALSE){dev.off()
    png(file = paste("~/Documents/TRAINING DATA/Models/plots/AC/",file.ID, 
                     "AC map.png", sep=""), 
        width=500, height=650, pointsize=16)}
  
    predMap(img, x.pred=x.pred, preds=preds, pred.oc=pred.oc)

  if(plot==FALSE){dev.off()}
        
    return(list(nnet=nnet.mod, table=table, value=value, v.table=v.table,v.value=v.value, R=R, P=P,F1=F1, preds=preds))
}





#Pred mMAP........................................................................


#pred.oc=vector{0,1}
predMap<-function(img, x.pred=x.pred, preds=preds, pred.oc=pred.oc){

  require(png)
  
  img<-readPNG("~/Documents/Presentations/BES 2012/Images/M2000122-2000152.uk.sstp.AVH.L3_median.01may00-31may00.v1.20122500252.rsg_grey.png")
  
  
  #Load geo.matrix
  load(file="/Users/annakrystalli/Documents/CPR/DATA/RAW DATA/SeaWiFS/r files/geo.matrix.RData")
  lon<-c(geo.matrix[1,,1],11.01279)
  lat<-c(rev(geo.matrix[,1,2]), 61.00876)

  
      x.pred<-x.pred[pred.oc==1,1:2]
      y.pred<-preds[pred.oc==1]
  
      pred.map<-matrix(max(y.pred)+1, ncol=926, nrow=1112)
      pred.map[img==1]<-max(y.pred)+2
      
      pred.map[cbind(x.pred$r,x.pred$c)]<-y.pred


  
layout(matrix(data=c(1,2), nrow=2, ncol=1), widths=c(1,1), heights=c(10,2))

  par(las=1, mar=c(2, 3, 2, 2) + 0.1, col.axis="darkslategray", 
      family="Helvetica", font.axis=2, oma=c(0.2,0.2,0.2,0.2), mgp=c(4,0.6,0) )

      K<-length(unique(y.pred))
      FUN.c<-colorRampPalette(c( "antiquewhite2",  "darkgoldenrod4"), space="Lab")
      
      cols<-FUN.c(n=max(y.pred))
      cols<-c(cols, "cadetblue3")
      cols<-c(cols, "black")

image(z=t(pred.map[dim(pred.map)[1]:1,]),
      x=lon, y=lat, 
      col=cols, xlab="", ylab="", cex.axis=0.8)
box("plot", lwd=1.5)

  par(mar=c(2, 3, 0, 2) + 0.1, font.lab=2, mgp=c(4,0.6,0))
  
  scale<-matrix(rep(sort(unique(y.pred)), each=as.integer(926/K))+1, 
                           nrow=50, 
                           ncol=as.integer(926/K)*K,
                           byrow=TRUE)
  cols<-FUN.c(n=max(y.pred))
  
  image(z=t(scale),axes=FALSE,
        col=cols,xlab="", ylab="", cex.lab=1)
  box("plot", lwd=1.5)          
  
  
  
  axis(1, at=seq(0,1-(1/K), length.out=K)+1/(2*K),
       labels=sort(unique(y.pred)), tick=FALSE, cex.axis=1)

}




