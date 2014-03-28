#Set up
rm(list = ls())

require(nnet)

#Load ave
ave<-read.csv("~/Documents/CPR/DATA/RAW DATA/Accepted values.csv")

save(ave,)
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
              varnames.m1<-c("lon", "lat",  "year", 
                            "month",  "T.m",  "Tf.m",  "Sed.m", 
                            "Sedf.m",  "Chf.m", "Ch.m",
                            "fdist.m",  "fside.m",
                            "fdist.c.m",  "fside.c.m", 
                            "bath", "NAO", "wNAO")

              varnames.m2<-c("lon", "lat",  "year", 
                             "month",  "T.m", "Sed.m", 
                             "Sedf.m",  "Ch.m", "fdens.m", 
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

trainNN<-function(x,y, x.val, y.val, x.pred, size, presence=TRUE, decay=0.1,  maxit=300, 
                  varnames, weights, file.ID=NULL, plot=TRUE){
  
      require(nnet)
       
        #prelims
          n<-length(y)
          n.val<-length(y.val)
          if(presence==FALSE){y.cat<-y}
      
        #prep y
          y<-class.ind(y)
          add<-setdiff(as.character(1:12),dimnames(y)[[2]])
          y<-cbind(y,matrix(0, nrow=length(y[,1]), ncol=length(add), dimnames=list(NULL,add)))
          
            y.val<-class.ind(y.val)
            add<-setdiff(as.character(1:12),dimnames(y.val)[[2]])
            y.val<-cbind(y.val,matrix(0, nrow=length(y.val[,1]), ncol=length(add), dimnames=list(NULL,add)))
      
        if(presence==TRUE){y<-cbind(y[,1], rowSums(y[,2:13]))
                             y.val<-cbind(y.val[,1], rowSums(y.val[,2:13]))}
    
        #nnet formula
           name<-varnames
           fmla <- as.formula(paste("y~ ", paste("x$",name ,collapse= "+", sep="")))
        
      
      
      
        #TRAIN NNET
          nnet.mod<-nnet(fmla,  size=size, maxit=maxit, decay=decay, weights=weights)
          
          save(nnet.mod, file=paste("~/Documents/TRAINING DATA/Models/",file.ID, "latest ANN Model.RData", sep=""))
      
        #predict
          x<-selectX(x.val, name)
          pr.nnet<-predict(nnet.mod, newdata=x, type="raw")
      
          if(presence==TRUE){
            hx<-max.col(nnet.mod$fitted.values)-1
            tr.table<-table(y[,2], hx) 
            tr.value<-1-(sum(abs(y[,2]- (hx)))/n)
    
                print(tr.table)
                print(tr.value)
            
            v.table<-table(y.val[,2], max.col(pr.nnet)-1) 
            v.value<-1-(sum(abs(y.val[,2]- (max.col(pr.nnet)-1)))/n.val)
                        print(v.table)
                        print(v.value)
            
            TP<-v.table[2,2]
            FP<-v.table[1,2]
            TN<-v.table[1,1]
            FN<-v.table[2,1]
            
            P<-TP/(TP+FP)
            R<-TP/(TP+FN)
            
            F1<-2*((P*R)/(P+R))
            
            x<-x.pred
            preds<-predict(nnet.mod, newdata=x, type="raw")
            
            plotOCpred(preds=preds, file.ID=file.ID, x.pred=x.pred, plot=plot)
            
            
            return(list(nnet=nnet.mod, hx=hx, tr.table=tr.table, tr.value=tr.value, v.table=v.table,v.value=v.value, R=R, P=P,F1=F1, preds=preds))}else{
              
                    table<-table(y.cat, max.col(nnet.mod$fitted.values)) 
                    value<-sum(diag(table))/n
                    
                    print(table)
                    print(value)    
                    return(list(nnet=nnet.mod, table=table, value=value))
          
          
                     }

        }


        #...CALL______________________________

chel.oc.m<-trainNN(x=X.o.tr.m, y=y.o.tr.m, x.val=X.o.v.m, 
                         y.val=y.o.v.m, x.pred=X.pred, maxit=200000,
                         presence=TRUE, size=30, decay=0.11,
                         varnames=varnames.m1, weights=NULL, 
                   file.ID="plot test", plot=TRUE)


#_____________________________________________________________________________________________
#LOOP
n<-10
cost.rep2500v1.0.1<-data.frame(J=rep(0,times=n))


for(i in 1:n){
  
  mod<-trainNN(x=X.o.tr.m, y=y.o.tr.m, x.val=X.o.v.m, 
          y.val=y.o.v.m, x.pred=X.pred, maxit=2500,
          presence=TRUE, size=30, decay=0.1,
          varnames=varnames.m1, weights=NULL, file.ID=paste("2500 rep var1",i), plot=FALSE)
  
  cost.rep2500v1.0.1[i,1]<-mod$nnet$value
}








#...AUXILLIARY FUNCTIONS



createWeightsvec<-function(y, min.wt){
                        require(nnet)
                          n<-length(y)
                          tbl<-table(y)
                          s<-max(tbl)/tbl
                          wt<-min.wt+(1-min.wt)*((s-min(s))/diff(range(s)))
                          y.cat<-class.ind(y)
                          wts<-y.cat %*% as.vector(wt)
                      
                          return(wts)}

plotOCpred<-function(preds, file.ID, x.pred, plot=TRUE){
 
  #Load geo.matrix
  load(file="/Users/annakrystalli/Documents/CPR/DATA/RAW DATA/SeaWiFS/r files/geo.matrix.RData")
  lon<-c(geo.matrix[1,,1],11.01279)
  lat<-c(rev(geo.matrix[,1,2]), 61.00876)
  
  
  require(png)
  
  
  img<-readPNG("~/Documents/Presentations/BES 2012/Images/M2000122-2000152.uk.sstp.AVH.L3_median.01may00-31may00.v1.20122500252.rsg_grey.png")
  
  #create matrix & set background
    pred.map<-matrix(0.25, ncol=926, nrow=1112)
 #Set land
  pred.map[img==1]<-0.5
  y.pred<-max.col(preds)-1

  #Assign prediction to appropriate pixels
  pred.map[cbind(x.pred$r,x.pred$c)]<-y.pred
  
  if(plot==FALSE){
  png(file = paste("~/Documents/TRAINING DATA/Models/plots/",file.ID, 
                   "ANN pred map.png", sep=""))}
  
  
  #Define image layout & set par 
  layout(matrix(data=c(1,2), nrow=2, ncol=1), widths=c(1,1), heights=c(10,2))
  
  par(las=1, col.axis="darkslategray",
      family="Helvetica", font.axis=2 )
  
  cols<-sort(unique(as.vector(pred.map)))
  
  cols[sort(unique(as.vector(pred.map)))==1]<-"indianred3"
  cols[sort(unique(as.vector(pred.map)))==0]<-"dodgerblue3"
  cols[sort(unique(as.vector(pred.map)))==0.25]<-"dodgerblue4"
  cols[sort(unique(as.vector(pred.map)))==0.5]<-"#EEE8CD"
  
  
  image(z=t(pred.map[dim(pred.map)[1]:1,]),
        x=lon, y=lat, 
        col=cols, xlab="", ylab="", cex.axis=0.8)

  box("plot", lwd=1.5)
  
  legend(4, 52, c("present", "absent"), fill=c("indianred3", "dodgerblue3"), 
         horiz=TRUE, border="black", bty="n", text.col="white")
 
  if(plot==TRUE){
    dev.copy(png,file=paste("~/Documents/TRAINING DATA/Models/plots/",
                            file.ID, "ANN pred map.png", sep=""))}
  dev.off()
}






colour<-rep("red", times=n.m)
colour[which(yp[,1]==1)]<-"snow2"

plot(x$T.m~x$Ch.m, col= colour, pch=21, cex=0.2 )

library(scatterplot3d)
scatterplot3d(x$fdist.m,x$lat,x$month, pch=21, color= colour, grid=TRUE, cex.symbols=0.4,
              type="p", main="3D Scatterplot Cfin")


chel.wts[chel.wts<=1]<-1

head(chel.ab$nnet$fitted)


chel.ab.av<-chel.ab$nnet$fitted.values %*% ave$ave

cor(chel.ab.av, ave$ave[match(y.a.tr.m,ave$cat)])

plot(ave$ave[match(y.a.tr.m,ave$cat)], chel.ab.av,  pch=20, 
     cex=0.4, col="darkseagreen", ylab="fitted", xlab="Accepted values")
abline(0,1)
