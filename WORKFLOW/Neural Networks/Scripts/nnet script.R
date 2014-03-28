#Set up
rm(list = ls())

require(nnet)
require(R.matlab)

selectX<-function(X, varnames){X[,which(names(X) %in% varnames)]}

load(file="/Users/annakrystalli/Documents/TRAINING DATA/normalized (min.max) x training data.RData")
load(file="/Users/annakrystalli/Documents/TRAINING DATA/y training data.RData")

                                                            n.m<-dim(y.m)[1]
                                                            n.w<-dim(y.w)[1]
                                                            
                                                            all.dat.m<-1:n.m
                                                            all.dat.w<-1:n.w


load(file="~/Documents/TRAINING DATA/Dataset splits monthly.RData")

                    train.dat.m<-sample(all.dat.m, n.m*0.7)
                    val.dat.m<-sample(setdiff(all.dat.m, train.dat.m), 
                                      2/3*length(setdiff(all.dat.m, train.dat.m)))
                    test.dat.m<-setdiff(all.dat.m, union(train.dat.m, val.dat.m))    
                                  
                save(val.dat.m,test.dat.m,train.dat.m, 
                     file="~/Documents/TRAINING DATA/Dataset splits monthly.RData")                


load(file="~/Documents/TRAINING DATA/Dataset splits weekly.RData")

                    train.dat.w<-sample(all.dat.w, n.w*0.7)
                    val.dat.w<-sample(setdiff(all.dat.w, train.dat.w), 
                                      2/3*length(setdiff(all.dat.w, train.dat.w)))
                    test.dat.w<-setdiff(all.dat.w, union(train.dat.w, val.dat.w))    
                                                                
                save(val.dat.w,test.dat.w,train.dat.w, 
                     file="~/Documents/TRAINING DATA/Dataset splits weekly.RData")                
                

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
                                
                                X.o.tr.m<-as.matrix(X.o.m[train.dat.m,])
                                X.a.tr.m<-X.o.tr.m[y.o.tr.m>=1,]   
                                    
                                X.o.v.m<-as.matrix(X.o.m[val.dat.m,])
                                X.a.v.m<-X.o.v.m[y.o.v.m>=1,]                   
                                                   
                                X.o.ts.m<-as.matrix(X.o.m[test.dat.m,])
                                X.a.ts.m<-X.o.ts.m[y.o.ts.m>=1,]                    

        #...WEEKLY..................
                      y.o.w<-y.w$cal.hel
                                y.o.tr.w<-y.o.w[train.dat.w]
                                y.a.tr.w<-y.o.tr.w[y.o.tr.w>=1]
                                
                                y.o.v.w<-y.o.w[val.dat.w]
                                y.a.v.w<-y.o.v.w[y.o.v.w>=1]
                                
                                y.o.ts.w<-y.o.w[test.dat.w]
                                y.a.ts.w<-y.o.ts.w[y.o.ts.w>=1]
                                
                                
                                X.o.w<-selectX(norm.x.w, varnames)
                                
                                X.o.tr.w<-as.matrix(X.o.w[train.dat.w,])
                                X.a.tr.w<-X.o.tr.w[y.o.tr.w>=1,]   
                                
                                X.o.v.w<-as.matrix(X.o.w[val.dat.w,])
                                X.a.v.w<-X.o.v.w[y.o.v.w>=1,]                   
                                                   
                                X.o.ts.w<-as.matrix(X.o.w[test.dat.w,])
                                X.a.ts.w<-X.o.ts.w[y.o.ts.w>=1,]

nn_params<-unlist(oct.weights)
X<-as.matrix(oct.data$X)
y<-as.vector(oct.data$y)


cost.val<-nnlrCostGrad(nn_params,
                   input_layer_size=400,
                   hidden_layer_size=25,
                   num_labels=10, X=X, y=y, lambda=0.1, pred="cat")

nn_params<-randInitialiseWeights(input_layer_size=16,
                      hidden_layer_size=12,
                      num_labels=12)
  


test.optim3<-optimx(nn_params, nnlrCost, nnlrGrad, method="BFGS", input_layer_size=16,
       hidden_layer_size=12,
       num_labels=12, X=X.ab, y=y.ab, lambda=1, pred="e")




pred<-nnlrPred(nn_params=unlist(test.optim3$par),
                       input_layer_size=16,
                       hidden_layer_size=12,
                       num_labels=12, X=X.ab, pred="e")

testFits(y.ab, pred, pred="e")



com.res<-vector("list", length=9)

for(i in (dim(X.a.tr.m)[2]-1):11){
com.res[[i]]<-permCost(n=dim(X.a.tr.m)[2],
                       r=i,
                       X=X.a.tr.m, 
                       y=y.a.tr.m, 
                       X.test=X.a.v.m, 
                       y.test=y.a.v.m, 
                       pred="e")}

save(com.res1, com.res2, file="~/Documents/TRAINING DATA/first model selection pass 2-9 calhel.RData")

featureSelect(com.res2, varnames.m)

var.selec<-featureSelect(com.res1, varnames)[[1]]
var.select<-c(var.selec ,featureSelect(com.res2, varnames.m)[[1]])


var.rank<-unlist(featureSelect(com.res1, varnames)[[1]])
var.rank<-c(var.rank, unlist(featureSelect(com.res2, varnames.m)[[1]]))
which(is.numeric(var.rank))
table(var.rank)

bar.plot.tab<-sort(table(var.rank))
names(bar.plot.tab)


FUN.c<-colorRamp(c( "white","darkseagreen"))
cols<-FUN.c((bar.plot.tab-min(bar.plot.tab))/diff(range(bar.plot.tab)))

par(las=1, mar=c(5, 4, 4, 2) + 0.1, col.axis="darkslategray", 
    family="HersheySans", font.axis=2 )
barplot(bar.plot.tab, names.arg=names(bar.plot.tab), horiz=TRUE,
        width=3,
        border="darkseagreen4",
        col= rgb(cols, maxColorValue=256),
        cex.axis=0.8)

(bar.plot.tab/diff(range(bar.plot.tab)))