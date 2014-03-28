#Set up
rm(list = ls())



input.folder<-"/data/bop10ak/inputs/"
output.folder<-"/data/bop10ak/outputs/"
spp<-"chel"



source('/data/bop10ak/R-3.0.0/bin/iceberg_oc_functions.R', chdir = TRUE)


n<-dim(X.o.tr.m)[1]


ensembleOC(X=X.o.tr.m, y=y.o.tr.m, x.val=X.o.v.m, y.val=y.o.v.m, size=5, decay=0.1, maxit=5000000, 
                     varnames=v1, T=10, detail.metrics=TRUE)
  
