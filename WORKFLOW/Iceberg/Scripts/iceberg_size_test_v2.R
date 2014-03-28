#Set up
rm(list = ls())



input.folder<-"/data/bop10ak/inputs/"
output.folder<-sprintf("%s%s",getwd(),"/outputs/")
dir.create(output.folder)
dir.create(paste(output.folder, "models/", sep=""))

spp<-"chel"

size<-as.numeric(commandArgs(TRUE))

source('/data/bop10ak/Rfunctions/iceberg_oc_functions.R', chdir = TRUE)


n<-dim(X.o.tr.m)[1]


ensembleOC(X=X.o.tr.m, y=y.o.tr.m, x.val=X.o.v.m, y.val=y.o.v.m, size=size, decay=0.1, maxit=5000000, 
                     varnames=v2, T=10, detail.metrics=TRUE)
  
