rm(list=ls())

an.IDCV<-"model assess NC CV"


varnames<-c("time",  "year", 
            "month",  "T.m", "Sed.m", 
            "Sedf.m",  "Ch.m", "fdens.m", 
            "fdist.m",  "fside.m",  "fdens.c.m", 
            "fdist.c.m",  "fside.c.m", 
            "bath", "NAO", "wNAO")

cv.no<-5
model="rf_i"

load(file="~/Documents/TRAINING DATA/spp.names")


source('~/Documents/WORKFLOW/randomForest/predict randomForest CV functions.R', chdir = TRUE)
#_________________________________________________________________________________________

combineCVRFs(an.IDCV, names.spp[5], model, cv.no)

spp.vec<-c("chel","cfin", "metrilu", "centrot", "tem")
for (spp  in spp.vec[1]){
  predictCVRfor(spp=spp, files=NULL,varnames=varnames, 
                an.IDCV=an.IDCV, model=model)}