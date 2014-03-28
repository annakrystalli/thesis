#Set up
rm(list = ls())

an.ID="Final/"
an.IDCV<-"model assess NC CV"

load(file="~/Documents/TRAINING DATA/spp.names")
t.ids<-c( "1998-4-", "1998-5-", "1998-6-", "1999-10-", "1999-4-", 
         "1999-5-", "1999-9-", "2000-5-", "2000-6-", 
         "2000-7-", "2000-8-", "2000-9-", "2001-3-",  
         "2001-8-", "2001-10-","2002-2-", 
         "2002-6-", "2002-7-", "2003-2-")



source('~/Documents/WORKFLOW/randomForest/Robs categorical mean calculation/paramSpaceSearch function.R', chdir = TRUE)

inits<-expand.grid(a=seq(0.5, 5, by=0.5), b=seq(0.5, 5, by=0.5))

paramSpaceSearch(nACs=4, an.IDCV="model assess NC CV", an.ID="Final/", t.ids=t.ids, 
                 thresh="PredPrev=Obs", pt=14, method="Nelder-Mead", inits, 
                 names.spp=names.spp[3:5])


ps.dat<-compileParamSearch(names.spp, nACs=3:5, t.ids, method="Nelder-Mead", an.ID)


