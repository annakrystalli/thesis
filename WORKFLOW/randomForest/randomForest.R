#Set up
rm(list = ls())
set.seed(seed=1)

spp<-"metrilu"
                                                      rforDataprep(resample=FALSE, 
                                                                   tr.split=0.7, 
                                                                   v.split=2/3)


source('~/Documents/WORKFLOW/randomForest/randomForest functions.R', chdir = TRUE)

#______________________________________________________________________________________________________

           


for(spp in  c("chel", "cfin","metrilu", "tem", "centrot")){
  
  set.seed(seed=1)
  source('~/Documents/WORKFLOW/randomForest/randomForest functions.R', chdir = TRUE)

growForest(x=x, x.oc=x.oc, x.t=x.t, x.toc=x.toc, y=y, y.oc=y.oc,
                     y.t=y.t, y.toc=y.toc, ntree=500,
                     varnames=v3, plot=TRUE, set.seed=TRUE, an.ID="No Geo/", spp=spp)

}

plotImps(plot=T, multi=T, pt=32, an.ID="No Geo/" )