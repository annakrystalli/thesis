#Set up
rm(list = ls())

input<-"~/Documents/TRAINING DATA/Models/randomForest/forests/"
require(randomForest)
require(png)

source('~/Documents/WORKFLOW/randomForest/rfor_diagnostics_functions.R', chdir = TRUE)

spp<-"chel"

corData(plot=F, an.ID=NULL, spp=NULL, dt="oc", lm.diag=T)
corData(plot=F, an.ID=NULL, spp=NULL, dt="ac", lm.diag=T)


plotALLcor(an.ID="No Geo/", plot.only=F)