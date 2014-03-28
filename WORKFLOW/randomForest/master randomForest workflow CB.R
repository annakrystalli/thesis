#Set up
rm(list = ls())
require(gdata)
set.seed(seed=1)

an.ID="model assess1/"

spp<-"chel"



source('~/Documents/WORKFLOW/randomForest/randomForest functions.R', chdir = TRUE)

rforDataprep(resample=TRUE, 
             tr.split=0.8, 
             v.split=2/3, test.criteria="sample")
#______________________________________________________________________________________________________




for(spp in  c("chel", "cfin","metrilu", "tem", "centrot")){
  
  set.seed(seed=1)
  source('~/Documents/WORKFLOW/randomForest/randomForest functions.R', chdir = TRUE)
  
  growForest(x=x, x.oc=x.oc, x.t=x.t, x.toc=x.toc, y=y, y.oc=y.oc,
             y.t=y.t, y.toc=y.toc, ntree=200,
             varnames=v4, plot=TRUE, set.seed=TRUE, an.ID=an.ID, spp=spp)
  
}


plotImps(plot=T, multi=T, pt=32, an.ID=an.ID )

#______________________________________________________________________________________________________


keep(an.ID, sure = TRUE)


source('~/Documents/WORKFLOW/randomForest/predict randomForest functions CB.R', chdir = TRUE)
#_________________________________________________________________________________________
spp.vec<-c("chel","cfin", "metrilu", "centrot", "tem")
run<-c("rf", "rf_i", "rf_c", "rf_b")

for (spp  in spp.vec){
  predictRfor(spp=spp, files=NULL,v=varnames, an.ID=an.ID, run="rf_c")}

#______________________________________________________________________________________________________


#Set up
keep(an.ID, sure = TRUE)
require(png)

source('~/Documents/WORKFLOW/randomForest/plotPredmaps functions.R', chdir = TRUE)

#_______________________________________________________________________________________________________

#Pred mMAP........................................................................

year.s<-1997:2010
spp.v<-c("chel", "cfin", "tem", "centrot", "metrilu")
cols.matrix<-matrix(c( "antiquewhite2",  "darkgoldenrod4",
                       "honeydew2", "aquamarine4", 
                       "lavenderblush2","palevioletred4",
                       "thistle1", "purple3",
                       "mistyrose3","indianred4"),
                    nrow=5, ncol=2, byrow=TRUE)


for(j in 1:5){
  for(i in 1:length(year.s)){
    
    predMultiMap(year=year.s[i], spp=spp.v[j], spp.col=cols.matrix[j,], an.ID=an.ID)
  }}




#______________________________________________________________________________________________________

#Set up
keep(an.ID, sure = TRUE)
run="rf_c"
source('~/Documents/WORKFLOW/AORS/AOR extract functions CB.R', chdir = TRUE)


input.folder<-paste("~/Documents/PREDICTED/", an.ID, run,"/normalised monthly maps/",sep="")

for (spp in c("chel","cfin", "metrilu", "tem", "centrot")){
  AORextract(spp=spp, input.folder=input.folder, b=NULL, a=0.95, y.plots=T, 
             an.ID=an.ID, years=1998:2010, no.plot=T, hists=T, season=T, run=run)}



plotYearlyAll(pt=28, y.plots=F, input.folder=input.folder, an.ID=an.ID,years=1998:2010)

#______________________________________________________________________________________________________

#Set up
keep(an.ID, sure = TRUE)

input<-paste("~/Documents/TRAINING DATA/Models/randomForest/,"an.ID,"forests/", sep="")
require(randomForest)
require(png)

source('~/Documents/WORKFLOW/randomForest/rfor_diagnostics_functions.R', chdir = TRUE)

#spp<-

corData(plot=F, an.ID=an.ID, spp=NULL, dt="oc", lm.diag=T)
corData(plot=F, an.ID=an.ID, spp=NULL, dt="ac", lm.diag=T)


plotALLcor(an.ID=an.ID, plot.only=T, log=F)
plotALLcor(an.ID=an.ID, plot.only=T, log=T)
