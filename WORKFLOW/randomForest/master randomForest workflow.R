#Set up
rm(list = ls())
#require(gdata)
set.seed(seed=1)

an.ID="Final/"
an.IDCV<-"model assess NC CV"
data="mod "
model="rf_i"
cv.no=5
thresh="PredPrev=Obs"
pr.time="night"

names.spp<-c("chel","cfin", "metrilu", "centrot", "tem")



source('~/Documents/WORKFLOW/randomForest/randomForest final functions.R', chdir = TRUE)

    #tune RF & save
          #tune<-rftune(y=y.m, y.oc, x=x.oc, varnames, size=5000, names.spp)
          #save(tune, file=paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID,"/tune.Rdata", sep=""))

    #reload tuning
    load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                    an.ID,"/tune.Rdata", sep=""))
              #______________________________________________________________________________________________________



  
  set.seed(seed=1)
 
  growForest(x=x.oc, y.oc=y.oc, y=y.m,
             ntree=300, tune, 
             varnames, plot=FALSE, set.seed=TRUE, an.ID, names.spp)
  


            
plotImps(plot=T, multi=T, pt=32, an.ID=an.ID )

#______________________________________________________________________________________________________


keep(an.ID, an.IDCV, data, model, names.spp,   sure = TRUE)


source('~/Documents/WORKFLOW/randomForest/predict randomForest functions.R', chdir = TRUE)
                #_________________________________________________________________________________________


        for (spp  in names.spp){
          predictRfor(spp=spp, files=NULL,v=varnames, an.ID=an.ID, pr.time="night" )}


    # metrilu day 
          predictRfor(spp="metrilu", files=NULL,v=varnames, an.ID=an.ID, pr.time="day" )

#______________________________________________________________________________________________________


#Set up
keep(an.ID, an.IDCV, data, model, names.spp,   sure = TRUE, pr.time)
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
    
    predMultiMap(year=year.s[i], spp=spp.v[j], spp.col=cols.matrix[j,], an.ID=an.ID,
                an.IDCV=an.IDCV, thresh=thresh)
  }}




#______________________________________________________________________________________________________
#Set up
keep(an.ID, an.IDCV, model, data,cv.no, sure = TRUE)

load(file="~/Documents/TRAINING DATA/spp.names")

source('~/Documents/WORKFLOW/randomForest/winCPR Validation Functions.R', chdir = TRUE)

 t.ids<-c("1998-6-","1999-5-","1999-4-","2000-5-", "2001-8-", "2001-10-","1999-9-", "2001-3-", "19998-10-")

inits<-expand.grid(a=seq(0.5, 5, by=0.5), b=seq(0.5, 5, by=0.5))

winCPRValidate(an.ID, an.IDCV, cv.no=5, names.spp, data="mod ",model="rf_i", plot.id = t.ids,
                         thresh=thresh, inits=inits, pr.time)
  

summariseVal(thresh, names.spp, an.ID)

#______________________________________________________________________________________________________

#....Perform time corrected metrilu analysis
#______________________________________________________________________________________________________
#Set up
keep(an.ID, an.IDCV, model, data,cv.no, sure = TRUE)

names.spp<-"metrilu"

source('~/Documents/WORKFLOW/randomForest/winCPR Validation TC Functions.R', chdir = TRUE)

t.ids<-c("1998-6-","1999-5-","1999-4-","2000-5-", "2001-8-", "2001-10-","1999-9-", "2001-3-", "19998-10-")

inits<-expand.grid(a=seq(0.5, 5, by=0.5), b=seq(0.5, 5, by=0.5))

winCPRValidateTC(an.ID, an.IDCV, cv.no=5, names.spp, data="mod ",model="rf_i", plot.id = t.ids,
               thresh=thresh, inits=inits, pr.time="night", BrCol="PuRd")


summariseVal(thresh, names.spp, an.ID)


#______________________________________________________________________________________________________



#Set up
keep(an.ID, sure = TRUE)

source('~/Documents/WORKFLOW/AORS/AOR extract functions.R', chdir = TRUE)


input.folder<-paste("~/Documents/PREDICTED/", an.ID,"normalised monthly maps/",sep="")

for (spp in c("chel","cfin", "metrilu", "tem", "centrot")){
  AORextract(spp=spp, input.folder=input.folder, b=NULL, a=0.95, y.plots=T, 
             an.ID=an.ID, years=1998:2010, no.plot=T, hists=F, season=F)}



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
