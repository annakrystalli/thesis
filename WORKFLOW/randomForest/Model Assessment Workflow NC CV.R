rm(list=ls())


#Input correct an.ID for tuning and y data ("mod " for merged categories)
an.ID="model assess NC CV/"
an.IDCV="model assess NC CV"
thresh="PredPrev=Obs"

#data= " mod" for merged categories or NULL for original ACs
data=" mod"

#Only New cats Independent & Balaned
source('~/Documents/WORKFLOW/randomForest/Model Assessment Functions new cats CV.R', chdir = TRUE)

#New cats & means of each sample Dependent only
source('~/Documents/WORKFLOW/randomForest/Model Assessment Functions new cats means.R', chdir = TRUE)

cv.no<-5
run=c("rf_i", "rf_b")

tune<-rftune(all.samples, y.m, y.moc, x.dat, varnames,test.size=0.2, 
               spl="dependent", cv.no, i=1)



tune.i<-rftune(all.samples, y.m, y.moc, x.dat, varnames,test.size=0.2, 
               spl="independent", cv.no, i=1)



tune.b<-rftune(all.samples, y.m, y.moc, x.dat, varnames,test.size=0.2, 
               spl="balanced", cv.no, i=1)

save(tune.b, tune.i,    
     file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                an.ID,"tune.Rdata", sep=""))


load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                an.ID,"tune.Rdata", sep=""))






for(spp.id in 1:5){
 
  for (i in 1:cv.no){ 
    
   an.ID<-paste("model assess NC CV",i, "/", sep="") 

   
   dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID, 
                    sep=""), 
              showWarnings = F)
   dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID, 
                    "forest plots/",sep=""), 
              showWarnings = F)
   dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID, 
                    "forests/",sep=""), 
              showWarnings = F)
   
 for(model in run){
   
  spp<-names.spp[spp.id]
  
 GrowRFs(x.dat=x.dat, y.m, tune.i=tune.i, tune.b=tune.b,  spp.id=spp.id, all.samples=all.samples, 
         cv.no=cv.no, spp=spp,
         varnames=varnames, model=model, ntree=200, ntree.b=500, i=i, an.ID=an.ID, min.cat=100)}}}



extractThresholds(an.IDCV, model, names.spp, cv.no)


compileCVResults(names.spp, y.m=y.m, y.moc=y.moc, x.dat=x.dat, 
                           cv.no=5, run=run,  an.IDCV=an.IDCV, thresh=thresh)  
  

tabulateCVResults(thresh, an.IDCV)

compileHistsNCCV("rf_i", pt=14, names.spp, thresh=thresh, an.IDCV) 
compileHistsNCCV("rf_b", pt=14, names.spp, thresh=thresh, an.IDCV) 

compileBoxplotsNCCV("rf_i", pt=14, names.spp, thresh=thresh, an.IDCV) 
compileBoxplotsNCCV("rf_b", pt=14, names.spp, thresh=thresh, an.IDCV) 

compileVarImp(an.ID)
