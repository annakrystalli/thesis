rm(list=ls())


#Input correct an.ID for tuning and y data ("mod " for merged categories)
an.ID="model assess1/"
#data= " mod" for merged categories or NULL for original ACs
data=" mod"

#Only New cats Independent & Balaned
source('~/Documents/WORKFLOW/randomForest/Model Assessment Functions new cats CV.R', chdir = TRUE)

#New cats & means of each sample Dependent only
source('~/Documents/WORKFLOW/randomForest/Model Assessment Functions new cats means.R', chdir = TRUE)



tune<-rftune(all.samples, y.m, y.moc, x.dat, varnames,test.size=0.2, 
               spl="dependent")



tune.i<-rftune(all.samples, y.m, y.moc, x.dat, varnames,test.size=0.2, 
               spl="independent")



tune.b<-rftune(all.samples, y.m, y.moc, x.dat, varnames,test.size=0.2, 
               spl="balanced")

save(tune,   
     file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                an.ID,"forests/tune.Rdata", sep=""))


load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                an.ID,"forests/tune.Rdata", sep=""))


cv.no<-5
run=c("rf_i", "rf_b")



for(spp.id in 4:5){
 
  for (i in 1:cv.no){ 
    
   an.ID<-paste("model assess CV",i, "/", sep="") 

   
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






for (i in 1:cv.no){ 
  
  an.ID<-paste("model assess CV",i, "/", sep="") 


load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                an.ID,"forests/",
                spp, "/", model, ".Rdata", sep=""))

compileResults(names.spp, y.m=y.m, y.moc=y.moc, x.dat=x.dat, 
               an.ID=an.ID, run=run)}

compile<-vector("list", 5)

for (i in 1:cv.no){ 
  
  an.ID<-paste("model assess CV",i, "/", sep="") 

  load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                an.ID,"forests/result summary.Rdata", sep=""))
  
  compile[[i]]<-results}
  
results.mean<-vector("list",5)
names(results.mean)<-names.spp

results.var<-vector("list",5)
names(results.var)<-names.spp
  
dimnames.oc<-dimnames(compile[[1]][[1]][[1]])
dimnames.ac<-dimnames(compile[[1]][[1]][[2]])
dim.oc<-dim(compile[[1]][[1]][[1]])
dim.ac<-dim(compile[[1]][[1]][[2]])

for(j in 1:5){
OC.comp<-NULL
AC.comp<-NULL

for(i in 1:cv.no){
  
  OC.comp<-cbind(OC.comp, as.vector(compile[[i]][[j]][[1]]))
  AC.comp<-cbind(AC.comp, as.vector(compile[[i]][[j]][[2]]))

}
results.mean[[j]]$OC<-matrix(rowMeans(OC.comp), nrow=dim.oc[1], ncol=dim.oc[2], 
                               dimnames=dimnames.oc)
results.var[[j]]$OC<-matrix(apply(OC.comp,1,var), nrow=dim.oc[1], ncol=dim.oc[2], 
                              dimnames=dimnames.oc)

results.mean[[j]]$AC<-matrix(rowMeans(AC.comp), nrow=dim.ac[1], ncol=dim.ac[2], 
                             dimnames=dimnames.ac)
results.var[[j]]$AC<-matrix(apply(AC.comp,1,var), nrow=dim.ac[1], ncol=dim.ac[2], 
                            dimnames=dimnames.ac)
}


an.ID<-paste("model assess CV",1, "/", sep="") 

save(results.mean, results.var, file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                an.ID,"forests/result overall summary.Rdata", sep=""))

compileVarImp(an.ID)
