rm(list=ls())


source('~/Documents/WORKFLOW/randomForest/Model Assessment Functions.R', chdir = TRUE)

an.ID="model assess1/"


                  tune<-rftune(all.samples, y.m, y.moc, x.dat, varnames,test.size=0.2, 
                               spl="dependent")
                  
                  tune.i<-rftune(all.samples, y.m, y.moc, x.dat, varnames,test.size=0.2, 
                                   spl="independent")
                  
                  tune.c<-rftune(all.samples, y.m, y.moc, x.dat, varnames,test.size=0.2, 
                                 spl="combined")
                   
                  tune.b<-rftune(all.samples, y.m, y.moc, x.dat, varnames,test.size=0.2, 
                                 spl="balanced")
                  
                  save(tune, tune.i, tune.c, tune.b, 
                       file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                                                              an.ID,"forests/tune.Rdata", sep=""))


load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                an.ID,"forests/tune.Rdata", sep=""))

for(spp.id in 1:5){
  spp<-names.spp[spp.id]

              assign(paste(spp, "rfs", sep="_"), GrowRFs(x.dat, y.m, tune, tune.c, tune.b, spp.id, all.samples,
                                varnames, test.size=0.2), envir=as.environment(1))}
              
              
              
              save(list=paste(names.spp, "rfs", sep="_"), 
                   file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                              an.ID,"forests/all_RFs.Rdata", sep=""))



compileResults(names.spp, y.m=y.m, y.moc=y.moc, x.dat=x.dat, 
                         an.ID=an.ID)


compileVarImp(an.ID)
