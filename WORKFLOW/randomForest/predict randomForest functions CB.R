require(randomForest)


varnames<-c("year", 
            "month",  "T.m", "Sed.m", 
            "Sedf.m",  "Ch.m", "fdens.m", 
            "fdist.m",  "fside.m",  "fdens.c.m", 
            "fdist.c.m",  "fside.c.m", 
            "bath", "NAO", "wNAO")


#_________________________________________________________________________________________________________________________

selectX<-function(X, varnames){X[,which(names(X) %in% varnames)]}
#_________________________________________________________________________________________________________________________


predictRfor<-function(input.folder="/Users/annakrystalli/Documents/SATELLITE/monthly/PROCESSED/", 
                      output.folder=paste("/Users/annakrystalli/Documents/PREDICTED/", 
                                          an.ID,  sep=""),
                      an.ID=NULL,
                      spp=spp,
                      files=NULL, v=varnames,
                      run, pr.time){
  
  dir.create(output.folder, showWarnings=F)
  output.folder<-paste(output.folder, run,sep="")
  dir.create(output.folder, showWarnings=F)
  
  load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/", an.ID,"forests/",spp,"/",run,".Rdata", sep=""))
  rm(list=c("rf.dat", "split.ac", "split.oc", "tune.c"))
  
  #DATA prep_________________________________________________________________________________
  
  output.folder<-paste(output.folder,"/normalised monthly maps/", sep="")
  dir.create(output.folder, showWarnings=F)
  
  output.folder<-paste(output.folder, spp, "/", sep="")
  dir.create(output.folder, showWarnings=F)
  
  
  output.folder<-paste(output.folder, pr.time, "/", sep="")
  dir.create(output.folder, showWarnings=F)
  
  if(is.null(files)){filenames<-list.files(path=paste(input.folder,
                                                      "normalised", sep=""),all.files = FALSE)}else{
                                                        
                                                        filenames<-paste(files, ".Rdata", sep="")}
  
  
  for (i in 1:length(filenames)){  
    load(paste(input.folder,
               "normalised/", filenames[i],sep=""))
    
    x<-selectX(X.pred, varnames=v)

    try(x<-cbind(time=factor(pr.time, levels=c("day", "night")), x), silent=TRUE)
    

    pred.oc<-try(predict(get(ls()[grep("rfOC", ls())]), newdata=x, type="response"), silent=TRUE)
    if(class(pred.oc)=="try-error"){next}
    pred.oc<-as.numeric(pred.oc)-1
    
    pred.ac<-predict(get(ls()[grep("rfAC", ls())]), newdata=x, type="response")
    pred.ac<-as.numeric(pred.ac)
    
    
    preds<-data.frame(r=X.pred$r, c=X.pred$c, pred.oc=pred.oc, pred.ac=pred.ac)
    
    save(preds,file=paste(output.folder,
                          filenames[i],sep="") )}
  
}


