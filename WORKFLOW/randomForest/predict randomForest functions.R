require(randomForest)


varnames<-c("year", "time",
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
                      pr.time){

  dir.create(output.folder, showWarnings=F)
  
load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/", an.ID,"forests/",spp,"/OCforest.Rdata", sep=""))
load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/", an.ID,"forests/",spp,"/ACforest.Rdata", sep=""))

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
  
  x<-try(cbind(time=factor(pr.time, levels=c("day", "night")),
           X.pred[, setdiff(v, "time")]), silent=TRUE)
  
  pred.oc<-try(predict(rforOC, newdata=x, type="prob")[,2], silent=TRUE)
    if(class(pred.oc)=="try-error"){next}
  pred.oc<-as.numeric(pred.oc)-1
  
  pred.ac<-predict(rforAC, newdata=x, type="response")
  pred.ac<-as.numeric(pred.ac)
  
  
  preds<-data.frame(r=X.pred$r, c=X.pred$c, pred.oc=pred.oc, pred.ac=pred.ac)
  
      save(preds,file=paste(output.folder,
                               filenames[i],sep="") )}
  
}


  