#Set up
rm(list = ls())

model<-"OC"
analysis<-"size"
spp<-"chel"
y.lim=c(0.5,1)


maxSelect(model="OC",
          analysis="size",
          spp="chel",
          y.lim=c(0.5,1))


maxSelect<-function(model, analysis, spp, y.lim){

        dir.create(paste("/Users/annakrystalli/Documents/Results/NNet chapter/",
                         model, "/summaries",sep=""))
        
        require(png)
        
        name.id<-grep(analysis,c("spp", "v1", "size", "lambda"))
        metric<-c("A" , "P" , "R" , "F1")
        
        setwd(grep(analysis, 
             list.dirs(paste("/Users/annakrystalli/Documents/Results/NNet chapter/", 
                             model, "/",sep="")), value=TRUE))
              files<-grep(spp, dir(), value=TRUE)
              n<-length(grep(spp, dir(), value=TRUE))
             
                             
        r.names<-c("hxv", "hxvp",  "hxva",  "hxvpa")
        
        for (j in 1:4){
                tab<-list(v1=NULL, v2=NULL)
                                     
                c1.names<-NULL
                c2.names<-NULL
                max.nnet<-NULL
                
          
                      for(i in 1:n){
                      
                          load(files[i])
                          ids<-strsplit(files[i], "_")[[1]][c(2, 3)]
                          vid<-grep(ids[1], c("v1", "v2"))
                          
                          tab[[vid]]<-cbind(tab[[vid]], get(ls(pattern=".metrics"))[,j])
                          if(vid==1){c1.names<-c(c1.names,ids[2])}else{c2.names<-c(c2.names,ids[2])}
                        
                          max.nnet<-rbind(max.nnet, as.numeric(cbind(ids[2], vid, max(metrics.t[,4]))))
                      }
          
                if(is.null(c1.names)==F){dimnames(tab[[1]])<-list(r.names, c1.names)}
                if(is.null(c2.names)==F){dimnames(tab[[2]])<-list(r.names, c2.names)}
                assign(paste(metric[j], ".table", sep=""),tab)} 
        
        
        
        
          max.t<-function(x=F1.table){
          return(assign(paste("max.", analysis, sep=""),
                 data.frame(v1=c(colnames(x[[1]])[which(x[[1]]==max(x[[1]]), arr.ind=TRUE)[2]], 
                                 signif(max(x[[1]]), digits=3),
                                 rownames(x[[1]])[which(x[[1]]==max(x[[1]]), arr.ind=TRUE)[1]]),
                            v2=c(colnames(x[[2]])[which(x[[2]]==max(x[[2]]), arr.ind=TRUE)[2]], 
                                 signif(max(x[[2]]), digits=3),
                                  rownames(x[[2]])[which(x[[2]]==max(x[[2]]), arr.ind=TRUE)[1]]))))}
        
          F1.max<-max.t()
        
        
        
        
        
                    v.m<-which(as.matrix(F1.max[2,])==max(as.matrix(F1.max[2,])))
                    r.m<-min(grep(as.character(F1.max[3,v.m]), r.names, fixed=TRUE))
              
              
        png(filename = paste("/Users/annakrystalli/Documents/Results/NNet chapter/",
                                   model, "/summaries/", spp, "." ,analysis,".png", sep=""),
            width = 600, height = 500, units = "px", pointsize = 11)
        
        
                            plot(P.table[[v.m]][r.m,order(as.numeric(colnames(P.table[[v.m]])))]~
                                   as.numeric(colnames(P.table[[v.m]]))[order(as.numeric(colnames(P.table[[v.m]])))], 
                                 type="l", pch=21,bg="lightskyblue3", cex=0.7, col="darkslategrey", lty=3,
                                 ylim=y.lim, xlab=analysis, ylab="F1 Score")
                            
                            
                            points(R.table[[v.m]][r.m,order(as.numeric(colnames(R.table[[v.m]])))]~
                                     as.numeric(colnames(R.table[[v.m]]))[order(as.numeric(colnames(R.table[[v.m]])))], 
                                   type="l", pch=21,bg="lightskyblue4", cex=0.7, col="darkslategrey", lty=4)
                            
                            
                            max.nnet.points<-max.nnet[max.nnet[,2]==max.nnet[which(max.nnet[,3]==max(max.nnet[,3])),2], c(1,3)]
                            max.nnet.points<-max.nnet.points[order(max.nnet.points[,1]),]
                            
                            points(max.nnet.points[,2]~max.nnet.points[,1], 
                                   type="l", bg="lightskyblue", col="lightskyblue2", lwd=1.3)
                            
                            points(F1.table[[v.m]][r.m,order(as.numeric(colnames(F1.table[[v.m]])))]~
                                    as.numeric(colnames(F1.table[[v.m]]))[order(as.numeric(colnames(F1.table[[v.m]])))], 
                                  type="b", pch=21,bg="lightskyblue", cex=0.5, col="darkslategrey", lwd=1.6)
                            
                            legend("topleft", legend=c("F1 - Ensemble", "Precision", "Recall", "F1 - Single network"), lwd=c(1.6, 1,1,1.3),
                                   col=c(rep("darkslategrey", 3),"lightskyblue2" ), lty=c(1,3,4,1), 
                                   pt.bg="lightskyblue", cex=0.7)
        
        dev.off() 
        
        
        
        max.nn<-max.nnet[which(max.nnet[,3]==max(max.nnet[,3])),]
        
        
         
        
        assign(paste(analysis, ".max.out", sep=""), 
               mget(c(ls()[grep(".table", ls())],
                      "max.nnet", "max.nn", "F1.max")))
               
               print(get(paste(analysis, ".max.out", sep="")))
        
               
               
        save(list=paste(analysis, ".max.out", sep=""), 
             file=paste("/Users/annakrystalli/Documents/Results/NNet chapter/", model,
                        "/summaries/",spp,".", analysis, ".max.out.Rdata", sep=""))
               
        setwd("~/")}