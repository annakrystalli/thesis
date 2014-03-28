paramSpaceSearch<-function(nACs=5:3, an.IDCV, an.ID, t.ids, thresh, pt, method, inits, names.spp){
  
  require(png)
  
  source('~/Documents/WORKFLOW/Plotting functions/image scale.R')
  source('~/Documents/WORKFLOW/randomForest/Robs categorical mean calculation/ac integration functions.R', chdir = TRUE)
  load(file="~/Documents/CPR/DATA/RAW DATA/Associated files/validation indices.RData")
  load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                  an.IDCV,"1/forests/thresholds.Rdata", sep=""))
  
  breaks <- read.csv("~/Documents/TRAINING DATA/Models/randomForest/WinCPR validation/breaks.csv")
  BKS<-list(AC5=breaks[-1,1], AC4=breaks[c(-1,-3),1],AC3=breaks[c(-1,-3,-4),1])
  AC.IDS<-list(AC5=1:5, AC4=c(1,1,2:4),AC3=c(1,1,1,2,3))
  
  
  dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",
                   an.ID,"integration plots/", sep=""),
             showWarnings = F)
  dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",
                   an.ID,"integration plots/",method,"/", sep=""),
             showWarnings = F)
  
  dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",
                   an.ID,"integration data/", sep=""),
             showWarnings = F)
  
  dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",
                   an.ID,"integration data/",method,"/",  sep=""),
             showWarnings = F)
  
  for(nAC in nACs){
    
    bks<-BKS[[paste("AC",nAC, sep="")]]
    ACids<-AC.IDS[[paste("AC",nAC, sep="")]]
    
    for(spp in names.spp){
      
      if(spp %in% c("tem", "centrot")){k=2;f=50}else{k=1; f=1}
      
      
      dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",
                       an.ID,"integration plots/", method,"/", spp, "/", sep=""),
                 showWarnings = F)
      
      dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",
                       an.ID,"integration data/", method,"/",spp, "/", sep=""),
                 showWarnings = F)  
      
      
      for(id in t.ids){
        
        load(paste("/Users/annakrystalli/Documents/PREDICTED/", 
                   an.ID,"normalised monthly maps/",spp,"/", 
                   id, ".RData", sep=""))
        
        # Convert probabilities to threshold dependend presence / absence
        if(any(preds$pred.oc<0)){preds$pred.oc<-preds$pred.oc+1}
        preds$pred.oc<-as.numeric(preds$pred.oc>=THRESH.spp[[spp]][thresh])
        
        
        x<-factor(ACids[factor(preds$pred.ac[preds$pred.oc==1], 
                               levels = c(1:5))], levels = c(1:nAC))
        

        
        Means<-NULL
        J<-NULL
        Probs<-NULL
        MeansG<-NULL
        Jg<-NULL
        ProbsG<-NULL
        InitG<-NULL
        InitN<-NULL
        
        for (i in 1:dim(inits)[1]){
          
          try(opt<-optlnormFun(table(x),
                           bks*f, inits[i,], method), silent=T)
          
          try(optg<-optGammaFun(table(x),
                            log(bks*f), inits[i,], method), silent=T)
          
          if(class(opt)=="try-error"){ 
            Means<-c(Means,NA)
            J<-c(J,NA)
            Probs<-c(Probs,NA)}else{
                                    Means<-c(Means,opt$mean)
                                    J<-c(J,opt$J)
                                    Probs<-c(Probs,prod(opt$probs))}
          
          if(class(opt)=="try-error"){ 
          MeansG<-c(MeansG,NA)
          Jg<-c(Jg,NA)
          ProbsG<-c(ProbsG, NA)}else{
                                      MeansG<-c(MeansG,exp(optg$mean+0.5*optg$var))
                                      Jg<-c(Jg,optg$J)
                                      ProbsG<-c(ProbsG, prod(optg$probs))}
          }
          
          
        
        write.csv(data.frame(inits, Means, J, Probs, MeansG, Jg, ProbsG),
                  paste("~/Documents/TRAINING DATA/Models/randomForest/",
                        an.ID,"integration data/", method,"/", spp, "/",id, 
                        " nAC =", nAC,".csv", sep=""))
        
        
        
        png(filename = paste("~/Documents/TRAINING DATA/Models/randomForest/",
                             an.ID,"integration plots/", method,"/", spp,"/nAC",
                             nAC," ",id,thresh,".png", 
                             sep=""), 
            width=11.7, height=8.3, units = "in", res = 72, pointsize=pt)
        
        
        layout(matrix(c(1,1,8,8,2,4,9,11,3,5,10,12,6,7,13,14), 4, 4, byrow=T), heights=c(1.7,3,0.5, 2.3))
        
        par(mar=c(1,4,3,0.5), mgp=c(1.8, 0.7, 0))
        
        col=rev(gray.colors(1000, 0.2, 0.7))
        
        #.....lnorm...............................................................................  
        hist(preds$pred.ac[preds$pred.oc==1], breaks=0:5, col="black", border="white",
             main=id, xlab="")
        #...........   
        
        par(mar=c(4,4,3,0.5), mgp=c(1.8, 0.7, 0))
        z<-matrix(J, length(unique(inits$a)), length(unique(inits$a)), byrow=T, 
                  dimnames=list(paste("a", sort(unique(inits$a))) ,
                                paste("b", sort(unique(inits$b)) )))
        
        
  
      
          zlim<-findOutlierZ(z)
        
          image.nan(z, zlim, inits, col=col, na.color='white', 
                               outside.below.color='blue', outside.above.color='red',
                     main="lnorm J")

        #...........  
        
        z<-matrix(abs(Means-median(Means)),length(unique(inits$a)), 
                  length(unique(inits$a)), byrow=T, 
                  dimnames=list(paste("a", sort(unique(inits$a))) ,
                                paste("b", sort(unique(inits$b)) )))
        
        z[is.infinite(z)]<-max(z[!is.infinite(z)])
        
        zlim<-findOutlierZ(z)
        
        image.nan(z, zlim, inits, col=col, na.color='white', 
                  outside.below.color='blue', outside.above.color='red',
                  main="dev from median J")
        
        #...........  
        par(mar=c(4,4,3,0.5), mgp=c(1.8, 0.7, 0)) 
        plot(log(Means)~J, pch=21, bg=grey(0.2, 0.5), col=grey(0.2, 0.5),
             main=paste("median =", signif(median(Means),3),
                        "mean =", signif(mean(Means),3),
                        "\ninit = a:",inits[which(abs(Means-median(Means[!is.infinite(Means)]))==min(abs(Means-median(Means[!is.infinite(Means)]))))[1],1],
                        "/", inits[which(abs(Means-mean(Means[!is.infinite(Means)]))==min(abs(Means-mean(Means[!is.infinite(Means)]))))[1],1],
                        "  b:",inits[which(abs(Means-median(Means[!is.infinite(Means)]))==min(abs(Means-median(Means[!is.infinite(Means)]))))[1],2],
                        "/", inits[which(abs(Means-mean(Means[!is.infinite(Means)]))==min(abs(Means-mean(Means[!is.infinite(Means)]))))[1],2]), cex.main=1)
        
        abline(h=median(log(Means)), lty=2, lwd=1.5)
        
        MeansNA<-Means[is.finite(Probs)]
        
        if(length(MeansNA)==0){plot.new()}else{
        boxplot(MeansNA~factor(rep(1, l=length(MeansNA))), outline=F,
                xlab="w/o outliers & prob=NA",
                main=paste("median =", signif(median(MeansNA),3),
                                "mean =", signif(mean(MeansNA[!is.infinite(MeansNA)]),3),
                                "\ninit = a:",inits[which(MeansNA==median(MeansNA))[1],1],
                                "/", inits[which(abs(MeansNA-mean(MeansNA[!is.infinite(MeansNA)]))==min(abs(MeansNA-mean(MeansNA[!is.infinite(MeansNA)]))))[1],1],
                                "  b:",inits[which(MeansNA==median(MeansNA))[1],2],
                                "/", inits[which(abs(MeansNA-mean(MeansNA[!is.infinite(MeansNA)]))==min(abs(MeansNA-mean(MeansNA[!is.infinite(MeansNA)]))))[1],2]),
                cex.main=1)}
        
        #.....Gamma......................................................... 
        par(mar=c(1,4,3,0.5), mgp=c(1.8, 0.7, 0))
        hist(preds$pred.ac[preds$pred.oc==1], breaks=0:5, col="black", border="white",
             main=paste(spp, "nAC =", nAC), xlab="")
        #...........
        z<-matrix(Jg, length(unique(inits$a)), 
                  length(unique(inits$a)), byrow=T, 
                  dimnames=list(paste("a", sort(unique(inits$a))) ,
                                paste("b", sort(unique(inits$b)) )))
        
        zlim<-findOutlierZ(z)
        
        image.nan(z, zlim, inits, col=col, na.color='white', 
                  outside.below.color='blue', outside.above.color='red',
                  main="lgamma J")
        
        #...........   

        z<-matrix(abs(MeansG-median(MeansG)),length(unique(inits$a)), 
                  length(unique(inits$a)), byrow=T, 
                  dimnames=list(paste("a", sort(unique(inits$a))) ,
                                paste("b", sort(unique(inits$b)) )))
        
        z[is.infinite(z)]<-max(z[!is.infinite(z)])
        
        zlim<-findOutlierZ(z)
        
        image.nan(z, zlim, inits, col=col, na.color='white', 
                  outside.below.color='blue', outside.above.color='red',
                  main="dev from median J")
        #...........   
        par(mar=c(4,4,3,0.5), mgp=c(1.8, 0.7, 0)) 
        plot(log(MeansG)~Jg, pch=21, bg=grey(0.2, 0.5), col=grey(0.2, 0.5),
             main=paste("median =", signif(median(MeansG[!is.infinite(MeansG)]),3),
                        "mean =", signif(mean(MeansG[!is.infinite(MeansG)]),3),
                        "\ninit = a:",inits[which(abs(MeansG-median(MeansG[!is.infinite(MeansG)]))==min(abs(MeansG-median(MeansG[!is.infinite(MeansG)]))))[1],1],
                        "/", inits[which(abs(MeansG-mean(MeansG[!is.infinite(MeansG)]))==min(abs(MeansG-mean(MeansG[!is.infinite(MeansG)]))))[1],1],
                        "  b:",inits[which(abs(MeansG-median(MeansG[!is.infinite(MeansG)]))==min(abs(MeansG-median(MeansG[!is.infinite(MeansG)]))))[1],2],
                        "/", inits[which(abs(MeansG-mean(MeansG[!is.infinite(MeansG)]))==min(abs(MeansG-mean(MeansG[!is.infinite(MeansG)]))))[1],2]), cex.main=1)
        
        abline(h=median(log(MeansG)), lty=2, lwd=1.5)
        
        MeansGNA<-MeansG[is.finite(ProbsG)]
        
        if(length(MeansGNA)==0){plot.new()}else{
            boxplot(MeansGNA~factor(rep(1, l=length(MeansGNA))), outline=F,
                xlab="w/o outliers & prob=NA", main=paste("median =", signif(median(MeansGNA),3),
                                                          "mean =", signif(mean(MeansGNA[!is.infinite(MeansGNA)]),3),
                                                          "\ninit = a:",inits[which(MeansGNA==median(MeansGNA))[1],1],
                                                          "/", inits[which(abs(MeansGNA-mean(MeansGNA[!is.infinite(MeansGNA)]))==min(abs(MeansGNA-mean(MeansGNA[!is.infinite(MeansGNA)]))))[1],1],
                                                          "  b:",inits[which(MeansGNA==median(MeansGNA))[1],2],
                                                          "/", inits[which(abs(MeansGNA-mean(MeansGNA[!is.infinite(MeansGNA)]))==min(abs(MeansGNA-mean(MeansGNA[!is.infinite(MeansGNA)]))))[1],2]),
                cex.main=1)}
        
        dev.off()
      }
    }
  }
}

#__________________________________________________________________________________

image.nan <- function(z, zlim, inits, col=rev(gray.colors(1000, 0.3, 0.85)), na.color='white', 
                      outside.below.color='blue', outside.above.color='red',
                      main="lnorm log median dev",...)
{
  
  zstep <- (zlim[2] - zlim[1]) / length(col); # step in the color palette
  newz.below.outside <- zlim[1] - zstep # new z for values below zlim
  newz.above.outside <- zlim[2] + zstep # new z for values above zlim
  newz.na <- zlim[2] + 2 * zstep # new z for NA
  
  if(any(z<zlim[1])){
  z[which(z<zlim[1])] <- newz.below.outside
  col <- c(outside.below.color, col)
  zlim[1] <- zlim[1] - zstep} 
  
  if(any(z>zlim[2])){
  z[which(z>zlim[2])] <- newz.above.outside # we affect newz.above.outside
  col <- c(col, outside.above.color)
  zlim[2] <- zlim[2] + 2 * zstep}
  
  if(any(!is.finite(z))){
  z[which(!is.finite(z))] <- newz.na # same for newz.na
  col <- c(col, na.color)}
  
  
  par(mar=c(4,4,3,0.5), mgp=c(1.8, 0.7, 0))
  image(z,  x=sort(unique(inits$b)), y=sort(unique(inits$a)), 
        ylab="a", xlab="b", col=col, main=main)
  
  par(mar=c(2,4,0.2,0.5), mgp=c(1.8, 0.7, 0)) 
  image.scale(z=z, col = col,
              axis.pos=1, add.axis=TRUE)}

#__________________________________________________________________________________


findOutlierZ<-function(z){
  
          Q<-quantile(z[is.finite(z)])
          zlim<-c(Q[2]-(1.5 * (Q[4]-Q[2])), Q[4]+(1.5 * (Q[4]-Q[2])))}


#__________________________________________________________________________________

compileParamSearch<-function(names.spp, nACs, t.ids, method, an.ID){
  
  tab<-NULL  
  MedDev<-function(x){sqrt(mean((x-median(x[is.finite(x)]))^2))/median(x[is.finite(x)])}
  
  
  for(spp in names.spp){
    
    for(nAC in nACs){
      
      for(id in t.ids){
        
        
        dat<-read.csv(paste("~/Documents/TRAINING DATA/Models/randomForest/",
                            an.ID,"integration data/", method,"/", spp, "/",id, 
                            " nAC =", nAC,".csv", sep=""))
  
        
        dat<-data.frame(dat, nAC=nAC, spp=spp, id=id, 
                        sdJ=MedDev(dat$J), sdM=MedDev(dat$Means), 
                        sdJg=MedDev(dat$Jg), sdMg=MedDev(dat$MeansG),
                        p.id=paste(dat$a, "-", dat$b, sep=""))
        tab<-data.frame(rbind(tab, dat))}}}
  
  write.csv(tab, paste("~/Documents/TRAINING DATA/Models/randomForest/",
                       an.ID,"integration data/", method,"/ParamSearch data.csv", sep=""))

  return(tab)
}




makeTransparent<-function(someColor, alpha=100)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}