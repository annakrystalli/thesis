require(maps)
require(raster)
require(png)
require(nnet)


source('~/Documents/WORKFLOW/randomForest/Robs categorical mean calculation/ac integration functions.R', chdir = TRUE)


unNormalise<-function(x, var.id){
  
  load(file="~/Documents/SATELLITE/Associated data/r files/min.max.ind pre processing file.RData")
  
  for(var in var.id){
    x.v<-x[,var]
    min<-as.numeric(min.max.tab$min[min.max.tab$var==var])
    max<-as.numeric(min.max.tab$max[min.max.tab$var==var])
    x[,var]<-min+(x.v*(max-min))}
  
  return(x)}

resMatrix<-function(res.tab){
  as.matrix(cbind(apply(res.tab[complete.cases(res.tab),], 
                        FUN=function(x , ...){sum(x==0)/length(x)}, 2),
                  apply(res.tab[complete.cases(res.tab),], 
                        FUN=function(x , ...){sum(x^2)/length(x)}, 2)),
            byrow=T, dimnames=list(names(res.tab), NULL))}

plotMaps<-function(x, cats=5, fix.cols=T, BrCol){
  if(fix.cols){
            cols <- rev(brewer.pal(cats, name=BrCol))
            cols <- c("#E0EEEE", cols)
            cols <- cols[1:(cellStats(x, stat='max')+1)] 
        plot(x, col=cols, axis.args=list(at=unique(x), 
                                         labels=unique(x)))}else{
                                           
        plot(log(x))}
}

plotValMaps <- function(an.ID, y.m, x.dat, id, w.spp, spp, pt1=12, pt2=14, 
                        pred.map, pr.map, map.med, map.int.ac, map.int.con, map.win,
                        BrCol="YlGnBu"){
  
        dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",
                         an.ID,"validation plots/", sep=""),
                   showWarnings = F)
        dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",
                         an.ID,"validation plots/",spp, "/", sep=""),
                   showWarnings = F)
        
            y<-y.m[x.dat$m.id==id,w.spp]
            x<-x.dat[x.dat$m.id==id, c("lon", "lat") ]
            
    png(filename = paste("~/Documents/TRAINING DATA/Models/randomForest/",
                       an.ID,"validation plots/", spp,"/",thresh,id, " val hist.png", 
                       sep=""), 
        width=8.3, height=11.7, units = "in", res = 300, pointsize=pt1)
  
        par(mfrow=c(2,1), las=1, cex.axis=0.7)
  
            hist(y, breaks=0:6, col="black", border="white",axes=F, 
                 right=F )
                axis(1, at=(0:5)+0.5, 0:5)
                axis(2)
            hist(pred.map, breaks=0:6, col="black", border="white",axes=F, 
                 right=F )
                axis(1, at=(0:5)+0.5, 0:5)
                axis(2)
            
        dev.off()
  

  
    png(filename = paste("~/Documents/TRAINING DATA/Models/randomForest/",
                         an.ID,"validation plots/", spp,"/",thresh,id, " val maps.png", 
                         sep=""), 
        width=11.7, height=8.3, units = "in", res = 300, pointsize=pt2)
    
        par(mfrow=c(2,3), las=1, cex.axis=0.7, mar=c(2, 2, 0.2, 0.2))
  
           cols <- rev(brewer.pal(5, name=BrCol))
           cols <- c("#E0EEEE", cols)
           y.cols<-cols[factor(y, levels=0:5)]      
        
              map("world", xlim=c(-4,11), 
                  ylim=c(51,61), fill=TRUE,
                  resolution=0)
              points(x, col=y.cols, pch=21, cex=0.1)
              axis(2, seq(51,61, l=diff(range(c(51,61)))+1), font=2)
              axis(1, seq(-4,11, l=diff(range(c(-4,11)))+1), font=2)
              
              
              plotMaps(pr.map, cats=5, fix.cols=T, BrCol="YlGnBu")
              map("world", xlim=c(-4,11), 
                  ylim=c(51,61), fill=TRUE, col="white",
                  resolution=0, add=T)
              
              plotMaps(map.med, cats=5, fix.cols=T)
              map("world", xlim=c(-4,11), 
                  ylim=c(51,61), fill=TRUE, col="white",
                  resolution=0, add=T)
              
              plotMaps(map.win, cats=5, fix.cols=T)
              map("world", xlim=c(-4,11), 
                  ylim=c(51,61), fill=TRUE, col="white",
                  resolution=0, add=T)
              
              plotMaps(map.int.con, cats=5, fix.cols=F)
              map("world", xlim=c(-4,11), 
                  ylim=c(51,61), fill=TRUE, col="white",
                  resolution=0, add=T)
              
              plotMaps(map.int.ac, cats=5, fix.cols=T)
              map("world", xlim=c(-4,11), 
                  ylim=c(51,61), fill=TRUE, col="white",
                  resolution=0, add=T)
              
        dev.off()
  
}



winCPRValidate<- function(an.ID, an.IDCV, cv.no=5, names.spp, data=" mod",
                          model="rf_i", plot.id=F, thresh, inits, pr.time){

        varnames<-c("year", "time", "lat", "lon",
                    "month",  "T.m", "Sed.m", 
                    "Sedf.m",  "Ch.m", "fdens.m", 
                    "fdist.m",  "fside.m",  "fdens.c.m", 
                    "fdist.c.m",  "fside.c.m", 
                    "bath", "NAO", "wNAO")
      
      # Species data
        all.spp<-c("tem", "centrot", "cfin", "chel", "metrilu")
        wCPR.spp<-c("tem","centrot","calfin", "calhel", "metrilu")
        tv<-c("tem", "centrot")
        ec<-c("chel", "cfin", "metrilu")
        
      # Load data #.......................................
        # Prepare accepted value coded raw data
        load(file="/Users/annakrystalli/documents/TRAINING DATA/y training data.RData")
            av <- read.csv("~/Documents/CPR/DATA/RAW DATA/Accepted values.csv")
                av.conv<-rbind(0, av)
                    for(spps in wCPR.spp){
                      y.m[,spps]<-av.conv[,if(spps%in% c("tem", "centrot")){3}else{2}][y.m[,spps]+1]
                    }
                y.av<-y.m
        
        load(file=paste("/Users/annakrystalli/documents/TRAINING DATA/y ", 
                        data, "training data.RData", sep=""))
        load("~/Documents/TRAINING DATA/normalized (min.max) x training data.RData")
        load(file="~/Documents/CPR/DATA/RAW DATA/Associated files/validation indices.RData")
        load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                        an.IDCV,"1/forests/thresholds.Rdata", sep=""))
        
        
        dir.create(path=paste("~/Documents/PREDICTED/",an.ID,"AORS/", 
                              sep=""), showWarnings = F)
        
        dir.create(path=paste("~/Documents/PREDICTED/",an.ID,"AORS/data", 
                              sep=""), showWarnings = F)
      
        dir.create(path=paste("~/Documents/PREDICTED/",an.ID,"AORS/data/aggregated maps", 
                              sep=""), showWarnings = F)
      # Data prep  #.......................................
          # X & y
              norm.x.m$time[norm.x.m$time>=0.25 & norm.x.m$time<0.75]<-"day"
              norm.x.m$time[norm.x.m$time!="day"]<-"night"
              norm.x.m$time<-as.factor(norm.x.m$time)
              
              all.samples<-paste(norm.x.m$tow,norm.x.m$sample.no., sep="-")
              
              x.dat<-data.frame(spl.id=sort(all.samples), 
                                norm.x.m[order(all.samples),])
              x.dat<-unNormalise(x.dat, varnames[varnames!="time"])
              x.dat$m.id <- paste(x.dat$year, "-",x.dat$month, "-", sep="")
        
              y.m<-y.m[order(all.samples),]
              y.av<-y.av[order(all.samples),]
              y.time<-x.dat$time
      
          rm(norm.x.m)
          

        
      # WinCPR Aux data
        winCPR.grid<-read.csv("~/Documents/CPR/DATA/RAW DATA/Associated files/north sea pixel grid.csv", header=F)
        breaks <- read.csv("~/Documents/TRAINING DATA/Models/randomForest/WinCPR validation/breaks.csv")
        bks.int<-breaks[-1,]
        breaks.win<-breaks
        breaks.win[2,]<-breaks[2,]*0.5
        bks.ag<-c(1, 51, 501, 4000)
      #.......................................

      # Raster data
        map.ext<-extent(c(-4,11,51,61))
        map.res <- raster(nrow=1112, ncol=926, ext=map.ext)

      #   
        val.DATA<-vector( "list", length(names.spp))
        val.SUM<-vector( "list", length(names.spp))
        names(val.DATA)<-names.spp
        names(val.SUM)<-names.spp

        
      # Clear vectors
        clear1<-c("all.samples", 
               "pr.ac", "pr.cv.ac", "pr.cv.oc", 
               "rf.dat", "rfAC", "rfOC", 
               "tune.i", "x.ind.ac", "x.ind.oc", "y.cv.ac", 
               "y.cv.oc")
        
for(spp in names.spp){

    # spp id set ups
      spp.id <-  which(all.spp %in% spp)
      if(spp %in% c("tem", "centrot")){k=2;f=50}else{k=1; f=1}
      w.spp <- wCPR.spp[spp.id]
  
      dir.create(path=paste("~/Documents/PREDICTED/",an.ID,"AORS/data/aggregated maps/",spp, 
                            sep=""), showWarnings = F)
      
# Compile cross-validation data       #....................................... 
      y.cv.oc<-NULL
      pr.cv.oc<-NULL
      x.ind.oc<-NULL
      y.cv.ac<-NULL
      pr.cv.ac<-NULL
      x.ind.ac<-NULL
      
      for(i in 1:cv.no){
        
        load(paste("~/Documents/TRAINING DATA/Models/randomForest/",
                   an.IDCV, i,"/forests/",spp,"/",model,".Rdata", sep=""))
        load(paste("~/Documents/TRAINING DATA/Models/randomForest/",
                   an.IDCV, i,"/forests/",spp,"/results.Rdata", sep=""))
        
        
        y.cv.oc<-c(y.cv.oc, res[[model]][["rf.p"]]$OC$y.er)
        y.cv.ac<-c(y.cv.ac, res[[model]][["rf.p"]]$AC$y.er)
        
        pr.cv.oc<-c(pr.cv.oc, res[[model]][["rf.p"]]$OC$probs[,2])
        pr.cv.ac<-c(pr.cv.ac, res[[model]][["rf.p"]]$AC$pr)
        
        x.ind.oc<-c(x.ind.oc, rf.dat$test.ind)
        x.ind.ac<-c(x.ind.ac, rf.dat$test.indAC)}
      
        pr.cv.oc<-as.numeric(pr.cv.oc>=THRESH.spp[[spp]][thresh])
        y.cv<-y.cv.oc
        y.cv[match(x.ind.ac, x.ind.oc)]<-y.cv.ac
        
        pr.ac<-rep(0, length(pr.cv.oc))
        pr.ac[match(x.ind.ac, x.ind.oc)]<-pr.cv.ac
        pr.extrct.id<-which(pr.cv.oc==1 & pr.ac==0, arr.ind=T)
        pr.cv <- pr.cv.oc * pr.ac
      
        x.ind<-x.ind.oc
      
        rm(list=clear1)
      # Set up validation table       #.......................................
      
        val.dat<-x.dat[x.ind,c("lon","lat","tow","sample.no.","year","month")]  
        val.dat$lon<-as.numeric(format(val.dat$lon, digits = 4))
        val.dat$lat<-as.numeric(format(val.dat$lat, digits = 8))
  
        val.index<-val.index[val.index$m.index %in% unique(val.index$m.index)[1:40],]
        val.index$lon<-as.numeric(format(val.index$lon, digits = 4))
        val.index$lat<-as.numeric(format(val.index$lat, digits = 8))  

        val.table<-cbind(val.dat,
                         val.index[match(paste(val.dat$tow, 
                                               val.dat$lon, val.dat$lat), 
                                         paste(val.index$tow, val.index$lon, 
                                               val.index$lat)),
                                 c("r.ind", "c.ind", "m.index")], 
                         y.cv,  pr.cv, x.ind, row.id=1:dim(val.dat)[1])
            
            val.table<-val.table[complete.cases(val.table),]
            val.table$rf.y<-NA
            val.table$rf.res<-NA
            val.table$win.y<-NA
            val.table$win.res<-NA    
            val.table$rf.int.y<-NA
            val.table$rf.int.res<-NA
            val.table$rf.med.y<-NA
            val.table$rf.med.res<-NA
      
   
      #.......................................      
      


    
    # Load spp WinCPR data & process
      wCPR.dat <- read.csv(paste("~/Documents/CPR/DATA/WinCPR/WinCSV/",
                              w.spp,".csv", sep=""), header=T)


      wCPR.dat<-wCPR.dat[wCPR.dat$Year %in% 1997:2001,]
      wCPR.dat[grep("X", names(wCPR.dat))]<-(10^wCPR.dat[grep("X", names(wCPR.dat))])-1
      wCPR.dat[wCPR.dat<0]<-NA
      wCPR.dat$id<-paste(wCPR.dat$Year, "-", wCPR.dat$Month, "-", sep="")
  

            # Create AOR vars
            
            oc.y<-NULL
            oc.rf<-NULL
            oc.win<-NULL
            oc.rfy<-NULL
            AB.yav<-NULL
            AB.yint<-NULL
            AB.rf<-NULL
            AB.rfy<-NULL
            AB.win<-NULL
            N.y<-NULL
            N.rf<-NULL
            N.win<-NULL
            yr<-NULL
            mm<-NULL
            nAC.yint<-NULL
            nAC.rf<-NULL
            nAC.rfy<-NULL

for(id in unique(val.table$m.index)){
          
      #WinCPR map prep........
        
        # Convert to ACs
        winAC<-cut(as.numeric(wCPR.dat[wCPR.dat$id == id,4:dim(wCPR.dat)[2]]),
            breaks=c(-1,breaks.win[-1,k]), right=T, labels=0:5)
        
        # Map onto grid
        winACgrid<-matrix(as.numeric(winAC[as.matrix(winCPR.grid)])-1, 22, 13)
        
        # Make raster
        map<-raster(winACgrid, -4,9,51,61, crs="+proj=longlat +datum=WGS84")
        
        #...Crop to size
        map<-crop(map, y=map.ext)
        
        #...Resample
        map.win <- resample(map, map.res, method="ngb")


  # PREDICTED processing # ..........................................
        
      # Load pred data and map onto grid  
        load(paste("/Users/annakrystalli/Documents/PREDICTED/", 
                            an.ID,"normalised monthly maps/",spp,"/", 
                            pr.time,"/",id, ".RData", sep=""))
        
      # Convert probabilities to threshold dependend presence / absence
        if(any(preds$pred.oc<0)){preds$pred.oc<-preds$pred.oc+1}
        preds$pred.oc<-as.numeric(preds$pred.oc>=THRESH.spp[[spp]][thresh])
        
        pred.map<-matrix(NA, ncol=926, nrow=1112) 
        

          pred.map[cbind(preds$r,preds$c)]<-0
          pred.map[cbind(preds[preds$pred.oc==1,"r"], 
                         preds[preds$pred.oc==1,"c"])]<-preds[preds$pred.oc==1,
                                                              "pred.ac"]
         
         pr.map<-raster(pred.map, -4,11,51,61, crs="+proj=longlat +datum=WGS84")
         
      # AOR data calculation................................................. 

        
        # Compile...............
            #...oc....      
                  N.rf<-c(N.rf, dim(preds)[[1]])
                  oc.rf<-c(oc.rf,sum(preds$pred.oc)/dim(preds)[[1]])
        
            #...lognormal integrated mean....  
              op<-initSearchOpt(preds, inits, breaks, spp)
              AB.rf<-c(AB.rf, op$mean)
              nAC.rf<-c(nAC.rf, op$nAC)

            #...winCPR mean....
              win.dat<-na.omit(as.numeric(wCPR.dat[wCPR.dat$id == id,4:dim(wCPR.dat)[2]]))
              win.dat[win.dat<0.5*f]<-0
              AB.win<-c(AB.win, mean(win.dat[win.dat!=0]))
              N.win<-c(N.win, length(win.dat))
              oc.win<-c(oc.win, sum(win.dat[is.finite(win.dat)]!=0)/sum(is.finite(win.dat)))
        
            #...raw data....
              N.y<-c(N.y,length(y.av[x.dat$m.id==id, w.spp]))          
              y.dat<-y.m[x.dat$m.id==id, w.spp][y.m[x.dat$m.id==id, w.spp]!=0]          
              AB.yav<-c(AB.yav, mean(y.av[x.dat$m.id==id, w.spp][y.av[x.dat$m.id==id, w.spp]!=0]))
           

              op<-initSearchOpt(y.dat, inits, breaks, spp)
              AB.yint<-c(AB.yint, op$mean)
              nAC.yint<-c(nAC.yint, op$nAC)

              oc.y<-c(oc.y, length(y.dat)/length(y.av[x.dat$m.id==id, w.spp]))

              rfy.y<-pr.map[cbind(val.table[val.table$m.index==id, "r.ind"], 
                                    val.table[val.table$m.index==id, "c.ind"])]
              op<-initSearchOpt(rfy.y[rfy.y>0], inits, breaks, spp)
              AB.rfy<-c(AB.rfy, op$mean)
              nAC.rfy<-c(nAC.rfy, op$nAC) 
              oc.rfy<-c(oc.rfy, sum(rfy.y[is.finite(rfy.y)]!=0)/sum(is.finite(rfy.y)))

            #...Auxilliary data....
              mm<-c(mm, as.numeric(strsplit(id, "-")[[1]][2]))
              yr<-c(yr, as.numeric(strsplit(id, "-")[[1]][1]))
              
       
      # AGGREGATE pred map
          
        # using integrated gamma function
          # continuous abundance
          map.int.con <- aggregate(pr.map, fact=c(71 , 50), 
                              fun=function(x, ...){if(all(is.na(x))){return(NA)}else{
                                              x.ac<-na.omit(x[x>0])
                                              n.ac<-length(x.ac)
                                              if(n.ac==0){return(0)}else{
                                              n<-length(na.omit(x))
                                              (n.ac/n)*initSearchOpt(y.dat, inits, breaks, spp)$mean}}}, 
                              expand=F)



          # AC abundance
          map.int.ac <- cut(map.int.con,
                         breaks=c(-10,breaks[-1,k]), right=T, labels=0:5)-1
          
       # using median      
          map.med <- aggregate(pr.map, fact=c(71 , 50), 
                               fun=median, 
                               expand=F)
          
      save(map.med, file=paste("~/Documents/PREDICTED/",an.ID,"AORS/data/aggregated maps/", spp,"/", 
                                   id, "medAGG.RData", sep=""))
      
          map.res.red <- raster(nrow=1100, ncol=923, ext=extent(map.win))
 
        
          map.win <- resample(map.win, map.res, method="ngb")
          map.int.ac <- resample(map.int.ac, map.res, method="ngb")
          map.med <- resample(map.med, map.res, method="ngb")

      save(map.med, file=paste("~/Documents/PREDICTED/",an.ID,"AORS/data/aggregated maps/", spp,"/", 
                               id, "medHR.RData", sep=""))
      
          if(id %in% plot.id){plotValMaps(an.ID, y.m, x.dat, id, 
                      w.spp, spp, pt1=12, pt2=14, 
                      pred.map, pr.map, map.med, 
                      map.int.ac, map.int.con, map.win)}
        
          val.table[val.table$m.index==id, "rf.y"]<-pr.map[cbind(val.table[val.table$m.index==id, "r.ind"], 
              val.table[val.table$m.index==id, "c.ind"])]
        
          val.table[val.table$m.index==id, "win.y"]<-map.win[cbind(val.table[val.table$m.index==id, "r.ind"], 
              val.table[val.table$m.index==id, "c.ind"])]
        
          val.table[val.table$m.index==id, "rf.int.y"]<-map.int.ac[cbind(val.table[val.table$m.index==id, "r.ind"], 
              val.table[val.table$m.index==id, "c.ind"])]
          
          val.table[val.table$m.index==id, "rf.med.y"]<-map.med[cbind(val.table[val.table$m.index==id, "r.ind"], 
              val.table[val.table$m.index==id, "c.ind"])]}
        
        val.table$pr.cv[na.omit(match(pr.extrct.id, val.table$row.id))]<-val.table$rf.y[na.omit(match(pr.extrct.id, val.table$row.id))]
        val.table$pr.cv.res<-val.table$pr.cv-val.table$y.cv
        val.table$rf.res<-val.table$rf.y-val.table$y.cv
        val.table$rf.int.res<-val.table$rf.int.y-val.table$y.cv
        val.table$rf.med.res<-val.table$rf.med.y-val.table$y.cv
        val.table$win.res<-val.table$win.y-val.table$y.cv
        
        
       val.DATA[[spp]]<-val.table

      AOR.data<-cbind(oc.y, oc.rf, oc.win, AB.yav, AB.yint, nAC.yint,
                      AB.rf, nAC.rf, AB.win, AB.rfy, oc.rfy, nAC.rfy,
                      N.y, N.rf, N.win, yr, mm)
      
      
      save(AOR.data,file=paste("~/Documents/PREDICTED/", an.ID,"AORS/data/",
                               thresh, spp," monthly AOR.Rdata",sep=""))}
 
        save(val.DATA, file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                                  an.ID, thresh,"validation data.RData", sep=""))

       return(val.DATA)}

        
         
        
extractVALres<-function(mod.out){
  v<-vector(length=4)
  
  options(scipen=999)
  
  v[1]<-round(mod.out$Acc$overall[1],3)  
  v[2]<-mod.out$k$weighted.kappa
  v[3]<-mod.out$F$average[4]
  v[4]<-mod.out$TSS$mean 
  
  return(v)}    
        

measureVALPerformance<-function(y, pr){
  
  require(nnet)
  require(PerfMeas)
  require(verification)
  require(caret)
  require(psych)
  require(randomForest)
  require(PresenceAbsence)
  require(caTools)
  
  
    Acc<-confusionMatrix(factor(pr, levels=0:5), factor(y, levels=0:5))
    F<-F.measure.single.over.classes(class.ind(factor(y, levels=0:5)), 
                                     class.ind(factor(pr, levels=0:5)))
    
    k<-cohen.kappa(x=cbind(y, pr))
  
    mean.Acc<-colMeans(Acc$byClass, na.rm=T)
    TSS<-list(class=Acc$byClass[,1] + Acc$byClass[,2] - 1, mean=mean.Acc[1] + mean.Acc[2] - 1)

    return(list(Acc=Acc, mean.Acc=mean.Acc, F=F, k=k, TSS=TSS))
                             
}       
        


summariseVal<-function(thresh, names.spp, an.ID){
  
  val.SUMMARY<-vector("list", length(names.spp))
  names(val.SUMMARY)<-names.spp
  
  load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                  an.ID, thresh, "validation data.RData", sep=""))
  
  for(spp in names.spp){
    
    prs<-c(names(val.DATA[[spp]])[grep(".y", names(val.DATA[[spp]]))], "pr.cv")
    mat<-matrix(NA, nrow=length(prs), ncol=4, 
                dimnames=(list(prs, c("Acc", "w k", "F", "TSS"))))
    
    for(i in 1:length(prs)){
      mat[i,]<-extractVALres(mod.out=measureVALPerformance(y=val.DATA[[spp]]$y.cv,
                                                           pr=val.DATA[[spp]][,prs[i]]))}
    
    val.SUMMARY[[spp]]<-signif(mat, digits=2)
  }
  
  save(val.SUMMARY, file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                               an.ID, thresh, "validation summary.RData", sep=""))
}
        