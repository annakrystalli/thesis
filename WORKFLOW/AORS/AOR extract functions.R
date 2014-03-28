library(calibrate)

    normalize <- function(x, a=NULL, b=NULL) {
            if(is.null(a) & is.null(b)){x.n<-(x-min(x)) / diff(range(x))}
            if(!is.null(a)){
              x.n <- (a*diff(range(x))+((x-min(x))*(1-a))) / diff(range(x))}
            if(!is.null(b)){
              x.n <- (b*diff(range(x))-((max(x)-x)*(b))) / diff(range(x))}
            x.n
          } 

#______________________________________________________________________________

    plotAORseason<-function(AB=AB, oc=oc, mm=mm, yr=yr, year, plot=F, a, b, an.ID=NULL){
      
            library(calibrate)
            x=AB[yr==year & oc>0][order(mm[yr==year & oc>0])]
            y=oc[yr==year & oc>0][order(mm[yr==year & oc>0])]
            m=mm[yr==year & oc>0][order(mm[yr==year & oc>0])]
      
            dir.create(path=paste("~/Documents/PREDICTED/", 
                                  an.ID,"AORS/seasonal/",sep=""), showWarnings=F)
            nm=length(m)
            x.n<-normalize(x)
            
            #Set up colors
                nma<-round(normalize(((2*(y*x.n))/(y+x.n)),a=a, b=b), digits=2)
                cols<-rgb(red=y, green=0, blue=x.n, 
                          alpha=nma,
                          maxColorValue = 1)
            
            dir.create(path=paste("~/Documents/PREDICTED/", 
                                  an.ID,"AORS/seasonal/",spp,sep=""), showWarnings=F)
            
            if(plot!=T){png(filename=paste("~/Documents/PREDICTED/", an.ID,"AORS/seasonal/",spp,"/", year, ".png", sep=""),
                                width=600, height=600)} 
            #Set up par
                par(bg="black", col="white", font=2, fg="white", col.axis="white", col.main="white")
          
            
              plot(y~x, col=cols, pch=c(24, rep(21, times=nm-2),23), type="p", lty=3, bg=cols, 
                   cex=c(1.3, rep(0.8, times=nm-2),1.5) , font=2, xlab="abundance", 
                   ylab="occupancy", col.lab="white", cex.axis=0.8, main=paste(spp, year),
                   ylim=c(0,1), xlim=c(0, ceiling(max(x))))
              
                  arrows(x0=x[1:(length(x))-1], y0=y[1:(length(y))-1], 
                         x1=x[2:(length(x))], y1=y[2:(length(y))], 
                         col=cols[2:(length(m))],
                         code=2, lty=3, lwd=2, length=0.2)
                  
                  textxy(X=x, Y=y, labs=m, dcol="white", cx=0.8)
                  
            if(plot!=T){dev.off()}
    }

#_____________________________________________________________________________________

yearlyAORs<-function(AB, oc, N, yr=yr, mm=mm, overall=T, plot=F, spp=spp, 
                     an.ID=NULL, years=NULL, no.plot=T, PR, log="log"){
  
                dir.create(path=paste("~/Documents/PREDICTED/", an.ID,"AORS/yearly/", sep=""), showWarnings=F)
                dir.create(path=paste("~/Documents/PREDICTED/", an.ID,"AORS/data/",sep=""), showWarnings=F)
                
                if(log=="log"){AB<-log(AB)
                               rm.0<-which(is.infinite(AB))
                               if(length(rm.0)==0){}else{
                                 AB<-AB[-rm.0]
                                 oc<-oc[-rm.0]
                                 yr<-yr[-rm.0]
                                 N<-N[-rm.0]
                                 mm<-mm[-rm.0]}}
                
                x=AB
                y=oc
                n=N
                y<-as.integer(y*n)/n
                
                glm.dat<-NULL
                
                cols.matrix<-matrix(c( "antiquewhite2",  "darkgoldenrod4",
                                       "honeydew1", "aquamarine4", 
                                       "seashell1","palevioletred4",
                                       "thistle1", "purple3",
                                       "whitesmoke","turquoise4"),
                                    nrow=5, ncol=2, byrow=TRUE)
                
                
                spps=c("chel", "cfin", "tem", "centrot", "metrilu")
                spp.col<-cols.matrix[which(spps==spp, arr.ind=T),]
                FUN.c<-colorRampPalette(spp.col, space="Lab")
  
          #Fit & predict overall glm
          glm.all<-glm(y~x, family=binomial, weights=n)
          newdat.all<-data.frame(x=seq(min(x),max(x), length.out=30))
          newy.all<-predict.glm(glm.all, newdata=newdat.all,type="response")
                
           glm.dat<-c(glm.all$coefficients, glm.all$deviance)
  
                
                      
          if(overall==T){
            glm.tab<-matrix(glm.dat, ncol=1, nrow=3, 
                            dimnames=list(c("a", "b", "dev"),"overall"))
                
                if(no.plot==T){}else{
                  
            m.fun<-function(X){which(X==max(X), arr.ind=T)}
            m.max<-round(mean(tapply(AB, INDEX=as.factor(yr), FUN=m.fun)))
            
as.numeric(names(table(m.max))[which.max(table(m.max))]) 
                      months<-sort(unique(as.factor(mm)))
                      main.lab<-paste(spp,"overall AOR across all monthly data")
                      cols.l<-FUN.c(n=12)
                      cols<-cols.l[mm]
    
              if(plot!=T){if(PR==T){png(filename=paste("~/Documents/PREDICTED/", an.ID,
                                             "AORS/yearly/",spp,
                                             " PROC overall.png", sep=""),  pointsize = 12,
                              width=600, height=600)}else{
                                png(filename=paste("~/Documents/PREDICTED/", an.ID,
                                                                             "AORS/yearly/",spp,
                                                                             " overall.png", sep=""),  pointsize = 12,
                                                              width=600, height=600)}} 
                      
                    par(bg="black", col="white",font=1, fg="white", col.axis="white",
                          col.main="white", col.lab="white")
                      
                    plot(y~x, pch=21, bg=cols, col=cols, cex=0.9, 
                         xlab="abundance", ylab="occupancy", 
                         font=2, font.lab=2,cex.lab=0.9, cex.axis=0.9, 
                         main=main.lab, ylim=c(0,1), xlim=c(0, round(max(x))))
                    
                      points(y=newy.all,x=unlist(newdat.all), type="l", lwd=1.3)
                      legend(x="bottomright", pch=21, col=cols.l, pt.bg=cols.l, legend=months, 
                             cex=0.8, bty="n")  
                      
                if(plot!=T){dev.off()}}}
            
            
  
  
      if(overall==F){
        if(is.null(years)){years<-sort(unique(yr))}
        
        if(no.plot==T){}else{
        cols.matrix<-matrix(c( "antiquewhite2",  "darkgoldenrod4",
                               "honeydew1", "aquamarine4", 
                               "seashell1","palevioletred4",
                               "thistle1", "purple3",
                               "whitesmoke","turquoise4"),
                            nrow=5, ncol=2, byrow=TRUE)
        
                            spps=c("chel", "cfin", "tem", "centrot", "metrilu")
                            spp.id<-which(spps==spp, arr.ind=T)
                            spp.col<-cols.matrix[spp.id,]
                            FUN.c<-colorRampPalette(spp.col, space="Lab")
        
        
        sp.names<-c("Calanus helgolandicus", "Calanus finmarchicus", "Temora longicornis", 
                    "Centropages typicus", "Metridia lucens")
                  main.lab<-paste(sp.names[spp.id]," intra annual AORs")
                  cols.l<-FUN.c(n=length(years))
                  cols<-cols.l[as.factor(yr)]
    
          if(plot!=T){if(PR==T){png(filename=paste("~/Documents/PREDICTED/", an.ID,"AORS/yearly/",spp,log,
                                         " PROC yearly.png", sep=""),
                          width=600, height=600,  pointsize = 12)}else{
                            png(filename=paste("~/Documents/PREDICTED/", an.ID,"AORS/yearly/",spp,log,
                                 " yearly.png", sep=""),
                                 width=600, height=600,  pointsize = 12)}} 
          
              par(bg="black", col="white",font=1, fg="white", col.axis="white",
                  col.main="white", col.lab="white")
              
              plot(y~x, pch=21, bg=cols, col=cols, cex=0.8, xlab=paste(log, " density",sep=""), ylab="occupancy", 
                   font=2, font.lab=2,cex.lab=0.9, cex.axis=0.9, main=main.lab, 
                   ylim=c(0,1), xlim=c(0, 6))
}
              
                          for(year in years){
                            
                            #Fit & predict overall glm
                            x.yr<-x[yr==year]
                            y.yr<-y[yr==year]
                            n.yr<-n[yr==year]
                            
                            glm.yr<-glm(y.yr~x.yr, family=binomial, weights=n.yr)
                            newdat<-data.frame(x.yr=seq(min(x.yr),max(x.yr), length.out=30))
                            newy<-predict.glm(glm.yr, newdata=newdat, type="response")
                            
                            glm.dat.yr<-c(glm.yr$coefficients, glm.yr$deviance)
                            glm.dat<-c(glm.dat, glm.dat.yr)
                            
                    if(no.plot==F){ points(y=newy,x=unlist(newdat), type="l", lwd=1.5,
                                   col=cols.l[years==year])}}
          
          
          if(no.plot==F){legend(x="bottomright", pch=21, col=cols.l, pt.bg=cols.l, legend=years, 
                     cex=0.8, bty="n")  
    
        if(plot!=T){dev.off()}}
   
     glm.tab<-matrix(glm.dat, ncol=length(years)+1, nrow=3, 
                     dimnames=list(c("a", "b", "dev"),c("overall", years)))
      print(glm.tab)  
   }

  if(PR==T){save(glm.tab, file=paste("~/Documents/PREDICTED/", an.ID,"AORS/data/",spp,spp.id,
                    " AOR PROC glm tab.RData", sep=""))}else{
                      save(glm.tab, file=paste("~/Documents/PREDICTED/", an.ID,"AORS/data/",spp,spp.id,
                                               " AOR glm tab.RData", sep=""))}
}


#_____________________________________________________________________________________

AORextract<-function(spp, a, b, y.plots=F, an.ID=NULL, an.IDCV,
                     years=NULL, season=F, no.plot=T, hists=T, thresh){
  
                      input.folder<-paste("~/Documents/PREDICTED/",an.ID,"normalised monthly maps/", sep="")
  
                      av <- read.csv("~/Documents/CPR/DATA/RAW DATA/Accepted values.csv")
                      tv<-c("tem", "centrot")
                      ec<-c("chel", "cfin", "metrilu")
                      
                      require(nnet)
                      
                      dir.create(path=paste("~/Documents/PREDICTED/",an.ID,"AORS/", 
                                       sep=""), showWarnings = F)
                      
                      dir.create(path=paste("~/Documents/PREDICTED/",an.ID,"AORS/data", 
                                            sep=""), showWarnings = F)
                      oc<-NULL
                      AB<-NULL
                      N<-NULL
                      yr<-NULL
                      mm<-NULL
  
                      load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                                      an.IDCV,"1/forests/thresholds.Rdata", sep=""))
    
      file.names<-list.files(path=paste(input.folder, spp, sep=""), all.files = FALSE)
                      
                      dir.create(path=paste("~/Documents/PREDICTED/", 
                                            an.ID,"monthly hists/", 
                                            sep=""), showWarnings = F) 
                      dir.create(path=paste("~/Documents/PREDICTED/", 
                                            an.ID,"monthly hists/", 
                                            spp,sep=""), showWarnings = F)                    
      #Histograms.............................                  
          for(i in 1:length(file.names)){
          
                      load(paste(input.folder,spp,"/", file.names[i], sep=""))
                      
                      if(any(preds$pred.oc<0)){preds$pred.oc<-preds$pred.oc+1}
                      
                      preds$pred.oc<-as.numeric(preds$pred.oc>=THRESH.spp[[spp]][thresh])
                      
                      if(hists==T){
                                #Hist plots
                                png(filename=paste("~/Documents/PREDICTED/", an.ID,
                                                   "monthly hists/", 
                                                   spp,"/", 
                                                   file.names[i], ".png", sep=""), 
                                    width=400, height=500) 
                                
                                hist(preds$pred.ac[preds$pred.oc==1], 
                                     breaks=0:12, labels=as.character(1:12), freq=F, 
                                     ylim=c(0,1), xlab="Abundance Categories", 
                                     main=paste(spp,file.names[i]), col="darkslategray3", 
                                     border="darkslategray", cex.axis=1)
                                dev.off()}
            #...................................              
                          
                n<-dim(preds)[[1]]
                oc<-c(oc,sum(preds$pred.oc)/n)
                
                ab<-class.ind(preds$pred.ac[preds$pred.oc==1])
                      add<-setdiff(as.character(1:12),dimnames(ab)[[2]])
                      ab<-cbind(ab,matrix(0, nrow=length(preds$pred.ac[preds$pred.oc==1]), 
                                          ncol=length(add), dimnames=list(NULL,add)))
                      ab<-ab[,order(as.numeric(colnames(ab)))]
                     if(spp%in%ec){ab<-ab%*%av$ave}else{ab<-ab%*%av$avt}
            
                      
                      
                      
              AB<-c(AB,mean(ab))
              N<-c(N,n)
              mm<-c(mm, as.numeric(strsplit(file.names[i], "-")[[1]][2]))
              yr<-c(yr, as.numeric(strsplit(file.names[i], "-")[[1]][1]))
            }
  
            AB[is.nan(AB)]<-0
                      
  save(oc,AB,N,mm,yr,file=paste("~/Documents/PREDICTED/", an.ID,"AORS/data/",
                                spp," monthly AOR.Rdata",sep=""))
  
      #PLOTS
      if(season==T){
              #plot seasonal
              if(is.null(years)){years<-unique(yr)}
              
              for(year in years){
                plotAORseason(AB=AB, oc=oc, mm=mm, year=year, yr=yr, plot=F, 
                              b=b, a=a, an.ID=an.ID)}}
              
              if(y.plots==F){png(filename=paste("~/Documents/PREDICTED/", 
                                                an.ID,"AORS/yearly/",spp,
                                                             " combo yearly.png", sep=""),
                                              width=600, height=1200)
             par(mfrow=c(2,1))
              plot<-T}else{plot<-F}                
                              
              #plot overall
              yearlyAORs(AB=AB, oc=oc, N=N, yr=yr, mm=mm, overall=T, plot=plot, 
                         spp=spp, an.ID=an.ID, years=years, no.plot=T, PR=F)
              
              #plot yearly
              yearlyAORs(AB=AB, oc=oc, N=N, yr=yr, mm=mm, overall=F, plot=plot, 
                         spp=spp, an.ID=an.ID, years=years, no.plot=T, PR=F)
                              
                              if(y.plots==F){dev.off()}
}


#_____________________________________________________________________________________
plotYearly<-function(pt=12, y.plots=F, spp=spp, 
                     an.ID=NULL, years=NULL, no.plot=F, PR=T, log="log"){
  
  if(PR==T){load(file=paste("~/Documents/PREDICTED/", an.ID,"AORS/data/",
                              spp," PROC monthly AOR.Rdata",sep=""))
              mm<-MM
              yr<-YR}else{
            load(file=paste("~/Documents/PREDICTED/", an.ID,"AORS/data/",
                              spp," monthly AOR.Rdata",sep=""))}
  
  if(y.plots==F){if(PR==T){png(filename=paste("~/Documents/PREDICTED/", an.ID,"AORS/yearly/",spp,
                                              " PROC combo yearly.png", sep=""),
                               width=600, height=1200,  pointsize = pt)}else{
                                 png(filename=paste("~/Documents/PREDICTED/", an.ID,"AORS/yearly/",spp,
                                    " combo yearly.png", sep=""),
                     width=600, height=1200,  pointsize = pt)}
                 par(mfrow=c(2,1))
                 plot<-T}else{plot<-F}   
  
  
  #plot overall
  yearlyAORs(AB=AB, oc=oc, N=N, yr=yr, mm=mm, overall=T, plot=plot, spp=spp, 
             an.ID=an.ID, years=years, no.plot=no.plot, PR=PR)
  
  #plot yearly
  yearlyAORs(AB=AB, oc=oc, N=N, yr=yr, mm=mm, overall=F, plot=plot, spp=spp, 
             an.ID=an.ID, years=years, no.plot=no.plot, PR=PR)
  
  if(y.plots==F){dev.off()}}

#_____________________________________________________________________________________
plotYearlyAll<-function(pt=12, y.plots=F, input.folder=input.folder, 
                        an.ID=NULL, years=NULL, no.plot=F, plot=T){
  
  

  
  if(y.plots==F){png(filename=paste("~/Documents/PREDICTED/", an.ID,
                                    "AORS/yearly/all combo yearly.png",sep=""),
                     width=5*600, height=1200,  pointsize = pt)
                 par(mfcol=c(2,5))
                 plot<-T}else{plot<-F}   
  
  for(spp in c("chel", "cfin","metrilu", "tem", "centrot")){
    
  load(file=paste("~/Documents/PREDICTED/", an.ID,"AORS/data/",
                  spp," monthly AOR.Rdata",sep=""))
  
  #plot overall
  yearlyAORs(AB=AB, oc=oc, N=N, yr=yr, mm=mm, overall=T, plot=plot, spp=spp, 
             an.ID=an.ID, years=years, no.plot=no.plot)
  
  #plot yearly
  yearlyAORs(AB=AB, oc=oc, N=N, yr=yr, mm=mm, overall=F, plot=plot, spp=spp, 
             an.ID=an.ID, years=years, no.plot=no.plot)}
  
  if(y.plots==F){dev.off()}}
#_____________________________________________________________________________________
imputeMonths<-function(imp.var, yr, mm, dn.wt=1){
  
  r<-length(unique(yr))
  c<-length(unique(mm))
  imp.mat<-matrix(NA, r,c)
  
  imp.mat[cbind(as.numeric(factor(yr)), mm)]<-imp.var
  imp.mat<-imp.mat[-1,]
  
  yr.means<-rowMeans(imp.mat, na.rm=T)
  
  
  yr.dev<-vector(length=13)
  mm.means<-colMeans(imp.mat, na.rm=T)
  
  for(i in 1:13){
    yr.dev[i]<-yr.means[i]/mean(colMeans(imp.mat[,!is.na(imp.mat[i,])], na.rm=T))}
  
  imp.id<-which(is.na(imp.mat), arr.ind=T)
  imp.mat[imp.id]<-mm.means[imp.id[,2]]*yr.dev[imp.id[,1]]*dn.wt
  
  as.vector(imp.mat)}

#_____________________________________________________________________________________
processAORdat<-function(an.ID){
  
  YR<-rep(1998:2010, times=12)
  MM<-rep(1:12, each=13)
  
  for (spp in c("chel","cfin", "metrilu", "tem", "centrot")){
    
    load(paste("~/Documents/PREDICTED/",an.ID,"/AORS/data/",spp," monthly AOR.Rdata", sep="") ) 
    
    AB<-imputeMonths(AB, yr, mm, dn.wt=1)
    oc<-imputeMonths(oc, yr, mm, dn.wt=1)
    oc[oc>1]<-1
    N<-imputeMonths(N, yr, mm, dn.wt=0.1)
    oc<-as.integer(oc*N)/N
    
    save(AB, oc, N, YR, MM, file=paste("~/Documents/PREDICTED/",
                                       an.ID,"/AORS/data/",spp," PROC monthly AOR.Rdata", sep=""))}}
#_____________________________________________________________________________________

interannualAOR<-function(plot=F, spp=spp,  xlim, pt=12,
                     an.ID=NULL, years=NULL, no.plot=F, PR=PR, log="log"){
  
  if(PR==T){load(file=paste("~/Documents/PREDICTED/", an.ID,"AORS/data/",
                            spp," PROC monthly AOR.Rdata",sep=""))
            mm<-MM
            yr<-YR}else{
              load(file=paste("~/Documents/PREDICTED/", an.ID,"AORS/data/",
                              spp," monthly AOR.Rdata",sep=""))}
  

  dir.create(path=paste("~/Documents/PREDICTED/", an.ID,"AORS/yearly/", sep=""), showWarnings=F)
  dir.create(path=paste("~/Documents/PREDICTED/", an.ID,"AORS/data/",sep=""), showWarnings=F)
  
  
  if(log=="log"){AB<-log(AB)
  rm.0<-which(is.infinite(AB))
                 if(length(rm.0)==0){}else{
                  AB<-AB[-rm.0]
                  oc<-oc[-rm.0]
                  yr<-yr[-rm.0]
                  N<-N[-rm.0]
                  mm<-mm[-rm.0]}}
  
  x.e=unlist(lapply(split(AB, yr), mean))
  y.e=unlist(lapply(split(oc, yr), mean))
  n.e=unlist(lapply(split(N, yr), mean))
  y.e<-as.integer(y.e*n.e)/n.e
  yr.e<-unique(yr)
  
  std <- function(x) sd(x)/sqrt(length(x))
  
  x.e.sd=unlist(lapply(split(AB, yr), std))
  y.e.sd=unlist(lapply(split(oc, yr), std))
  n.e.sd=unlist(lapply(split(N, yr), std))

  
  glm.dat<-NULL
  

  
  #Fit & predict inter annual glm
  glm.inter<-glm(y.e~x.e, family=binomial, weights=n.e)
  newdat.inter<-data.frame(x.e=seq(min(x.e),max(x.e), length.out=30))
  newy.inter<-predict.glm(glm.inter, newdata=newdat.inter,type="response")
  
  glm.dat<-c(glm.inter$coefficients, glm.inter$deviance, summary(glm.inter)$coefficients[,4], cor(y.e, x.e))
  

    if(is.null(years)){years<-sort(unique(yr))}
    
    if(no.plot==T){}else{
      cols.matrix<-matrix(c( "antiquewhite2",  "darkgoldenrod4",
                             "honeydew1", "aquamarine4", 
                             "seashell1","palevioletred4",
                             "thistle1", "purple3",
                             "whitesmoke","turquoise4"),
                          nrow=5, ncol=2, byrow=TRUE)
      
      spps=c("chel", "cfin", "tem", "centrot", "metrilu")
      spp.id<-which(spps==spp, arr.ind=T)
      spp.col<-cols.matrix[spp.id,]
      sp.names<-c("Calanus helgolandicus", "Calanus finmarchicus", "Temora longicornis", 
                  "Centropages typicus", "Metridia lucens")
      FUN.c<-colorRampPalette(spp.col, space="Lab")
      

      main.lab<-sp.names[spp.id] 
      cols.l<-FUN.c(n=length(years))
      cols.y<-cols.l[as.factor(yr.e)]
      
      if(plot!=T){if(PR==T){png(filename=paste("~/Documents/PREDICTED/", an.ID,"AORS/yearly/",spp,
                                               " PROC inter annual.png", sep=""),
                                width=600, height=600,  pointsize = pt)}else{
                                  png(filename=paste("~/Documents/PREDICTED/", an.ID,"AORS/yearly/",spp,
                                                     " inter annual.png", sep=""),
                                      width=600, height=600,  pointsize = pt)}} 
      
      par(bg="transparent", col="white",font=1, fg="white", col.axis="white",
          col.main="white", col.lab="white", las=1)
      
      plot(y.e~x.e, pch=21, bg=cols.y, col=cols.y, cex=0.9, xlab=paste(log," density", sep=""), ylab="occupancy",
           font=2, font.lab=2,cex.lab=1.1, cex.axis=1.1, main=main.lab, cex.main=1.2,
           ylim=c(0,1), xlim=xlim)
      
      rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
             "black")
      
      points(y.e~x.e, pch=21, bg=cols.y, col=cols.y, cex=0.9, ylim=c(0,1), xlim=xlim)
      
      points(y=newy.inter,x=unlist(newdat.inter), type="l", lwd=3.5)
    
      legend(x="bottomright", pch=21, col=cols.l, pt.bg=cols.l, 
                          legend=years, 
                          cex=0.8, bty="n")  
                   
                   if(plot!=T){dev.off()}}
    
   
    x=AB
    y=oc
    n=N
    y<-as.integer(y*n)/n
    
    
  
    
    #Fit & predict overall glm
    glm.all<-glm(y~x, family=binomial, weights=n)
    newdat.all<-data.frame(x=seq(min(x),max(x), length.out=30))
    newy.all<-predict.glm(glm.all, newdata=newdat.all,type="response")
    
    glm.dat<-c(glm.dat, c(glm.all$coefficients, glm.all$deviance), 
               summary(glm.all)$coefficients[,4], cor(y, x))
   
  
      if(is.null(years)){years<-sort(unique(yr))}
      
      if(no.plot==T){}else{


        cols<-cols.l[as.factor(yr)]
        
        if(plot!=T){if(PR==T){png(filename=paste("~/Documents/PREDICTED/", an.ID,"AORS/yearly/",spp,log,
                                                 " PROC intra.png", sep=""),
                                  width=600, height=600,  pointsize = pt)}else{
                                    png(filename=paste("~/Documents/PREDICTED/", an.ID,"AORS/yearly/",spp,log,
                                                       " intra.png", sep=""),
                                        width=600, height=600,  pointsize = pt)}} 
        
        par(bg="transparent", col="white",font=1, fg="white", col.axis="white",
            col.main="white", col.lab="white", las=1)
        
        plot(y~x, pch=21, bg=cols, col=cols, cex=0.9, xlab=paste(log, " density",sep=""), ylab="occupancy", 
             font=2, font.lab=2,cex.lab=1.1, cex.axis=1.1, main=main.lab, cex.main=1.2,
             ylim=c(0,1), xlim=xlim)
        
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
               "black")
        
        points(y~x, pch=21, bg=cols, col=cols, cex=0.9, ylim=c(0,1), xlim=xlim)
      }
      
      for(year in years){
        
        #Fit & predict overall glm
        x.yr<-x[yr==year]
        y.yr<-y[yr==year]
        n.yr<-n[yr==year]
        
        glm.yr<-glm(y.yr~x.yr, family=binomial, weights=n.yr)
        newdat<-data.frame(x.yr=seq(min(x.yr),max(x.yr), length.out=30))
        newy<-predict.glm(glm.yr, newdata=newdat, type="response")
        
        glm.dat.yr<-c(glm.yr$coefficients, glm.yr$deviance, 
                      summary(glm.yr)$coefficients[,4], cor(y.yr, x.yr))
        glm.dat<-c(glm.dat, glm.dat.yr)
        
        if(no.plot==F){ points(y=newy,x=unlist(newdat), type="l", lwd=2,
                               col=cols.l[years==year])}}
      
      
      if(no.plot==F){legend(x="bottomright", pch=21, col=cols.l, pt.bg=cols.l, legend=years, 
                            cex=0.8, bty="n")  
                     
                     if(plot!=T){dev.off()}}
      
      glm.tab<-matrix(glm.dat, ncol=length(years)+2, nrow=6, 
                      dimnames=list(c("interecept", "slope", "dev", "p interecept", "p slope",
                                      "correlation"),c("inter","overall", years)))
  
      var.tab<-matrix(c(var(glm.tab[2,3:dim(glm.tab)[2]]),
      var(glm.tab[2,3:dim(glm.tab)[2]])/mean(glm.tab[2,3:dim(glm.tab)[2]]),
      sd(glm.tab[2,3:dim(glm.tab)[2]]),
      std(glm.tab[2,3:dim(glm.tab)[2]]),
      mean(glm.tab[2,3:dim(glm.tab)[2]]),                  
                        var(glm.tab[6,3:dim(glm.tab)[2]]),
                        var(glm.tab[6,3:dim(glm.tab)[2]])/mean(glm.tab[2,3:dim(glm.tab)[2]]),
                        sd(glm.tab[6,3:dim(glm.tab)[2]]),
                        std(glm.tab[6,3:dim(glm.tab)[2]]),
                        mean(glm.tab[6,3:dim(glm.tab)[2]])),5,2,
                      dimnames=list(c("variance",  "std variance", "sd", "se",
                                      "mean"),c("slope","correlation")))
      print(spp)                  
      print(signif(glm.tab, digits=3))  
      print(var.tab)
    
    if(PR==T){save(glm.tab, var.tab, file=paste("~/Documents/PREDICTED/", an.ID,"AORS/data/",spp,spp.id,
                                       " AOR PROC inter glm tab.RData", sep=""))}else{
                                         save(glm.tab, var.tab, file=paste("~/Documents/PREDICTED/", an.ID,"AORS/data/",spp,spp.id,
                                                                  " AOR glm inter tab.RData", sep=""))}
  
  if(PR==T){write.csv(glm.tab, file=paste("~/Documents/PREDICTED/", an.ID,"AORS/data/",spp,spp.id,
                                              " AOR PROC glm table.csv", sep=""))}else{
                                                write.csv(glm.tab, file=paste("~/Documents/PREDICTED/", an.ID,"AORS/data/",spp,spp.id,
                                                                         " AOR glm table.csv", sep=""))}
  if(PR==T){write.csv(var.tab, file=paste("~/Documents/PREDICTED/", an.ID,"AORS/data/",spp,spp.id,
                                              " AOR PROC variance table.csv", sep=""))}else{
                                                write.csv(var.tab, file=paste("~/Documents/PREDICTED/", an.ID,"AORS/data/",spp,spp.id,
                                                                         " AOR variance table.csv", sep=""))}
 #....ALL..............................
  
  if(no.plot==T){}else{
    

    cols.l<-FUN.c(n=length(years))
    cols<-cols.l[as.factor(yr)]
    
    if(plot!=T){if(PR==T){png(filename=paste("~/Documents/PREDICTED/", an.ID,"AORS/yearly/",spp,log,
                                             " PROC all.png", sep=""),
                              width=600, height=600,  pointsize = pt)}else{
                                png(filename=paste("~/Documents/PREDICTED/", an.ID,"AORS/yearly/",spp,log,
                                                   " all.png", sep=""),
                                    width=600, height=600,  pointsize = pt)}} 
    
    par(bg="transparent", col="white",font=1, fg="white", col.axis="white",
        col.main="white", col.lab="white", las=1)
    
    plot(y~x, pch=1, bg=cols, col=cols, cex=0.9, xlab=paste(log, " density",sep=""), ylab="occupancy", 
         font=2, font.lab=2,cex.lab=1.1, cex.axis=1.1, main=main.lab, cex.main=1.2,
         ylim=c(0,1), xlim=xlim)
    
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
           "black")
    points(y~x, pch=1, bg=cols, col=cols, cex=0.9, ylim=c(0,1), xlim=xlim)
  }
  
  for(year in years){
    
    #Fit & predict overall glm
    x.yr<-x[yr==year]
    y.yr<-y[yr==year]
    n.yr<-n[yr==year]
    
    glm.yr<-glm(y.yr~x.yr, family=binomial, weights=n.yr)
    newdat<-data.frame(x.yr=seq(min(x.yr),max(x.yr), length.out=30))
    newy<-predict.glm(glm.yr, newdata=newdat, type="response")
    
   
    if(no.plot==F){points(y=newy,x=unlist(newdat), type="l", lwd=2.75,
                          col=cols.l[years==year])}}
  
  
  if(no.plot==F){
                points(y=y.e,x=x.e, type="p", cex=1.2,
                        bg=cols.y, col="white", pch=23) 
                points(y=newy.inter,x=unlist(newdat.inter), type="l", lwd=5,
                       col="white")
                legend(x="bottomright", pch=21, col=cols.l, pt.bg=cols.l, legend=years, 
                        cex=0.8, bty="n") } 
                 
                 if(plot!=T){dev.off()}}
    
  
  
  
  
  

