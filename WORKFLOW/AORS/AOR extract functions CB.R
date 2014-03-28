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

plotAORseason<-function(AB=AB, oc=oc, mm=mm, yr=yr, year, plot=F, a, b, 
                        an.ID=NULL, run){
  
  library(calibrate)
  x=AB[yr==year & oc>0][order(mm[yr==year & oc>0])]
  y=oc[yr==year & oc>0][order(mm[yr==year & oc>0])]
  m=mm[yr==year & oc>0][order(mm[yr==year & oc>0])]
  
  dir.create(path=paste("~/Documents/PREDICTED/", 
                        an.ID,run, "/AORS/seasonal/",sep=""), showWarnings=F)
  nm=length(m)
  x.n<-normalize(x)
  
  #Set up colors
  nma<-round(normalize(((2*(y*x.n))/(y+x.n)),a=a, b=b), digits=2)
  cols<-rgb(red=y, green=0, blue=x.n, 
            alpha=nma,
            maxColorValue = 1)
  
  dir.create(path=paste("~/Documents/PREDICTED/", 
                        an.ID,run, "/AORS/seasonal/",spp,sep=""), showWarnings=F)
  
  if(plot!=T){png(filename=paste("~/Documents/PREDICTED/", an.ID,run, "/AORS/seasonal/",spp,"/", year, ".png", sep=""),
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
                     an.ID=NULL, years=NULL, no.plot=T, run){
  
  dir.create(path=paste("~/Documents/PREDICTED/", an.ID,run, "/AORS/yearly/", sep=""), showWarnings=F)
  dir.create(path=paste("~/Documents/PREDICTED/", an.ID,run, "/AORS/data/",sep=""), showWarnings=F)
  
  x=AB
  y=oc
  n=N
  glm.dat<-NULL
  
  cols.matrix<-matrix(c( "antiquewhite2",  "darkgoldenrod4",
                         "honeydew1", "aquamarine4", 
                         "seashell1","palevioletred4",
                         "thistle1", "purple3",
                         "whitesmoke","turquoise4"),
                      nrow=5, ncol=4, byrow=TRUE)
  
  
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
      main.lab<-paste(spp,"overall AOR across all years")
      cols.l<-FUN.c(n=12)
      cols<-cols.l[mm]
      
      if(plot!=T){png(filename=paste("~/Documents/PREDICTED/", an.ID,run, 
                                     "/AORS/yearly/",spp,
                                     " overall.png", sep=""),  pointsize = 12,
                      width=600, height=600)} 
      
      par(bg="black", col="white",font=1, fg="white", col.axis="white",
          col.main="white", col.lab="white")
      
      plot(y~x, pch=21, bg=cols, col=cols, cex=0.8, 
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
      spp.col<-cols.matrix[which(spps==spp, arr.ind=T),]
      FUN.c<-colorRampPalette(spp.col, space="Lab")
      
      
      main.lab<-paste(spp,"Yearly AORs versus overall")
      cols.l<-FUN.c(n=length(years))
      cols<-cols.l[as.factor(yr)]
      
      if(plot!=T){png(filename=paste("~/Documents/PREDICTED/", an.ID,run, "/AORS/yearly/",spp,
                                     " yearly.png", sep=""),
                      width=600, height=600,  pointsize = 12)} 
      
      par(bg="black", col="white",font=1, fg="white", col.axis="white",
          col.main="white", col.lab="white")
      
      plot(y~x, pch=21, bg=cols, col=cols, cex=0.8, xlab="abundance", ylab="occupancy", 
           font=2, font.lab=2,cex.lab=0.9, cex.axis=0.9, main=main.lab, 
           ylim=c(0,1), xlim=c(0, ceiling(max(x)*1.15)))
      
      
      points(y=newy.all,x=unlist(newdat.all), type="l", lwd=2)}
    
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
    
    
    if(no.plot==F){legend(x="bottomright", pch=21, col=cols.l, pt.bg=cols.l, 
                          legend=years, 
                          cex=0.8, bty="n")  
                   
                   if(plot!=T){dev.off()}}
    
    glm.tab<-matrix(glm.dat, ncol=length(years)+1, nrow=3, 
                    dimnames=list(c("a", "b", "dev"),c("overall", years)))
  }
  
  save(glm.tab, file=paste("~/Documents/PREDICTED/", an.ID,run, "/AORS/data/",spp,
                           " AOR glm tab.RData", sep=""))
}


#_____________________________________________________________________________________

AORextract<-function(spp, input.folder=input.folder, a, b, y.plots=F, an.ID=NULL, 
                     years=NULL, season=F, no.plot=T, hists=T, run){
  
  av <- read.csv("~/Documents/CPR/DATA/RAW DATA/Accepted values.csv")
  tv<-c("tem", "centrot")
  ec<-c("chel", "cfin", "metrilu")
  
  require(nnet)
  
  dir.create(path=paste("~/Documents/PREDICTED/",an.ID,run, "/AORS/", 
                        sep=""), showWarnings = F)
  
  dir.create(path=paste("~/Documents/PREDICTED/",an.ID,run, "/AORS/data", 
                        sep=""), showWarnings = F)
  oc<-NULL
  AB<-NULL
  N<-NULL
  yr<-NULL
  mm<-NULL
  
  
  
  file.names<-list.files(path=paste(input.folder, spp, sep=""), all.files = FALSE)
  
  dir.create(path=paste("~/Documents/PREDICTED/", 
                        an.ID,run, "/monthly hists/", 
                        sep=""), showWarnings = F) 
  dir.create(path=paste("~/Documents/PREDICTED/", 
                        an.ID,run, "/monthly hists/", 
                        spp,sep=""), showWarnings = F)                    
  #Histograms.............................                  
  for(i in 1:length(file.names)){
    
    load(paste(input.folder,spp,"/", file.names[i], sep=""))
    
    if(hists==T){
      #Hist plots
      png(filename=paste("~/Documents/PREDICTED/", an.ID,run, 
                         "/monthly hists/", 
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
  
  save(oc,AB,N,mm,yr,file=paste("~/Documents/PREDICTED/", an.ID,run, "/AORS/data/",
                                spp," monthly AOR.Rdata",sep=""))
  
  #PLOTS
  if(season==T){
    #plot seasonal
    if(is.null(years)){years<-unique(yr)}
    
    for(year in years){
      plotAORseason(AB=AB, oc=oc, mm=mm, year=year, yr=yr, plot=F, 
                    b=b, a=a, an.ID=an.ID)}}
  
  if(y.plots==F){png(filename=paste("~/Documents/PREDICTED/", 
                                    an.ID,run, "/AORS/yearly/",spp,
                                    " combo yearly.png", sep=""),
                     width=600, height=1200)
                 par(mfrow=c(2,1))
                 plot<-T}else{plot<-F}                
  
  #plot overall
  yearlyAORs(AB=AB, oc=oc, N=N, yr=yr, mm=mm, overall=T, plot=plot, 
             spp=spp, an.ID=an.ID, years=years, no.plot=T)
  
  #plot yearly
  yearlyAORs(AB=AB, oc=oc, N=N, yr=yr, mm=mm, overall=F, plot=plot, 
             spp=spp, an.ID=an.ID, years=years, no.plot=T)
  
  if(y.plots==F){dev.off()}
}


#_____________________________________________________________________________________
plotYearly<-function(pt=12, y.plots=F, spp=spp, input.folder=input.folder, 
                     an.ID=NULL, years=NULL, no.plot=F, run){
  
  
  load(file=paste("~/Documents/PREDICTED/", an.ID,run, "/AORS/data/",
                  spp," monthly AOR.Rdata",sep=""))
  
  if(y.plots==F){png(filename=paste("~/Documents/PREDICTED/", an.ID,run, "/AORS/yearly/",spp,
                                    " combo yearly.png", sep=""),
                     width=600, height=1200,  pointsize = pt)
                 par(mfrow=c(2,1))
                 plot<-T}else{plot<-F}   
  
  
  #plot overall
  yearlyAORs(AB=AB, oc=oc, N=N, yr=yr, mm=mm, overall=T, plot=plot, spp=spp, 
             an.ID=an.ID, years=years, no.plot=no.plot)
  
  #plot yearly
  yearlyAORs(AB=AB, oc=oc, N=N, yr=yr, mm=mm, overall=F, plot=plot, spp=spp, 
             an.ID=an.ID, years=years, no.plot=no.plot)
  
  if(y.plots==F){dev.off()}}

#_____________________________________________________________________________________
plotYearlyAll<-function(pt=12, y.plots=F, input.folder=input.folder, 
                        an.ID=NULL, years=NULL, no.plot=F, plot=T, run){
  
  
  
  
  if(y.plots==F){png(filename=paste("~/Documents/PREDICTED/", an.ID,
                                    run, "/AORS/yearly/all combo yearly.png",sep=""),
                     width=5*600, height=1200,  pointsize = pt)
                 par(mfcol=c(2,5))
                 plot<-T}else{plot<-F}   
  
  for(spp in c("chel", "cfin","metrilu", "tem", "centrot")){
    
    load(file=paste("~/Documents/PREDICTED/", an.ID,run, "/AORS/data/",
                    spp," monthly AOR.Rdata",sep=""))
    
    #plot overall
    yearlyAORs(AB=AB, oc=oc, N=N, yr=yr, mm=mm, overall=T, plot=plot, spp=spp, 
               an.ID=an.ID, years=years, no.plot=no.plot)
    
    #plot yearly
    yearlyAORs(AB=AB, oc=oc, N=N, yr=yr, mm=mm, overall=F, plot=plot, spp=spp, 
               an.ID=an.ID, years=years, no.plot=no.plot)}
  
  if(y.plots==F){dev.off()}}
