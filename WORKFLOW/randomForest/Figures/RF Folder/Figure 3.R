rm(list=ls())

cv.no=5
#Input correct an.ID for tuning and y data ("mod " for merged categories)
an.ID="Final/"
an.IDCV="model assess NC CV"
thresh="PredPrev=Obs"

#data= " mod" for merged categories or NULL for original ACs
data="mod "
model<-"rf_i"

require(maps)
require(raster)
require(png)
require(nnet)


#FUNCTIONS>>>.........................................................
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


plotMaps<-function(x, cats=5, fix.cols=T, legend){
  if(fix.cols){
    cols <- rev(terrain.colors(cats+1))
    cols[1] <- "#E0EEEE"
    cols <- cols[1:(cellStats(x, stat='max')+1)] 
    plot(x, col=cols, colNA="white", legend=legend,
         axis.args=list(at=unique(x), labels=unique(x)))}else{
                                                      
                                                      plot(log(x))}
}


plotSppValMaps <- function(y.m, x.dat, id=spp.t.id[spp.id], w.spp, spp, pt1=12, pt2=14, 
                        pr.map, map.int.ac, map.win, spp.id, spp.lab){
  
  main.lab<-c(expression(bold(CPR[italic(bold(AC))])), 
                       expression(bold(Win[italic(bold(CPR))])),
                       expression(bold(SDM[italic(bold(AGG))])), 
                       expression(bold(SDM[italic(bold(HR))])))
  
  y<-y.m[x.dat$m.id==id,w.spp]
  x<-x.dat[x.dat$m.id==id, c("lon", "lat") ]
  
  
  cols <- rev(terrain.colors(6))
  cols[1] <- "#E0EEEE"
  y.cols<-cols[factor(y, levels=0:5)]      
  
  
  map("world", xlim=c(-4,11), 
      ylim=c(51,61), fill=TRUE,
      resolution=0, cex.main=0.85)
  points(x, col=y.cols, pch=21, cex=0.1)
  axis(2)
  axis(1)
  mtext(substitute(italic(s), list(s=spp.lab)), side=2, line=1.8, outer=F, las=3, cex=1.1)
  if(spp.id==1){mtext(main.lab[1], side=3, line=-0.4, outer=T, 
                      las=1, cex=1, at=0.115)}
  
  
  plotMaps(map.win, cats=5, fix.cols=T, legend=F)
  map("world", xlim=c(-4,11), 
      ylim=c(51,61), fill=TRUE, col="black",
      resolution=0, add=T)
  if(spp.id==1){mtext(main.lab[2], side=3, line=-0.4, outer=T, 
                      las=1, cex=1.1, at=0.365)}
  
  
  plotMaps(map.int.ac, cats=5, fix.cols=T, legend=F)
  map("world", xlim=c(-4,11), 
      ylim=c(51,61), fill=TRUE, col="black",
      resolution=0, add=T)
  if(spp.id==1){mtext(main.lab[3], side=3, line=-0.4, outer=T, 
                      las=1, cex=1.1, at=0.615)}
  
  
  plotMaps(pr.map, cats=5, fix.cols=T, legend=T)
  map("world", xlim=c(-4,11), 
      ylim=c(51,61), fill=TRUE, col="black",
      resolution=0, add=T)
  if(spp.id==1){mtext(main.lab[4], side=3, line=-0.4, outer=T, 
                      las=1, cex=1.1, at=0.855)}
  
}


pt2=10
spp.t.id=c("2000-5-","2000-5-", "1999-9-",   "2000-5-", "2001-10-")

#Figure3<-function(an.ID, an.IDCV, cv.no=5, data="mod ", 
                  #spp.t.id=c("2000-5-", "1999-9-", "2000-5-",  "2000-5-", "2001-10-"),
               #thresh=thresh){
              
              #PRE-PROCESSING>>>.........................................................
              
              
              varnames<-c("year", "time", "lat", "lon",
                          "month",  "T.m", "Sed.m", 
                          "Sedf.m",  "Ch.m", "fdens.m", 
                          "fdist.m",  "fside.m",  "fdens.c.m", 
                          "fdist.c.m",  "fside.c.m", 
                          "bath", "NAO", "wNAO")
              
              
              names.spp<-c("chel",  "tem", "centrot","cfin",  "metrilu")
              all.spp<-c("chel",  "tem", "centrot", "cfin",  "metrilu")
              wCPR.spp<-c("calhel", "tem", "centrot", "calfin",  "metrilu")
              spp.labels<-c("C. helgolandicus", "T. longicornis", "C. typicus", "C. finmarchicus", "M. lucens")
              tv<-c("tem", "centrot")
              ec<-c("chel", "cfin", "metrilu")
              
              # Load data #.......................................
              load(file=paste("/Users/annakrystalli/documents/TRAINING DATA/y ", 
                              data, "training data.RData", sep=""))
              load("~/Documents/TRAINING DATA/normalized (min.max) x training data.RData")
              load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                              an.IDCV,"1/forests/thresholds.Rdata", sep=""))
              
              
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
              
              
              rm(norm.x.m)


png(filename = "~/Documents/THESIS/RF Chapter/Figures/Figure3.png", 
    width=8.3, height=11, units = "in", res = 300, pointsize=pt2)

par(mfrow=c(5,4), las=1, cex.axis=0.9, mar=c(1.5, 1, 0, 0.9), oma=c(0,2,2,0), 
    mgp=c(1.5,0.6,0), tcl=-0.3, xpd=F)


for(spp in all.spp){
  
  # spp id set ups
  spp.id <-  which(all.spp %in% spp)
  if(spp %in% c("tem", "centrot")){k=2;f=50}else{k=1; f=1}
  w.spp <- wCPR.spp[spp.id]
  id=spp.t.id[spp.id]
  
# WinCPR Aux data
winCPR.grid<-read.csv("~/Documents/CPR/DATA/RAW DATA/Associated files/north sea pixel grid.csv", header=F)
breaks <- read.csv("~/Documents/TRAINING DATA/Models/randomForest/WinCPR validation/breaks.csv")
bks.int<-breaks[-1,]
breaks[2,]<-breaks[2,]*0.5

#.......................................

# Raster data
map.ext<-extent(c(-4,11,51,61))
map.res <- raster(nrow=1112, ncol=926, ext=map.ext)




# Clear vectors
clear1<-c("all.samples", 
          "pr.ac", "pr.cv.ac", "pr.cv.oc", 
          "rf.dat", "rfAC", "rfOC", 
          "tune.i", "x.ind.ac", "x.ind.oc", "y.cv.ac", 
          "y.cv.oc")




# Load spp WinCPR data & process
wCPR.dat <- read.csv(paste("~/Documents/CPR/DATA/WinCPR/WinCSV/",
                           w.spp,".csv", sep=""), header=T)


wCPR.dat<-wCPR.dat[wCPR.dat$Year %in% 1997:2001,]
wCPR.dat[grep("X", names(wCPR.dat))]<-expm1(wCPR.dat[grep("X", names(wCPR.dat))])*f
wCPR.dat[wCPR.dat<0]<-NA
wCPR.dat$id<-paste(wCPR.dat$Year, "-", wCPR.dat$Month, "-", sep="")

#WinCPR map prep........

# Convert to ACs
winAC<-cut(as.numeric(wCPR.dat[wCPR.dat$id == id,4:dim(wCPR.dat)[2]]),
           breaks=c(-1,breaks[-1,k]), right=T, labels=0:5)

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
           id, ".RData", sep=""))

# Convert probabilities to threshold dependend presence / absence
if(any(preds$pred.oc<0)){preds$pred.oc<-preds$pred.oc+1}
preds$pred.oc<-as.numeric(preds$pred.oc>=THRESH.spp[[spp]][thresh])

pred.map<-matrix(NA, ncol=926, nrow=1112) 


pred.map[cbind(preds$r,preds$c)]<-0
pred.map[cbind(preds[preds$pred.oc==1,"r"], 
               preds[preds$pred.oc==1,"c"])]<-preds[preds$pred.oc==1,
                                                    "pred.ac"]

pr.map<-raster(pred.map, -4,11,51,61, crs="+proj=longlat +datum=WGS84")


# AGGREGATE pred map

# using integrated gamma function
# continuous abundance
map.int.con <- aggregate(pr.map, fact=c(71 , 50), 
                         fun=function(x, ...){if(all(is.na(x))){return(NA)}else{
                           x.ac<-na.omit(x[x>0])
                           n.ac<-length(x.ac)
                           if(n.ac==0){return(0)}else{
                             n<-length(na.omit(x))
                             op.ac<-optGammaFun(table(factor(x.ac, levels = c(1:5))),
                                                log(bks.int[,1]))
                             (n.ac/n)*exp(op.ac$mean + 0.5 * op.ac$var)*f}}}, 
                         expand=F)
# AC abundance
map.int.ac <- cut(map.int.con,
                  breaks=c(-10,breaks[-1,k]), right=T, labels=0:5)-1



  if(spp.id==1){main<-T}else{main<-F}
  
  plotSppValMaps(y.m, x.dat, id= spp.t.id[spp.id], w.spp, spp, pt1=pt1, pt2=pt2, 
                             pr.map, map.int.ac, map.win, spp.lab=spp.labels[spp.id],
                             spp.id=spp.id)}

dev.off()

