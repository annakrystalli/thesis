require(maps)
require(raster)
require(png)
require(nnet)


resMatrix<-function(res.tab){
  as.matrix(cbind(apply(res.tab[complete.cases(res.tab),], 
                        FUN=function(x , ...){sum(x==0)/length(x)}, 2),
                  apply(res.tab[complete.cases(res.tab),], 
                        FUN=function(x , ...){sum(x^2)/length(x)}, 2)),
            byrow=T, dimnames=list(names(res.tab), NULL))}

plotMaps<-function(x, cats=5, fix.cols=T){
  if(fix.cols){
    cols <- rev(terrain.colors(cats+1))
    cols[1] <- "#E0EEEE"
    cols <- cols[1:(cellStats(x, stat='max')+1)] 
    plot(x, col=cols, axis.args=list(at=unique(x), 
                                     labels=unique(x)))}else{
                                       
                                       plot(log(x))}
}


# COMPILE ARRAY of monthly maps (rf & win) ___________________________________________________________________________

an.ID="Final/"
an.IDCV<-"model assess NC CV"
data="mod "
model="rf_i"
cv.no=5
thresh="PredPrev=Obs"

# Species data
names.spp<-c("chel", "cfin",  "metrilu", "centrot", "tem")
wCPR.spp<-c("calhel", "calfin",  "metrilu", "centrot", "tem")
tv<-c("tem", "centrot")
ec<-c("chel", "cfin", "metrilu")

load(file="~/Documents/CPR/DATA/RAW DATA/Associated files/validation indices.RData")
load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                an.IDCV,"1/forests/thresholds.Rdata", sep=""))


# Set up validation table       #.......................................


t.id<-unique(val.index$m.index)[1:40]
rm(val.index)


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


for(spp in names.spp){
      
      # spp id set ups
      spp.id <-  which(names.spp %in% spp)
      if(spp %in% c("tem", "centrot")){k=2;f=50}else{k=1; f=1}
      w.spp <- wCPR.spp[spp.id]
      
      dir.create(path=paste("~/Documents/PREDICTED/",an.ID,"model deviation arrays/", 
                            sep=""), showWarnings = F)
    
    # Master Array
    
    M<-array(NA, dim=c(1112, 926, length(t.id), 2), dimnames=list(lat=NULL, lon=NULL,
                                                                  t.id=t.id,
                                                                  mod=c("rf", "win")))
    
    
    
    # Load spp WinCPR data & process
    wCPR.dat <- read.csv(paste("~/Documents/CPR/DATA/WinCPR/WinCSV/",
                               w.spp,".csv", sep=""), header=T)
    
    
    wCPR.dat<-wCPR.dat[wCPR.dat$Year %in% 1997:2001,]
    wCPR.dat[grep("X", names(wCPR.dat))]<-(10^wCPR.dat[grep("X", names(wCPR.dat))])-1
    wCPR.dat[wCPR.dat<0]<-NA
    wCPR.dat$id<-paste(wCPR.dat$Year, "-", wCPR.dat$Month, "-", sep="")
    
    
    #.......................................
    for(id in t.id){
          
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
          M[, ,id, "win"]<-as.matrix(map.win)
          
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
          
          
          M[, ,id, "rf"]<-pred.map}
      
    save(M, file=paste("~/Documents/PREDICTED/",an.ID,"model deviation arrays/", spp, "M array.RData", 
                          sep=""))}




# CALCULATE PIXEL COHEN'S KAPPA ___________________________________________________________________________

    
    pixelCohen<-function(m){
      require(psych)
      
      ck<-try(cohen.kappa(m), silent=T) 
      
      if(any(class(ck) == "try-error")){NA}else{ck$weighted.kappa}
    }

    #.....................................................................



for(spp in names.spp[2:5]){

load(file=paste("~/Documents/PREDICTED/",an.ID,"model deviation arrays/", spp, "M array.RData", 
                   sep=""))

m<-matrix(NA, 1112, 926)

for(i in 1:1112){
  for(j in 1:926){
    
m[i,j]<-pixelCohen(M[i,j,,])
    
  }
}

save(m, file=paste("~/Documents/PREDICTED/",an.ID,"model deviation arrays/", spp, "m wk map.RData", 
                   sep=""))}


require(RColorBrewer)
source('~/Documents/WORKFLOW/Plotting functions/image scale.R', chdir = TRUE)
img<-readPNG("~/Documents/Presentations/BES 2012/Images/M2000122-2000152.uk.sstp.AVH.L3_median.01may00-31may00.v1.20122500252.rsg_grey.png")

#Load geo.matrix
load(file="/Users/annakrystalli/Documents/CPR/DATA/RAW DATA/SeaWiFS/r files/geo.matrix.RData")
lon<-c(geo.matrix[1,,1],11.01279)
lat<-c(rev(geo.matrix[,1,2]), 61.00876)

spp.labels<-c(expression(italic("C. helgolandicus")), 
              expression(italic("C. finmarchicus")),
              expression(italic("M. lucens")),
              expression(italic("C. typicus")),
              expression(italic("T. longicornis")))

names(spp.labels)<-names.spp
plot.col<-"gray20"
cols<-c(plot.col, rev(brewer.pal(7, name="RdYlBu")))
margin=10
cex.axis=0.8


png(filename = "~/Documents/THESIS/RF Chapter/Figures/wk map.png", 
    pointsize=12, units = "in", res=200, width=6, height=5)

par(mfrow=c(2,3), mgp=c(1.8, 0.6 , 0),las=1, col.axis=plot.col, 
   family="Helvetica", font.axis=2 , font.lab=2, mar=c(4, 3, 2,0.5),
   cex.axis=cex.axis)

for(spp in names.spp){
  
  
  load(file=paste("~/Documents/PREDICTED/",an.ID,"model deviation arrays/", 
                  spp, "m wk map.RData", sep=""))
  mp<-m
  mp[img==1]<- -(1+(2/7))
  
  image(t(mp[1112:1,]), x=lon, y=lat, col=cols, zlim=c(-(1+(2/7)), 1),
        main=spp.labels[spp], cex.axis=cex.axis, xlab="", ylab="")
    box("plot", lwd=1.5)}

  par(mar=c(4,4, 2, margin))
 image.scale(z=m,  col = cols[-1], ylab = expression(kappa[italic(W)]),
             cex.axis=cex.axis*0.9, axis.pos=2, add.axis=TRUE) 


dev.off()

