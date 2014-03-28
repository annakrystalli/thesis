#plot sample pixel locations on a satellite image

#Load geo.matrix
load(file="/Users/annakrystalli/Documents/CPR/DATA/RAW DATA/SeaWiFS/r files/geo.matrix.RData")
lon<-c(geo.matrix[1,,1],11.01279)
lat<-c(rev(geo.matrix[,1,2]), 61.00876)


require(png)

par(las=1, mar=c(3, 3, 2, 2) + 0.1,  
    family="Helvetica", font.axis=1 )

par(bg="black", col="white",font=1, fg="white", col.axis="white",
    col.main="white", col.lab="white")

#...Tf.................................................................................
par(bg="black", col="white",font=1, fg="white", col.axis="white",
    col.main="white", col.lab="white")

img<-readPNG("~/Documents/SATELLITE/weekly/avhrr_sst_fronts/images/M2000129-2000136.uk.front_step4_sstp.AVH.L3_fcomp.08may00-15may00.v1.20122781342.rsg_grey.png")


layout(matrix(data=c(1,2), nrow=2, ncol=1), widths=c(1,1), heights=c(10,2))

      par(las=1, mar=c(3, 3, 2, 2) + 0.1,  oma=c(0.2,0.2,0.2,0.2), mex=0.5,
          family="Helvetica", font.axis=2 )
      
                          FUN.c<-colorRamp(c( "#F0FFFF", "#76EEC6" ))
                          
                          cols<-rgb(FUN.c(sort(unique(as.vector(img)))), maxColorValue=255)
                          
                          
                          cols[sort(unique(as.vector(img)))==1]<-"white"
                          cols[sort(unique(as.vector(img)))==0]<-"black"
                  
                  image(z=t(img[dim(img)[1]:1,]),
                        x=lon, y=lat, 
                        col=cols, xlab="", ylab="", cex.axis=0.8)
                  box("plot", lwd=1.5)
      
      par(las=1, mar=c(7, 3, 1, 2) + 0.1,  
          family="Helvetica", font.axis=1 , font.lab=2, mgp=c(4,1,0))
      
                  scale<-matrix(seq(0,1, length.out=926), nrow=50, ncol=926, byrow=TRUE)
                  cols<-rgb(FUN.c(sort(unique(as.vector(img)))), maxColorValue=255)
                  cols<-c("#F0F8FF",cols)
                  image(z=t(scale),axes=FALSE,
                          col=cols, xlab="Temperature front intensity", ylab="", cex.lab=1.1)
                  box("plot", lwd=1.5)          
      
                  axis(1, at=seq(0, 1, length.out=6), labels=round(seq(0, 0.25, length.out=6), 2))
      


#...Chf.................................................................................

img<-readPNG("~/Documents/Presentations/BES 2012/Images/S2000122-chl_oc5.front_step2.fcomp.01may00-.rsg_grey.png")


layout(matrix(data=c(1,2), nrow=2, ncol=1), widths=c(1,1), heights=c(10,2))

  par(las=1, mar=c(3, 3, 2, 2) + 0.1, col.axis="darkslategray", oma=c(0.2,0.2,0.2,0.2), mex=0.5,
      family="Helvetica", font.axis=2 )
  
          FUN.c<-colorRamp(c( "aquamarine1","aquamarine4"))
          
          cols<-rgb(FUN.c(sort(unique(as.vector(img)))), maxColorValue=255)
          
          
          cols[sort(unique(as.vector(img)))==1]<-"#EEE8CD"
          cols[sort(unique(as.vector(img)))==0]<-"white"
          
          image(z=t(img[dim(img)[1]:1,]),
                x=lon, y=lat, 
                col=cols, xlab="", ylab="", cex.axis=0.8)
          box("plot", lwd=1.5)
  
  par(las=1, mar=c(7, 3, 1, 2) + 0.1, col.axis="darkslategray", 
      family="Helvetica", font.axis=1 , font.lab=2, mgp=c(4,1,0))
  
        scale<-matrix(seq(0,1, length.out=926), nrow=50, ncol=926, byrow=TRUE)
        cols<-rgb(FUN.c(sort(unique(as.vector(img)))), maxColorValue=255)
        cols<-c("white",cols)
        image(z=t(scale),axes=FALSE,
              col=cols, xlab="Chl a front intensity", ylab="", cex.lab=1.1)
        box("plot", lwd=1.5)          
        
        axis(1, at=seq(0, 1, length.out=6), labels=round(seq(0, 0.25, length.out=6), 2))




#...Sed f.................................................................................

img<-readPNG("~/Documents/Presentations/BES 2012/Images/S2000122-nLw_555.front_step2.fcomp.01may00-.rsg_grey.png")


layout(matrix(data=c(1,2), nrow=2, ncol=1), widths=c(1,1), heights=c(10,2))
  
  par(las=1, mar=c(3, 3, 2, 2) + 0.1, col.axis="darkslategray", oma=c(0.2,0.2,0.2,0.2), mex=0.5,
      family="Helvetica", font.axis=2 )
          
          FUN.c<-colorRamp(c( "beige","bisque4"))
          
          cols<-rgb(FUN.c(sort(unique(as.vector(img)))), maxColorValue=255)
          
          
          cols[sort(unique(as.vector(img)))==1]<-"#EEE8CD"
          cols[sort(unique(as.vector(img)))==0]<-"white"
          
          image(z=t(img[dim(img)[1]:1,]),
                x=lon, y=lat, 
                col=cols, xlab="", ylab="", cex.axis=0.8)
          box("plot", lwd=1.5)

  par(las=1, mar=c(7, 3, 1, 2) + 0.1, col.axis="darkslategray", 
      family="Helvetica", font.axis=1 , font.lab=2, mgp=c(4,1,0))
    
        scale<-matrix(seq(0,1, length.out=926), nrow=50, ncol=926, byrow=TRUE)
        cols<-rgb(FUN.c(sort(unique(as.vector(img)))), maxColorValue=255)
        cols<-c("white",cols)
        image(z=t(scale),axes=FALSE,
              col=cols, xlab="SPM proxy front intensity", ylab="", cex.lab=1.1)
        box("plot", lwd=1.5)          
        
        axis(1, at=seq(0, 1, length.out=6), labels=round(seq(0, 0.25, length.out=6), 2))


#...fdist.................................................................................

img<-readPNG("~/Documents/Presentations/BES 2012/Images/M2000122-2000152-front_step4_sstp.AVH.L3_simplified_dist_fcomp.01may00-31may00.v1m2.20122500252.rsg_grey.png")
img<-img[,,1]

            layout(matrix(data=c(1,2), nrow=2, ncol=1), widths=c(1,1), heights=c(10,2))
            
            par(las=1, mar=c(3, 3, 2, 2) + 0.1, oma=c(0.2,0.2,0.2,0.2), mex=0.5,
                family="Helvetica", font.axis=2 )
            
            FUN.c<-colorRamp(c( "#20B2AA" , "black" ))
            
            cols<-rgb(FUN.c(sort(unique(as.vector(img)))), maxColorValue=255)
            
            
            cols[sort(unique(as.vector(img)))==0]<-"white"
            cols[sort(unique(as.vector(img)))==1]<-"black"
            
            image(z=t(img[dim(img)[1]:1,]),
                  x=lon, y=lat, 
                  col=cols, xlab="", ylab="", cex.axis=0.8)
            box("plot", lwd=1.5)
            
            par(las=1, mar=c(7, 3, 1, 2) + 0.1, 
                family="Helvetica", font.axis=1 , font.lab=2, mgp=c(4,1,0))
            
            scale<-matrix(seq(0,1, length.out=926), nrow=50, ncol=926, byrow=TRUE)
            cols<-rgb(FUN.c(sort(unique(as.vector(img)))), maxColorValue=255)
            
            image(z=t(scale),axes=FALSE,
                  col=cols, xlab="Distance to nearest front (km)", ylab="", cex.lab=1.1)
            box("plot", lwd=1.5)          
            
            axis(1, at=seq(0, 1, length.out=6), labels=round(seq(0, 250, length.out=6), 2))

#...fdist.................................................................................

img<-readPNG("~/Documents/Presentations/BES 2012/Images/M2000122-2000152-front_step4_sstp.AVH.L3_simplified_dist_fcomp.01may00-31may00.v1m2.20122500252.rsg_grey.png")
img<-img[,,1]

layout(matrix(data=c(1,2), nrow=2, ncol=1), widths=c(1,1), heights=c(10,2))

par(las=1, mar=c(3, 3, 2, 2) + 0.1, col.axis="darkslategray", oma=c(0.2,0.2,0.2,0.2), mex=0.5,
    family="Helvetica", font.axis=2 )

          FUN.c<-colorRamp(c( "thistle1","mediumpurple4"))
          
          cols<-rgb(FUN.c(sort(unique(as.vector(img)))), maxColorValue=255)
          
          
          cols[sort(unique(as.vector(img)))==0]<-"#EEE8CD"
          cols[sort(unique(as.vector(img)))==1]<-"white"
          
          image(z=t(img[dim(img)[1]:1,]),
                x=lon, y=lat, 
                col=cols, xlab="", ylab="", cex.axis=0.8)
          box("plot", lwd=1.5)

par(las=1, mar=c(7, 3, 1, 2) + 0.1, col.axis="darkslategray", 
    family="Helvetica", font.axis=1 , font.lab=2, mgp=c(4,1,0))
      
      scale<-matrix(seq(0,1, length.out=926), nrow=50, ncol=926, byrow=TRUE)
      cols<-rgb(FUN.c(sort(unique(as.vector(img)))), maxColorValue=255)
      
      image(z=t(scale),axes=FALSE,
            col=cols, xlab="Distance to nearest front (km)", ylab="", cex.lab=1.1)
      box("plot", lwd=1.5)          
      
      axis(1, at=seq(0, 1, length.out=6), labels=round(seq(0, 250, length.out=6), 2))


#...Tfside.................................................................................

img<-readPNG("~/Documents/Presentations/BES 2012/Images/M2000122-2000152-front_step4_sstp.AVH.L3_simplified_side_fcomp.01may00-31may00.v1m2.20122500252.rsg_grey.png")
img<-img[,,1]

layout(matrix(data=c(1,2), nrow=2, ncol=1), widths=c(1,1), heights=c(10,2))

  par(las=1, mar=c(3, 3, 2, 2) + 0.1, col.axis="darkslategray", oma=c(0.2,0.2,0.2,0.2), mex=0.5,
      family="Helvetica", font.axis=2 )
  
            FUN.c<-colorRamp(c( "steelblue","tomato3"))
            
            cols<-rgb(FUN.c(sort(unique(as.vector(img)))[-1]), maxColorValue=255)
            
            
            cols<-c("#EEE8CD", cols)
            
            image(z=t(img[dim(img)[1]:1,]),
                  x=lon, y=lat, 
                  col=cols, xlab="", ylab="", cex.axis=0.8)
            box("plot", lwd=1.5)
  
  par(las=1, mar=c(7, 3, 1, 2) + 0.1, col.axis="darkslategray", 
      family="Helvetica", font.axis=1 , font.lab=2, mgp=c(4,1,0))
  
          scale<-matrix(c(rep(0.392156862745098, times=300),
                          rep(0, times=13),
                          rep(0.588235294117647, times=300), 
                          rep(0, times=13),
                          rep(0.784313725490196, times=300)),
                        nrow=50, ncol=926, byrow=TRUE)
          
          cols<-rgb(FUN.c(sort(unique(as.vector(img)))[-1]), maxColorValue=255)
          cols<-c("white", cols)
         

          image(z=t(scale),axes=FALSE,
                col=cols, xlab="Temperature front side", ylab="", cex.lab=1.1)
          box("plot", lwd=1.5, col="white")          
          
          axis(1, at=c(0.165, 0.5, 0.833), labels=c("cold", "front", "warm"), tick=FALSE)


#...Chfside.................................................................................

img<-readPNG("~/Documents/Presentations/BES 2012/Images/M2000122-2000152-front_step2_chl_oc5.SEA.L3_simplified_side_oc5_fcomp.01may00-31may00.v2m2.20122650138.rsg_grey.png")
img<-img[,,1]

layout(matrix(data=c(1,2), nrow=2, ncol=1), widths=c(1,1), heights=c(10,2))
  
  par(las=1, mar=c(3, 3, 2, 2) + 0.1, oma=c(0.2,0.2,0.2,0.2), mex=0.5,
      family="Helvetica", font.axis=2 )
  
          FUN.c<-colorRamp(c( "#F0FFFF", "#20B2AA"))
          
          cols<-rgb(FUN.c(sort(unique(as.vector(img)))[-1]), maxColorValue=255)
          
          
          cols<-c("white", cols)
          cols[3]<-"#54FF9F"
          
          image(z=t(img[dim(img)[1]:1,]),
                x=lon, y=lat, 
                col=cols, xlab="", ylab="", cex.axis=0.8)
          box("plot", lwd=1.5)
          
  par(las=1, mar=c(7, 3, 1, 2) + 0.1, 
      family="Helvetica", font.axis=1 , font.lab=2, mgp=c(4,1,0))
  
          scale<-matrix(c(rep(0.392156862745098, times=300),
                          rep(0, times=13),
                          rep(0.588235294117647, times=300), 
                          rep(0, times=13),
                          rep(0.784313725490196, times=300)),
                        nrow=50, ncol=926, byrow=TRUE)
          
          cols<-rgb(FUN.c(sort(unique(as.vector(img)))[-1]), maxColorValue=255)
          cols<-c("black", cols)
          cols[3]<-"#54FF9F"          

          image(z=t(scale),axes=FALSE,
                col=cols, xlab="Chl a front side", ylab="", cex.lab=1.1)
          box("plot", lwd=1.5, col="black")          
          
          axis(1, at=c(0.165, 0.5, 0.833), labels=c("low", "front", "high"), tick=FALSE)

#...Ch.................................................................................

img<-readPNG("~/Documents/Presentations/BES 2012/Images/S2000122-chl_oc5.median.01may00-_grey.png")

img[img!=1]<-10^(((img[img!=1]*255)*0.015)-2)
img[img!=1]<-(img[img!=1]-0.015)/(30-0.015)
img[img==min(img)]<-0

layout(matrix(data=c(1,2), nrow=2, ncol=1), widths=c(1,1), heights=c(10,2))

  par(las=1, mar=c(3, 3, 2, 2) + 0.1, col.axis="darkslategray", oma=c(0.2,0.2,0.2,0.2), mex=0.5,
      family="Helvetica", font.axis=2 )
            

            FUN.c<-colorRamp(c( "honeydew2","seagreen"))
            
            cols<-rgb(FUN.c(seq(0,1, length.out=32)), maxColorValue=255)


            cols[sort(unique(as.vector(img)))==1]<-"#EEE8CD"

            
            image(z=t(img[dim(img)[1]:1,]),
                  x=lon, y=lat, 
                  col=cols, xlab="", ylab="", cex.axis=0.8)
            box("plot", lwd=1.5)
  
  par(las=1, mar=c(7, 3, 1, 2) + 0.1, col.axis="darkslategray", 
      family="Helvetica", font.axis=1 , font.lab=2, mgp=c(4,1,0))
  
          scale<-matrix(seq(0,1, length.out=926), nrow=50, ncol=926, byrow=TRUE)
          lab<-seq(0,  0.9090512, length.out=6)
          lab<-as.character(round(10^(((lab*255)*0.015)-2),2))


          cols<-rgb(FUN.c(sort(unique(as.vector(img)))), maxColorValue=255)

          image(z=t(scale),axes=FALSE,
                col=cols, xlab=expression(paste("Chl a concentration (mg m"^"-3",")"))
                , ylab="", cex.lab=1.1)
          box("plot", lwd=1.5)          
          
          axis(1, at=seq(0, 1, length.out=6), labels=lab)
          
#...T.................................................................................

img<-readPNG("~/Documents/Presentations/BES 2012/Images/M2000122-2000152.uk.sstp.AVH.L3_median.01may00-31may00.v1.20122500252.rsg_grey.png")


img[img!=1]<-(img[img!=1]-0.1176471)/(0.8235294-0.1176471)
img[img==min(img)]<-0


layout(matrix(data=c(1,2), nrow=2, ncol=1), widths=c(1,1), heights=c(10,2))
        
          par(las=1, mar=c(3, 3, 2, 2) + 0.1, col.axis="darkslategray", oma=c(0.2,0.2,0.2,0.2), mex=0.5,
              family="Helvetica", font.axis=2 )
          
          
          FUN.c<-colorRampPalette(c( "dodgerblue4","dodgerblue3","dodgerblue2", "gold", "firebrick3", "firebrick4"), space="Lab")
          
          cols<-FUN.c(n=length(unique(as.vector(img))))
          
          
          cols[sort(unique(as.vector(img)))==1]<-"#EEE8CD"
          cols[sort(unique(as.vector(img)))==0]<-"white"
          
          image(z=t(img[dim(img)[1]:1,]),
                x=lon, y=lat, 
                col=cols, xlab="", ylab="", cex.axis=0.8)
          box("plot", lwd=1.5)
  
  par(las=1, mar=c(7, 3, 1, 2) + 0.1, col.axis="darkslategray", 
      family="Helvetica", font.axis=1 , font.lab=2, mgp=c(4,1,0))
  
            scale<-matrix(seq(0,1, length.out=926), nrow=50, ncol=926, byrow=TRUE)
            lab<-seq(0,  18, length.out=7)
            
            
            
            cols<-FUN.c(n=length(unique(as.vector(img))))
            
            image(z=t(scale),axes=FALSE,
                  col=cols, xlab=expression(paste("Temperature (C"^"o",")"))
                  , ylab="", cex.lab=1.1)
            box("plot", lwd=1.5)          
            
            axis(1, at=seq(0, 1, length.out=7), labels=lab)
  
#...Sed.................................................................................

img<-readPNG("~/Documents/Presentations/BES 2012/Images/S2000122-nLw_555.median.01may00-_grey.png")


layout(matrix(data=c(1,2), nrow=2, ncol=1), widths=c(1,1), heights=c(10,2))
  
  par(las=1, mar=c(3, 3, 2, 2) + 0.1, col.axis="darkslategray", oma=c(0.2,0.2,0.2,0.2), mex=0.5,
      family="Helvetica", font.axis=2 )
  
  
          FUN.c<-colorRamp(c( "antiquewhite2","darkgoldenrod4"))
          
          cols<-rgb(FUN.c(sort(unique(as.vector(img)))), maxColorValue=255)
          
          
          cols[sort(unique(as.vector(img)))==1]<-"#EEE8CD"
          
          
          image(z=t(img[dim(img)[1]:1,]),
                x=lon, y=lat, 
                col=cols, xlab="", ylab="", cex.axis=0.8)
          box("plot", lwd=1.5)
  
  par(las=1, mar=c(7, 3, 1, 2) + 0.1, col.axis="darkslategray", 
      family="Helvetica", font.axis=1 , font.lab=2, mgp=c(4,1,0))
  
          scale<-matrix(seq(0,1, length.out=926), nrow=50, ncol=926, byrow=TRUE)
          lab<-round(seq(0.02,  5, length.out=6),1)
          lab[1]<-0.02
          
          
          cols<-rgb(FUN.c(sort(unique(as.vector(img)))), maxColorValue=255)
          
          image(z=t(scale),axes=FALSE,
                col=cols, xlab=expression(paste("SPM proxy (mW cm"^"-2","μm"^"-1","sr"^"-1",")"))
                , ylab="", cex.lab=1.1)
          box("plot", lwd=1.5)          
          
          axis(1, at=seq(0, 1, length.out=6), labels=lab)


#...Bath.................................................................................
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

load(file="~/Documents/SATELLITE/Associated data/r files/Bathymetry.RData")
img<-bath.m
img<-range01(img)
img2<-readPNG("~/Documents/Presentations/BES 2012/Images/S2000122-nLw_555.median.01may00-_grey.png")


layout(matrix(data=c(1,2), nrow=2, ncol=1), widths=c(1,1), heights=c(10,2))

par(las=1, mar=c(3, 3, 2, 2) + 0.1, col.axis="darkslategray", oma=c(0.2,0.2,0.2,0.2), mex=0.5,
    family="Helvetica", font.axis=2 )

FUN.c<-colorRamp(c("lightskyblue3","midnightblue"))
cols<-rgb(FUN.c(sort(unique(as.vector(img)))), maxColorValue=255)

FUN.c<-colorRampPalette(c("deepskyblue2",
                          "deepskyblue4", "navy","navy","navy",
                            "midnightblue","midnightblue"), space="Lab")
n<-length(sort(unique(as.vector(img))))
cols<-FUN.c(n=n)


img[img2==1]<-0
cols[sort(unique(as.vector(img)))==0]<-"#EEE8CD"


image(z=t(img[dim(img)[1]:1,]),
      x=lon, y=lat, 
      col=cols, xlab="", ylab="", cex.axis=0.8)
box("plot", lwd=1.5)

par(las=1, mar=c(7, 3, 1, 2) + 0.1, col.axis="darkslategray", 
    family="Helvetica", font.axis=1 , font.lab=2, mgp=c(4,1,0))

scale<-matrix(seq(0,1, length.out=926), nrow=50, ncol=926, byrow=TRUE)
lab<-round(seq(0, 1130/1000, length.out=6), 1)



cols<-FUN.c(n=n)

image(z=t(scale),axes=FALSE,
      col=cols, xlab="Depth (km)"
      , ylab="", cex.lab=1.1)
box("plot", lwd=1.5)          

axis(1, at=seq(0,1, length.out=6), labels=lab)




#....WinCPR...Chel................................................................

image(z=t(map[dim(map)[1]:1,]))
image(y=seq(51.25, 61.75, by=0.5), x=seq(-3.5, 8.5,by=1),
      t(map[dim(map)[1]:1,]),col=gray(seq(0.9,0,-1/12)), 
      cex.main=0.7,xlab="Longditude",ylab="Latitude",
      cex.axis=0.6, cex.lab=0.6)

#Open Associated gridding files
north.sea.pixel.grid <- read.csv("~/Documents/CPR/DATA/RAW DATA/Associated files/north sea pixel grid.csv", header=F)
linexcoo <- read.csv("~/Documents/CPR/DATA/RAW DATA/Associated files/linexcoo.csv", header=F)
chel<-read.csv("~/Documents/Presentations/BES 2012/Win cpr chel.csv")

north.sea.pixel.gridm<-as.matrix(north.sea.pixel.grid)

map<-matrix(0, ncol=15, nrow=22)

for(i in 1:13){
  map[,i]<-chel$value[match(north.sea.pixel.gridm[,i], chel$pix.ind)]}
map[map<=0]<-0


require(raster)
require(ncdf4)


map<-raster(map)
exttremes<-extent(c(-4, 11,51,62))
map<-setExtent(x=map, ext=exttremes)

plot(map)

extent2<-extent(c(-4, 11,51,61))
map<-crop(map,extent2)
bath.res <- raster(nrow=1112, ncol=926, ext=extent2)
map <- resample(map, bath.res, method='ngb')


img<-readPNG("~/Documents/SATELLITE/weekly/avhrr_sst_fronts/images/M2000129-2000136.uk.front_step4_sstp.AVH.L3_fcomp.08may00-15may00.v1.20122781342.rsg_grey.png")

chel.map<-as.matrix(map)
chel.map[chel.map>0]<-0.5
chel.map[chel.map==0]<-0.001
chel.map[is.na(chel.map)]<-0
chel.map[img==1]<-1


layout(matrix(data=1, nrow=1, ncol=1), widths=1, heights=1)

FUN.c<-colorRamp(c( "darkseagreen1","aquamarine4"))

cols<-rgb(FUN.c(sort(unique(as.vector(chel.map)))), maxColorValue=255)

cols[sort(unique(as.vector(chel.map)))==1]<-"dodgerblue4"
cols[sort(unique(as.vector(chel.map)))==0]<-"dodgerblue3"
cols[sort(unique(as.vector(chel.map)))==0.5]<-"indianred3"
cols[sort(unique(as.vector(chel.map)))==0.001]<-"indianred3"

par(las=1, col.axis="darkslategray", 
    family="Helvetica", font.axis=2 , font.lab=2)

image(z=t(chel.map[dim(chel.map)[1]:1,]),
      x=lon, y=lat, 
      col=cols, xlab="", ylab="", cex.axis=0.8)
box("plot", lwd=1.5)


legend(4, 52, c("present", "absent"), fill=c("indianred3", "dodgerblue3"), 
       horiz=TRUE, border="black", bty="n", text.col="white")

dev.off()

#Pred mMAP........................................................................
img<-readPNG("~/Documents/Presentations/BES 2012/Images/M2000122-2000152.uk.sstp.AVH.L3_median.01may00-31may00.v1.20122500252.rsg_grey.png")


pred.map<-matrix(0.25, ncol=926, nrow=1112)
pred.map[img==1]<-0.5
y.pred<-max.col(chel.oc.m.extra.30$preds)-1
for (i in 1:length(y.pred)){
pred.map[X.pred$r[i],X.pred$c[i]]<-y.pred[i]}



layout(matrix(data=c(1,2), nrow=2, ncol=1), widths=c(1,1), heights=c(10,2))

par(las=1, col.axis="darkslategray",
    family="Helvetica", font.axis=2 )


FUN.c<-colorRamp(c( "darkseagreen1","aquamarine4"))
cols<-rgb(FUN.c(sort(unique(as.vector(pred.map)))), maxColorValue=255)

cols[sort(unique(as.vector(pred.map)))==1]<-"indianred3"
cols[sort(unique(as.vector(pred.map)))==0]<-"dodgerblue3"
cols[sort(unique(as.vector(pred.map)))==0.25]<-"dodgerblue4"
cols[sort(unique(as.vector(pred.map)))==0.5]<-"#EEE8CD"

cols<-c("dodgerblue4","#EEE8CD","dodgerblue3","indianred3")

cols[sort(unique(as.vector(pred.map)))==0.25]<-"dodgerblue3"

image(z=t(pred.map[dim(pred.map)[1]:1,]),
      x=lon, y=lat, 
      col=cols, xlab="", ylab="", cex.axis=0.8)
abline
box("plot", lwd=1.5)

legend(4, 52, c("present", "absent"), fill=c("indianred3", "dodgerblue3"), 
       horiz=TRUE, border="black", bty="n", text.col="white")
