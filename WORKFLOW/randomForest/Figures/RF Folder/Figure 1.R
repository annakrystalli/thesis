rm(list=ls())

#plot sample pixel locations on a satellite image

#Load geo.matrix
load(file="/Users/annakrystalli/Documents/CPR/DATA/RAW DATA/SeaWiFS/r files/geo.matrix.RData")
lon<-c(geo.matrix[1,,1],11.01279)
lat<-c(rev(geo.matrix[,1,2]), 61.00876)


require(png)

png(filename = "~/Documents/THESIS/RF Chapter/Figures/Figure1a.png", 
    pointsize=10, units = "in", res=300, width=8*1/3, height=11.7*1.7/5)

    layout(matrix(data=c(1,2), nrow=2, ncol=1), widths=c(1,1), heights=c(10,2))
    
    par(las=1, mar=c(3, 3, 2, 2) + 0.1,  
        family="Helvetica", font.axis=1 )


load(file="~/Documents/SATELLITE/Associated data/r files/Bathymetry.RData")
img<-bath.m
img2<-readPNG("~/Documents/Presentations/BES 2012/Images/S2000122-nLw_555.median.01may00-_grey.png")

img[img2==1]<-NA
img2[img2!=1]<-NA

bath<-raster(img, -4,11,51,61, crs="+proj=longlat +datum=WGS84")
land<-raster(img2, -4,11,51,61, crs="+proj=longlat +datum=WGS84")


par(las=1, mar=c(3, 3, 2, 2) + 0.1,  oma=c(0.2,0.2,0.2,0.2), mex=0.5,
    family="Helvetica", font.axis=2, xpd=NA)

contour(bath, nlevels=35, axes=F, labcex=0.8, col="black", cex.axis=0.8)
box("plot", lwd=1.5) 
axis(1, at=seq(-4, 10, by=2), cex.axis=0.8)
axis(2, at=seq(52, 60, by=2), cex.axis=0.8)
plot(land, col="black", add=T, legend=F)

plot.new()

par(family="Helvetica", font=1 , font=2)
mtext("Bathymetry", side=1, line=1,outer=F, cex=0.9)

dev.off()

#...Tf.................................................................................





png(filename = "~/Documents/THESIS/RF Chapter/Figures/Figure1b.png", 
    pointsize=8, units = "in", res=200, width=8*1/3, height=11.7*1.7/5)

    img<-readPNG("~/Documents/SATELLITE/monthly/avhrr_sst_fronts/images/M2000122-2000152.uk.front_step4_sstp.AVH.L3_fcomp.01may00-31may00.v1.20122500252.rsg_grey.png")

layout(matrix(data=c(1,2), nrow=2, ncol=1), widths=c(1,1), heights=c(10,2))

    par(las=1, mar=c(3, 3, 2, 2) + 0.1,  oma=c(0.2,0.2,0.2,0.2), mex=0.5,
        family="Helvetica", font.axis=2 )

          FUN.c<-colorRamp(c( "gray70", "black" ))
          
          cols<-rgb(FUN.c(sort(unique(as.vector(img)))), maxColorValue=255)
          
          
          cols[sort(unique(as.vector(img)))==1]<-"black"
          cols[sort(unique(as.vector(img)))==0]<-"white"

    image(z=t(img[dim(img)[1]:1,]),
          x=lon, y=lat, 
          col=cols, xlab="", ylab="", cex.axis=1)
    box("plot", lwd=1.5)



  par(las=1, mar=c(7, 3, 1, 2) + 0.1,  
      family="Helvetica", font.axis=1 , font.lab=2, mgp=c(4,1,0))

            scale<-matrix(seq(0,1, length.out=926), nrow=50, ncol=926, byrow=TRUE)
            cols<-rgb(FUN.c(sort(unique(as.vector(img)))), maxColorValue=255)
            cols<-c("#F0F8FF",cols)

    image(z=t(scale),axes=FALSE,
          col=cols, xlab="Temperature front intensity", ylab="", cex.lab=1.1)
          

    axis(1, at=seq(0, 1, length.out=6), labels=round(seq(0, 0.25, length.out=6), 2),
         cex.axis=0.8)

dev.off()


#...Chfside.................................................................................

png(filename = "~/Documents/THESIS/RF Chapter/Figures/Figure1c.png", 
    pointsize=8, units = "in", res=200, width=8*1/3, height=11.7*1.7/5)

img<-readPNG("~/Documents/Presentations/BES 2012/Images/M2000122-2000152-front_step2_chl_oc5.SEA.L3_simplified_side_oc5_fcomp.01may00-31may00.v2m2.20122650138.rsg_grey.png")
img<-img[,,1]

layout(matrix(data=c(1,2), nrow=2, ncol=1), widths=c(1,1), heights=c(10,2))

par(las=1, mar=c(3, 3, 2, 2) + 0.1, oma=c(0.2,0.2,0.2,0.2), mex=0.5,
    family="Helvetica", font.axis=2 )



cols<-c("black", "grey50", "black", "grey90")


image(z=t(img[dim(img)[1]:1,]),
      x=lon, y=lat, 
      col=cols, xlab="", ylab="", cex.axis=1)
box("plot", lwd=1.5)

par(las=1, mar=c(7, 3, 1, 2) + 0.1, 
    family="Helvetica", font.axis=1 , font.lab=2, mgp=c(4,1,0))

scale<-matrix(c(rep(0.392156862745098, times=300),
                rep(0, times=13),
                rep(0.588235294117647, times=300), 
                rep(0, times=13),
                rep(0.784313725490196, times=300)),
              nrow=50, ncol=926, byrow=TRUE)

cols<-c("white", "grey50", "black", "grey90")          

image(z=t(scale),axes=FALSE,
      col=cols, xlab="Chl a front side", ylab="", cex.lab=1.1)
box("plot", lwd=1.5, col="white")          

axis(1, at=c(0.165, 0.5, 0.833), labels=c("low", "front", "high"), 
     cex.axis=0.8, tick=FALSE)

dev.off()