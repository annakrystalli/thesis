#Set up
rm(list = ls())

require(maps)

    #Load processed CPR
      load(file="/Users/annakrystalli/Documents/CPR/DATA/RAW DATA/SeaWiFS/r files/CPR clean & tow geoloc.RData")
      
    #Plot cpr.dat (cpr spl coordinates)
      map("world", xlim=c(-4,11), ylim=c(51,61), fill=TRUE, resolution=0, mar = c(0.1, 0.1, par("mar")[3], 0.1))
      segments(x0=cpr.dat$lon, y0=cpr.dat$lat, x1=cpr.dat$lon1, y1=cpr.dat$lat1, lwd=0.5, col=1:20)
      segments(x0=cpr.dat$lon, y0=cpr.dat$lat, x1=cpr.dat$lon2, y1=cpr.dat$lat2, lwd=0.5, col=1:20)
      map.axes()

    #Plot cpr.overlap.dat (pixellated cpr samples)
      map("world", xlim=c(-4,11), ylim=c(51,61), fill=TRUE, mar = c(0.1, 0.1, par("mar")[3], 0.1))
      points(x=as.numeric(levels(y.plot0$lon))[y.plot0$lon], 
             y=as.numeric(levels(y.plot0$lat))[y.plot0$lat], pch=20, cex=0.5)
      points(x=as.numeric(levels(y.plot1$lon))[y.plot1$lon], 
             y=as.numeric(levels(y.plot1$lat))[y.plot1$lat], pch=20, cex=0.5, col="red")

legend(4, 52, c("present", "absent"), fill=c("indianred3", "dodgerblue3"), 
       horiz=TRUE, border="black", bty="n", text.col="white")

    #plot sample pixel locations on a satellite image
    
      require(png)
      
      img.val<-readPNG("~/Documents/SATELLITE/monthly/fcomp_simplified_side/images/M1997182-1997212-front_step4_sstp.AVH.L3_simplified_side_fcomp.01jul97-31jul97.v1m2.20122501715.rsg_grey.png")
      img.val<-img.val[,,1]*255
      img.val[cbind(cpr.overlap.dat$r.ind,cpr.overlap.dat$c.ind)]<--255
cols<-grey(0:(length(unique(as.vector(img.val)))-1)/length(unique(as.vector(img.val))))
      image(z=t(img.val[dim(img.val)[1]:1,]),x=lon, y=lat, 
             xlab="", ylab="", cex.axis=0.8, col=cols)
box("plot", lwd=1.5)


img.mat<-matrix(0, ncol=926, nrow=1112)
img.mat[img.val==0]<-0.5
img.mat[cbind(cpr.overlap.dat$r.ind,cpr.overlap.dat$c.ind)]<-1

cols<-rev(grey(0:(length(unique(as.vector(img.mat)))-1)/length(unique(as.vector(img.mat)))))
cols[1]<-"white"
image(z=t(img.mat[dim(img.mat)[1]:1,]),x=lon, y=lat, 
      xlab="", ylab="", cex.axis=0.8, col=cols)
box("plot", lwd=1.5)


      #Load latest trainind dataset
      load(file="/Users/annakrystalli/Documents/TRAINING DATA/Latest training save.RData")

        data.col<-X[,22:45]
        
        data.info<-vector("list", dim(data.col)[2])
        names(data.info)<-names(data.col)
        
        data.stats<-data.frame(matrix(NA, nrow=dim(data.col)[2], ncol=4), row.names=names(data.col))
        names(data.stats)<-c("NTP", "0", "254", "255")
        
        for(i in 1:dim(data.col)[2]){
                data.info[i]<-tabulateData(data.col[i])

                try(data.stats[i,1]<-data.info[[i]][data.info[[i]][,1]=="NTP",2], silent=TRUE)
                try(data.stats[i,2]<-data.info[[i]][data.info[[i]][,1]==0,2], silent=TRUE)
                try(data.stats[i,3]<-data.info[[i]][data.info[[i]][,1]==254,2], silent=TRUE)
                try(data.stats[i,4]<-data.info[[i]][data.info[[i]][,1]==255,2], silent=TRUE)
                
                }

              m.dat<-data.stats[1:12,]
              w.dat<-data.stats[13:24,]

w.data<-data.frame(NTP=w.dat[,1], 
                   cloud=w.dat[cbind(1:12,c(2,3,2,3,3,2,2,2,2,2,2,2))], 
                   land=w.dat[cbind(1:12, c(rep(4,times=6),rep(NA,times=6)))], 
                   row.names=row.names(w.dat))
w.data$total=rowSums(w.data[,1:3], na.rm=TRUE)





barplot(as.matrix(t(w.data[,1:3])),  
        col=c("aquamarine","darkseagreen3","darkslategrey"), 
        legend.text=names(w.data[1:3]), 
        args.legend=list(bty="n",horiz=TRUE, cex=0.7, x="topleft"),
        border="white",ylim=c(0,320000),
        cex.axis=0.7,
        cex.names=0.6,
        ylab="Samples lost",
        main="Weekly")

m.data<-data.frame(NTP=m.dat[,1], 
                   cloud=m.dat[cbind(1:12,c(2,3,2,3,3,2,2,2,2,2,2,2))], 
                   land=m.dat[cbind(1:12, c(rep(4,times=6),rep(NA,times=6)))], 
                   row.names=row.names(m.dat))
m.data$total=rowSums(m.data[,1:3], na.rm=TRUE)
m.data[is.na(m.data)]<-0





barplot(as.matrix(t(m.data[,1:3])),  
        col=c("aquamarine","darkseagreen3","darkslategrey"), 
        legend.text=names(m.data[1:3]), 
        args.legend=list(bty="n",horiz=TRUE, cex=0.7),
        border="white",ylim=c(0,320000),
        cex.axis=0.7,
        cex.names=0.6,
        ylab="Samples lost",
        main="Monthly")



    tabulateData<-function(data.col){
      tb<-list(data.frame(table(data.col)))
      names(tb[[1]])<-c("DN", "freq")
      tb
    }


save(data.stats, data.info,  file="~/Documents/SATELLITE/match validation/data.stats.RData")      
load(file="~/Documents/SATELLITE/match validation/data.stats.RData")
rm(data.info)
rm(data.stats)


        f.dat[,3]<-as.numeric(f.dat[,2])/186432
        tb.t$prop=as.numeric(tb.t[,3])/rep(as.numeric(f.dat[,2]), times=13)        

plot(jitter(master.w$calfin)~jitter(master.w$T.w), pch=20, cex=0.4)
plot(jitter(master.m$calfin)~jitter(master.m$T.m), pch=20, cex=0.4)

plot(jitter(master.w$calfin)~jitter(master.w$Tf.w), pch=20, cex=0.4)
plot(jitter(master.m$calfin)~jitter(master.m$Tf.m), pch=20, cex=0.4)

plot(jitter(master.w$calfin)~jitter(log(master.w$Ch.w)), pch=20, cex=0.4)
plot(jitter(master.m$calfin)~jitter(log(master.m$Ch.m)), pch=20, cex=0.4)

#NA...plot(jitter(master.w$calfin)~jitter(master.w$Chf.w), pch=20, cex=0.4)
plot(jitter(master.m$calfin)~jitter(master.m$Chf.m)), pch=20, cex=0.4)   

plot(jitter(master.w$calfin)~jitter(master.w$Sed.w), pch=20, cex=0.4)
plot(jitter(master.m$calfin)~jitter(master.m$Sed.m), pch=20, cex=0.4)

#NA...plot(jitter(master.w$calfin)~jitter(master.w$Sedf.w), pch=20, cex=0.4)
plot(jitter(master.m$calfin)~jitter(master.m$Sedf.m), pch=20, cex=0.4)

plot(jitter(master.w$calfin)~jitter(master.w$fdens.w), pch=20, cex=0.4)
plot(jitter(master.m$calfin)~jitter(master.m$fdens.m), pch=20, cex=0.4)

plot(jitter(master.w$calfin)~jitter(master.w$fdist.w), pch=20, cex=0.4)
plot(jitter(master.m$calfin)~jitter(master.m$fdist.m), pch=20, cex=0.4)

plot(jitter(master.w$calfin)~jitter(master.w$fside.w), pch=20, cex=0.4)
plot(jitter(master.m$calfin)~jitter(master.m$fside.m), pch=20, cex=0.4)

plot(jitter(master.w$chlorophyll_index)~jitter(log(master.w$Ch.w)), pch=20, cex=0.4)
plot(jitter(master.m$chlorophyll_index)~jitter(log(master.m$Ch.m)), pch=20, cex=0.4)

hist(master.w$calfin)
hist(master.w$calfin[master.w$calfin!=0])

hist(master.w$T.w)
hist(master.w$Tf.w)
hist(master.w$Tf.w[master.w$Tf.w>0.0001])

hist(log(master.w$Ch.w))
hist(master.w$Chf.w)
hist(master.w$Ch.w[master.w$Ch.w!=0])
names(master.m)


#Quick t test

spp.m<-master.w$metrilu

  
aov.res<-aov(spp.m~master.w$fside.w)
summary(aov.res)
print(model.tables(aov.res,"means"),digits=3)


kw.res<-kruskal.test(spp.m~master.w$fside.w)
kw.res

#COLD (0) VS WARM (1)
t.test.dat<-master.w[master.w$fside.w!=0,]
t.test.dat$fside.w[t.test.dat$fside.w==-1]<-0
t.test(t.test.dat$metrilu~t.test.dat$fside.w)
wilcox.test(t.test.dat$metrilu~t.test.dat$fside)

#COLD (1) VS FRONT (0)
t.test.dat<-master.w[master.w$fside.w!=1,]
t.test.dat$fside.w[t.test.dat$fside.w==-1]<-1
t.test(t.test.dat$metrilu~t.test.dat$fside.w)
wilcox.test(t.test.dat$metrilu~t.test.dat$fside.w)

#WARM (1) VS FRONT (0)
t.test.dat<-master.w[master.w$fside.w!=-1,]
t.test(t.test.dat$metrilu~t.test.dat$fside.w)
wilcox.test(t.test.dat$metrilu~t.test.dat$fside.w)



na.x<-which(is.na(x))
na.norm.x.m<-which(is.na(norm.x.m))

which(is.na(master.w))
which(is.na(master.m))
master.m$time[na.mmtime]