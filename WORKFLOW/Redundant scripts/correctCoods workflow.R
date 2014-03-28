rm(list=ls())

dat<-read.csv("~/Downloads/ThornwickHcoords.csv")
bar<-read.csv("~/Downloads/barriercoordinates.csv")

source('~/Documents/WORKFLOW/Redundant scripts/correctCoods functions.R', chdir = TRUE)

dat<-dat[-which(is.na(dat$y)),]
wall.dat<-dat[dat$y>0, 3:4]
boulder.dat<-dat[dat$y<0, 3:4]

wall.bar<-bar[1:14,]
wall.bar<-rbind(wall.bar, c(max(dat$x)+1,0))
wall.bar<-rbind(c(min(dat$x)-1,0), wall.bar)


boulder.bar<-bar[15:28,]
boulder.bar<-rbind(boulder.bar, c(max(dat$x)+1,0))
boulder.bar<-rbind(c(min(dat$x)-1,0), boulder.bar)


new.wall<-correctCoods(data=wall.dat, barrier.dat=wall.bar, dat.type="wall")
new.boulder<-correctCoods(data=boulder.dat, barrier.dat=boulder.bar, 
                          dat.type="boulder")


new.thornwick<-dat
new.thornwick[new.thornwick$y>0, 3:4]<-new.wall
new.thornwick[new.thornwick$y<0, 3:4]<-new.boulder

write.csv(new.thornwick)






plot(dat$y~dat$x, cex=0.5, ylim=c(-100,80))
lines(wall.bar$y ~ wall.bar$x, type="b", cex=0.1, pch=21, bg="black")
lines(boulder.bar$y ~ boulder.bar$x, type="b", cex=0.1, pch=21, bg="black")
abline(h=0, lty=2)
text(dat$x, dat$y, labels=paste(dat[,1], dat[,2]), cex=0.4,pos=4)

require(png)

png(filename = "~/Downloads/Sneaky data points.png",
    width = 1500, height = 1200, units = "px", pointsize = 9,
    bg = "white",  res = 300)

plot(jitter(dat$y)~jitter(dat$x), cex=0.5, ylim=c(-100,80))
lines(wall.bar$y ~ wall.bar$x, type="b", cex=0.1, pch=21, bg="black")
lines(boulder.bar$y ~ boulder.bar$x, type="b", cex=0.1, pch=21, bg="black")
abline(h=0, lty=2)
text(dat$x, dat$y, labels=paste(dat[,1], dat[,2]), cex=0.4,pos=4)

dev.off()