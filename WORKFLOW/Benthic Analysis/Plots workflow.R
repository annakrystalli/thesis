
load(file="OVERALL RESULTS.RData")  
load(file="RESULTS.RData") 



figure2(res.both, res.var, res.spp, cols=c("sienna4",
                                           "forestgreen", 
                                           "deepskyblue4",
                                           "orchid",
                                           "darkslateblue",
                                           "peru"),
        lwds=7)

figure3(res, t=c(1,4,13), pt)

plotPanel1(res.both, res.var, res.spp, an.ID="both", y="spp", 
           cols=brewer.pal(10,"BrBG"),
           zlim=c(-12, 8), ylim=range(res.both$n),
           l.col="darkslategray3",
           f.col="darkslategray3", f=0.3,
           wth=1.1, hgt=6, pt=10,   FD.scale=c(0,45),
           colB="coral3", colC="chartreuse2", lwdl=3,
           mar.scale=c(5.5, 5, 1.5, 1),
           mar.image=c(5.5, 4.5, 1.5, 1),
           mar.side=c(5.5, 0.4, 1.5, 0.4))


figure5(res.both, res.var, res.spp, cols=brewer.pal(10,"BrBG"),
        zlim=c(-12, 8), ylim=range(res.both$n),
        l.col="darkslategray3",
        f.col="darkslategray3", cax=0.8,
        widths=c(2.5,10,1,1,1,10,1,1,1), heights=4, pt=9,   FD.scale=c(0,45),
        mar.scale=c(5.5, 5, 1.5, 0),
        mar.image=c(5.5, 4.5, 1.5, 0.4),
        mar.side=c(5.5, 0.25, 1.5, 0.4))








#...VARIOUS PLOTS..........................................

scatterPlots(res, an.ID, pt=10)





scatterPlots(res.both, res.var, res.spp, pt=10, cex.p=0.19, col="black", 
             bg=sapply(c(1, 0.5, 0.1), FUN=function(x){grey(x, alpha=0.6)}))








an.ID<-"var"
load(file=paste("plots/",an.ID," TD vs #spp data.RData"))

#Plot cluster TD vs cluster size (funnel) for each number of traits
for(i in 1:length(C.SIZES)){
  plot(unlist(TDs[[i]])~C.SIZES[[i]]$cluster.size, main=names(C.SIZES[i]), pch=21, col=grey(level=0.2,alpha=0.3),
       bg=grey(level=0.2,alpha=0.3), xlab="cluster size", ylab="cluster TD",  ylim=c(0,100), xlim=c(0,200))}     



# plot standard deviation of clusters from expected TD for cluster size....SD
 .....#BOTH.....
 an.ID<-"both"
      png(paste("plots/",an.ID, " SD plots.png", sep=""), width=8.1, height=8.1, units="in", res=200, pointsize=15)     
      par(mfrow=c(4,4), mar=c(4,4,2.5,0.1), oma=c(0.1,0.1,0.1,0.1))
      for(i in 1:length(res.both$SE)){
        plot(res.both$SE[[i]][,"Ddevsd"]~res.both$SE[[i]][,"th"], 
             ylim=c(-12, 10), xlim=c(0,1), pch=21, cex=0.75, col="white",
             bg=grey(level=0.2,alpha=0.4), cex.main=0.9,
             xlab=expression(h[italic(n)]), ylab="SDs from mean",
             main=paste("A) t = ",res.both$var[i], "  s = ", res.both$n[i]))
        abline(h=0, lwd=1, col="grey")
        abline(h=-2, lwd=0.6, lty=2)
        abline(h=2, lwd=0.6, lty=2)
      }
      
      dev.off()



#....VAR
  an.ID<-"var"
    png(paste("plots/",an.ID, " SD plots.png", sep=""), width=8.1, height=8.1, units="in", res=200, pointsize=15)     
    par(mfrow=c(3,3), mar=c(4,4,2.5,0.1), oma=c(0.1,0.1,0.1,0.1))
    for(i in 1:length(res.var$SE)){
      plot(res.var$SE[[i]][,"Ddevsd"]~res.var$SE[[i]][,"th"], 
           ylim=c(-12, 10), xlim=c(0,1), pch=21, cex=0.75, col="white",
           bg=grey(level=0.2,alpha=0.4), cex.main=0.9,
           xlab=expression(h[f]), ylab="SDs from mean",
           main=paste("A) t = ",res.var$var[i], "  s = ", res.var$n[i]))
      abline(h=0, lwd=1, col="grey")
      abline(h=-2, lwd=0.6, lty=2)
      abline(h=2, lwd=0.6, lty=2)
    }
    
    dev.off()


#....SPP
  an.ID<-"spp"
    png(paste("plots/",an.ID, " SD plots.png", sep=""), width=8.1, height=8.1, units="in", res=200, pointsize=15)     
    par(mfrow=c(3,3), mar=c(4,4,2.5,0.1), oma=c(0.1,0.1,0.1,0.1))
    for(i in 1:length(res.spp$SE)){
      plot(res.spp$SE[[i]][,"Ddevsd"]~res.spp$SE[[i]][,"th"], 
           ylim=c(-12, 10), xlim=c(0,1), pch=21, cex=0.75, col="white",
           bg=grey(level=0.2,alpha=0.4), cex.main=0.9,
           xlab=expression(h[f]), ylab="SDs from mean",
           main=paste("A) t = ",res.spp$var[i], "  s = ", res.spp$n[i]))
      abline(h=0, lwd=1, col="grey")
      abline(h=-2, lwd=0.6, lty=2)
      abline(h=2, lwd=0.6, lty=2)
    }
    
    dev.off()


