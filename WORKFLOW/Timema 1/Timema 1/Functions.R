all.dat<-read.table(file="raw data/obsTimDataAllBushes.txt", header=T)

all.dat<-cbind(all.dat, area=pi*(all.dat[,"Length"]/2)* (all.dat[,"Width"]/2)/100^2)
all.dat<-cbind(all.dat, volume=3/4*pi*(all.dat[,"area"])* (all.dat[,"Height"]/200))
all.dat<-cbind(all.dat,  densg=all.dat[,"Green"]/all.dat[,"area"])
all.dat<-cbind(all.dat,  denss=all.dat[,"Striped"]/all.dat[,"area"])
all.dat<-cbind(all.dat,  denst=all.dat[,"total"]/all.dat[,"area"])

all.dat<-cbind(all.dat,  mal=all.dat[,"hostAdZ"]*all.dat[,"denss"] + all.dat[,"hostCeZ"]*all.dat[,"densg"])
all.dat<-cbind(all.dat,  ad=all.dat[,"hostAdZ"]*all.dat[,"densg"] + all.dat[,"hostCeZ"]*all.dat[,"denss"])


require(rgeos)
require(raster)


dens.all.res<-computeAORsData(all.data, fun=mean, nr=12, nc=7, fac=10, xlim=c(0,11), 
                              vars=c("gr", "st", "ad", "mal"))


ab.all.res<-computeAORsData(all.data, fun=sum, nr=12, nc=7, fac=10, xlim=c(0,11), 
                            vars=c("gr", "st", "ad", "mal"))


plotAORs(dens.all.res$AORs, dens.all.res$Lines, xlim=c(0,11), cex=0.8)
plotAORs(ab.all.res$AORs, ab.all.res$Lines, xlim=c(0,11), cex=0.8)


dens.all.res<-computeAORsData(all.data, fun=mean, nr=24, nc=14, fac=20, xlim=c(0,11), 
                              vars=c("gr", "st", "ad", "mal"))

ab.all.res<-computeAORsData(all.data, fun=sum, nr=24, nc=14, fac=20, xlim=c(0,11), 
                            vars=c("gr", "st", "ad", "mal"))

#...Plot AORS

plotAORs(dens.all.res$AORs, dens.all.res$Lines, xlim=c(0,12), cex=0.8)
plotAORs(ab.all.res$AORs, ab.all.res$Lines, xlim=c(0,11), cex=0.8)




#__________________________________________________________________________________________

#     STATS
#__________________________________________________________________________________________

full.tab<-NULL
for(i in c("gr", "st", "ad", "mal")){
  full.tab<-rbind(full.tab,AORs[[i]])}

index<-NULL
for(i in c("gr", "st", "ad", "mal")){
  index<-c(index,rep(i,times=dim(AORs[[i]])[1]))}
index<-factor(index)

full.tab<-data.frame(full.tab,index)

full.glm<-glm(dat.oc~dat.ab*index, data=full.tab, family=binomial, weights=n.oc)
summary(full.glm)
summary(anova(full.glm, test="Chisq"))

gI<-all.dat$Green>0
chisq.test(table(all.dat$host,gI))

sI<-all.dat$Striped>0
chisq.test(table(all.dat$host,sI))

chisq.test(table(gI,sI))

chi.ab<-rbind(Green=tapply(all.dat$Green, INDEX=all.dat$host, FUN=sum), 
                Striped=tapply(all.dat$Striped, INDEX=all.dat$host, FUN=sum))

chi.ex<-chisq.test(chi.ab)$expected

chisq.test(chi.ab)$expected


barp<-rbind(G.ab=chi.ab["Green",], G.ex=chi.ex["Green",], 
            S.ab=chi.ab["Striped",], S.ex=chi.ex["Striped"])


par(lwd=3, font=1)
barplot(barp, beside=T, col=c("limegreen", "black", "palegreen1", "black"),
        border=c( "forestgreen", "forestgreen", "yellow2", "yellow2"), 
        space=c(0.1,0.4), lwd=1.3, cex.lab=1.3, 
        names.arg=c(expression(italic(Adenostoma)),expression(italic(Ceanothus))),
        legend.text=c("Gr obs", "Gr exp", "St obs", "St exp"),
        args.legend=list(border=c("forestgreen", "forestgreen", "yellow2", "yellow2"),
                         bty="n", x="topleft", cex=0.8))
#__________________________________________________________________________________________

#     PLOTS
#__________________________________________________________________________________________


#...Plot AORS

plotAORs(AORs, Lines, xlim=c(0,11), cex=0.8)
  
#...Plot Histograms of Habitat patch size distribution

par(mfcol=c(2,1), font=1)
          
          hist(all.dat[all.dat$host=="A", "area"], breaks=seq(0,36, by=2), col="black", border="white",
               main=expression(italic(Adenostoma)), ylim=c(0,90), 
               xlab=expression(paste("plant area (m"^2,")", sep="")))
          
                legend("topright", cex=0.8, , bty="n",
                       legend=c(paste("n = ", sum(all.dat$host=="A")),
                                paste("mean area = ",
                                      signif(mean(all.dat$area[all.dat$host=="A"]),
                                             2)),
                                paste("total area = ",
                                      signif(sum(all.dat$area[all.dat$host=="A"]),
                                             3))))
          
          
          hist(all.dat[all.dat$host=="C", "area"], breaks=seq(0,36, by=2), col="black", border="white",
               main=expression(italic(Ceanothus)), ylim=c(0,90), 
               xlab=expression(paste("plant area (m"^2,")", sep="")))
          
                legend("topright", cex=0.8, , bty="n",
                       legend=c(paste("n = ", sum(all.dat$host=="C")),
                                paste("mean area = ",
                                      signif(mean(all.dat$area[all.dat$host=="C"]),
                                             2)),
                                paste("total area = ",
                                      signif(sum(all.dat$area[all.dat$host=="C"]),
                                             3))))


#....Plot densities / counts as a function of area........

    par(mfcol=c(2,3), mgp=c(2,0.6,0), mar=c(4,4,2.5,0.3), lwd=1)
        
        for(i in c("ad", "mal", "denss", "densg", "Striped","Green")){
          
          plotPlantArea(all.dat, i,  density, ylim=c(0,20), cex=1.8, lwd=2.5)}




#...Correlation plot.........................................



        #Calculate correlations
                corA<-cor(all.dat[all.dat$host=="A","Green"],all.dat[all.dat$host=="A","Striped"],
                          method = "spearman")
                
                corC<-cor(all.dat[all.dat$host=="C","Green"],all.dat[all.dat$host=="C","Striped"],
                          method = "spearman")


        #Plot
par(mfrow=c(1,2), lwd=3)

    ylim=c(0,20)
    xlim=c(0,20)

    plot(Striped~Green, data=all.dat[all.dat$host=="A",], bg=grey(0.15,0.2), ylim=ylim,
         xlim=xlim, pch=21, col="white", cex=1.5, main=expression(italic(Adenostoma)))

                        legend("bottomright",
                               legend=substitute(paste(rho, " = ", c, " ***"), 
                                                 list(c=signif(corA,3))),
                               bty="n", cex=0.8)

    abline(0,1, lty=2, lwd=1)

    plot(Green~Striped, data=all.dat[all.dat$host=="C",], bg=grey(0.15,0.2), ylim=ylim,
         xlim=xlim, pch=25, col="white", cex=1.5, main=expression(italic(Ceanothus)))

                          legend("bottomright",
                                 legend=substitute(paste(rho, " = ", c, " ***"), 
                                                   list(c=signif(corC,3))),
                                 bty="n", cex=0.8)

    abline(0,1, lty=2, lwd=1)

#...Frequency histograms.........................................

par(mfrow=c(2,2), lwd=0.5)

        ylim=c(0, 110)
        breaks=seq(0,20, by=1)
        col= c("red",rep("black", times=length(breaks)-2))
        border="white"
        

        hist(all.dat[all.dat$host=="C","Green"],
             col=col, border=border,
             xlim=c(0,20), breaks=breaks, ylim=ylim,
             main=expression(italic(Ceanothus)), xlab="Green")

        hist(all.dat[all.dat$host=="A","Green"], 
             col=col, border=border,
             xlim=c(0,20), breaks=breaks, ylim=ylim,
             main=expression(italic(Adenostoma)),xlab="Green" )

        hist(all.dat[all.dat$host=="C","Striped"], 
             col=col, border=border,
             xlim=c(0,20), breaks=breaks, ylim=ylim,
             main=expression(italic(Ceanothus)), xlab="Striped")

        hist(all.dat[all.dat$host=="A","Striped"], 
             col=col, border=border,
             xlim=c(0,20), breaks=breaks, ylim=ylim,
             main=expression(italic(Adenostoma)),xlab="Striped")



#....Boxplots
Cols<-vector("list",n.var)
names(Cols)<-names(AORs)

Cols[["gr"]]<-c("forestgreen", "limegreen")
Cols[["st"]]<-c("palegreen1", "yellow2")
Cols[["ad"]]<-c("goldenrod3", "gold4")
Cols[["mal"]]<-c("wheat2", "wheat4")

par(mfrow=c(1,5), lwd=2, lty=1, ylim=c(0,20))

boxplot(denss ~ host, data=all.dat[all.dat$denss!=0,], col=Cols[["gr"]][2],
        border=Cols[["gr"]][1], main="Striped", ylim=c(0,20))

boxplot(densg ~ host, data=all.dat[all.dat$densg!=0,], col=Cols[["gr"]][2],
        border=Cols[["gr"]][1], main="Green", ylim=c(0,20))

boxplot(denst ~ host, data=all.dat[all.dat$denst!=0,], col=Cols[["gr"]][2],
        border=Cols[["gr"]][1], main="Total", ylim=c(0,20))

boxplot(ad ~ host, data=all.dat[all.dat$ad!=0,], col=Cols[["ad"]][1],
        border=Cols[["ad"]][2], main="adapted", ylim=c(0,20))
boxplot(mal ~ host, data=all.dat[all.dat$mal!=0,], col=Cols[["ad"]][1],
        border=Cols[["ad"]][2], main="maladapted", ylim=c(0,20))

#__________________________________________________________________________________________

#    FUNCTIONS
#__________________________________________________________________________________________

computeAOR<-function(all.dat, field="denss", nr=12, nc=7, fac=5, xlim, fun=mean){

        ext<-extent(matrix(c(0.5,35.5,-2.5,57.5),2,2, byrow=T))
        
  #Prepare data
        sp.dat<-SpatialPointsDataFrame(SpatialPoints(cbind(all.dat$X, all.dat$Y)), data=all.dat)
        spc.dat<-gBuffer(sp.dat, byid=T, width=sqrt(sp.dat$Width/200*sp.dat$Length/200))
        
        all.dat.oc<-all.dat[all.dat[,field]>0, ]
        sp.dat.oc<-SpatialPointsDataFrame(SpatialPoints(cbind(all.dat.oc$X, all.dat.oc$Y)),
                                          data=all.dat.oc)
        spc.dat.oc<-gBuffer(sp.dat.oc, byid=T, width=sqrt(sp.dat.oc$Width/200*sp.dat.oc$Length/200))
      
        
  #Calculate field specific AOR


      ab.fine<-rasterize(spc.dat, field=field,fun=mean,
                raster(ncols=nc*fac, nrows=nr*fac, 
                       ext=ext), update=T)
      if(fun(c(1,2))==3){ab.fine<-res(ab.fine)[1]^2*ab.fine}
        
      ab.coarse<-aggregate(ab.fine, fact=fac, na.rm=T, fun=fun)
      dat.ab<-as.vector(ab.coarse)

        
        dat.fine<-rasterize(spc.dat, field="PlantID",
                            raster(ncols=nc*fac, nrows=nr*fac, 
                                   ext=ext), update=T)
        
        dat.fine.oc<-rasterize(spc.dat.oc, field="PlantID",
                               raster(ncols=nc*fac, nrows=nr*fac, 
                                      ext=ext), update=T)
        
        dat.coarse<-as.vector(aggregate(dat.fine, fact=fac, na.rm=F,
                                        fun=function(x,...){length(na.omit(unique(x)))}))
        dat.coarse.oc<-as.vector(aggregate(dat.fine.oc, fact=fac, 
                                           na.rm=F,fun=function(x,...){length(na.omit(unique(x)))}))
        dat.oc<-dat.coarse.oc/dat.coarse
        n.oc<-dat.coarse
        if(fun(c(1,2))==3){dat.ab<-dat.ab/dat.coarse.oc
                          xlab="mean no.indiv / occupied patch"}else{xlab="mean density / occupied patch"}
        
      par(mfrow=c(1,1))  

        plot(dat.oc*100~dat.ab, col="white", bg=grey(0.13,0.5), pch=21, cex=1.3, 
             ylab="% patches occupied", xlab=xlab,
             main=field, xlim=xlim)
        

        
        dat<-data.frame(dat.oc, dat.ab, n.oc)
        dat<-dat[n.oc!=0,]
        dat$dat.ab[is.nan(dat$dat.ab)]<-0
        dat$dat.ab[is.na(dat$dat.ab)]<-0
        
        return(dat)

}


computeAORsData<-function(all.data, fun, nr, nc, fac, xlim=c(0,11), vars=c("gr", "st", "ad", "mal")){
  
  n.var<-length(vars)
  
  field.names<-c("densg", "denss", "ad", "mal")
  names(field.names)<-c("gr", "st", "ad", "mal")
  
  AORs<-vector("list",n.var)
  names(AORs)<-vars
  GLMs<-vector("list",n.var)
  names(GLMs)<-vars
  Lines<-vector("list",n.var)
  names(Lines)<-vars
  
  for(i in vars){
    
    AORs[[i]]<-computeAOR(all.dat, field=field.names[i], nr, nc, fac, xlim, fun)
    
    GLMs[[i]]<-glm(dat.oc~dat.ab, data=data.frame(AORs[[i]]), family=binomial, weights=n.oc)
    
    Lines[[i]]<-cbind(y=predict(GLMs[[i]], 
                                newdata=data.frame(dat.ab=seq(0,max(AORs[[i]][,"dat.ab"]), 
                                                              l=25)),
                                type="response"),
                      x=data.frame(x=seq(0,max(AORs[[i]][,"dat.ab"]), l=25)))
    
    print(i)
    print(summary(GLMs[[i]]))}
  
  return(list(AORs=AORs, GLMs=GLMs, Lines=Lines, 
              meta.data=list(sub.res=5/fac, xres=60/nr, yres=35/nc,
                             nr=nr, nc=nc)))}


plotAORs<-function(AORs, Lines, xlim=c(0,11), cex=0.8){
  n.var<-length(AORs)
  
  Cols<-vector("list",n.var)
  names(Cols)<-names(AORs)
  Lty<-vector("list",n.var)
  names(Lty)<-names(AORs)
  Lwd<-vector("list",n.var)
  names(Lty)<-names(AORs)
  Cex<-vector("list",n.var)
  names(Lty)<-names(AORs)
  
  Cols[["gr"]]<-c("forestgreen", "limegreen")
  Cols[["st"]]<-c("palegreen1", "yellow2")
  Cols[["ad"]]<-c("goldenrod3", "gold4")
  Cols[["mal"]]<-c("wheat2", "wheat4")
  
  Lty[["gr"]]<-2
  Lty[["st"]]<-3
  Lty[["ad"]]<-1
  Lty[["mal"]]<-4
  
  Lwd[["gr"]]<-6.5
  Lwd[["st"]]<-6
  Lwd[["ad"]]<-7
  Lwd[["mal"]]<-5.5
  
  Cex[["gr"]]<-1.4
  Cex[["st"]]<-1.2
  Cex[["ad"]]<-1.6
  Cex[["mal"]]<-1
  
  
  dev.off()
  par(family="sans", las=1, font=2)
  
  for(i in c("ad", "gr", "st",  "mal")){
    if(i=="ad"){plot(jitter(dat.oc)~jitter(dat.ab), data=AORs[[i]], 
                     col=Cols[[i]][1], bg=Cols[[i]][2], pch=21, cex=cex, 
                     ylab="proportion patches occupied", 
                     xlab="mean density/ occupied patch",
                     xlim=xlim)}else{
                       points(dat.oc~dat.ab, data=AORs[[i]], 
                              col=Cols[[i]][1], bg=Cols[[i]][2], pch=21, cex=Cex[[i]])  
                     }}
  
  for(i in c("ad", "gr", "st",  "mal")){
    lines(y~x, data=Lines[[i]], col=Cols[[i]][1], lwd=Lwd[[i]], lty=Lty[[i]])}
  
  legend("bottomright", legend=names(unlist(Cex)), lty=unlist(Lty), pch=21,
         pt.cex=unlist(Cex), pt.bg=matrix(unlist(Cols), 4,2,byrow=T)[,2], 
         lwd=unlist(Lwd),
         col=matrix(unlist(Cols), 4,2,byrow=T)[,1], bty="n")
  
  
}
 

plotPlantArea<-function(all.dat, i,  density, ylim=c(0,20), cex=1.8, lwd=2.5){
 
            main.labels<-c("green", "striped","adapted", "maladapted", "green", "striped")
            names(main.labels)<-c("densg", "denss", "ad", "mal", "Green", "Striped")
            
            pch<-c(21, 25)[all.dat$host]
            cols<-"white"
          
            bgs<-c("red", "blue")[(all.dat[,i]!=0)+1]
            fm.pl<-formula(paste(i, "~ area"))
            
  
      if(i %in% c("Green", "Striped")){
        
        plot(fm.pl, data=all.dat, ylim=ylim, ylab="individuals / plant",
             xlab=expression(area (m^2)), pch=pch, bg=bgs, col=cols, cex=cex, main=main.labels[i])}else{
               
              plot(fm.pl, data=all.dat, ylim=ylim, ylab=expression(density (ind /m^2)),
                   xlab=expression(area (m^2)), pch=pch, bg=bgs, col=cols, cex=cex, main=main.labels[i])
      
      
                      if(i=="ad"){legend("topright", legend=c(levels(all.dat$host), "occupied", "unoccupied"), 
                                                  pch=c(21, 25, 22, 22), pt.cex=1,5, 
                                                  pt.bg=c("black", "black", "blue", "red"),
                                                  col=c("white"), bty="n")}
              fitHyperbola(all.dat, i , lwd=lwd)}
      }


fitHyperbola<-function(all.dat, i , lwd=2.5){
  
                    dat<-all.dat[all.dat[,i]!=0,]
                    
                    fm<-formula(paste(i, "~ areaI"))
                    areaI<-I(1/dat$area)
                    X<-seq(min(dat$area),max(dat$area) ,l=100)
                    
                    newX<-data.frame(areaI=I(1/X))
                    newY<-predict(lm(fm,data=dat), newdata=newX)
                    points(newY ~ X, type="l", lwd=lwd)
                    lines(1/X ~ X, lwd=1.3, lty=2)}

