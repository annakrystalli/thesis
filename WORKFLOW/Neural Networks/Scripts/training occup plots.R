#Set up
rm(list = ls())

require(scatterplot3d)

load(file="/Users/annakrystalli/Documents/TRAINING DATA/normalized (min.max) x training data.RData")
load(file="/Users/annakrystalli/Documents/TRAINING DATA/y training data.RData")

sort(apply(y.w,2,FUN=function(x){length(which(x>=1))}))

            #...1)tem  2)centrot 3)calfin 4)calhel 5)metrilu
            # var1 = month,  var2= lat/fdens



#.............LOOP plot3doccup Function...........................................................................

              var.ind<-data.frame(var1=as.character(c("Ch.m", "Ch.m", "Ch.m")), 
                                  var2=as.character(c("month", "lat", "lon")))
              colour.ind<-c("hotpink4","seagreen4", "turquoise4", "tan4", "plum4")

      #...through each species........................
                  for (j in 1:5){
                  y<-data.frame(y.m[,j])
                  names(y)<-names(y.m)[j]
                  x<-norm.x.m
                  
                  
                  #...through each var combo........................ 
                  
                      for(k in 1:length(var.ind[,1])){
                        var1=var.ind$var1[k]
                        var2=var.ind$var2[k]
                      
                  plot3d.occup(y,x,var1=var1, var2=var2)}}
      




#....plot3D.occup FUNCTION..........................................................................................
        plot3d.occup<-function(y, x, var1, var2){
          
            if(length(grep( ".m", names(x)))>=1){time<-"monthly"}else{time<-"weekly"}
            spp<-names(y)
            n<-length(y[,1])
            y[y>=1]<-1
            colour<-rep("snow3", times=n)
            colour[which(y==1)]<-colour.ind[j]
            
            p.cex<-rep(0.2, times=n)
            p.cex[which(y==1)]<-0.5
            
            p.pch<-rep(1, times=n)
            p.pch[which(y==1)]<-19
            
            plot.var<-names(x)[-which(names(x)%in% c(var1, var2) , arr.ind=TRUE)]
            
            zv<-x[,which(names(x)==var1, arr.ind=TRUE)]
            xv<-x[,which(names(x)==var2, arr.ind=TRUE)]
            
          for (i in 1:length(plot.var)){
                      yv<-x[,which(names(x)==plot.var[i], arr.ind=TRUE)]
                      var3<-plot.var[i]
                 
                        png(filename = paste("~/Documents/TRAINING DATA/3d presence-absence plots/", time, "/", 
                                             spp, " ",var1,"-",var2,"-", var3,".png", sep=""),
                            width = 800, 
                            height = 600)
                        
                        scatterplot3d(xv,yv,zv, pch=p.pch, color= colour, grid=TRUE, cex.symbols=p.cex,
                                      type="p", main=paste("3D Scatterplot", spp, time), xlab=var2, ylab=var3, 
                                      zlab=var1)
                      
                      dev.off()
                    }
      }

#..............................................................................................


# Do the same, but with colors corresponding to value
corrx<-cor(x,x)

colorfun <- colorRamp(c("#3366CC","white","#CC0000"), space="Lab")
plotcorr(corrx, col=rgb(colorfun((corrx+1)/2), maxColorValue=255), 
         mar = c(0.1,0.1,1,0.1), main="monthlyx correlations")


corryx<-cor(y.m,norm.x.m)

colorfun <- colorRamp(c("#3366CC","white","#CC0000"), space="Lab")
plotcorr(corryx, col=rgb(colorfun((corryx+1)/2), maxColorValue=255), main="monthly yx correlations",
         mar = c(0.1,0.1,1,0.1), type="full", cex=6)


corrx.w<-cor(norm.x.w,norm.x.w)

colorfun <- colorRamp(c("#3366CC","white","#CC0000"), space="Lab")
plotcorr(corrx.w, col=rgb(colorfun((corrx.w+1)/2), maxColorValue=255), main="weekly yx correlations")


corryx.w<-cor(y.w,norm.x.w)

colorfun <- colorRamp(c("#3366CC","white","#CC0000"), space="Lab")
plotcorr(corryx.w, col=rgb(colorfun((corryx.w+1)/2), maxColorValue=255), main="weekly yx correlations",
         mar = c(0.1,0.1,1,0.1), type="full", cex=4)

corryy.w<-cor(y.w,y.w)
"colorfun <- colorRamp(c("#3366CC","white","#CC0000"), space="Lab")
plotcorr(corryy.w, col=rgb(colorfun((corryy.w+1)/2), maxColorValue=255), main="weekly yy correlations",
         mar = c(0.1,0.1,1,0.1), type="full", cex=4)

