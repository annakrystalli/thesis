#Set up
rm(list = ls())

#Load ave
ave<-read.csv("~/Documents/CPR/DATA/RAW DATA/Accepted values.csv")

ave.lm<-lm(log(ave)~cat, data=ave)
avm.info<-data.frame(cbind(ave[c(1, 4:12),1:3], 
                             avem=round(
                               exp(
                                 predict.lm(ave.lm, newdata=data.frame(cat=c(1,4:12)))
                                 )
                             )
                    )
                    )


plot(predict.lm(ave.lm, newdata=data.frame(cat=c(1,4:12)))~1:10)

plot(c(1,4:12),log(avm.info$avem), xlab="abundance category", ylab="log abundance", type="b", 
     pch=21, col="darkslategray4", bg="cadetblue1",
     col.axis="darkslategray",col.lab="darkslategray")

points(c(1,4:12),log(avm.info$ave), xlab="abundance category", ylab="log abundance", type="b", 
     pch=21, col="darkslategray", bg="cadetblue3",
     col.axis="darkslategray",col.lab="darkslategray", lty=2, cex=0.7)

legend(x="topleft", legend=c("avem", "ave"), pt.bg=c("cadetblue1","cadetblue3"), 
       pch=21, col=c("darkslategray4","darkslategray"), 
       lty=c(1,2), pt.cex=c(1,0.7))

#...Plot histograms of original vs merged AC distributions

hist(y.a.tr.m)
y.a.tr.mg.m<-y.a.tr.m
y.a.tr.mg.m[y.a.tr.mg.m<=3]<-1


par(mfrow=c(2,1))

hist(y.a.tr.m, col="darkslategray4", width=2, 
     xlab="orignal y AC freq. dist", 
     main="Fig 4. Original vs Merged y AC freq. dist")

hist(y.a.tr.mg.m, col="darkslategray4", width=2, 
     xlab="merged y AC freq. dist", 
     main=NULL)