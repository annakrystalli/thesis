an.ID="Final/"
load(file="~/Documents/TRAINING DATA/spp.names")
thresh="PredPrev=Obs"

load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                             an.ID, thresh, "validation summary.RData", sep=""))

print(val.SUMMARY)
cols<-c("palevioletred4","purple3","aquamarine4","darkgoldenrod4","indianred4")
names(cols)<-names.spp

for(spp in names.spp){
load(file=paste("~/Documents/PREDICTED/", an.ID,"AORS/data/",
                         thresh, spp," monthly AOR.Rdata",sep=""))

AOR.data[is.nan(AOR.data)]<-0

print(spp)

par(mfrow=c(3,2), mar=c(5,4,0,0))
plot(AOR.data[,"AB.rf"] ~ AOR.data[,"AB.yint"], 
     ylim=c(0, max(AOR.data[,"AB.yint"][is.finite(AOR.data[,"AB.yint"])])),
     xlim=c(0, max(AOR.data[,"AB.yint"][is.finite(AOR.data[,"AB.yint"])])),
     bg="black", 
     pch=21, col="grey93", cex=1.7, xlab="raw integrated mean abundance", ylab="interpolated")
            points(AOR.data[,"AB.win"] ~ AOR.data[,"AB.yint"], bg=cols[spp], pch=21,
                   col="grey93", cex=1.7)
            abline(0,1)
            #legend("topleft", legend=c("rf int", "rf av", "winCPR"), pt.bg=c("black", "grey", cols[spp]), pch=21, col="grey93", cex=1.7)
    

   plot(AOR.data[,"AB.rf"] ~ AOR.data[,"AB.yav"], 
                 ylim=c(0,max(AOR.data[,"AB.yav"][is.finite(AOR.data[,"AB.yav"])])), 
                 bg="black", pch=21, col="grey93", cex=1.7,
                 xlab="raw accepted value mean abundance", ylab="interpolated")
            points(AOR.data[,"AB.win"] ~ AOR.data[,"AB.yav"], bg=cols[spp], pch=21, col="grey93", cex=1.7)
            abline(0,1)
            #legend("topleft", legend=c("rf int", "rf av", "winCPR"), pt.bg=c("black", "grey", cols[spp]), pch=21, col="grey93", cex=1.7)





pop.rf<-(AOR.data[,"AB.rf"]*AOR.data[,"oc.rf"])

pop.yav<-(AOR.data[,"AB.yav"]*AOR.data[,"oc.y"])
pop.yint<-(AOR.data[,"AB.yint"]*AOR.data[,"oc.y"])
pop.win<-(AOR.data[,"AB.win"]*AOR.data[,"oc.win"])


plot( pop.rf~ pop.yint, ylim=c(0,max(pop.yint[is.finite(pop.yint)])),
      xlim=c(0,max(pop.yint[is.finite(pop.yint)])),
      bg="black", pch=21, col="grey93", cex=1.7, ylab="interpolated popn mean", 
      xlab="raw integrated popn mean")
        points( pop.win~ pop.yint,  bg=cols[spp], pch=21, col="grey93", cex=1.7)
        #legend("topleft", legend=c("rf int", "rf av", "winCPR"), pt.bg=c("black", "grey", cols[spp]), pch=21, col="grey93", cex=1.7)
        abline(0,1)

plot( pop.rf~ pop.yav, ylim=c(0,max(pop.yav[is.finite(pop.yav)])), 
      bg="black", pch=21, col="grey93", cex=1.7, ylab="interpolated popn mean", xlab="raw accepted value popn mean")
        points( pop.win~ pop.yav,  bg=cols[spp], pch=21, col="grey93", cex=1.7)
        #legend("topleft", legend=c("rf int", "rf av", "winCPR"), pt.bg=c("black", "grey", cols[spp]), pch=21, col="grey93", cex=1.7)
        abline(0,1)



plot(AOR.data[,"oc.rf"] ~ AOR.data[,"oc.y"], ylim=c(0,1), xlim=c(0,1),
     bg="black", pch=21, col="grey93", cex=1.7, xlab="raw occupancy",
     ylab="interpolated occupancy")
        points(AOR.data[,"oc.win"] ~ AOR.data[,"oc.y"], bg=cols[spp], 
               pch=21, col="grey93", cex=1.7)
        abline(0,1)
#legend("topleft", legend=c("rf",  "winCPR"), pt.bg=c("black",  cols[spp]), pch=21, col="grey93", cex=1.7)
plot.new()
legend("topleft", legend=c("rf int", "winCPR"), pt.bg=c("black",  cols[spp]), pch=21, col="grey93", cex=1.7)
}



for(spp in names.spp){
  load(file=paste("~/Documents/PREDICTED/", an.ID,"AORS/data/",
                  thresh, spp," monthly AOR.Rdata",sep=""))
  
  AOR.data[is.nan(AOR.data)]<-0
  
  plot(AOR.data[,"AB.yint"] ~ AOR.data[,"AB.yav"], ylim=c(0,max(c(AOR.data[,"AB.yav"],AOR.data[,"AB.yint"]))),
       xlim=c(0,max(c(AOR.data[,"AB.yav"],AOR.data[,"AB.yint"]))),
       bg=cols[spp], pch=21, col="black", cex=1.7,
       xlab="raw accepted value", ylab="raw integrated")
  abline(0,1)}
plot.new()
legend("topleft", legend=names.spp, 
       pt.bg=cols, pch=21, col="black", cex=1.7)