
#Bubbleplots.......................
png(paste("plots/",an.ID, "bubble plots.png", sep=""), width=8.1, height=8.1, units="in", res=200, pointsize=15)     
par(mfrow=c(3,3))
for(i in 1:length(res.var$SE)){
  symbols(res.var$SE[[i]][,"Ddevse"]~res.var$SE[[i]][,"th"], 
          circles= sqrt( res.var$SE[[i]][,"Dn"]/ pi ),
          inches=0.09, ylim=c(-120, 10), xlim=c(0,1),
          fg="white", bg=grey(level=0.2,alpha=0.3), cex.main=0.8,
          xlab=expression(h[f]), ylab="SE deviation from mean",
          main=paste("A) t = ",res.var$var.n.all[i], "  s = ", res.var$n.all[i]))
  abline(h=0, lwd=1, col="grey")
  abline(h=-2, lwd=0.6, lty=2)
}

dev.off()


bubblePlot(an.ID, mfrow=c(3,3), res=res.both,ylim=c(-12, 10), inches=0.09, Dd="Ddevsd")


#Playing around with GAMs



gamExtract<-function(res){cbind(FD=res$FD, s=res$n, t=res$var,  
                                sd=res$SE[[1]][,"Ddevsd"])}

gamBind<-function(res.both, res.var, res.spp){
  
  data.frame(rbind(cbind(sapply(c("FD","dsd", "n", "var"),FUN=function(x){
    nMetrics(res.both, an.ID="spp")[[x]]}), 
                         f=factor("A", levels=c("A", "B", "C"))),    
                   
                   cbind(sapply(c("FD","dsd", "n", "var"),FUN=function(x){
                     nMetrics(res.var, an.ID="var")[[x]]}), 
                         f=factor("B", levels=c("A", "B", "C"))), 
                   
                   cbind(sapply(c("FD","dsd", "n", "var"),FUN=function(x){
                     nMetrics(res.spp, an.ID="spp")[[x]]}), 
                         f=factor("C", levels=c("A", "B", "C")))) )
}


gam.data<-gamBind(res.both, res.var, res.spp)

require(mgcv)

k=3

gam1<-gam(FD ~ s(var, k=k)+s(n, k=k), family=gaussian, data=gam.data)
gam2<-gam(FD ~ s(var, k=k)+s(n, k=k)+dsd, family=gaussian, data=gam.data)
gam3<-gam(FD ~ s(var, k=k)+s(n, k=k)+s(dsd, k=k), family=gaussian, data=gam.data)

par(mfrow=c(1,2))
gamDiag<-function(x){
  plot(x)
  gam.check(x)
  print(summary(x))}



gamDiag(gam1)
gamDiag(gam2)
gamDiag(gam3)


plot(x=gam1)
gam.check(gam2)

plot(x=gam3)
gam.check(gam3)
