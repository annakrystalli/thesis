plotTraj<-function(res, SD, an.ID, pt, cex.l=1, trials){
  
  

  
  plot.SEQ<-list(both=c(seq(1,13,2),seq(2,12,2)), var=c(seq(5,13,2),seq(6,14,2)), 
                 spp=14+c(seq(5,13,2),seq(4,12,2)))
  plot.seq<-plot.SEQ[[an.ID]]
  
  
  
  if(an.ID=="both"){
    png(paste("plots/traj ", an.ID, ".png", sep=""), 
        width=8.1, height=11, units="in", res=200, pointsize=pt) 
    
    
      par(mfcol=c(7,2), mar=c(1.8,2.5,1.5,0.3), mgp=c(1.8, 0.6,0))
      
      j.seq<-c(seq(1,13,2),seq(2,14,2))}else{
        
    
      j.seq<-c(seq(1,13,2),seq(2,14,2), 2*seq(1,13,2),2*seq(2,14,2))
      
      if(an.ID=="var"){
        
        png(paste("plots/traj ", an.ID, ".png", sep=""), 
            width=8.1, height=11, units="in", res=200, pointsize=pt) 
        
        par(mfcol=c(7,4), mar=c(1.8,2.5,1.5,0.3), mgp=c(1.8, 0.6,0))}}
    
  
  for(j in j.seq){
    
    
    if(j %in% plot.seq){
    
    if(an.ID=="spp"){r.var<-"n"
                     t.var<-"spp"}else{r.var<-"var"
                                       t.var<-"var"}  
    trial.id<-which(trials[j, t.var] == res[[r.var]])  
      
    sd<-SD[[trial.id]]  
    
    spps<-unique(unlist(res$C.IDs[[trial.id]]))
    
    for(spp in spps){
      
      sd.spp<-as.data.frame(sd[unlist(lapply(res$C.IDs[[trial.id]][[1]], FUN=function(x){spp %in% x})),])
      
    
      r.dat<-NULL

      
                  for(k in 1:length(SD)){
                    r.dat<-c(r.dat, SD[[k]][,"Ddevsd"])}
                    xr<-range(r.dat)
                  
                    bg<-mean(range01all(sd.spp$Ddevsd))
                    mg<-0.1
      
      
      ylim<-c(-15, 2)
      
      if(spp == spps[1]){  
        plot(jitter(Ddevsd) ~ jitter(th), data=sd.spp, type="b", cex=0.5, pch=21, bg=col, col=col, ylim=ylim,
             xlab="",
             ylab=expression(paste(mu," "[sigma],"", Delta[italic(n)]^"+")))}else{
               if(!is.nan(bg)){
                 col=gray((mg+((1-mg)*bg)), 0.8)
                 lty=which(spps==spp)
                 #col=which(spps==spp)
                 lines(jitter(Ddevsd) ~ jitter(th), data=sd.spp, type="b", cex=0.5, 
                       pch=21, bg=col, col=col, ylim=ylim, lty=lty) 
               }}
      abline(h=-2, lty=2, lwd=2)
      abline(h=0, lty=1, lwd=2)
      legend("bottomright", legend=paste("s = ", res$n[trial.id], "  t = ", res$var[trial.id]), bty="n", cex=cex.l)
      
      
    }}else{plot.new()}}
  
  if(an.ID %in% c("both", "spp")){dev.off()}
  
}
