rm(list=ls())

makeTransparent<-function(someColor, alpha=100)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

id_outliers <- function(x, na.rm = TRUE, ...) {
  vars<-c("AB.yint", "AB.rfint", "AB.win")
  id<-NULL
  
  for(i in 1:length(vars)){
  qnt <- quantile(x[, vars[i]], probs=c(.25, .75), na.rm = na.rm)
  H <- 1.5 * IQR(x, na.rm = na.rm) 
  id<-c(id, which(x[, vars[i]] < (qnt[1] - H) | x[, vars[i]] > (qnt[2] + H), arr.ind=T))}
  
-unique(id)
}


reshapeDat<-function(x=data.frame(AOR.data), var="AB",spp){
  
  x <- x[,-grep(".yav", names(x))]
  
  if(var=="pop"){
    x$pop.rf<-(x[,"AB.rf"]*x[,"oc.rf"])
    x$pop.rfy<-(x[,"AB.rfy"]*x[,"oc.rfy"])
    x$pop.y<-(x[,"AB.yint"]*x[,"oc.y"])
    x$pop.win<-(x[,"AB.win"]*x[,"oc.win"])
    if(spp=="metrilu"){x$pop.rfyt<-(x[,"AB.rfyt"]*x[,"oc.rfyt"])}
  }
  
  names(x)<-gsub(".yint", ".y",  names(x), fixed=T)
  names(x)[names(x)=="N.y"] <- "N.rfy"       
  if(spp=="metrilu"){x$N.rfyt<-x$N.rfy}
    
  var.names <- sort(names(x)[grep(var,  names(x))])
  n.names <- sort(names(x)[grep("N.",  names(x))])
  t.names <- c("yr", "mm")
  
  x <- x[,c(var.names, n.names, t.names)]
  
  names(x)[grep(".y",names(x), fixed=T)]<-"x"
  var.names[grep(".y",var.names, fixed=T)]<-"x"
  idvar="x"
  
  
  reshape(x, varying=list(setdiff(var.names, idvar), n.names), idvar=t.names, 
          v.names=c("y","n"), direction="long", timevar="mod", sep=".",
          times=gsub("N.", "",n.names))}





fitVarModel<-function(data, watch=T, spp){
  if(spp=="metrilu"){data$mod<-factor(data$mod, levels=c("rfyt","rfy", "rf", "win"))}else{
    data$mod<-factor(data$mod, levels=c("rfy", "rf", "win"))}
  
    mod<-lm(y ~ x * mod + offset(x), data=data)
  
    if(watch){print(mod)
              print(summary(mod))
              plot(mod)}
  
    AIC<-mod$aic
  
     inf<-NULL
    
    while(any(cooks.distance(mod)>1)){
            
      if(watch){print(names(which(cooks.distance(mod)==max(cooks.distance(mod)) & cooks.distance(mod)>1)))}
            
      
      inf<-c(inf, names(which(cooks.distance(mod)==max(cooks.distance(mod)) & cooks.distance(mod)>1)))
      
            infl.yr<-as.numeric(strsplit(
                    names(which(cooks.distance(mod)==max(cooks.distance(mod)) & cooks.distance(mod)>1)), 
                    split=".", fixed=T)[[1]][1])
            
            infl.mm<-as.numeric(strsplit(
              names(which(cooks.distance(mod)==max(cooks.distance(mod)) & cooks.distance(mod)>1)), 
              split=".", fixed=T)[[1]][2])
            
            data<-data[!(data$yr==infl.yr & data$mm==infl.mm),]
            mod<-glm(y ~ x * mod + offset(x), data=data, family="gaussian")
            
            if(watch){
            print(summary(mod))
            print(plot(mod))}
            AIC<-c(AIC, mod$aic)}

return(list(mod=mod, data=data, aic=AIC, inf=inf))
    }
   
plot4panels<-function(data, mod, var, cex, spp.id, 
                      col.sdm, col.sdmy, col.win, pchs=pchs, 
                      lwd=2){

  
  if(var=="oc"){xlab="observed occupancy"
                ylab="interpolated occupancy"}else{xlab="observed density"
                      ylab="interpolated density"}
  
          spp<-names.spp[spp.id]
          
          if(spp.id==1){ylab<-ylab}else{ylab<-" "} 

       max<-max(c(data$x, data$y), na.rm=T)   
       
  plot(y ~ x, data[data$mod=="rf", ],
       ylim=c(0, max),
       xlim=c(0, max),
       main=if(var=="AB"){spp.labels[spp.id]}else{""},
       pch=pchs[1], col=col.sdm, cex=cex, 
       cex.axis=0.85, xlab=xlab, 
       ylab=ylab)
  abline(0,1)
  
  points(y ~ x, data[data$mod=="rfy", ],
         col=col.sdmy, pch=pchs[2], cex=cex)
  
  if(spp=="metrilu"){points(y ~ x, data[data$mod=="rfyt", ],
         col="grey20", pch=pchs[2], cex=cex*0.6)}
  
  points(y ~ x, data[data$mod=="win", ],
         col=col.win[[spp]], pch=pchs[3], cex=cex)
  
  
      newX<-expand.grid(x=seq(min(data$x, na.rm=T),max(data$x, na.rm=T), l=15), 
                        mod=factor(unique(data$mod)))
      newY<-predict(mod, newdata=newX)
      new.dat<-data.frame(newX, y=newY)

  lines(y ~ x, data=new.dat[new.dat$mod=="rfy", ],
        lty=2, lwd=lwd*0.75, col=col.sdmy)
  
  if(spp=="metrilu"){
  lines(y ~ x, data=new.dat[new.dat$mod=="rfyt", ],
        lty=3, lwd=lwd*0.75, col="grey20")}
  
  lines(y ~ x, data=new.dat[new.dat$mod=="win", ],
                lty=1, lwd=lwd, col=col.win[[spp]]) 
  
  lines(y ~ x, data=new.dat[new.dat$mod=="rf", ],
                lty=1, lwd=lwd, col=col.sdm)
  

  
  
  if(var=="AB"){legend("topleft", 
                       legend=c(expression("SDM"[italic(hr)]),
                                expression(paste("SDM"[italic(hr)], "y")),
                                expression(Win[italic(CPR)]),
                                if(spp=="metrilu"){expression(paste("SDM"[italic(hr)], "y.tc"))}else{
                                  NULL}),         
                       col=c(col.sdm, col.sdmy, col.win[[spp]],if(spp=="metrilu"){
                         "grey20"}else{
                         NULL}), bty="n",
                       pch=c(pchs,if(spp=="metrilu"){
                         pchs[2]}else{
                           NULL}), cex=0.6, bg="transparent")}
}

#_________________________________________________________________________________________________________


an.ID="Final/"
thresh="PredPrev=Obs"
names.spp<-c( "chel","cfin" , "metrilu" ,  "centrot" ,"tem" )
vars=c("AB", "oc", "pop")

alpha<-0.8
col.sdmy<-grey(0.5, alpha)
col.win<-makeTransparent(c("darkgoldenrod4","aquamarine4","palevioletred4","purple3",
                           "indianred4"), alpha=alpha*255)
        names(col.win)<-c( "chel" ,"cfin"  ,"metrilu", "centrot", "tem")
col.sdm<-grey(0, alpha)
pchs<-c(6, 3 ,1)

mods<-c("rfyt","rfy", "rf", "win")

cex=0.9
spp.labels<-c(expression(italic("C. helgolandicus")), 
              expression(italic("C. finmarchicus")),
              expression(italic("M. lucens")),
              expression(italic("C. typicus")),
              expression(italic("T. longicornis")))






#_________________________________________________________________________________________________________

model.list<-vector("list",5)
names(model.list)<-names.spp

spp.list<-vector("list",3)
names(spp.list)<-c("AB", "oc", "pop")

watch=F
    
    for(spp.id in 1:5){
      
      spp<-names.spp[spp.id]
      if(spp=="metrilu"){tc<-"time corrected/"}else{tc<-NULL}
      load(file=paste("~/Documents/PREDICTED/", an.ID,"AORS/data/",tc,
                      thresh, spp," monthly AOR.Rdata",sep=""))
      
            AOR.data[!is.finite(AOR.data)]<-0
  
      
      for(var in vars){

            df<-reshapeDat(var=var, spp=spp)
             
            spp.list[[var]]<-fitVarModel(data=df, watch=F, spp=spp)}
      
      model.list[[spp]]<-spp.list}
            
     


png(filename = "~/Documents/THESIS/RF Chapter/Figures/Figure4.png", 
                pointsize=11, units = "in", res=200, width=8, height=5)
            
        par(mfcol=c(3,5), mar=c(2.5,2.5,1.8,0),  mgp=c(1.3,0.3,0), tcl=-0.3, oma=c(0,0,0,1))

for(spp.id in 1:5){
    spp<-names.spp[spp.id]
    
  for(var in vars){
    
        plot4panels(data=model.list[[spp]][[var]]$data,
                    mod=model.list[[spp]][[var]]$mod, var,
                            cex=0.7, spp.id, col.sdm, col.sdmy, col.win, 
                            pchs=pchs, lwd=1.5)}}


dev.off()
      
 c.names<-c("spp","var", "mod", "a", "p.a", "b", "p.b") 
 ids<-expand.grid(mods, vars, names.spp)[,3:1]
 ids<-ids[!(ids[,1]!="metrilu" & ids[,3]=="rfyt"),]
 na<-matrix(NA, nrow=dim(ids)[1], ncol=4)
 mod.table<-data.frame(ids, na)
 names(mod.table)<-c.names

for(spp in names.spp){
  if(spp=="metrilu"){mods<-c("rfyt","rfy", "rf", "win")}else{mods<-c("rfy", "rf", "win")}
  for(var in vars){
    for(mod in mods){
  
  mod.table[mod.table$spp==spp & mod.table$var==var & mod.table$mod==mod,"a"]<-coef(summary(model.list[[spp]][[var]][["mod"]]))[1*which(mods==mod),1]
  mod.table[mod.table$spp==spp & mod.table$var==var & mod.table$mod==mod, "p.a"]<-coef(summary(model.list[[spp]][[var]][["mod"]]))[1*which(mods==mod),4]
  mod.table[mod.table$spp==spp & mod.table$var==var & mod.table$mod==mod,"b"]<-coef(summary(model.list[[spp]][[var]][["mod"]]))[2*which(mods==mod),1]
  mod.table[mod.table$spp==spp & mod.table$var==var & mod.table$mod==mod,"p.b"]<-coef(summary(model.list[[spp]][[var]][["mod"]]))[2*which(mods==mod),4]
 }}}

mod.table$p.a<-cut(mod.table$p.a, breaks=c(0, 0.001, 0.01, 0.5, 1), 
         labels=c("***", "**", "*", "-" ))

mod.table$p.b<-cut(mod.table$p.b, breaks=c(0, 0.001, 0.01, 0.5, 1), 
                   labels=c("***", "**", "*", "-" ))

mod.table

write.csv(mod.table, file= "~/Documents/THESIS/RF Chapter/Tables/aoc mod table.csv")
  