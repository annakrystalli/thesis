method="Nelder-Mead"


tapply(ps.dat$J, FUN=mean, INDEX=ps.dat$p.id, na.rm=T, simplify=T)
x=ps.dat$J
sdx<-sd(x)
agg<-tapply(x, FUN=function(x){(x-mean(x))/sd(x)}, 
       INDEX=list(X$spp, X$nAC, X$id), simplify=T)

sum(!is.finite(ps.dat$J))
sum(!is.finite(ps.dat$Mean))
sum(!is.finite(ps.dat$Probs))
sum(!is.finite(ps.dat$Jg))
sum(!is.finite(ps.dat$Meang))
sum(!is.finite(ps.dat$ProbsG))


agg<-tapply(ps.dat[,metric], FUN=mean, 
            INDEX=list(X$p.id), simplify=T)

foo=median
metric="sdM"
method="Nelder-Mead"
cex.res=0.9

for(spp.id in names.spp){
  
  png(filename = paste("~/Documents/TRAINING DATA/Models/randomForest/",
                       an.ID,"integration plots/", method,"/", spp.id," ",metric," boxplots.png", 
                       sep=""), 
      width=11.7*cex.res, height=6.9*cex.res, units = "in", res = 72, pointsize=pt)
  
  
  par(mfrow=c(3,1), mar=c(4,2,2,0.5))
  
  for(nAC.id in 3:5){
    
    agg.dat<-ps.dat[ps.dat$nAC == nAC.id & ps.dat$spp ==spp.id,]
    agg<-tapply(agg.dat[, metric], FUN=foo, 
                INDEX=list(agg.dat$p.id), simplify=T)

    fm<-as.formula(paste(metric , "~ factor(p.id, levels = names(sort(agg)))"))
                   
    boxplot(fm, data=subset(ps.dat, subset=nAC == nAC.id & spp ==spp.id), 
            main=paste(metric, spp.id, nAC.id), cex=0.4, las=2, ylim=c(0,10))}
  
  dev.off()
}


for(spp.id in names.spp){
  
  png(filename = paste("~/Documents/TRAINING DATA/Models/randomForest/",
                       an.ID,"integration plots/", method,"/", spp.id," ",metric,"nAC boxplots.png", 
                       sep=""), 
      width=9*cex.res, height=4*cex.res, units = "in", res = 72, pointsize=pt)
  
  
  par(mfrow=c(1,3), mar=c(4,3,2,0.5))
  
  for(nAC.id in 3:5){
    
    fm<-as.formula(paste(metric , "~ factor(nAC)"))
    
    boxplot(fm, data=subset(ps.dat, subset=nAC == nAC.id & spp ==spp.id), names=nAC.id,
            main=paste(metric, spp.id, nAC.id), cex=0.6, las=2, ylim=c(0,10),
            ylab=paste("abs sd from", metric))
      abline(h=max(agg), lty=2)}
  
  dev.off()
}


pt=12
foo=median
metric="Means"
method="Nelder-Mead"
cex.res=0.9

for(spp.id in names.spp){
  
  png(filename = paste("~/Documents/TRAINING DATA/Models/randomForest/",
                       an.ID,"integration plots/", method,"/", spp.id," ",metric," boxplots.png", 
                       sep=""), 
      width=11.7*cex.res, height=6.9*cex.res, units = "in", res = 72, pointsize=pt)
  
  
  par(mfrow=c(3,1), mar=c(4,2,2,0.5))
  
  for(nAC.id in 3:5){
    
    agg.dat<-ps.dat[ps.dat$nAC == nAC.id & ps.dat$spp ==spp.id,]
    agg<-tapply(agg.dat[, metric], FUN=foo, 
                INDEX=list(agg.dat$p.id), simplify=T)
    
    fm<-as.formula(paste(metric , "~ factor(id)"))
    
    boxplot(fm, data=subset(ps.dat, subset=nAC == nAC.id & spp ==spp.id), 
            main=paste(metric, spp.id, nAC.id), cex=0.4, las=2)}
  
  dev.off()
}





par(mar=c(4,5,2,0.5))

for(spp.id in names.spp){
  
  png(filename = paste("~/Documents/TRAINING DATA/Models/randomForest/",
                       an.ID,"integration plots/", method,"/", spp.id," ",metric," boxplots.png", 
                       sep=""), 
      width=11.7*cex.res, height=6.9*cex.res, units = "in", res = 72, pointsize=pt)
  
boxcols<-c("black", "black","aquamarine4","blueviolet", "cornflowerblue")  
bxt=60
wst=80

for(nAC.id in 5:3){

  fm<-as.formula(paste(metric , "~ factor(id)"))
  
  if(nAC.id==5){boxplot(fm, data=subset(ps.dat, subset=nAC == nAC.id & spp ==spp.id), 
        main=paste(metric, spp.id), cex=0.6, las=2, 
        whiskcol=makeTransparent(nAC.id, wst), boxcol=boxcols[nAC.id],
        staplecol=boxcols[nAC.id], medcol=boxcols[nAC.id],
        outcol=makeTransparent(nAC.id, bxt), outbg=makeTransparent(nAC.id, bxt), pch=21,
        whisklwd=2, col=makeTransparent(nAC.id, bxt), 
        ylim=range(ps.dat[ps.dat$spp ==spp.id,metric]),
        ylab="mean abundance")

  legend("topright", legend=5:3, fill=makeTransparent(5:3, bxt), bty="n",
        border=boxcols[nAC.id])}
  
  if(nAC.id!=5){boxplot(fm, data=subset(ps.dat, subset=nAC == nAC.id & spp ==spp.id), 
        las=2, add=T, cex=0.6,whiskcol=makeTransparent(nAC.id, wst), 
        boxcol=boxcols[nAC.id],  staplecol=boxcols[nAC.id], 
        medcol=boxcols[nAC.id], outcol=makeTransparent(nAC.id, bxt), 
        outbg=makeTransparent(nAC.id, bxt), pch=21,
        whisklwd=2, col=makeTransparent(nAC.id),
        ylim=range(ps.dat[ps.dat$spp ==spp.id,metric]))}}

dev.off()
}









orderd<-"b"
foo=median
metric="sdM"
method="Nelder-Mead"
cex.res=0.9

INITS<-list(a=paste(inits[order(inits$a),"a"], inits[order(inits$a),"b"], sep="-"),
            b=paste(inits[order(inits$b),"a"], inits[order(inits$b),"b"], sep="-"))
            

spp="metrilu"
id="2000-9-"
nAC=5


p.dat<-ps.dat[ps.dat$nAC==5,]


lapply(split(p.dat, p.dat$spp), FUN=function(x){sum(!is.finite(x$ProbsG))/length(x$ProbsG)})


for(spp in names.spp){
  tit=spp
 plot(sdM ~ J, data=ps.dat[ps.dat$spp==spp & ps.dat$id==id & ps.dat$nAC==nAC,], main=tit)}




ps.dat[which(ps.dat$Means==max(ps.dat[ps.dat$spp==spp & ps.dat$id==id & ps.dat$nAC==nAC, "Means"]))[1],]
ps.dat[which(ps.dat$Means==min(ps.dat[ps.dat$spp==spp & ps.dat$id==id & ps.dat$nAC==nAC, "Means"]))[1],]

thresh="PredPrev=Obs"
spp="cfin"
id="2002-6-"

load(paste("/Users/annakrystalli/Documents/PREDICTED/",
an.ID,"normalised monthly maps/",spp,"/",
id, ".RData", sep=""))

if(any(preds$pred.oc<0)){preds$pred.oc<-preds$pred.oc+1}
preds$pred.oc<-as.numeric(preds$pred.oc>=THRESH.spp[[spp]][thresh])

table(preds$pred.ac[preds$pred.oc==1])/sum(preds$pred.oc)



spp="tem"
id="1998-4-"

load(paste("/Users/annakrystalli/Documents/PREDICTED/",
           an.ID,"normalised monthly maps/",spp,"/",
           id, ".RData", sep=""))

if(any(preds$pred.oc<0)){preds$pred.oc<-preds$pred.oc+1}
preds$pred.oc<-as.numeric(preds$pred.oc>=THRESH.spp[[spp]][thresh])

table(preds$pred.ac[preds$pred.oc==1])/sum(preds$pred.oc)




l<-split(ps.dat$Means, f=list(as.factor(ps.dat$nAC), ps.dat$spp, ps.dat$id))


l<-split(p.dat$Means, f=list( p.dat$spp, p.dat$id))

lap<-lapply(l, FUN=function(x){mean(sqrt((x-median(x))^2/median(x)))})
dev.ord<-sort(unlist(lap), decreasing=T)
names(dev.ord)







source('~/Documents/WORKFLOW/randomForest/Robs categorical mean calculation/ac integration functions2.R', chdir = TRUE)
spp="tem"
id="1999-10-"

load(paste("/Users/annakrystalli/Documents/PREDICTED/",
           an.ID,"normalised monthly maps/",spp,"/",
           id, ".RData", sep=""))

if(any(preds$pred.oc<0)){preds$pred.oc<-preds$pred.oc+1}
preds$pred.oc<-as.numeric(preds$pred.oc>=THRESH.spp[[spp]][thresh])

bks<-BKS$AC5
x<-factor(preds$pred.ac[preds$pred.oc==1], 
                       levels = c(1:5))

if(spp %in% c("tem", "centrot")){k=2;f=50}else{k=1; f=1}


par(mfrow=c(3,4))
for(i in 1:dim(inits)[1]){
opt<-optlnormFun(table(x),
                 bks*f, inits[i,])
}

#Try other nACs
nAC=4

bks<-BKS[[paste("AC",nAC, sep="")]]
ACids<-AC.IDS[[paste("AC",nAC, sep="")]]

x<-factor(ACids[factor(preds$pred.ac[preds$pred.oc==1], 
                       levels = c(1:5))], levels = c(1:nAC))

if(spp %in% c("tem", "centrot")){k=2;f=50}else{k=1; f=1}


par(mfrow=c(3,4))
for(i in 1:dim(inits)[1]){
  opt<-optlnormFun(table(x),
                   bks*f, inits[i,])
}
lapply(split(tem.5, id), FUN=function(x){boxplot(Means ~ 1, data=x)}
       
       
       
       t.ids<-c( "1998-4-", "1998-5-", "1998-6-", "1999-10-", "1999-4-", 
                 "1999-5-", "1999-9-", "2000-5-", "2000-6-", 
                 "2000-7-", "2000-8-", "2000-9-", "2001-3-",  
                 "2001-8-", "2001-10-","2002-2-", 
                 "2002-6-", "2002-7-", "2003-2-")
       
       skew.tab<-matrix(NA, nrow=length(t.ids), ncol=5, dimnames=list(t.ids, names.spp))
       
       for(spp in names.spp){
         for(id in t.ids){
        
           load(paste("/Users/annakrystalli/Documents/PREDICTED/",
                      an.ID,"normalised monthly maps/",spp,"/",
                      id, ".RData", sep=""))
         
         if(any(preds$pred.oc<0)){preds$pred.oc<-preds$pred.oc+1}
         preds$pred.oc<-as.numeric(preds$pred.oc>=THRESH.spp[[spp]][thresh])
         
  
         x<-preds$pred.ac[preds$pred.oc==1]
           
 
         skew.tab[id, spp] <- skewness(x)[1]
         
       }}