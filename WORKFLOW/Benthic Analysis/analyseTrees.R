require(vegan)
require(cluster)
require(ade4)

analyseTrees<-function(FD.tab, int, plot.int, an.ID, metric="gower", type="asymm",select.spp=NULL, 
                       vars=NULL, var.names=var.names, ylim=NULL, xlim=NULL, varstep= F){
  
  
  if(length(unique(select.spp))>1){plot.id<-select.spp}else{plot.id<-int}

  
  C.SIZES<-vector("list", length(plot.id))
  LMs<-vector("list", length(plot.id))
  VARS<-vector("list", length(plot.id))
  TREES<-vector("list", length(plot.id))
  C.IDs<-vector("list", length(plot.id))
  TDs<-vector("list", length(plot.id))
  t.null<-vector("list", length(plot.id))
  SE<-vector("list", length(plot.id))
  
  list.names<-NULL
  
  load(file="TD analysis table_all.RData")
  
  compl<-NULL
  for(i in 1:(length(FD.tab)-5)){
    compl<-c(compl,sum(complete.cases(FD.tab[6:(length(FD.tab)-i+1)])))}
  compl<-rev(compl)
  
  cors<-NULL
  cors.all<-NULL
  n.all<-NULL
  var.n.all<-NULL

  TD<-NULL
  FD<-NULL

  mods<-NULL
  taxo.sum<-NULL
  
  png(paste("plots/",an.ID, " plots.png", sep=""), width=8.1, height=11.1, 
      units="in", res=200, pointsize=15)
  par(mfrow=c(length(plot.int),2), mar=c(1,4,2.5,0.5), oma=c(4,0.1,0.1,0.1))
  label.mat<-matrix(1:(2*length(plot.int)), nrow=length(plot.int), byrow=T)
  
  if(length(select.spp)==1){select.spp<-rep(select.spp, times=length(int))}
  
  k<-0
  
  for(j in 1:length(int)){
    
    #determine variable indicator vector to included in iteration of analysis
    P1<-which(compl>=int[j])
    vars<-P1+5
    
    Dplus<-NULL
    Dexp<-NULL
    Dsd<-NULL
    Dn<-NULL
    
    #determine spp with data for all variables indicator vector
    if(is.null(select.spp)){spp<-complete.cases(FD.tab[,vars])}else{
      spp<-complete.cases(FD.tab[,which(compl>=select.spp[j])+5])}
    
    print(j)
   
    #Subset data to be included in analysis. Get rid of modality columns with no data.
    if(any(apply(as.data.frame(FD.tab[spp,vars]), FUN=sum, 2)==0)){
      data<-as.data.frame(FD.tab[spp,vars[apply(as.data.frame(FD.tab[spp,vars]), FUN=sum, 2)!=0]])}else{
        data<-as.data.frame(FD.tab[spp,vars])}   
    
    #Calculate number of modalities (ie number of columns)
    mods<-rbind(mods, dim(data))
    
    #calculate number of spp / vars
    var.n<-sum(unique(compl)>=int[j])
    n<-sum(spp)
    
 
    
    
    #Specify categorical types to set as binary (asymm / symm)
    cat.type<-vector("list",1)
    names(cat.type)<-type
    cat.type[[1]]<-names(data)[apply(data, FUN=function(x){all(x %in% c(NA, 0, 1))}, 2)]
    
    #Fit functional tree
    d.m<-daisy(data, metric=metric, type=cat.type)
    tree<-hclust(d.m, method="average")
    
    #Fit null taxonomic height trend for number of spp
    t.null[[j]]<-taxoNullify(spp, varstep = varstep)
    
    
    if(plot.id[j] %in% plot.int){k<-k+1
                                 plot(tree, main=paste(letters, ")             ", sep="")[label.mat[k,1]], xlab="", sub="", 
                                      labels=FALSE, ylim=c(0,1), ylab=expression("h"[f]),cex.main=0.85)}
    
    cluster.list<-lapply(apply(tree$merge, 1, list), unlist)
    
    for(i in 1:length(cluster.list)){
      if(cluster.list[[i]][1]<0){a<-abs(cluster.list[[i]][1])}else{a<-abs(cluster.list[[cluster.list[[i]][1]]])}
      if(cluster.list[[i]][2]<0){b<-abs(cluster.list[[i]][2])}else{b<-abs(cluster.list[[cluster.list[[i]][2]]])}
      cluster.list[[i]]<-c(a, b)}
    
    cluster.ids<-vector("list", length(cluster.list))
    cluster.size<-sapply(cluster.list, FUN=length, simplify=T)
    
    for(i in 1:length(cluster.list)){
      cluster.ids[[i]]<-FD.tab[spp,"AphiaID_acc"][cluster.list[[i]]]}
    
    
    
    clust.td<-vector("list", length(cluster.list))
    
    taxa<-taxo.table[which(rownames(taxo.table) %in% FD.tab$AphiaID_acc[spp]),] 
    taxa<-cbind(Species=rownames(taxa), taxa)
    taxa.dist <- taxa2dist(taxa, varstep = varstep, check=F)


    taxo.sum<-rbind(taxo.sum,apply(taxa, FUN=function(x){length(unique(x))}, 2))
    
    for(i in 1:length(cluster.ids)){
      
      
      clust.mat <- matrix(0, ncol = nrow(taxa))
      colnames(clust.mat) <- rownames(taxa)
      clust.mat[,which(rownames(taxa) %in% cluster.ids[[i]])]<-1
      TDdat<-taxondive(comm = clust.mat, dis = taxa.dist)
      clust.td[[i]]<-TDdat$Dplus
      
    Dplus<-c(Dplus, TDdat$Dplus)
    Dsd<-c(Dsd, TDdat$sd.Dplus)
    Dexp<-c(Dexp, TDdat$EDplus)
    Dn<-c(Dn, TDdat$Species)}
    
    th<-tree$height/max(tree$height) 
    Der<-Dsd/sqrt(Dn)
    Ddevse<-(Dplus-Dexp)/Der
    Ddevsd<-(Dplus-Dexp)/Dsd
    
    
    SE[[j]]<-cbind(th,Dn,Dexp, Dsd, Dplus, Der, Ddevse, Ddevsd)
      
 
    if(j==length(plot.id)){xaxt<-"s"
                           xlab<-expression("std h"[f])}else{xaxt<-"n"
                                                             xlab<-""}    
    if(plot.id[j] %in% plot.int){plot(unlist(clust.td)~th, xlab=xlab, 
                                      ylab=expression(Delta[f]^"+"), 
                                      pch=21, bg=grey(level=0.2,alpha=0.3), col=grey(level=0.2,alpha=0.3), cex=0.9, 
                                      main=paste(letters, ")         t = ", var.n, "  s = ",n, sep="")[label.mat[k,2]],
                                      xlim=c(0,1), ylim=c(0,100), xaxt=xaxt, cex.main=0.85)}
    
    lm.all<-lm(y~x, data=data.frame(y=unlist(clust.td),x=th), weights=cluster.size)
    pr.lm.all<-predict(lm.all, newdata=data.frame(x=seq(range(th)[1], 
                                                        range(th)[2], l=20)))
    
    if(plot.id[j] %in% plot.int){
      points(pr.lm.all ~ seq(range(th)[1], 
                             range(th)[2], l=20), type="l", lwd=3, lty=1)
      
      text(x=c(0.85,0.87), y=c(20, 10), 
           labels=c(paste("slope = ", round(summary(lm.all)$coefficients[2,1],1),
                          ", p = ", round(summary(lm.all)$coefficients[2,4],3), sep=""),
                    paste("r.sq = ", round(summary(lm.all)$r.squared, 2))), cex=0.8)
      
      points((0:100)/100, t.null[[j]]$pr.tax[,1], type="l", lwd=1.5, lty=2)
      points((0:100)/100, t.null[[j]]$pr.tax[,2], type="l", lwd=0.7, lty=3)
      points((0:100)/100, t.null[[j]]$pr.tax[,3], type="l", lwd=0.7, lty=3)}
    
    
    cors<-c(cors,cor(unlist(clust.td)[unlist(clust.td)!=100], th[unlist(clust.td)!=100]))
    cors.all<-c(cors.all,cor(unlist(clust.td), th))
    n.all<-c(n.all, n)
    var.n.all<-c(var.n.all,var.n)
    
          
    print(list(var=var.n, n=n, n.all=n.all))
                
    #Calculate overall TD & FD
    td.taxa<-taxo.table[which(rownames(taxo.table) %in% FD.tab$AphiaID_acc[spp]),]
    td.taxa<-cbind(Species=rownames(td.taxa), td.taxa)
    td.dist <- taxa2dist(td.taxa, varstep = varstep, check=F)
    
    td.mat <- matrix(1, ncol = nrow(td.taxa))
    colnames(td.mat) <- rownames(td.taxa)
    
        TD<-c(TD,taxondive(comm = td.mat, dis = td.dist)$D)  
        FD<-c(FD,treedive(comm=td.mat, tree))
        
    
    TREES[[j]]<-list(tree=tree)
    VARS[[j]]<-list(data=names(data))
    LMs[[j]]<-list(lm.all=lm.all)
    C.IDs[[j]]<-list(cluster.ids=cluster.ids)
    TDs[[j]]<-list(clust.td=clust.td)
    C.SIZES[[j]]<-list(cluster.size=cluster.size)
    
    list.names<-c(list.names,paste("s_", n.all[j],"-v_",var.n.all[j], sep=""))
  }
  
  dev.off()
  
  names(LMs)<-list.names
  names(VARS)<-list.names
  names(TREES)<-list.names
  names(C.IDs)<-list.names
  names(TDs)<-list.names
  names(C.SIZES)<-list.names
  taxo.sum<-cbind(taxo.sum[,-1], n=n.all, var=var.n.all)
  mods<-cbind(mods, n=n.all, var=var.n.all)
  
  save(LMs, VARS, file=paste("models/",an.ID," TD vs h models.RData"))
  save(C.SIZES, TDs, file=paste("plots/",an.ID," TD vs #spp data.RData"))
  save(SE, n=n.all, var=var.n.all, file=paste("plots/",an.ID," SE data.RData"))
  
  
  
  return(list(SE=SE, VARS=VARS, TREES=TREES,C.IDs=C.IDs,TDs=TDs,cors=cors,
              cors.all=cors.all, n=n.all, var=var.n.all, 
              TD=TD, FD=FD, t.null=t.null, taxo.sum=taxo.sum, mods=mods))}



AnalysisPlots<-function(res, an.ID=an.ID, ylim3, ylim4){
  spp=0
  var=0
  
  if(length(unique(res$n))!=1){spp=1}
  if(length(unique(res$var.n))!=1){var=1}
  
  png(paste("plots/",an.ID, "analysis plots.png", sep=""), width=8.1, height=4.05*(spp+var), 
      units="in", res=200, pointsize=12)
  
  par(mfrow=c(var+spp,2))
  if(spp==1){plot(res$cors~res$n.all, pch=21, bg="grey", cex=0.9, ylab="strength of correlation", 
                  xlab="number of species", ylim=ylim3)
             lm.spp<-lm(y~x, data=data.frame(y=res$cors, x=res$n.all))
             pr.lm.spp<-predict(lm.spp, newdata=data.frame(x=seq(range(res$n.all)[1], range(res$n.all)[2], l=20)))
             points(pr.lm.spp~ seq(range(res$n.all)[1], range(res$n.all)[2], l=20),type="l" ,lwd=2)}
  
  if(spp==1){plot(res$cors.all~res$n.all, pch=21, bg="grey", cex=0.9, ylab="strength of correlation (all)", xlab="number of species"
                  , ylim=ylim4)
             lm.spp.all<-lm(y~x, data=data.frame(y=res$cors.all, x=res$n.all))
             pr.lm.spp.all<-predict(lm.spp.all, newdata=data.frame(x=seq(range(res$n.all)[1], range(res$n.all)[2], l=20)))
             points(pr.lm.spp.all~ seq(range(res$n.all)[1], range(res$n.all)[2], l=20),type="l" ,lwd=2)}
  
  if(var==1){plot(res$cors~res$var.n.all, pch=21, bg="grey", cex=0.9, ylab= "strength of correlation", xlab="number of traits", ylim=ylim3)
             lm.var<-lm(y~x, data=data.frame(y=res$cors, x=res$var.n.all))
             pr.lm.var<-predict(lm.var, newdata=data.frame(x=seq(range(res$var.n.all)[1], range(res$var.n.all)[2], l=20)))
             points(pr.lm.var~ seq(range(res$var.n.all)[1], range(res$var.n.all)[2], l=20),type="l" ,lwd=2)}
  
  
  if(var==1){plot(res$cors.all~res$var.n.all, pch=21, bg="grey", cex=0.9, ylab="strength of correlation (all)", xlab="number of traits"
                  , ylim=ylim4)
             lm.var.all<-lm(y~x, data=data.frame(y=res$cors.all, x=res$var.n.all))
             pr.lm.var.all<-predict(lm.var.all, newdata=data.frame(x=seq(range(res$var.n.all)[1], range(res$var.n.all)[2], l=20)))
             points(pr.lm.var.all~ seq(range(res$var.n.all)[1], range(res$var.n.all)[2], l=20),type="l" ,lwd=2)}
  
  dev.off()
  
  stats<-NULL
  if(spp==1){stats<-c(stats,
                      extractLMdat(lm.spp.all))}
  if(var==1){stats<-c(stats, 
                      extractLMdat(lm.var.all))}
  lab.spp<-c(paste(an.ID,"spp"))
  lab.var<-c(paste(an.ID,"var"))
  
  cor.mat<-matrix(stats,nrow=3, ncol=1*(spp+var), byrow=F, dimnames=list(c("slope", "p", "r.sq"), 
                                                                         c(lab.spp[spp*(1)], lab.var[var*(1)])))
  return(cor.mat)}

extractLMdat<-function(mod){
  return(c(round(summary(mod)$coefficients[2,1],3),round(summary(mod)$coefficients[2,4],3), round(summary(mod)$r.squared, 2)))}


findPlotLims<-function(res){
  ylim1<-NULL
  xlim1<-NULL
  
  for(i in 1:length(res$TREES)){
    if(any(is.infinite(range(res$TREES[[i]]$tree$height, na.rm=T)))){next}else{xlim1<-c(xlim1, 
                                                                                        try(range(res$TREES[[i]]$tree$height, na.rm=T), silent=T))}
  }
  
  for(i in 1:length(res$TDs)){
    if(any(is.infinite(range(res$TDs[[i]]$clust.td, na.rm=T)))){next}else{ylim1<-c(ylim1,
                                                                                   try(range(res$TDs[[i]]$clust.td, na.rm=T),silent=T))}
  }
  
  return(list(xlim1=range(xlim1), ylim1=range(ylim1)))}


findCorLims<-function(res.both, res.spp, res.var){
  
  ylim3<-range(c(res.both$cors, res.var$cors, res.spp$cors), na.rm=T)
  ylim4<-range(c(res.both$cors.all, res.var$cors.all, res.spp$cors.all), na.rm=T)
  
  return(list(ylim3=ylim3, ylim4=ylim4))
}



taxoNullify<-function(spp, varstep = varstep){
  
  load(file="TD analysis table_all.RData")
  
  td.taxa<-taxo.table[spp,]
  td.taxa<-cbind(Species=rownames(td.taxa), td.taxa)
  
  #td.taxa<-cbind(Species=rownames(td.taxa), td.taxa) use to plot??
  td.dist <- taxa2dist(td.taxa, varstep = varstep, check=F)
  tax.tree<-hclust(td.dist, method="average")


  
  cluster.list<-lapply(apply(tax.tree$merge, 1, list), unlist)
  
  for(i in 1:length(cluster.list)){
    if(cluster.list[[i]][1]<0){a<-abs(cluster.list[[i]][1])}else{a<-abs(cluster.list[[cluster.list[[i]][1]]])}
    if(cluster.list[[i]][2]<0){b<-abs(cluster.list[[i]][2])}else{b<-abs(cluster.list[[cluster.list[[i]][2]]])}
    cluster.list[[i]]<-c(a, b)}
  
  cluster.ids<-vector("list", length(cluster.list))
  
  cluster.size<-sapply(cluster.list, FUN=length, simplify=T)
  
  for(i in 1:length(cluster.list)){
    cluster.ids[[i]]<-row.names(td.taxa)[cluster.list[[i]]]}
  
  clust.td<-vector("list", length(cluster.list))
  

  
  for(i in 1:length(cluster.ids)){
    
    clust.mat <- matrix(0, ncol = nrow(td.taxa))
    colnames(clust.mat) <- rownames(td.taxa)
    clust.mat[,which(rownames(td.taxa) %in% cluster.ids[[i]])]<-1
    clust.td[[i]]<-taxondive(comm = clust.mat, dis = td.dist)$Dplus}
  
  tax.lm<-lm(y ~ x, data=data.frame(y=unlist(clust.td), x=tax.tree$height, weights=cluster.size))
  pr.tax<-predict(tax.lm, newdata=data.frame(x=0:100), interval="confidence")
  
  return(list(tax.td=unlist(clust.td), pr.tax=pr.tax, height=tax.tree$height, tax.lm=tax.lm, 
              cluster.size=cluster.size, cluster.ids=cluster.ids))}







maxCompl<-function(FD.table, var.table){
  
  require(permute)
  
  FD.traits<-names(FD.table[6:length(FD.table)])
  p.traits<-allPerms(1:7)
  traits<-as.character(var.table[1:13,1])
  
  sums<-NULL
  for(j in 1:dim(p.traits)[1]){
    trait.ord<-rep(NA, times=length(traits))
    
    traits.order<-traits[c(1,2,3,(p.traits[j,]+3), 11, 12, 13)]
    for(i in 1:length(traits)){
      trait.ord[grep(traits.order[i],FD.traits)]<-i}
    
    trait.ord<-rank(trait.ord,ties.method="first")
    FD.trait<-FD.traits[order(trait.ord)]
    
    data<-FD.table[,FD.trait]
    
    compl<-NULL
    for(k in 1:length(data)){
      compl<-c(compl,sum(complete.cases(data[1:(length(data)-k+1)])))}
    compl<-rev(compl)
    
    sums<-c(sums,sum(compl))
  }
  
  
  j<-which(sums==max(sums))
  trait.ord<-rep(NA, times=length(traits))
  
  traits.order<-traits[c(1,2,3,(p.traits[j,]+3), 11, 12, 13)]
  for(i in 1:length(traits)){
    trait.ord[grep(traits.order[i],FD.traits)]<-i}
  
  trait.ord<-rank(trait.ord,ties.method="first")
  trait.select<-FD.traits[order(trait.ord)]
  
  return(trait.select)}


AnalysisPlotsAll<-function(res.both, res.var, res.spp, ylim4){
  
  
  png(paste("plots/",an.ID, "analysis plots ALL.png", sep=""), width=8.1, height=8.1, 
      units="in", res=200, pointsize=12)
  
  par(mfrow=c(2,2))
  
  plot(res.both$cors.all~res.both$var.n.all, pch=21, bg="grey", cex=0.9, ylab= "strength of correlation", 
       xlab="number of traits", ylim=ylim4, main=expression(A[1]))
  lm.var<-lm(y~x, data=data.frame(y=res.both$cors.all, x=res.both$var.n.all))
  pr.lm.var<-predict(lm.var, newdata=data.frame(x=seq(range(res.both$var.n.all)[1],
                                                      range(res.both$var.n.all)[2], l=20)))
  points(pr.lm.var~ seq(range(res.both$var.n.all)[1], range(res.both$var.n.all)[2], l=20),
         type="l" ,lwd=2)
  
  
  plot(res.both$cors.all~res.both$n.all, pch=21, bg="grey", cex=0.9, ylab="strength of correlation", 
       xlab="number of species"
       , ylim=ylim4, main=expression(A[2]))
  lm.spp.all<-lm(y~x, data=data.frame(y=res.both$cors.all, x=res.both$n.all))
  pr.lm.spp.all<-predict(lm.spp.all, newdata=data.frame(x=seq(range(res.both$n.all)[1], 
                                                              range(res.both$n.all)[2], l=20)))
  points(pr.lm.spp.all~ seq(range(res.both$n.all)[1], range(res.both$n.all)[2], l=20),
         type="l" ,lwd=2)
  
  
  plot(res.var$cors.all~res.var$var.n.all, pch=21, bg="grey", cex=0.9, ylab= "strength of correlation", 
       xlab="number of traits", ylim=ylim4, main="B")
  lm.var<-lm(y~x, data=data.frame(y=res.var$cors.all, x=res.var$var.n.all))
  pr.lm.var<-predict(lm.var, newdata=data.frame(x=seq(range(res.var$var.n.all)[1],
                                                      range(res.var$var.n.all)[2], l=20)))
  points(pr.lm.var~ seq(range(res.var$var.n.all)[1], range(res.var$var.n.all)[2], l=20),
         type="l" ,lwd=2)
  
  
  plot(res.spp$cors.all~res.spp$n.all, pch=21, bg="grey", cex=0.9, ylab="strength of correlation", 
       xlab="number of species"
       , ylim=ylim4, main="C")
  lm.spp.all<-lm(y~x, data=data.frame(y=res.spp$cors.all, x=res.spp$n.all))
  pr.lm.spp.all<-predict(lm.spp.all, newdata=data.frame(x=seq(range(res.spp$n.all)[1], 
                                                              range(res.spp$n.all)[2], l=20)))
  points(pr.lm.spp.all~ seq(range(res.spp$n.all)[1], range(res.spp$n.all)[2], l=20),
         type="l" ,lwd=2)
  
  dev.off()}

#Bubbleplots.......................
bubblePlot<-function(an.ID, mfrow=c(3,3), res,ylim=c(-120, 10), inches=0.09, Dd="Ddevsd"){
png(paste("plots/",an.ID, "bubble plots.png", sep=""), width=8.1, height=8.1, units="in", res=200, 
    pointsize=15)     
if(Dd=="Ddevsd"){ylab<-"SE"}else{ylab<-"SD"}
par(mfrow=mfrow)
for(i in 1:length(res$SE)){
  symbols(res$SE[[i]][,Dd]~res$SE[[i]][,"th"], 
          circles= sqrt( res$SE[[i]][,"Dn"]/ pi ),
          inches=inches, ylim=ylim, xlim=c(0,1),
          fg="white", bg=grey(level=0.2,alpha=0.3), cex.main=0.8,
          xlab=expression(h[f]), ylab=ylab,
          main=paste("A) t = ",res$var.n.all[i], "  s = ", res$n.all[i]))
  abline(h=0, lwd=1, col="grey")
  abline(h=-2, lwd=0.6, lty=2)
  abline(h=2, lwd=0.6, lty=2)}

  dev.off()}


nMetrics<-function(res, an.ID){
SD<-lapply(res$SE, function(x){x[is.nan(x[,"Ddevsd"]),"Ddevsd" ]<-0
                               return(x)})            
dsd<-unlist(lapply(SD, function(x){mean(x[,"Ddevsd"])}), use.names=F)            
dplus<-unlist(lapply(SD, function(x){mean(x[,"Dplus"])}) , use.names=F)
sd<-unlist(lapply(SD, function(x){mean(x[,"Dsd"])}), use.names=F)
th<-unlist(lapply(SD, function(x){mean(x[,"th"])}), use.names=F)
m<-unlist(lapply(SD, FUN=nrow))

mbins<-lapply(SD,function(x){xs<-split(x[,"Ddevsd"], cut(x[,"th"], breaks=seq(0,1,l=11)))
                             m<-sapply(xs, mean)           
                             m[is.nan(m)]<-NA
                             return(m)})

mat<-matrix(unlist(mbins), ncol=10, nrow=length(mbins), byrow=T)

if(an.ID=="var"){mat<-rbind(matrix(NA, ncol=10, nrow=13-length(dplus), byrow=T),mat)}
if(an.ID=="spp"){mat<-rbind(mat, matrix(NA, ncol=10, nrow=13-length(dplus), byrow=T))}

return(list(dsd=dsd, dplus=dplus, sd=sd, th=th, n=res$n, var=res$var, 
            mat=mat, mbins=mbins, TD=res$TD, FD=res$FD))}


plotParams<-function(res, y, an.ID){ 
  if(y=="spp"){ marg<-10
                ybin<-c(diff(res$n),marg)
                axl<-c(min(res$n), max(res$n)+marg)
                ylabs<-res$n
                imlab<-expression(italic(s))
                if(an.ID=="both"){ylabs<-paste(res$n,"(", res$var,")", sep=" ")
                                  imlab<-expression(paste(italic(s), " (", italic(t),")",
                                                          sep=" "))}
                ylabs[c(2,4)]<-""
                yaxes<-c(res$n, axl[2])
                main="A"
                
  }else{marg<-1
        ybin<-c(abs(diff(res$var)),marg)
        axl<-c(min(res$var)-1, max(res$var))
        ylabs<-res$var
        imlab<-expression(italic(t))
        if(an.ID=="both"){ylabs<-paste(res$var, 
                                       "(",res$n,")", sep=" ")
                          imlab<-expression(paste(italic(t), 
                                                  " (",italic(s),")", sep=" "))}
        yaxes<-c(res$var, axl[1])
        main="A"}   
  
  return(list(marg=marg, ybin=ybin, axl=axl, ylabs=ylabs,
              imlab=imlab, yaxes=yaxes))}


scatterPlots<-function(res.both, res.var, res.spp, pt, cex.p=0.15, 
                       col="black", bg="gray97"){
  
  nSD.b<-nMetrics(res.both)
  nSD.v<-nMetrics(res.var) 
  nSD.s<-nMetrics(res.spp)
  
 png(paste("plots/","scatter plots.png", sep=" "), 
      width=8.1, height=8.1, units="in", res=200, pointsize=pt)
  
  par(mfrow=c(2,2), bg="white", col="black", mar=c(4.5, 4, 0.4, 0.3),las=1)

    plot(nSD.b$dsd~nSD.b$dplus, pch=21,col=col, bg=bg[1], 
       cex=cex.p*pt ,ylab="SD",xlab="dplus")
          points(nSD.v$dsd~nSD.v$dplus, pch=22,col=col, bg=bg[2], 
                 cex=cex.p*pt)
          points(nSD.s$dsd~nSD.s$dplus, pch=23,col=col, bg=bg[3], 
                 cex=cex.p*pt)

  plot(res.both$TD~res.both$FD, pch=21,col=col, bg=bg[1],
       cex=cex.p*pt,ylab="TD", xlab="FD")
        points(res.var$TD~res.var$FD, pch=22,col=col, bg=bg[2], 
               cex=cex.p*pt)
        points(res.spp$TD~res.spp$FD, pch=23,col=col, bg=bg[3], 
               cex=cex.p*pt)


  plot(nSD.b$dsd~res.both$FD, pch=21,col=col, bg=bg[1], 
       cex=cex.p*pt,ylab="SD", xlab="FD", ylim=c(-3.5,0))
        points(nSD.v$dsd~res.var$FD, pch=22,col=col, bg=bg[2], 
               cex=cex.p*pt) 
        points(nSD.s$dsd~res.spp$FD, pch=23,col=col, bg=bg[3], 
              cex=cex.p*pt)
  legend("topright", legend=c("A", "B", "C"), pt.bg=bg, 
         pch=c(21,22,23), bty="n", cex=1.5)
               
               
  plot(nSD.b$dsd~res.both$TD, pch=21,col=col, bg=bg[1], 
       cex=cex.p*pt,ylab="SD", xlab="TD")
      points(nSD.v$dsd~res.var$TD, pch=22,col=col, bg=bg[2], 
              cex=cex.p*pt)
       points(nSD.s$dsd~res.spp$TD, pch=23,col=col, bg=bg[3], 
              cex=cex.p*pt)       
  dev.off()}     



plotPanel1<-function(res.both, res.var, res.spp, an.ID, y, cols=brewer.pal(10,"BrBG"),
                    zlim=c(-12, 8), ylim=range(res.both$n),
                    l.col="darkslategray3",
                    f.col="darkslategray3", f=0.3,
                    wth=1.1, hgt=6, pt=10,   FD.scale=c(0,45),
                    colB="coral3", colC="chartreuse2", lwdl=3,
                    mar.scale=c(5.5, 5, 1.5, 1),
                    mar.image=c(5.5, 4.5, 1.5, 1),
                    mar.side=c(5.5, 0.4, 1.5, 0.4)){
  
  resA<-nMetrics(res.both, an.ID="both")
  resB<-nMetrics(res.var, an.ID="var")
  resC<-nMetrics(res.spp, an.ID="spp")
  
  ppA<-plotParams(res.both, "spp", an.ID="both")
  ppB<-plotParams(res.var, "var", an.ID="var")
  ppC<-plotParams(res.spp, y="spp", an.ID="spp")
        
  res<-resA
  pp<-ppA
  
  png(paste("plots/",unique(an.ID,y), "panel plots.png", sep=" "), 
      width=8.1, height=6, units="in", res=200, pointsize=pt) 
  
  layout(cbind(2,2,matrix(rep(c(rep(1, 10/wth),3,4,5), hgt), nrow=hgt, ncol=10/wth+3, byrow=T)))
  
  
  
  if(an.ID=="both"){par(mgp=c(5.2,1,0))
                    mar.image[c(1,2)]<-7.5
                    mar.scale[1]<-7.5
                    mar.side[1]<-7.5}
  par(mar=mar.image, las=1)
  
  yaxes<-pp$yaxes
  
  if(y=="var"){pp$yaxes<-rev(pp$yaxes)}
  image(x=seq(0,1,l=11), y=pp$yaxes, z=t(res$mat), yaxt="n",
        col=cols,  cex.lab=1.7, xaxp=c(0, 0.8, 4),
        ylim=range(pp$yaxes), zlim=zlim, cex.axis=1.3,
        ylab=pp$imlab,  xlab=expression(italic(h[n])))
  
  par(xpd=T)
  legend("bottomright",ncol=2,legend=c("B","C"), 
         col=c(colB,colC), lwd=lwdl, lty=1, bty="n")
  par(xpd=F)
  
  axis(2, at=pp$yaxes[-length(pp$yaxes)], labels=pp$ylabs, cex.axis=1.1, font=2)
  
            abline(v=seq(0.1, 0.9, l=9), col="azure1", lwd=0.8,
                   h=pp$yaxes[c(-1, -length(pp$yaxes))])
            
            abline(h=unique(resB$n), lty=3, col="black")
            abline(h=ppA$yaxes[length(ppC$yaxes)], lty=4, col="black")
  
  par(mar=mar.scale, las=1, mgp=c(3,1,0))
  image.scale(z=res$mat, zlim=zlim, col = cols,  xlab="",
              ylab=expression(paste(mu[italic(h)]," "[sigma],"", Delta[italic(n)]^"+")), 
              horiz=F, ylim=c(-10, 2), cex.lab=1.5, cex.axis=1.1)
  
  
  par(bg="white", col="black", mar=mar.side, las=1, yaxs="i")
  
  ybin<-ppA$ybin
  if(y=="var"){X<-rev(res$var)
               ybin<- -ybin}
  if(y=="spp"){X<-res$n}
  
  plot(list(x=lowess(cbind(X,res$dsd), f=f)[[2]],
            y=lowess(cbind(X,res$dsd), f=f)[[1]]+ybin/2),
       cex.lab=1.3,ylab=NULL, ylim=pp$axl,
       xlab=expression(paste(mu," "[sigma],"", Delta[italic(n)]^"+")), 
       type="l", col = l.col, xlim=c(-3.5,0),
       frame.plot=F,axes=FALSE)
  Axis(side=1)

  
  polygon(x=c(-4,lowess(cbind(X,res$dsd), f=f)[[2]],-4),
          y=c(min(X)+ybin[1]/2,lowess(cbind(X,res$dsd), f=f)[[1]]+ybin/2,max(X)+ybin[length(ybin)]/2),
          density = -2, angle = 90,
          col = f.col, border=l.col)
  abline(v=-2, lty=4, lwd=1.2)
  
          Xb<-rev(resB$var)
                  ybinb<- -ppB$ybin
  
          Xc<-resC$n
                  ybinc<-ppC$ybin 
  
              lines(x=rev(lowess(cbind(X,resB$dsd), 
                                    f=f)[[2]]),
                      y=lowess(cbind(X,resB$dsd), 
                                                  f=f)[[1]]+rev(ybin[14-1:length(Xb)])/2,
                     
                      col =colB, lty=1, lwd=lwdl)
                
              lines(x=lowess(cbind(Xc,resC$dsd), f=f)[[2]],
                      y=lowess(cbind(Xc,resC$dsd), 
                                                  f=f)[[1]]+ybin[1:length(Xc)]/2,
                       col =colC, lty=1, lwd=lwdl)
             
  
  plot(list(x=lowess(cbind(X,res$FD), f=f)[[2]],
            y=lowess(cbind(X,res$FD), f=f)[[1]]+ybin/2),
       cex.lab=1.3,ylab=ylab,ylim=pp$axl,
       xlab="FD", type="l", xlim=FD.scale,
       frame.plot=F, col = l.col, 
       axes=FALSE)
  Axis(side=1)
  
  polygon(x=c(0,lowess(cbind(X,res$FD), f=f)[[2]],0),
          y=c(min(X)+ybin[1]/2,lowess(cbind(X,res$FD), f=f)[[1]]+ybin/2,max(X)+ybin[length(ybin)]/2),
          density = -2, angle = 90,
          col = f.col, border=l.col)
  
            
  lines(x=rev(lowess(cbind(X,resB$FD), 
                 f=f)[[2]]),
        y=lowess(cbind(X,resB$FD), 
                 f=f)[[1]]+rev(ybin[14-1:length(Xb)])/2,col =colB, lty=1, lwd=lwdl)           
            
            lines(x=lowess(cbind(Xc,resC$FD), f=f)[[2]],
                    y=lowess(cbind(Xc,resC$FD), f=f)[[1]]+ybinc/2,
                    col =colC, lty=1, lwd=lwdl) 
  
  
  
  plot(list(x=lowess(cbind(X,res$TD), f=f)[[2]],
            y=lowess(cbind(X,res$TD), f=f)[[1]]+ybin/2),
       cex.lab=1.3,ylab=ylab, ylim=axl, col= l.col,
       xlab=expression(Delta^"+"), 
       type="l", xlim=c(92,96),
       frame.plot=F, 
       axes=FALSE)
  Axis(side=1)
  
  polygon(x=c(0,lowess(cbind(X,res$TD), f=f)[[2]],0),
          y=c(min(X)+ybin[1]/2,lowess(cbind(X,res$TD), f=f)[[1]]+ybin/2,max(X)+ybin[length(ybin)]/2),
          density = -2, angle = 90,
          col = f.col, border=l.col)
  
  
            lines(x=rev(lowess(cbind(X,resB$TD), 
                           f=f)[[2]]),
                  y=lowess(cbind(X,resB$TD), 
                           f=f)[[1]]+rev(ybin[14-1:length(Xb)])/2,
                  col =colB, lty=1, lwd=lwdl)           
            
            lines(x=lowess(cbind(Xc,resC$TD), f=f)[[2]],
                  y=lowess(cbind(Xc,resC$TD), f=f)[[1]]+ybinc/2,
                  col =colC, lty=1, lwd=lwdl) 
  

  
  dev.off()}


figure5<-function(res.both, res.var, res.spp, cols=brewer.pal(10,"BrBG"),
                     zlim=c(-12, 8), ylim=range(res.both$n),
                     l.col="darkslategray3",f=0.3,
                     f.col="darkslategray3", cax=0.8,
                     widths=c(3,6,1,1,1,6,1,1,1), heights=4, pt=10,   FD.scale=c(0,45),
                     mar.scale=c(5.5, 5, 1.5, 0),
                     mar.image=c(5.5, 4.5, 1.5, 0),
                     mar.side=c(5.5, 0.25, 1.5, 0.4)){
  
  resA<-nMetrics(res.both, an.ID="both")
  resB<-nMetrics(res.var, an.ID="var")
  resC<-nMetrics(res.spp, an.ID="spp")
  
  ppA<-plotParams(res.both, "spp", an.ID="both")
  ppB<-plotParams(res.var, "var", an.ID="var")
  ppC<-plotParams(res.spp, y="spp", an.ID="spp")
  
  res<-resA
  pp<-ppA
  
  Xb<-rev(resB$var)
  ybinb<- -ppB$ybin
  
  Xc<-resC$n
  ybinc<-ppC$ybin
  
  png(paste("plots/f5.png", sep=" "), 
      width=8.1, height=3, units="in", res=200, pointsize=pt) 
  
  layout(matrix(c(2,1,3,4,5, 6, 7, 8, 9),
                nrow=1, ncol=9, byrow=T),
         heights=heights, widths=widths)
  
  
  y<-"var"

        par(mar=mar.image, las=1)
        
        yaxes<-pp$yaxes
        
        if(y=="var"){yaxes<-rev(yaxes)}
        image(x=seq(0,1,l=11), y=pp$yaxes, z=t(resB$mat), yaxt="n",
              col=cols,  cex.lab=1.7, xaxp=c(0, 0.8, 4),
              ylim=range(pp$yaxes), zlim=zlim, cex.axis=1.3,
              ylab=ppB$imlab,  xlab=expression(italic(h[n])))
        
       ylabs<-resA$var
        ylabs[c(2,4)]<-""
        axis(2, at=pp$yaxes[-length(pp$yaxes)], labels=ylabs, cex.axis=1.1, font=2)
        
        abline(v=seq(0.1, 0.9, l=9), col="azure1", lwd=0.8,
               h=pp$yaxes[c(-1, -length(pp$yaxes))])
        
      
        abline(h=ppA$yaxes[length(ppB$yaxes)], lty=4, col="black")
        
        par(mar=mar.scale, las=1, mgp=c(3,1,0))
        image.scale(z=res$mat, zlim=zlim, col = cols,  xlab="",
                    ylab=expression(paste(mu[italic(h)]," "[sigma],"", Delta[italic(n)]^"+")), 
                    horiz=F, ylim=c(-10, 2), cex.lab=1.5, cex.axis=1.1)
        
        
        par(bg="white", col="black", mar=mar.side, las=1, yaxs="i")
        
        ybin<-ppA$ybin
        if(y=="var"){X<-rev(rev(res$n)[1:max(resB$var)])}
        if(y=="spp"){X<-resA$n}
        
        
      
        
        plot(list(x=lowess(cbind(X,resB$dsd), f=f)[[2]],
                  y=lowess(cbind(X,resB$dsd), 
                           f=f)[[1]]+rev(ybin[14-1:length(Xb)])/2),
             cex.lab=1.3,ylab=NULL, ylim=ppA$axl,
             xlab=expression(paste(mu," "[sigma],"", Delta[italic(n)]^"+")), 
             type="l", col = l.col, xlim=c(-3.5,0),
             frame.plot=F,axes=FALSE)
        Axis(side=1, cex.axis=cax)
      
      
                polygon(x=c(-4,lowess(cbind(X,resB$dsd), f=f)[[2]],-4),
                        y=c(min(X)+ppA$ybin[which(res$n==min(X))]/2,
                            lowess(cbind(X,resB$dsd), 
                                                      f=f)[[1]]+
                              rev(ybin[14-1:length(Xb)])/2,
                        max(X)+ppA$ybin[which(res$n==max(X))]/2),
                        density = -2, angle = 90,
                        col = f.col, border=l.col)
                abline(v=-2, lty=4, lwd=1.2)
       
        
      
        
        plot(list(x=lowess(cbind(X,resB$FD), f=f)[[2]],
                  y=lowess(cbind(X,resB$FD), 
                           f=f)[[1]]+rev(ybin[14-1:length(Xb)])/2),
             cex.lab=1.3,ylab=NULL, ylim=ppA$axl,
             xlab="FD", 
             type="l", col = l.col, xlim=FD.scale,
             frame.plot=F,axes=FALSE)
        Axis(side=1, cex.axis=cax)
        
                
                polygon(x=c(0,lowess(cbind(X,resB$FD), f=f)[[2]],0),
                        y=c(min(X)+ppA$ybin[which(res$n==min(X))]/2,
                            lowess(cbind(X,resB$FD), 
                                   f=f)[[1]]+rev(ybin[14-1:length(Xb)])/2,
                            max(X)+ppA$ybin[which(res$n==max(X))]/2),
                        density = -2, angle = 90,
                        col = f.col, border=l.col)
        
        
        plot(list(x=lowess(cbind(X,resB$TD), f=f)[[2]],
                  y=lowess(cbind(X,resB$TD), 
                           f=f)[[1]]+ rev(ybin[14-1:length(Xb)])/2),
             cex.lab=1.3,ylab=ylab, ylim=ppA$axl, col= l.col,
             xlab="TD", lwd=1,
             type="l", xlim=c(92,96),
             frame.plot=F, 
             axes=FALSE)
        Axis(side=1, cex.axis=cax)
        
        
        
        polygon(x=c(0,lowess(cbind(X,resB$TD), f=f)[[2]],0),
                y=c(min(X)+ybin[1]/2,lowess(cbind(X,resB$TD), 
                                            f=f)[[1]]+rev(ybin[14-1:length(Xb)])/2,
                    max(X)+ppA$ybin[which(res$n==max(X))]/2),
                density = -2, angle = 90,
                col = f.col, border=l.col)
        
  
  yaxes<-pp$yaxes
  par(mar=mar.image, las=1)
  
  y="spp"
  
  
          if(y=="var"){pp$yaxes<-rev(pp$yaxes)}
          image(x=seq(0,1,l=11), y=pp$yaxes, z=t(resC$mat), yaxt="n",
                col=cols,  cex.lab=1.7, xaxp=c(0, 0.8, 4),
                ylim=range(pp$yaxes), zlim=zlim, cex.axis=1.3,
                ylab=ppC$imlab,  xlab=expression(italic(h[n])))
          
          ylabs<-resA$n
          ylabs[c(2,4)]<-""
          axis(2, at=pp$yaxes[-length(pp$yaxes)], labels=ylabs, cex.axis=1.1, font=2)
          
          abline(v=seq(0.1, 0.9, l=9), col="azure1", lwd=0.8,
                 h=pp$yaxes[c(-1, -length(pp$yaxes))])
          
          
          abline(h=ppA$yaxes[length(ppC$yaxes)], lty=4, col="black")
          
        
          par(bg="white", col="black", mar=mar.side, las=1, yaxs="i")
          
          ybin<-ppA$ybin
          if(y=="var"){X<-rev(res$var)
                       ybin<- -ybin}
          if(y=="spp"){X<-resC$n}
          
        
          plot(list(x=lowess(cbind(X,resC$dsd), f=f)[[2]],
                    y=lowess(cbind(X,resC$dsd), 
                             f=f)[[1]]+ybin[1:length(X)]/2),
               cex.lab=1.3,ylab=NULL, ylim=ppA$axl,
               xlab=expression(paste(mu," "[sigma],"", Delta[italic(n)]^"+")), 
               type="l", col = l.col, xlim=c(-3.5,0),
               frame.plot=F,axes=FALSE)
          Axis(side=1, cex.axis=cax)
          
          
          polygon(x=c(-4,lowess(cbind(X,resC$dsd), f=f)[[2]],-4),
                  y=c(min(X)+ppA$ybin[which(res$n==min(X))]/2,
                      lowess(cbind(X,resC$dsd), 
                             f=f)[[1]]+ybin[1:length(X)]/2,
                      max(X)+ppA$ybin[which(res$n==max(X))]/2),
                  density = -2, angle = 90,
                  col = f.col, border=l.col)
          abline(v=-2, lty=4, lwd=1.2)
          
          
          
          
          plot(list(x=lowess(cbind(X,resC$FD), f=f)[[2]],
                    y=lowess(cbind(X,resC$FD), 
                             f=f)[[1]]+ybin[1:length(X)]/2),
               cex.lab=1.3,ylab=NULL, ylim=ppA$axl,
               xlab="FD", 
               type="l", col = l.col, xlim=FD.scale,
               frame.plot=F,axes=FALSE)
          Axis(side=1, cex.axis=cax)
          
          
          polygon(x=c(0,lowess(cbind(X,resC$FD), f=f)[[2]],0),
                  y=c(min(X)+ppA$ybin[which(res$n==min(X))]/2,
                      lowess(cbind(X,resC$FD), 
                             f=f)[[1]]+ybin[1:length(X)]/2,
                      max(X)+ppA$ybin[which(res$n==max(X))]/2),
                  density = -2, angle = 90,
                  col = f.col, border=l.col)
          
                
          plot(list(x=lowess(cbind(X,resC$TD), f=f)[[2]],
                    y=lowess(cbind(X,resC$TD), 
                             f=f)[[1]]+ybin[1:length(X)]/2),
               cex.lab=1.3,ylab=NULL, ylim=ppA$axl,
               xlab="TD", 
               type="l", col = l.col, xlim=c(92,96),
               frame.plot=F,axes=FALSE)
          Axis(side=1, cex.axis=cax)
          
          
                  polygon(x=c(0,lowess(cbind(X,resC$TD), f=f)[[2]],0),
                          y=c(min(X)+ppA$ybin[which(res$n==min(X))]/2,
                              lowess(cbind(X,resC$TD), 
                                     f=f)[[1]]+ybin[1:length(X)]/2,
                              max(X)+ppA$ybin[which(res$n==max(X))]/2),
                          density = -2, angle = 90,
                          col = f.col, border=l.col)
          
  dev.off()}


figure2<-function(res.both, res.var, res.spp, cols, lwds){
  
  png("plots/f2.png", 
      width=6, height=6, units="in", res=200, pointsize=pt)
  
 ids<-c(2:6, 1)

      p<-apply(res.both$taxo.sum[,1:6], FUN=function(x){x/max(x)}, 2)
      plot(p[,1]*100~res.both$taxo.sum[,"n"], type="l", lwd=lwds, col=cols[1], 
           ylab="% max taxonomic level", xlab=expression(italic(s)))
      
              for(i in 2:dim(p)[2]){
                points(p[,i]*100~ res.both$taxo.sum[,"n"], col=cols[i], 
                       type="l", lwd=lwds)
              }
             
      
      labs<-c("Species", colnames(p)[-6])
      legend("bottomright", legend=rev(labs), col=rev(cols[order(ids)]), lty=1, lwd=3, 
             bty="n")
  
  dev.off()
      
}

plotTree<-function(res, t, j){
  
  l.id<-which(res$var==t, arr.ind=T)
  
  tree<-res$TREES[[l.id]]$tree
  
  plot(rev(as.dendrogram(tree, hang= -1)),horiz=T, leaflab = "none",
       xlim=c(0,1),
           main=paste("t = ", res$var[l.id],", s = ", res$n[l.id],  sep=""))
  abline(v=max(tree$height), lty=2)
  
  mtext(paste(letters[1:3][j]), line=3, side=2, outer=F, las=1, 
        at=res$n[l.id], font=2)
}

plotSD<-function(res, t, j){
    
  l.id<-which(res$var==t, arr.ind=T)
  
    plot(res$SE[[l.id]][,"Ddevsd"]~res$SE[[l.id]][,"th"], 
         ylim=c(-12, 2), xlim=c(0,1), pch=21, cex=1, col="white",
         bg=grey(level=0.2,alpha=0.4), 
         xlab=expression(h[f]), ylab="SDs from mean",
         main="")
    abline(h=0, lwd=4)
    abline(h=-2, lwd=2, lty=5, , col=grey(0.3))
    abline(h=2, lwd=2, lty=5, , col=grey(0.3))
  
  mtext(paste(letters[4:6][j]), line=3, side=2, outer=F, las=1, at=1, font=2)
}
    
    
figure3<-function(res, t=c(1,4,13), pt){
  
  png("plots/f3.png", 
      width=8.1, height=6, units="in", res=200, pointsize=pt)
  
  
  par(mfcol=c(2,length(t)))
  
  for(i in 1:length(t)){
  
  par(mar=c(4, 4 , 3.8, 0.3))
  plotTree(res, t=t[i], j=i)
  
  par(mar=c(5, 4 , 0, 0.3))
  plotSD(res, t=t[i], j=i)
  
}
  
  
  dev.off()}


#.....................Image scale
#This function creates a color scale for use with e.g. the image()
#function. Input parameters should be consistent with those
#used in the corresponding image plot. The "horiz" argument
#defines whether the scale is horizonal(=TRUE) or vertical(=FALSE).
#Depending on the orientation, x- or y-limits may be defined that
#are different from the z-limits and will reduce the range of
#colors displayed.

image.scale <- function(z, zlim, col = heat.colors(12),
                        breaks, horiz=TRUE, ylim=NULL, xlim=NULL, ...){
  if(!missing(breaks)){
    if(length(breaks) != (length(col)+1)){stop("must have one more break than colour")}
  }
  if(missing(breaks) & !missing(zlim)){
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1)) 
  }
  if(missing(breaks) & missing(zlim)){
    zlim <- range(z, na.rm=TRUE)
    zlim[2] <- zlim[2]+c(zlim[2]-zlim[1])*(1E-3)#adds a bit to the range in both directions
    zlim[1] <- zlim[1]-c(zlim[2]-zlim[1])*(1E-3)
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
  }
  poly <- vector(mode="list", length(col))
  for(i in seq(poly)){
    poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
  }
  xaxt <- ifelse(horiz, "s", "n")
  yaxt <- ifelse(horiz, "n", "s")
  if(horiz){YLIM<-c(0,1); XLIM<-range(breaks)}
  if(!horiz){YLIM<-range(breaks); XLIM<-c(0,1)}
  if(missing(xlim)) xlim=XLIM
  if(missing(ylim)) ylim=YLIM
  plot(1,1,t="n",ylim=ylim, xlim=xlim, xaxt=xaxt, yaxt=yaxt, xaxs="i", yaxs="i", ...)  
  for(i in seq(poly)){
    if(horiz){
      polygon(poly[[i]], c(0,0,1,1), col=col[i], border=NA)
    }
    if(!horiz){
      polygon(c(0,0,1,1), poly[[i]], col=col[i], border=NA)
    }
  }
}