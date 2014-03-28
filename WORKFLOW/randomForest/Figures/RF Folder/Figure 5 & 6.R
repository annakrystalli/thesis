rm(list=ls())
data=" mod"
unNormalise<-function(x, var.id){
  
  load(file="~/Documents/SATELLITE/Associated data/r files/min.max.ind pre processing file.RData")
  
  for(var in var.id){
    x.v<-x[,var]
    min<-as.numeric(min.max.tab$min[min.max.tab$var==var])
    max<-as.numeric(min.max.tab$max[min.max.tab$var==var])
    x[,var]<-min+(x.v*(max-min))}
  
  return(x)}

varnames<-c("year",
            "month",  "T.m", "Sed.m", 
            "Sedf.m",  "Ch.m", "fdens.m", 
            "fdist.m",  "fside.m",  "fdens.c.m", 
            "fdist.c.m",  "fside.c.m", 
            "bath", "NAO", "wNAO")

load(file=paste("/Users/annakrystalli/documents/TRAINING DATA/y", data, " training data.RData", sep=""))
load("~/Documents/TRAINING DATA/normalized (min.max) x training data.RData")
load(file="~/Documents/SATELLITE/Associated data/r files/Bathymetry.RData")

x<-norm.x.m


time<-norm.x.m$time*24
time[time>=4 & time<20]<-"day"
time[time!="day"]<-"night"
rm(norm.x.m)

x$time[x$time>=0.25 & x$time<0.75]<-"day"
x$time[x$time!="day"]<-"night"
x$time<-as.factor(x$time)
x<-unNormalise(x, varnames)

all.samples<-paste(x$tow,x$sample.no., sep="-")

x<-data.frame(spl.id=sort(all.samples), x[order(all.samples),])
y.m<-y.m[order(all.samples),]
all.samples<-sort(all.samples)
time<-time[order(all.samples)]

names.spp<-names(y.m)
names.spp[3]<-"cfin"
names.spp[4]<-"chel"
names(y.m)<-names.spp

y.moc<-y.m
y.moc[y.m!=0]<-1

route<-gsub("[^A-Z]", "", x$tow)

route.table<-table(route, y.moc$cfin)
route.hist<-rev(sort(table(route)))
route.oc<-rev(sort(route.table[,2]/rowSums(route.table)))
route.all.oc<-rev(sort(route.table[,2]/sum(route.table[,2])))


png(filename = "~/Documents/THESIS/RF Chapter/Figures/Figure5.png", 
    pointsize=9.5, units = "in", res=300, width=5, height=8)


layout(matrix(1:3, 3,1, byrow=F), widths=1, heights=c(2.5,1,1))
par(mar=c(2,5, 0.1, 0.2), oma=c(0,0,0.7,0))
boxplot(x$bath ~ factor(route, levels=names(route.oc)), names=rep("",9), ylab="depth (m)")
abline(h=50, lty=2)

barplot(route.oc,  axisnames=F, ylab="Total spl occ")
barplot(route.hist[names(route.oc)], ylab="No. samples", cex.lab=1.2)

dev.off()







png(filename = "~/Documents/THESIS/RF Chapter/Figures/Figure6.png", 
    pointsize=9.5, units = "in", res=300, width=5, height=8)

time.table<-table(x$time, route)
time.ratio<-rev(sort(time.table[2,]/colSums(time.table)))
route.table<-table(route, y.moc$metrilu)
route.hist<-rev(sort(table(route)))
route.oc<-rev(sort(route.table[,2]/rowSums(route.table)))
route.all.oc<-rev(sort(route.table[,2]/sum(route.table[,2])))

layout(matrix(1:4, 4,1, byrow=F), widths=1, heights=c(2,1,1,1))
par(mar=c(2,5, 0.1, 0.2), oma=c(0,0,0.7,0))
boxplot(x$bath ~ factor(route, levels=names(route.oc)), names=rep("",9), ylab="depth (m)")
abline(h=50, lty=2)

boxplot(time.ratio[names(route.oc)]~ factor(names(route.oc), levels=names(route.oc)),
        names=rep("",9), ylab="Prop night", ylim=c(0,1))
abline(h=0.5, lty=2)

barplot(route.oc,  axisnames=F, ylab="Total spl occ", ylim=c(0,1))
barplot(route.hist[names(route.oc)], ylab="No. samples", cex.lab=1.2)

dev.off()






baths<-as.vector(bath.m[bath.m!=0])
sum(baths>100)/length(baths)
sum(x$bath>100)/length(x$bath)

table(y.moc$metrilu, x$time)



boxplot(x$bath ~ factor(route, levels=names(route.all.oc)),  ylab="depth (m)")
abline(h=50, lty=2)



png(filename = "~/Documents/THESIS/RF Chapter/Figures/Figure7.png", 
    pointsize=9.5, units = "in", res=300, width=8, height=11)

par(mfcol=c(5,2))




for(spp in names.spp){

  data=""
  load(file=paste("/Users/annakrystalli/documents/TRAINING DATA/y", data, " training data.RData", sep=""))
  names.spp<-names(y.m)
  names.spp[3]<-"cfin"
  names.spp[4]<-"chel"
  names(y.m)<-names.spp
  
  hist(y.m[y.m[,spp]!=0,spp], breaks=0:12, include.lowest = F, main=spp)
}


data=" mod"
load(file=paste("/Users/annakrystalli/documents/TRAINING DATA/y", data, " training data.RData", sep=""))
names.spp<-names(y.m)
names.spp[3]<-"cfin"
names.spp[4]<-"chel"
names(y.m)<-names.spp

for(spp in names.spp){

  hist(y.m[y.m[,spp]!=0,spp], breaks=0:5, include.lowest = F, main=spp)
}


dev.off()