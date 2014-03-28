load(file=paste("/Users/annakrystalli/documents/TRAINING DATA/y training data.RData", sep=""))

y<-y.m


for(i in 1:5){
y[,i]<-c(0,1,1,1,2,3,3,4,4,4,5,5,5)[as.factor(y.m[,i])]}

for(i in 1:5){
  print(names.spp[i])
  print(table(y.m[,i]))
  print(table(y[,i]))}


y.m<-y

save(y.m, file=paste("/Users/annakrystalli/documents/TRAINING DATA/y mod training data.RData", sep=""))

load(file=paste("/Users/annakrystalli/documents/TRAINING DATA/y mod training data.RData", sep=""))


mAC.tab<-matrix(0, 6, 5)
m<-as.matrix(apply(y.m, FUN=table, 2))
l<-lapply(m, FUN=length)

for(i in 1:5){
mAC.tab[1:l[[i]],i]<-unlist(m[i])}

colnames(mAC.tab)<-rownames(m)


#mAC frequency distribution barplot
barplot(mAC.tab, col=c("white", paste("darkslategray", 1:4, sep=""), "black"),
        xlim=c(-1, 6))

      legend(legend=0:5, x=-0.9, y=200000,
             fill=c("white", paste("darkslategray", 1:4, sep=""), "black"))


layout()
barplot(mAC.tab[,i], col=c("white", paste("darkslategray", 1:4, sep=""), "black"),
         horiz=T, width=log(diff(start.br)))

legend(legend=0:5, x=-0.9, y=200000,
       fill=c("white", paste("darkslategray", 1:4, sep=""), "black"))


start.br<-c(0,1,4,12,51, 501, 40001)
diff(start.br)