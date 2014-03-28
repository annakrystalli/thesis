source("analyseTrees.R")

setwd("/Users/annakrystalli/Documents/WORKFLOW/Benthic Analysis/")

load(file="FD analysis table_all.RData")
load(file="TD analysis table_all.RData")
load(file="trait.order.RData")

#Order columns in FD.table
FD.tab<-cbind(FD.table[,1:5], FD.table[,trait.select])

#Make a vector of the number of complete cases for each modality (each trait can have a number of modalities)
compl<-NULL
for(i in 1:(length(FD.tab)-5)){
  compl<-c(compl,sum(complete.cases(FD.tab[6:(length(FD.tab)-i+1)])))}
compl<-rev(compl)

taxa<-taxo.table
taxa<-cbind(Species=rownames(taxa), taxa)
taxa.dist <- taxa2dist(taxa, varstep = F, check=F)

int.td<-NULL
int.sd<-NULL
TAXA<-vector("list", length(unique(compl)))
TAXA.int<-vector("list", length(unique(compl))-1)
SPP<-vector("list", length(unique(compl)))
SPP.int<-vector("list", length(unique(compl))-1)

for(i in 1:length(unique(compl))){
  SPP[[i]]<-FD.tab$AphiaID_acc[complete.cases(FD.tab[,which(compl %in% unique(compl)[1:i])+5])]}

for(i in 1:(length(unique(compl)))){
  
  if(i==length(unique(compl))){}else{
    SPP.int[[i]]<-setdiff(SPP[[i]] ,SPP[[i+1]])   
    clust.mat <- matrix(0, ncol = nrow(taxa))
    colnames(clust.mat) <- rownames(taxa)
    clust.mat[,which(rownames(taxa) %in% SPP.int[[i]])]<-1
    int.td<-c(int.td, taxondive(comm = clust.mat, dis = taxa.dist)$Dplus)
    int.sd<-c(int.sd, taxondive(comm = clust.mat, dis = taxa.dist)$sd.Dplus)
    TAXA.int[[i]]<-taxa[rownames(taxa) %in% SPP.int[[i]],][order(taxa[rownames(taxa) %in% SPP.int[[i]],]$Phylum),]}
  TAXA[[i]]<-taxa[rownames(taxa) %in% SPP[[i]],][order(taxa[rownames(taxa) %in% SPP[[i]],]$Phylum),]                                      
}                                  
n.spp<-unique(compl)     
save(n.spp, SPP, SPP.int, TAXA, TAXA.int, int.td, int.sd, file="report data.RData")

print(int.td)

plot(int.td , n.spp[-1], ylim=range(n.spp), type="b", pch=21, bg="black",
     ylab="no. spp included", xlab="TD spp added")


print(TAXA.int)