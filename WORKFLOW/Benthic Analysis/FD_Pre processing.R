rm(list=ls())


user<-"annakrystalli"

setwd(dir=paste("/Users/",
               user, "/Dropbox/Data Handling Project, Tom Webb and Jane Hosegood/Exported Access Files/", 
               sep=""))

#List all files
trait.files<-list.files(path=paste("/Users/",
                                   user, "/Dropbox/Data Handling Project, Tom Webb and Jane Hosegood/Exported Access Files/", 
                                   sep=""))

trait.ids<-gsub(" ", "_", trait.files)
trait.ids<-gsub(".csv", "", trait.ids)

#Open all tables and assign
for(i in 1:length(trait.files)){
assign(trait.ids[i], read.csv(trait.files[i]))}


#Compile trait table
trait.ids<-setdiff(trait.ids, "Taxonomy")
n.tr<- length(trait.ids)

master.traits<-cbind(Taxonomy, matrix(NA, nrow=dim(Taxonomy)[1], ncol=length(trait.ids), dimnames=list(NULL, trait.ids)))

#Identify no. trait records per species
for (i in 1:length(trait.ids)){
master.traits[which(master.traits$AphiaID %in% get(trait.ids[i])$AphiaID),trait.ids[i]]<-1}


master.traits<-cbind(master.traits,
                     total.spp.traits=rowSums(master.traits[,5:length(master.traits)], na.rm=T))

#Subset only species with >=4 traits
sub.traits<-master.traits[master.traits$total.spp.traits>=4,]


colSums(table(sub.traits$AphiaID, sub.traits$total.spp.traits))
var.table<-data.frame(var=names(rev(sort(colSums(sub.traits[,trait.ids], na.rm=T)))),
           no.spp=rev(sort(colSums(sub.traits[,trait.ids], na.rm=T))), row.names=NULL)


#STart new table to add trait columns
master.table<-sub.traits[,1:4]
n.sub<-dim(master.table)[1]

#Export species list to look up on WoRMS
setwd("/Users/annakrystalli/Documents/WORKFLOW/Benthic analysis/")
species.list<-master.table[,c("AphiaID", "Species")]
save(species.list, file="Species list 4.RData")
write.csv(species.list[,2], file="Species list 4.csv")
write.csv(species.list[,1], file="Species Aphia list 4.csv")

#CLEAN
Sociability$Sociability[Sociability$Sociability=="129868"] <-"solitary"
Sociability$Sociability<-as.character(Sociability$Sociability)
Reproductive_Frequency[Reproductive_Frequency$ReproductiveFrequency %in% c("Annual", "<annual"),"ReproductiveFrequency"]<-"annual"
Migration$Migration[which(Migration$AphiaID == 107703)]<-"regular"
Developmental_Mechanism[Developmental_Mechanism$DevelopmentalMechanism=="unknown","DevelopmentalMechanism"]<-"oviparous"


#Aggregate categories
  #Aggregate all suspension feeding & deposit feeding
    fm<-as.character(Feeding_Method$FeedingMethod)
    fm[grep("suspension", fm)]<-"suspension feeder"
    fm[grep("deposit", fm)]<-"deposit feeder"
    Feeding_Method$FeedingMethod<-factor(fm)


  #Aggregate all epi- under epifauna
    hb<-as.character(Habitat$Habitat)
    hb[grep("epi", hb)]<-"epifaunal"
    Habitat$Habitat<-factor(hb)

  #Aggregate all plankton, algae & macroalgae, micrpalgae & micro-orgs
      #meio & benthic
    dt<-as.character(Diet$Diet)
    dt[grep("plankton", dt)]<-"plankton"
    dt[dt=="algae"]<-"macroalgae"
    dt[dt=="microalgae"]<-"micro-organisms"
    dt[dt=="meiobenthic organisms"]<-"benthic organisms"
    Diet$Diet<-factor(dt)

  

#Make categorical variables binary
cat.traits<-c("Developmental_Mechanism", 
              "Diet",  "Feeding_Method", "Habitat",  
              "Larval_Feeding_Strategy","Migration", "MovementMethod", 
              "Reproductive_Frequency", "Reproductive_Period", 
              "Sociability")
cat.trait.ids<-gsub("_", "", cat.traits)

for(i in 1:length(cat.traits)){
  
  
  cat.trait<-get(cat.traits[i])
  cat.trait<-cat.trait[cat.trait$AphiaID %in% master.table$AphiaID,]
  
  cat.tab<-table(cat.trait[, "AphiaID"], as.character(cat.trait[, cat.trait.ids[i]]))
  col.names<-paste(cat.traits[i], gsub(" ", "_",dimnames(cat.tab)[[2]]), sep="-")
  mat<-matrix(NA, ncol=length(col.names), nrow=n.sub, dimnames=list(NULL, col.names))
  mat[match(as.numeric(dimnames(cat.tab)[[1]]), master.table$AphiaID),]<-cat.tab
  
  master.table<-cbind(master.table, mat)
  
}

    #Correct migration-no evidence BUT row 40 (Buccinum undatum, both migratory & non-migratory entries!!)
        
        mg<-which(master.table[,"Migration-regular"]>=1 | master.table[,"Migration-irregular/single_migration"]>=1) 
        no.ev<-which(master.table[, "Migration-no_evidence"]>=1)
        master.table[no.ev[no.ev %in% mg],"Migration-no_evidence"]<-0
        master.table[which(master.table[,"Migration-no_evidence"]==1),"Migration-non-migratory"]<-1
        master.table<-master.table[,names(master.table) !="Migration-no_evidence"]

#.........................................................................................
#SELECTION OF INDIVIDUAL VALUES PER CONTINUOUS TRAIT WHEN MULTIPLE AVAILABLE. ADD TO MASTER TABLE

#Reproductive_Timing

Reproductive_Timing<-Reproductive_Timing[Reproductive_Timing$AphiaID %in% master.table$AphiaID,]

start.lab<-c("January","February",  "March", 
          "April", "May", "June", "July","August","September", "October", "November", "December", "Summer","Winter", "Spring", "Autumn")

start.no<-c(1:12,6, 12, 3, 9)
end.no<-c(1:12,8,2,5,11)

start<-start.no[match(Reproductive_Timing$ReproductiveTimingStart, start.lab)]
end<-end.no[match(Reproductive_Timing$ReproductiveTimingEnd, start.lab)]

mat<-matrix(NA, ncol=12, nrow=n.sub, dimnames=list(NULL, paste("Reproductive_Timing",1:12, sep="-")))
mat1<-matrix(0, ncol=12, nrow=dim(Reproductive_Timing)[[1]], dimnames=list(NULL, 1:12))

for(i in 1:dim(Reproductive_Timing)[[1]]){
  if(start[i]>end[i]){mat1[i, c(start[i]:12, 1:end[i])]<-1}else{
mat1[i, start[i]:end[i]]<-1}}

mat[match(Reproductive_Timing$AphiaID, master.table$AphiaID),]<-mat1

master.table<-cbind(master.table, mat)
             


#Fecundity

Fecundity<-Fecundity[Fecundity$AphiaID %in% master.table$AphiaID,]

IDs<-unique(Fecundity$AphiaID)
fec.tab<-table(Fecundity$AphiaID, Fecundity$Type)

value<-NULL
type<-NULL

for(i in 1:length(IDs)){
  if(fec.tab[as.character(IDs[i]), "maximum"]!=0){value<-c(value,Fecundity[Fecundity$AphiaID==IDs[i] 
                                                                        & Fecundity$Type=="maximum", "Fecundity"])
                                    type<-c(type, "maximum")}else{
  
  if(fec.tab[as.character(IDs[i]), "unknown"]!=0){value<-c(value,Fecundity[Fecundity$AphiaID==IDs[i]
                                                                        & Fecundity$Type=="unknown", "Fecundity"])
                                    type<-c(type, "unknown")}else{
  
  if(fec.tab[as.character(IDs[i]), "average"]!=0){value<-c(value,Fecundity[Fecundity$AphiaID==IDs[i]
                                                                        & Fecundity$Type=="average", "Fecundity"])
                                    type<-c(type, "average")}else{
                                      
  if(fec.tab[as.character(IDs[i]), "minimum"]!=0){value<-c(value,Fecundity[Fecundity$AphiaID==IDs[i]
                                                                        & Fecundity$Type=="minimum", "Fecundity"])
                                    type<-c(type, "minimum")}}}}}
  
mat<-data.frame(matrix(NA, ncol=2, nrow=n.sub, dimnames=list(NULL, paste("Fecundity",c("value","type"), sep="-"))))  
mat[match(IDs, master.table$AphiaID),]<-cbind(as.numeric(value),type)

master.table<-cbind(master.table, mat)


#Larval_Duration

Larval_Duration<-Larval_Duration[Larval_Duration$AphiaID %in% master.table$AphiaID,]
ld.tab<-table(Larval_Duration$AphiaID, Larval_Duration$Type)

IDs<-unique(Larval_Duration$AphiaID)
value<-NULL
type<-NULL

for(i in 1:length(IDs)){
  if(ld.tab[as.character(IDs[i]), "maximum"]!=0){value<-c(value, max(Larval_Duration[Larval_Duration$AphiaID==IDs[i] 
                                                          & Larval_Duration$Type=="maximum", "LarvalDuration"]))
                                                  type<-c(type, "maximum")}else{
                                                    
  if(ld.tab[as.character(IDs[i]), "unknown"]!=0){value<-c(value, max(Larval_Duration[Larval_Duration$AphiaID==IDs[i]
                                                          & Larval_Duration$Type=="unknown", "LarvalDuration"]))
                                                  type<-c(type, "unknown")}else{
                                                                                                      
  if(ld.tab[as.character(IDs[i]), "average"]!=0){value<-c(value,max(Larval_Duration[Larval_Duration$AphiaID==IDs[i]
                                                          & Larval_Duration$Type=="average", "LarvalDuration"]))
                                                  type<-c(type, "average")}else{
                                                                                                                                                        
  if(ld.tab[as.character(IDs[i]), "minimum"]!=0){value<-c(value,max(Larval_Duration[Larval_Duration$AphiaID==IDs[i]
                                                          & Larval_Duration$Type=="minimum", "LarvalDuration"]))
                                                  type<-c(type, "minimum")}}}}}

        mat<-data.frame(matrix(NA, ncol=2, nrow=n.sub, dimnames=list(NULL, paste("Larval_Duration",c("value","type"), sep="-"))))  
        mat[match(IDs, master.table$AphiaID),]<-cbind(as.numeric(value),type)

master.table<-cbind(master.table, mat)




#Lifespan - Use only BIOTIC as don't trust other measures need looking into
      Lifespan<-Lifespan[Lifespan$AphiaID %in% master.table$AphiaID,]
      Lifespan<-Lifespan[Lifespan$Type=="midpoint of BIOTIC category",]
      
      mat<-data.frame(matrix(NA, ncol=1, nrow=n.sub, dimnames=list(NULL, "Lifespan")))  
      mat[match(Lifespan$AphiaID, master.table$AphiaID),]<-Lifespan$Lifespan
      
      master.table<-cbind(master.table, mat)


#Age_at_Maturity
      Age_at_Maturity<-Age_at_Maturity[Age_at_Maturity$AphiaID %in% master.table$AphiaID,]
  
          
          mat<-data.frame(matrix(NA, ncol=2, nrow=n.sub, 
                                 dimnames=list(NULL, paste("Age_at_Maturity",c("value","type"), sep="-"))))

          mat[match(Age_at_Maturity$AphiaID, master.table$AphiaID),1]<-Age_at_Maturity$AgeAtMaturity
          mat[match(Age_at_Maturity$AphiaID, master.table$AphiaID),2]<-as.character(Age_at_Maturity$Type)
          
          master.table<-cbind(master.table, mat)

#"Size_of_Eggs"
          Size_of_Eggs<-Size_of_Eggs[Size_of_Eggs$AphiaID %in% master.table$AphiaID,]
          Size_of_Eggs<-Size_of_Eggs[-which(Size_of_Eggs$AphiaID==140472 
                                     & Size_of_Eggs$Notes=="width", arr.ind=T),]#Remove duplicate value for 140472. Keep length, discard shorter width
              
              IDs<-unique(Size_of_Eggs$AphiaID)
              egg.tab<-table(Size_of_Eggs$AphiaID, Size_of_Eggs$Type)
              value<-NULL
              type<-NULL
              
              for(i in 1:length(IDs)){
                if(egg.tab[as.character(IDs[i]), "maximum"]!=0){value<-c(value,max(Size_of_Eggs[Size_of_Eggs$AphiaID==IDs[i] & 
                                                                            Size_of_Eggs$Type=="maximum", "EggSize"]))
                                                               type<-c(type, "maximum")}else{
                                                                 
                if(egg.tab[as.character(IDs[i]), "unknown"]!=0){value<-c(value, max(Size_of_Eggs[Size_of_Eggs$AphiaID==IDs[i] & 
                                                                             Size_of_Eggs$Type=="unknown", "EggSize"]))
                                                                type<-c(type, "unknown")}else{
                                                                
                                                               value<-c(value, max(Size_of_Eggs[Size_of_Eggs$AphiaID==IDs[i] & 
                                                                             Size_of_Eggs$Type=="minimum", "EggSize"]))
                                                                type<-c(type, "minimum")}}}
              
              mat<-data.frame(matrix(NA, ncol=2, nrow=n.sub, dimnames=list(NULL, paste("Size_of_Eggs",c("value","type"), sep="-"))))  
              mat[match(IDs, master.table$AphiaID),]<-cbind(as.numeric(value),type)
              
              master.table<-cbind(master.table, mat)
              


#"Body_Size"
#Decision on data priority: BODY SIZE
#Use max from H&R if available
#If max not stated, use 'unknown' from H&R
#If H&R not available, use max from Biotic
#If Biotic not available, use max from any other reference
#If max not available from anywhere, use any other source
#ALL sizes to be entered in mm

#Decision on data priority: CONTINUOUS
#If H&R not available, use max from Biotic

Body_Size<-Body_Size[Body_Size$AphiaID %in% master.table$AphiaID,]
IDs<-unique(Body_Size$AphiaID)
ref.tab<-table(Body_Size$AphiaID, Body_Size$Reference)
no.hr<-Body_Size[Body_Size$AphiaID %in% setdiff(IDs,as.numeric(names(which(ref.tab[,2]!=0 | ref.tab[,3]!=0)))),]
#All non HR have unique BIOTIC entries 


table(Body_Size$AphiaID[Body_Size$Reference==3], Body_Size$Type[Body_Size$Reference==3])




value<-NULL
type<-NULL

for(i in 1:length(IDs)){
  
  
  if(any(Body_Size[Body_Size$AphiaID==IDs[i], "Reference"] %in% c(3,4))){
    sub.bs<-Body_Size[Body_Size$AphiaID==IDs[i], ][Body_Size[Body_Size$AphiaID==IDs[i], "Reference"] %in% c(3,4),]
    
    if("maximum" %in% sub.bs[, "Type"]){
      index<-which(sub.bs["maximum" %in% sub.bs[, "Type"], 
                          "BodySize"]==max(sub.bs["maximum" %in% sub.bs[, "Type"], "BodySize"]))[1]
      value<-c(value,sub.bs[,"BodySize"][index])
      type<-c(type,paste(sub.bs[,"Reference"], sub.bs[,"Type"], sep="_")[index])}else{
        
        
    if("unknown" %in% sub.bs[, "Type"]){
      index<-which(sub.bs["unknown" %in% sub.bs[, "Type"], 
                              "BodySize"]==max(sub.bs["unknown" %in% sub.bs[, "Type"], "BodySize"]))[1]
      value<-c(value,sub.bs[,"BodySize"][index])
      type<-c(type,paste(sub.bs[,"Reference"], sub.bs[,"Type"], sep="_")[index])}}}else{
         
        
        
    if(any(any(Body_Size[Body_Size$AphiaID==IDs[i], "Reference"] %in% c(1,22,29,35, 36)) &
              Body_Size[Body_Size$AphiaID==IDs[i], "Type"] == "maximum")){ 
              
              max.id<-which(Body_Size$BodySize[Body_Size$AphiaID==IDs[i]]==max(Body_Size$BodySize[Body_Size$AphiaID==IDs[i]]
                                  [any(Body_Size[Body_Size$AphiaID==IDs[i], 
                                  "Reference"] %in% c(1,22,29,35, 36)) &
                                  Body_Size[Body_Size$AphiaID==IDs[i], "Type"] == "maximum"]))[1]
              
              value<-c(value, Body_Size$BodySize[Body_Size$AphiaID==IDs[i]][max.id])
              type<-c(type, paste(Body_Size$Reference[Body_Size$AphiaID==IDs[i]][max.id], 
                                  Body_Size$Type[Body_Size$AphiaID==IDs[i]][max.id], sep="_"))}else{
                                    
                                    
    if(any(Body_Size[Body_Size$AphiaID==IDs[i], "Type"] %in% "maximum")){  
              sub.bs<-Body_Size[Body_Size$AphiaID==IDs[i], ][Body_Size[Body_Size$AphiaID==IDs[i], "Type"] %in% "maximum",]
              index<-which(sub.bs["maximum" %in% sub.bs[, "Type"], 
                                   "BodySize"]==max(sub.bs["maximum" %in% sub.bs[, "Type"], "BodySize"]))[1]
              
              value<-c(value,sub.bs[,"BodySize"][index])
              type<-c(type,paste(sub.bs[,"Reference"], sub.bs[,"Type"], sep="_")[index])}else{
                
                                        
    if(any(Body_Size[Body_Size$AphiaID==IDs[i], "Reference"] %in% c(1,22,29,35,36))){  
                                          
              sub.bs<-Body_Size[Body_Size$AphiaID==IDs[i], ][Body_Size[Body_Size$AphiaID==IDs[i], "Reference"] 
                                   %in% c(1,22,29,35, 36),]
                                          
              index<-which(sub.bs[, "BodySize"]==max(sub.bs[, "BodySize"]))[1]
              value<-c(value,sub.bs[,"BodySize"][index])
              type<-c(type,paste(sub.bs[,"Reference"], sub.bs[,"Type"], sep="_")[index])}else{
                                            
          max.id<-which(Body_Size[Body_Size$AphiaID==IDs[i], 
                                  "BodySize"] == max(Body_Size[Body_Size$AphiaID==IDs[i],"BodySize"]))[1]
          
               value<-c(value, Body_Size[Body_Size$AphiaID==IDs[i], "BodySize"][max.id])
               type<-c(type, paste(Body_Size$Reference[Body_Size$AphiaID==IDs[i]][max.id], 
                                   Body_Size$Type[Body_Size$AphiaID==IDs[i]][max.id],sep="_"))}  
                                      }}}}
    
mat<-data.frame(matrix(NA, ncol=2, nrow=n.sub, dimnames=list(NULL, paste("Body_Size",c("value","type"), sep="-"))))  
mat[match(IDs, master.table$AphiaID),]<-cbind(as.numeric(value),type)

master.table<-cbind(master.table, mat)

setwd("/Users/annakrystalli/Documents/WORKFLOW/Benthic analysis/")
save(master.table, file="/Users/annakrystalli/Documents/WORKFLOW/Benthic analysis/master traits table 4.RData")



#Prepare FD table
FD.traits<-setdiff(names(master.table), c("ID", "AphiaID","Species","Phylum",
                                          names(master.table)[grep(".type", names(master.table))]))

#Determine trait availability order
  trait.order<-rep(NA, times=length(var.table[,1]))
  for(i in 1:length(var.table[,1])){
  trait.order[grep(var.table[i,1],FD.traits)]<-i}
  trait.order<-rank(trait.order,ties.method="first")
  FD.traits<-FD.traits[order(trait.order)]
  
  #Order FD.table
  FD.table<-cbind(master.table[, 1:4], master.table[, FD.traits])



trait.ord<-rep(NA, times=length(var.table[,1]))
for(i in 1:length(var.table[,1])){
  trait.ord[grep(var.table[i,1],FD.traits)]<-i}
var.names<-var.table[,1][trait.ord]

#correct 0 in eggsize
FD.table[which(FD.table$Size_of_Eggs.value==0),"Size_of_Eggs.value"]<-NA

#Identify categorical and continuous variables
cat.traits<-names(FD.table)[unique(which(FD.table==0, arr.ind=T)[,2])]
con.traits<-setdiff(names(FD.table[,5:length(FD.table)]), cat.traits)

#Set all categorical traits to binary (remove fuzzy classification)
FD.table[,cat.traits][which(FD.table[,cat.traits]>=1, arr.ind=T)]<-1

#Set continuous traits to numeric
for(i in 1:length(con.traits)){
  FD.table[,con.traits[i]]<-as.numeric(FD.table[,con.traits[i]])
}


#Threw up a duplicate AphiaID entry for Spantagus purpureus (AphiaID 124481 & 124418. 
#124418 Correct. Data inspected, more data under incorrect and only one addition from 124481,Merged & 124481 Removed )
#Also caused confusion in taxo.table so remove unmatching spp from taxo table too
#Also 2 AphiaIDs (141145 & 130335 incorrectly assigned to species creating species name duplicates. Incorrect records REMOVED)

FD.table[FD.table$AphiaID==124418, 5:dim(FD.table)[2]]<-FD.table[FD.table$AphiaID==124481, 5:dim(FD.table)[2]]
FD.table[FD.table$AphiaID==124418,"Feeding_Method-deposit_feeder"]<-1
FD.table<-FD.table[-which(FD.table$AphiaID==124481),]
FD.table<-FD.table[-which(FD.table$AphiaID==141145),]
FD.table<-FD.table[-which(FD.table$AphiaID==130335),]
FD.table[FD.table$AphiaID==125399,"AphiaID"]<-125366

#Match to accepted WoRMs IDs
worms.taxo<-read.csv(file="worms spp match.csv")
worms.taxo<-unique(worms.taxo) 
#Remove any duplicate entries in worms taxonomy table
#Incomplete columns in worms taxo table filled by manual search in BIOTIC

#Remove any species form analysis where Aphia IDs do not match
cor.AphiaID<-intersect(worms.taxo$AphiaID, FD.table$AphiaID)
worms.taxo<-worms.taxo[worms.taxo$AphiaID %in% cor.AphiaID,]
  worms.taxo<-worms.taxo[match(cor.AphiaID, worms.taxo$AphiaID),]

FD.table<-FD.table[FD.table$AphiaID %in% cor.AphiaID,]
  FD.table<-FD.table[match(cor.AphiaID, FD.table$AphiaID),]
  
#Add acc column to FD.table
AphiaID_acc<-as.numeric(as.character(worms.taxo$AphiaID_accepted[match(FD.table$AphiaID, worms.taxo$AphiaID)]))
FD.table<-cbind(AphiaID_acc, FD.table)


#Make taxo.table
spp.abb<-abbreviate(worms.taxo$Species, 4)
taxo.names<-cbind(spp.abb, worms.taxo)
taxo.table<-worms.taxo[, c("Genus","Family","Order","Class","Phylum")]         
  rownames(taxo.table)<-taxo.names$AphiaID_accepted

FD.table.raw<-FD.table
FD.table[,con.traits]<-scale(FD.table[,con.traits])

FULL.RAW.tab<-cbind(taxo.names, FD.table.raw[,FD.traits])
FULL.SC.tab<-cbind(taxo.names, FD.table[,FD.traits])

names(FULL.RAW.tab)[names(FULL.RAW.tab)=="AphiaID_accepted"]<-"AphiaID_acc"   
names(FULL.SC.tab)[names(FULL.SC.tab)=="AphiaID_accepted"]<-"AphiaID_acc" 

  save(FD.table,FD.table.raw, var.names, var.table, file="FD analysis table_all.RData")
  save(taxo.table, taxo.names, file="TD analysis table_all.RData")
  write.csv(FULL.RAW.tab, "FD.table-unscaled-taxonomy combined.csv")
  write.csv(FULL.SC.tab, "FD.table-scaled-taxonomy combined.csv")