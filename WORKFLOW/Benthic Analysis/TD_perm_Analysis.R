rm(list=ls())

source("analyseTrees.R")
load(file="FD analysis table_all.RData")
load(file="TD analysis table_all.RData")

#The following function permutes majority of possible trait sequences to maximise 
#the total number of species trait records in the analysis as these vary according to the sequence 
#in which traits are ordered. No need to re-run as I have saved the order vector, just load object below:
    #trait.select<-maxCompl(FD.table, var.table)
    #save(trait.select, file="trait.order.RData")
    load(file="trait.order.RData")

#Order columns in FD.table
  FD.tab<-cbind(FD.table[,1:5], FD.table[,trait.select])

#Make a vector of the number of complete cases for each modality (each trait can have a number of modalities)
compl<-NULL
for(i in 1:(length(FD.tab)-5)){
  compl<-c(compl,sum(complete.cases(FD.tab[6:(length(FD.tab)-i+1)])))}
compl<-rev(compl)


#Analyse by varying both spp & traits
an.ID<-"both"

      min.spp<-15 #min number of spp to be used in analysis

      #int is vector indicating the number of spp to be used in each iteration of analysis
      int<-rev(unique(compl[compl>=min.spp]))
      #Create vector indicating SPP for which s/t combo results will be plotted
      plot.int<-c(15, 46, 79,  164, 179,  198)
      
      res.both<-analyseTrees(FD.tab, int, plot.int=plot.int, an.ID=an.ID, metric="gower", type="asymm", select.spp=NULL, 
                             vars=NULL, var.names=var.names, xlim=c(0,1), ylim=c(0,100), varstep= F)



#Analyse by varying traits only
an.ID<-"var"

      min.spp<-65 #min number of spp to be used in analysis

      #int is vector indicating the number of spp for which data is available
      #for each trait to be used in each iteration of analysis
      int<-rev(unique(compl[compl>=min.spp]))  
      plot.int<-c(65, 91, 101, 120, 164, 179, 198)

      res.var<-analyseTrees(FD.tab, int, plot.int=plot.int, an.ID=an.ID, metric="gower", type="asymm", select.spp=65, 
                            vars=NULL, var.names=var.names, xlim=c(0,1), ylim=c(0,100), varstep= F)
      

           
#Analyse by varying spp only

an.ID<-"spp"
    #int sets the traits to be used to those for which the repeated number of species (164) have data
    #the argument select.spp then defines the species across which analysis is iterated
    int<-rep(164,10)
    res.spp<-analyseTrees(FD.tab, int, plot.int=c(91  ,79  ,65 , 47  ,  17 , 15), an.ID=an.ID,
                          metric="gower", type="asymm", 
                          select.spp=rev(c(164, 120, 101, 91 , 79,  65 , 47 , 46 , 17 , 15)), 
                          vars=NULL, var.names=var.names, xlim=c(0,1), ylim=c(0,100), varstep= F)

  
#Save results of all three analyses
    save(res.both, res.spp, res.var, file="RESULTS.RData")                

#_________________________________________________________________________________________________


#Calculate overall metrics for s/t combo iterations under each treatment

  load(file="RESULTS.RData") 

analyses<-c("both", "var", "spp")
for(i in 1:3){
  
  an.ID<-analyses[i]
  res<-get(paste("res.",an.ID, sep=""))

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


save(SD, dsd, dplus, sd, th, m, file=paste("SD RESULTS ", an.ID, ".RData", sep=""))  }             




 
