load(file="FD analysis table_all.RData")

load(file="trait.order.RData")

traits<-as.character(var.table$var[1:13])
            trait.ord<-NULL

            for(i in 1:length(traits)){
              trait.ord<-rbind(trait.ord, 
                               cbind(traits[i], as.numeric(grep(traits[i],
                                                                trait.select))))}
            
            
            traits<-unique(trait.ord[order(as.numeric(trait.ord[,2])),1])




            trait.table<-vector("list", length(traits))

            for(i in 1:length(traits)){
              if(is.null(dim(FD.table[,grep(traits[i],
                                       names(FD.table))]))){trait.table[[i]]<-cbind(
                                         sd.mids=hist(FD.table[,grep(traits[i],
                                                      names(FD.table))])$mids,
                                        counts=hist(FD.table[,grep(traits[i],
                                                            names(FD.table))])$counts)}else{
              trait.table[[i]]<-try(as.matrix(colSums(FD.table[,grep(traits[i],
                                                                     names(FD.table))], na.rm=T)))}}

              names(trait.table)<-traits


print("NOTE: Continuous variables have been centered & scaled. Tables represent histogram bins")
print(trait.table)