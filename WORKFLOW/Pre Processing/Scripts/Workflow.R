#Set up
  rm(list = ls())
  
  #Packages
  require(png)
  require(stringr)

  #Files
  data.index<-read.csv("/Users/annakrystalli/Documents/SATELLITE/data type index.csv")
 #___________________________________________________________________________________________________ 
  
  #SET DIRECTORY
 
            #Set filename directory from which to batch open
              dir<-"/Users/annakrystalli/Documents/SATELLITE/monthly/SeaWiFS_OC5_8day_composites_fronts"
            
            
            #Get filename info to loop processing
              folder.info<-batchOpen(dir)
              dir.create(paste(folder.info$output.file, "/images",sep=""))

            
 
#LOOP THROUGH IMAGES________________________________________________________________________________  
  for (i in 1: length(folder.info$filenames)){
  
    file.name<-folder.info$filenames[i]


    
    imgProcess(file.name, data.type=folder.info$data.type, temp=folder.info$temp)

    
  }
    setwd("/Users/annakrystalli/")
    
    
    
    

  
#_____________________________________________________________________________________________________________________________________
#......Attach temp index to image filenames and save as r matrices in r files folder
  
  #Set input directory
      dir<-"/Users/annakrystalli/Documents/SATELLITE/monthly/SeaWiFS_OC5_8day_composites_mercator"
      
    #Run function on entire variable folder
      batchFolderRename(dir)   
        
   #Reset directory   
    setwd("/Users/annakrystalli/")  
    
    
    
    
    
    
    
    
    
  