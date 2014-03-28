
#source(file="~/....)

#to select wall data
#wall.data<-data[data$y>0,]
#boulder.data<-data[data$y<0,]

#wall.new.dat<-correctCoods(data=wall.dat, barrier.dat=barrier.dat, dat.type="wall")
#boulder.new.dat<-correctCoods(data=boulder.dat, barrier.dat=barrier.dat, dat.type="boulder")

#data[data$y>0,3:4]<-wall.new.dat
#data[data$y<0,3:4]<-boulder.new.dat

#barrier.dat->x,y dataframe of barrier coordinates, must be in x order (NO ID!!)
#data-> x, y data.frame containing x, y data to be corrected (NO ID!!)
#dat.type-> "wall" or "boulder"


#load()

correctCoods<-function(data, barrier.dat=barrier.dat, dat.type){
  
  #This finds which coordinate
      bins<-whichBin(data[,1], breaks=barrier.dat$x, right=T)
      
      y1.dat<-cbind(barrier.dat[bins[,1],2],barrier.dat[bins[,2],2])
      
      if(dat.type=="boulder"){id2<-max.col(y1.dat)
                              id<-c(2,1)[id2]}else{id<-max.col(y1.dat)
                                                   id2<-c(2,1)[id]}
  
  

  theta <- atan2(abs(barrier.dat[diag(bins[,id]),2]-barrier.dat[diag(bins[,id2]),2]),
                  abs(barrier.dat[diag(bins[,id]),1]-barrier.dat[diag(bins[,id2]),1]))
  
  h1<-tan(theta)*abs(data[,1]-barrier.dat[diag(bins[,id]),1])
  
  if(dat.type=="boulder"){
  h<-h1+(barrier.dat[diag(bins[,id]),2]-data[,2])
  h<--h}else{h<-h1+(data[,2]-barrier.dat[diag(bins[,id]),2])}
  
    
  new.dat<-data.frame(x=data$x, new.y=h)
  
  return(new.dat)
    
}
  


whichBin<-function(x,breaks,right=TRUE) {
  lenx<-length(x)
  # any leftovers must be out of range
  wb<-rep(lenx,lenx)
  if(right) wb[x<=breaks[1]]<-0
  else wb[x<breaks[1]]<-0
  for(bin in 1:(length(breaks)-1)) {
    if(right) wb[x>breaks[bin] & x<=breaks[bin+1]]<-bin
    else wb[x>=breaks[bin] & x<breaks[bin+1]]<-bin
  }
  wb<-cbind(wb, wb+1)
  
  return(wb)}

