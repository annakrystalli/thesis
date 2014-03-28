range01 <- function(x){(x-min(x))/(max(x)-min(x))}

#......SIGMID

sigmoid<-function(z){g <- 1.0 / (1.0 + exp(-z))}



#.......Sigmoid gradient

sigmoidGradient<-function(z){
  g <- sigmoid(z)*(1-sigmoid(z))}

#....Logistic regression Cost function


nnlrCostGrad<-function(nn_params,
                         input_layer_size,
                         hidden_layer_size,
                         num_labels, X, y, lambda, pred){    
        
                  Theta1 <- matrix(nn_params[1:(hidden_layer_size * (input_layer_size + 1))],
                                   ncol=hidden_layer_size, nrow=(input_layer_size + 1), byrow=TRUE)
                  
                  Theta2 <- matrix(nn_params[(1 + (hidden_layer_size * (input_layer_size + 1))):length(nn_params)], 
                                   ncol=num_labels, nrow=(hidden_layer_size + 1), byrow=TRUE)
                  
                  if(pred!="cat"){av<-read.csv(file="~/Documents/CPR/DATA/RAW DATA/Accepted values.csv", header=TRUE)}
                  if(pred=="t"){Theta3 <- as.vector(av$avt)}
                  if(pred=="e"){Theta3 <- as.vector(av$ave)}
                
                  #THETA3 NOT TO BE UPDATED!!!!!
                  
                  m <- length(X[,1])
                
                  J <- 0
                  Theta1_grad <- matrix(0,nrow=dim(Theta1)[1],ncol=dim(Theta1)[2])
                  Theta2_grad <- matrix(0,nrow=dim(Theta2)[1],ncol=dim(Theta2)[2])
                
                
                  X<-cbind(1,X)
                  if(pred=="cat"){ require(nnet)
                                  y.cat<-class.ind(y)
                                  add<-setdiff(as.character(1:num_labels),dimnames(y.cat)[[2]])
                                  y.cat<-cbind(y.cat,matrix(0, nrow=length(y.cat[,1]), ncol=length(add), dimnames=list(NULL,add)))}
                
                z2<-X %*% Theta1
                a2<-sigmoid(X %*% Theta1)
                a2<-cbind(1,a2);
                
                z3<-a2 %*% Theta2
                a3<-sigmoid(a2 %*% Theta2)
                
                if(pred!="cat"){a4 <- a3 %*% Theta3
                               y.ab<-Theta3[match(y,av$cat)]}
                
                if(pred=="cat"){
                for (i in 1:num_labels){
                Ji<-(1/m)*sum((log(a3[,i]) * -(y.cat[,i]))-(log(matrix(1,m, 1)-a3[,i]) *(matrix(1, m, 1)-(y.cat[,i]))))
                J<-J+Ji}}
                
                if(pred!="cat"){J=(1/(2*m))*sum((a4-y.ab)^2)}
                
                #Cost regularisation
                J=J+(lambda/(2*m))*(sum(Theta1[-1,]^2)+sum(Theta2[-1,]^2))
                
                
                delta_1 <- matrix(0,(dim(Theta1)[1]), dim(Theta1)[2])
                delta_2 <- matrix(0,(dim(Theta2)[1]), dim(Theta2)[2])
                
                  for (i in 1:m){
                    if(pred!="cat"){d4 <- a4[i]-y.ab[i]
                                   d3 <- (Theta3 * d4) *sigmoidGradient(z3[i,])}
                    if(pred=="cat"){d3 <- a3[i,]-y.cat[i,]}
                
                  d2 <- (Theta2[-1,] %*% d3)*sigmoidGradient(z2[i,])
                
                delta_2 <- delta_2+(a2[i,] %*% t(d3))
                delta_1 <- delta_1+(X[i,] %*% t(d2))
                }
                
                
                Theta1_grad <- (1/m)*delta_1+(lambda/m)*rbind(0,Theta1[-1,])
                Theta2_grad <- (1/m)*delta_2+(lambda/m)*rbind(0,Theta2[-1,])
                
                return(list(J=J, Theta1_grad=Theta1_grad, Theta2_grad=Theta2_grad))
                }
      
#.......................................................................................................


        nnlrCost<-function(nn_params,
                               input_layer_size,
                               hidden_layer_size,
                               num_labels, X, y, lambda, pred){    
          
          
                    Theta1 <- matrix(nn_params[1:(hidden_layer_size * (input_layer_size + 1))],
                                     ncol=hidden_layer_size, nrow=(input_layer_size + 1), byrow=TRUE)
                    
                    Theta2 <- matrix(nn_params[(1 + (hidden_layer_size * (input_layer_size + 1))):length(nn_params)], 
                                     ncol=num_labels, nrow=(hidden_layer_size + 1), byrow=TRUE)
                    
                    if(pred!="cat"){av<-read.csv(file="~/Documents/CPR/DATA/RAW DATA/Accepted values.csv", header=TRUE)}
                    if(pred=="t"){Theta3 <- as.vector(av$avt)}
                    if(pred=="e"){Theta3 <- as.vector(av$ave)}
                    
                    #THETA3 NOT TO BE UPDATED!!!!!
                    
                    m <- length(X[,1])
                    
                    J <- 0
          
                    
                    
                    X<-cbind(1,X)
                    if(pred=="cat"){ require(nnet)
                                     y.cat<-class.ind(y)
                                     add<-setdiff(as.character(1:num_labels),dimnames(y.cat)[[2]])
                                     y.cat<-cbind(y.cat,matrix(0, nrow=length(y.cat[,1]), ncol=length(add), dimnames=list(NULL,add)))}
                    
                    z2<-X %*% Theta1
                    a2<-sigmoid(X %*% Theta1)
                    a2<-cbind(1,a2)
                    
                    z3<-a2 %*% Theta2
                    a3<-sigmoid(a2 %*% Theta2)
                    
                    if(pred!="cat"){a4 <- a3 %*% Theta3
                                    y.ab<-Theta3[match(y,av$cat)]}
                    
                    if(pred=="cat"){
                      for (i in 1:num_labels){
                        Ji<-(1/m)*sum((log(a3[,i]) * -(y.cat[,i]))-(log(matrix(1,m, 1)-a3[,i]) *(matrix(1, m, 1)-(y.cat[,i]))))
                        J<-J+Ji}}
                    
                    if(pred!="cat"){J=(1/(2*m))*sum((a4-y.ab)^2)}
                    
                    #Cost regularisation
                    J=J+(lambda/(2*m))*(sum(Theta1[-1,]^2)+sum(Theta2[-1,]^2))
                    
                   
                    return(J)
                  }

#.......................................................................................................


nnlrGrad<-function(nn_params,
                   input_layer_size,
                   hidden_layer_size,
                   num_labels, X, y, lambda, pred){
  
                          Theta1 <- matrix(nn_params[1:(hidden_layer_size * (input_layer_size + 1))],
                                           ncol=hidden_layer_size, nrow=(input_layer_size + 1), byrow=TRUE)
                          
                          Theta2 <- matrix(nn_params[(1 + (hidden_layer_size * (input_layer_size + 1))):length(nn_params)], 
                                           ncol=num_labels, nrow=(hidden_layer_size + 1), byrow=TRUE)
                          
                          if(pred!="cat"){av<-read.csv(file="~/Documents/CPR/DATA/RAW DATA/Accepted values.csv", header=TRUE)}
                          if(pred=="t"){Theta3 <- as.vector(av$avt)}
                          if(pred=="e"){Theta3 <- as.vector(av$ave)}
                          
                          #THETA3 NOT TO BE UPDATED!!!!!
                          
                          m <- length(X[,1])
                          
                        
                          Theta1_grad <- matrix(0,nrow=dim(Theta1)[1],ncol=dim(Theta1)[2])
                          Theta2_grad <- matrix(0,nrow=dim(Theta2)[1],ncol=dim(Theta2)[2])
                          
                          
                          X<-cbind(1,X)
                          if(pred=="cat"){ require(nnet)
                                           y.cat<-class.ind(y)
                                           add<-setdiff(as.character(1:num_labels),dimnames(y.cat)[[2]])
                                           y.cat<-cbind(y.cat,matrix(0, nrow=length(y.cat[,1]), ncol=length(add), dimnames=list(NULL,add)))}
                          
                          z2<-X %*% Theta1
                          a2<-sigmoid(X %*% Theta1)
                          a2<-cbind(1,a2)
                          
                          z3<-a2 %*% Theta2
                          a3<-sigmoid(a2 %*% Theta2)
                          
                          if(pred!="cat"){a4 <- a3 %*% Theta3
                                          y.ab<-Theta3[match(y,av$cat)]}

                          
                          
                          delta_1 <- matrix(0,(dim(Theta1)[1]), dim(Theta1)[2])
                          delta_2 <- matrix(0,(dim(Theta2)[1]), dim(Theta2)[2])
                          
                          for (i in 1:m){
                            if(pred!="cat"){d4 <- a4[i]-y.ab[i]
                                            d3 <- (Theta3 * d4) *sigmoidGradient(z3[i,])}
                            if(pred=="cat"){d3 <- a3[i,]-y.cat[i,]}
                            
                            d2 <- (Theta2[-1,] %*% d3)*sigmoidGradient(z2[i,])
                            
                            delta_2 <- delta_2+(a2[i,] %*% t(d3))
                            delta_1 <- delta_1+(X[i,] %*% t(d2))
                          }
                          
                          
                          Theta1_grad <- (1/m)*delta_1+(lambda/m)*rbind(0,Theta1[-1,])
                          Theta2_grad <- (1/m)*delta_2+(lambda/m)*rbind(0,Theta2[-1,])
                          
                          
                          grad<-c(t(Theta1_grad), t(Theta2_grad))
}


#....................................................................................

nnlrPred<-function(nn_params,
                   input_layer_size,
                   hidden_layer_size,
                   num_labels, X, pred){
 
  
  
  Theta1 <- matrix(nn_params[1:(hidden_layer_size * (input_layer_size + 1))],
                   ncol=hidden_layer_size, nrow=(input_layer_size + 1), byrow=TRUE)
  
  Theta2 <- matrix(nn_params[(1 + (hidden_layer_size * (input_layer_size + 1))):length(nn_params)], 
                   ncol=num_labels, nrow=(hidden_layer_size + 1), byrow=TRUE)
  

  if(pred!="cat"){av<-read.csv(file="~/Documents/CPR/DATA/RAW DATA/Accepted values.csv", header=TRUE)}
  if(pred=="t"){Theta3 <- as.vector(av$avt)}
  if(pred=="e"){Theta3 <- as.vector(av$ave)}
  

  X<-cbind(1,X)
  
  a2<-sigmoid(X %*% Theta1)
  a2<-cbind(1,a2)
  

  a3<-sigmoid(a2 %*% Theta2)
  
  if(pred!="cat"){fitted <- a3 %*% Theta3}else{fitted<-max.col(a3, "first")}
  
  return(fitted)
}



#.....................................................................................

randInitialiseWeights<-function(input_layer_size=21,
                                hidden_layer_size=21,
                                num_labels=12){
  
  epsilon1<-sqrt(6)/sqrt(input_layer_size+hidden_layer_size)
  epsilon2<-sqrt(6)/sqrt(num_labels+hidden_layer_size)
  
  Theta1<-runif((input_layer_size+1)*hidden_layer_size, max=epsilon1, min=-epsilon1)
  Theta2<-runif((hidden_layer_size+1)*num_labels, max=epsilon2, min=-epsilon2)
  
  nn_params<-c(Theta1, Theta2)}

#.....................................................................................


testFits<-function(y, fitted, pred){
  
  m<-length(y)
  if(pred=="cat"){
    table<-table(y, fitted) 
    value<-sum(diag(table))/m
    
    print(table)
    print(value)}
  
  if(pred!="cat"){
    if(pred=="t"){Theta3 <- as.vector(av$avt)
                  y.ab<-Theta3[match(y,av$cat)]}
    if(pred=="e"){Theta3 <- as.vector(av$ave)
                  y.ab<-Theta3[match(y,av$cat)]}
    value<-cor(y.ab, fitted)
    plot(fitted~jitter(y.ab), pch=20, cex=0.3)
    print(value)
  }
  
}



#.FEATURE SECTION......................................................................................................

selectX<-function(X, varnames){X[,which(names(X) %in% varnames)]}

#......linear sum of squares cost of features 
nnlrCostFS<-function(X, X.test, y, y.test, pred){    
  
            require(corpcor)
            
            if(pred!="cat"){av<-read.csv(file="~/Documents/CPR/DATA/RAW DATA/Accepted values.csv", header=TRUE)}
            if(pred=="t"){Theta3 <- as.vector(av$avt)}
            if(pred=="e"){Theta3 <- as.vector(av$ave)}
            
            if(pred!="cat"){
            Theta<-pseudoinverse(X) %*%  Theta3[match(y,av$cat)]
            J=sum(((X.test %*% Theta)-y.test)^2)}
            
          
            if(pred=="cat"){
              require(nnet)
              y.cat<-class.ind(y)
              add<-setdiff(as.character(1:12),dimnames(y.cat)[[2]])
              y.cat<-cbind(y.cat,matrix(0, nrow=length(y.cat[,1]), ncol=length(add), dimnames=list(NULL,add)))
              
              Theta<-pseudoinverse(X) %*%  y.cat
            
                J <- 0
                m <- length(X[,1])
                a2<-X %*% Theta
                
                for (i in 1:12){
                  Ji<-(1/m)*sum((log(a2[,i]) * -(y.cat[,i]))-(log(matrix(1,m, 1)-a2[,i]) *(matrix(1, m, 1)-(y.cat[,i]))))
                  J<-J+Ji}}                                    
                                         
          
                  return(J)}
  
#......linear sum of squares cost of test set using permutations of r features

permCost<-function(n, r, X, X.test, y, y.test, pred){
          com<-combn(n,r)
          res<-vector(mode="integer",length=dim(com)[2])
          
          for(i in 1:dim(com)[2]){
            res[i]<-nnlrCostFS(X[,com[,i]], X.test[,com[,i]], y, y.test, pred=pred)                  
          }
          return(list(res=res,com=com))}

#......Results of feature selection output (supply results from permCost function)

  featureSelect<-function(perm.costs, varnames){
  
              J<-lapply(perm.costs, "[",1)
              var.id<-lapply(perm.costs, "[",2)
              J<-lapply(J, as.vector)
              mods<-length(J)
              
              
              #Create result objects
              J.min.id<-vector("integer", length=mods)
              min.J<-vector("integer", length=mods)
              model.min<-vector("list", mods)
              
              
              for(i in 1:mods){
                J.min.id[i]<- which.min(unlist(J[[i]]))
                min.J[i]<-min(unlist(J[[i]]))
                mod.in<-var.id[[i]][[1]][,J.min.id[i]]
                mod.var<-varnames[mod.in]
                
                
                model.min[[i]]<-mod.var}
              
              return(list(model.min=model.min, min.J=min.J))}
              



lapplyBy(~sample, data=, fun=sd)