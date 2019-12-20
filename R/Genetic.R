#================================================================================
#                         6- Genetic function
#================================================================================

Genetic <- 
  function(ratings, active_user, near_user, Threshold_KNN, max_scour, min_scour,
           PopSize=100, MaxIteration=50, CrossPercent=70, MutatPercent=20){
    
    #---------------------------------conditions
    #1 ratings
    if (missing(ratings) || !is.matrix(ratings)){
      stop("No ratings supplied.")
    }
    #2 active_user
    if (missing(active_user) || !is.numeric(active_user) ||
          any(active_user<=0) || any(!is.finite(active_user))){
      stop("No active_user specified.")
    }
    #3 near_user
    if (missing(near_user) || !is.numeric(near_user) ||
          !is.vector(near_user)){
      stop("No near_user supplied.")
    } 
    #4 Threshold_KNN
    if ( missing(Threshold_KNN) || !is.numeric(Threshold_KNN) || 
           any(!is.finite(Threshold_KNN)) || any(Threshold_KNN<=0)){
      stop("No Threshold_KNN supplied.")
    }
    #5,6 max_scour 
    if (missing(max_scour) || missing(min_scour) || 
          !is.numeric(max_scour) || !is.numeric(min_scour) || 
          any(!is.finite(max_scour)) || any(!is.finite(min_scour)) ||
          any(max_scour<=min_scour)){
      stop("The scour is invalidly specified.")
    }
    #7 PopSize
    if (!is.numeric(PopSize) || any(!is.finite(PopSize)) ||
          any(PopSize<=0)){
      stop("No PopSize specified.")
    }
    #8 MaxIteration
    if (!is.numeric(MaxIteration) || any(!is.finite(MaxIteration)) ||
          any(MaxIteration<=0)){
      stop("No MaxIteration specified.")
    }
    #9 CrossPercent
    if (!is.numeric(CrossPercent) || any(CrossPercent<=0) ||
          any(CrossPercent>=100)){
      stop("The CrossPercent is invalidly specified.")
    }
    #10 MutatPercent
    if (!is.numeric(MutatPercent) || any(MutatPercent<=0) ||
          any(MutatPercent>=100)){
      stop("The MutatPercent is invalidly specified.")
    }
    #---------------------------------conditions
    
    time_GA_start<-Sys.time() 
    
    pre_Genes       <- c(rep(NaN, dim(ratings)[1]))
    pre_GA          <- c(rep(NaN, dim(ratings)[1]))
    MAE_Genes       <- c(rep(  0, PopSize))
    save_MAE_GA     <- c(rep(NaN, MaxIteration))
    ElitPercent     <- PopSize - CrossPercent - MutatPercent# 10
    CrossNum        <- round(CrossPercent/100*PopSize)
    if(CrossNum%%2!=0) { CrossNum <-CrossNum - 1 }
    MutatNum        <- round((MutatPercent/100)*PopSize)
    ElitNum         <- PopSize - CrossNum - MutatNum
    KNN             <- length(near_user)
    #--------------------------------------------------------------Not NaN Items###3
    
    nnn<-0;
    for (i in 1:dim(ratings)[1])
    {  if(!is.na(ratings[i,active_user])) {nnn<-nnn+1}  }
    
    notNaNItem<-c(rep(NaN,nnn))
    
    nnn<-0;
    for (i in 1:dim(ratings)[1])
    {  if(!is.na(ratings[i,active_user])) { nnn<-nnn+1 ; notNaNItem[nnn]<-i }  }
    
    #------------------------------------------------------------- End Not NaN Items
    
    users_mu        <- colMeans(ratings, na.rm=T)
    
    # Initial Population
    
    Pop1<- matrix(runif(KNN*(PopSize-1),-1,1),nrow=PopSize-1)
    
    Approximate<-NewKNN(ratings,active_user, Threshold_KNN, max_scour, min_scour)
    
    sim_NewKNN <- Approximate$sim_NewKNN
    
    Pop<- rbind(sim_NewKNN,Pop1)
    
    for (ii in 1:PopSize)
    {     
      pre_Genes<- Prediction(ratings, active_user, near_user, Pop[ii,], KNN) 
      
      MAE_Genes[ii]<-mean(abs(pre_Genes[notNaNItem]-ratings[notNaNItem,active_user])) 
    }
    
    sort_MAE_Genes<-sort(MAE_Genes,index.return = TRUE)
    
    MAE_Genes_Indx<-sort_MAE_Genes$ix
    
    Pop <- Pop[MAE_Genes_Indx,]
    
    ElitPop<-matrix(rep(NaN,ElitNum*KNN),nrow=ElitNum)
    
    save_pop <- matrix(rep(NaN,(MaxIteration)*KNN),ncol=KNN)
    
    save_pop[1,]<-Pop[1,]
    
    save_MAE_GA[1]<-sort_MAE_Genes$x[1]
    
    for (Iter in  2:MaxIteration)  # ==========================================iter
      # Elitism
    {
      ElitPop <- Pop[1:ElitNum,]
      
      # Cross Over
      CrossPop <-matrix(rep(NaN,KNN),ncol=KNN)
      
      R<- sample(1:PopSize,PopSize)
      
      ParentIndexes <- R[1:CrossNum]
      
      for (ii in 1:(CrossNum/2) )
      {
        Par1Indx <- ParentIndexes[ii*2-1]
        Par2Indx <- ParentIndexes[ii*2]
        
        Par1 <- Pop[Par1Indx,]
        Par2 <- Pop[Par2Indx,]
        
        Beta1 <- runif(1,0,1)
        Beta2 <- runif(1,0,1)
        Off1  <- Beta1*Par1 + (1-Beta1)*Par2
        Off2  <- Beta2*Par1 + (1-Beta2)*Par2
        
        CrossPop <-rbind(CrossPop,Off1 , Off2)
      } 
      CrossPop <-CrossPop[-1,]
      # Mutation
      MutatPop <- matrix(runif(KNN*(MutatNum),-1,1),nrow=MutatNum)
      
      # New Population
      Pop <- rbind(ElitPop , CrossPop , MutatPop)
      
      for (ii in 1:PopSize)
      {
        pre_Genes<- Prediction(ratings, active_user, near_user, Pop[ii,], KNN)
        
        MAE_Genes[ii]<-mean(abs(pre_Genes[notNaNItem]-ratings[notNaNItem,active_user]))
      }
      
      sort_MAE_Genes<-sort(MAE_Genes,index.return = TRUE)
      
      MAE_Genes_Indx<-sort_MAE_Genes$ix
      
      Pop <- Pop[MAE_Genes_Indx,]
      
      save_pop[Iter,]<-Pop[1,]
      
      save_MAE_GA[Iter]<-sort_MAE_Genes$x[1]
      
    }#========================================================================iter
    
    sim_GA <- c(rep(NaN, KNN))
    
    sim_GA<-save_pop[MaxIteration,]
    
    pre_GA<- Prediction(ratings, active_user, near_user, sim_GA, KNN) 
    
    item_GA<-ItemSelect(ratings, active_user, pre_GA)
    
    time_Genetic<-Sys.time()-time_GA_start
    
    obj_Genetic <- list(    call = match.call(),
                            sim_GA = sim_GA,
                            pre_GA = pre_GA,
                            item_GA = item_GA,
                            save_MAE_GA = save_MAE_GA,
                            time_Genetic = time_Genetic)
    
    class(obj_Genetic) <- "Genetic"
    return(obj_Genetic)
  }