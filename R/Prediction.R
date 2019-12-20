#================================================================================
#                          3- Prediction function
#================================================================================

Prediction <-
  function(ratings, active_user, near_user, sim_x, KNN){
    
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
    #4 sim_x
    if (missing(sim_x) || !is.vector(sim_x)){
      stop("No sim_x supplied.")
    }
    #5 KNN
    if (missing(KNN) || !is.numeric(KNN) || any(!is.finite(KNN)) || any(KNN<=0)){
      stop("No KNN supplied.")
    }
    #------------------------------End conditions
    
    pre_y<-c(rep(NaN,KNN))
    
    AVE_activeUser <- mean(ratings[,active_user],na.rm=T)
    
    users_mu <- colMeans(ratings, na.rm = T)
    
    AVE_near <- users_mu[near_user]
    
    for (i in 1:dim(ratings)[1])
    {
      a<-0; d<-0
      
      for (j in 1:KNN)
      {
        if(!is.na(ratings[i,near_user[j]])&& !is.na(ratings[i,active_user]))
          
        { a<-a+(sim_x[j]*(ratings[i,near_user[j]]-AVE_near[j])) }
        
        d<-d+abs(sim_x[j])
      }
      
      c<-a/d
      
      if(is.na(c)) { c<-0 }
      
      pre_y[i]<-AVE_activeUser+c
    }
    return(pre_y)
  }