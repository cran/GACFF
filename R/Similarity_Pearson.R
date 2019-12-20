
#================================================================================
#                        1- Similarity_Pearson function
#================================================================================

Similarity_Pearson <-
  function(ratings, active_user, near_user){
    
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
    #---------------------------------End conditions
    
    sim_ac         <- matrix(rep(NaN,dim(ratings)[2]),ncol=1)
    
    AVE_activeUser <- mean(ratings[,active_user],na.rm=T)
    
    users_mu <- colMeans(ratings, na.rm = T)
    
    AVE_near       <- users_mu[near_user]
    
    for (i in 1:dim(ratings)[2])
    {
      a<-0; b<-0; c<-0; flag<-0
      
      for (j in 1:dim(ratings)[1])
      {
        if (!is.na(ratings[j,active_user])&& !is.na(ratings[j,i]))
        {
          flag<-1
          a<-a+(ratings[j,active_user]-AVE_activeUser)*(ratings[j,i]-AVE_near[i])
          b<-b+(ratings[j,active_user]-AVE_activeUser)^2
          c<-c+(ratings[j,i]-AVE_near[i])^2
        }
      }
      if((c==0 || b==0)&& flag==1){ sim_ac[i,1]<-0 }else{ sim_ac[i,1]<-a/sqrt(b*c) }
    }
    
    sim_ac[active_user,1]<- NaN 
    
    return(sim_ac)
  }