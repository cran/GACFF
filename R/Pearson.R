#================================================================================
#                        2- Pearson function
#================================================================================
Pearson <-
  function(ratings, active_user, Threshold_KNN){
    
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
    #3 Threshold_KNN
    if ( missing(Threshold_KNN) || !is.numeric(Threshold_KNN) || 
           any(!is.finite(Threshold_KNN)) || any(Threshold_KNN<=0)){
      stop("No Threshold_KNN supplied.")
    }
    #------------------------------End conditions
    
    time_Pearson_start<-Sys.time()
    #--------------------------------------------------------------Not NaN Items###1
    nnn<-0;
    for (i in 1:dim(ratings)[1])
    {  if(!is.na(ratings[i,active_user])) {nnn<-nnn+1}  }
    
    notNaNItem<-c(rep(NaN,nnn))
    
    nnn<-0;
    for (i in 1:dim(ratings)[1])
    {  if(!is.na(ratings[i,active_user])) { nnn<-nnn+1 ; notNaNItem[nnn]<-i }  }
    
    #------------------------------------------------------------ End Not NaN Items
    
    users_mu        <- colMeans(ratings, na.rm=T)###
    
    sim_Pearson_ac  <- matrix(rep(NaN,dim(ratings)[2]),ncol=1)
    
    pre_Pearson     <- c(rep(NaN, dim(ratings)[1]))
    
    sim_Pearson_ac<-Similarity_Pearson(ratings, active_user, 1:dim(ratings)[2])
    
    #------------------------------------------------Sim Pearson without empty items
    
    sort_sim_Pearson<- sort(sim_Pearson_ac,na.last = TRUE,method = "radix",
                            decreasing = TRUE, index.return = TRUE)
    h<-1;
    while(!is.na(sort_sim_Pearson$x[h])){ h<-h+1 }
    h<-h-1
    
    sim_Pearson<-c(rep(0,h))
    
    near_user_Pearson<-c(rep(0,h))
    
    for(i in 1:h)
    {
      sim_Pearson[i]<-sort_sim_Pearson$x[i]
      
      near_user_Pearson[i]<-sort_sim_Pearson$ix[i]
    }
    #-------------------------------------------End sim Pearson without empty items
    
    if (h<Threshold_KNN) {N_neighbors<-h} else {N_neighbors<-Threshold_KNN}
    
    pre_Pearson<-Prediction(ratings, active_user, near_user_Pearson, 
                            sim_Pearson, N_neighbors)
    
    item_Pearson<-ItemSelect(ratings, active_user, pre_Pearson)
    
    time_Pearson<-Sys.time()-time_Pearson_start
    
    obj_Pearson <- list(       call = match.call(),
                               sim_Pearson = sim_Pearson,
                               pre_Pearson = pre_Pearson,
                               item_Pearson = item_Pearson,
                               near_user_Pearson = near_user_Pearson,
                               time_Pearson = time_Pearson)
    
    class(obj_Pearson) <- "Pearson"
    return(obj_Pearson) 
  }