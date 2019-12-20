#' @export
#================================================================================
#                           7- Results function
#================================================================================
Results <-
  function(ratings, active_users, Threshold_KNN, max_scour, min_scour,
           PopSize=100, MaxIteration=50, CrossPercent=70, MutatPercent=20){
    
    #---------------------------------conditions
    #1 ratings 
    if (missing(ratings) || !is.matrix(ratings)){
      stop("No ratings supplied.")
    }
    #2 active_users
    if (missing(active_users) || !is.numeric(active_users) ||
          !is.vector(active_users)){
      stop("No active_users supplied.")
    }
    #3 Threshold_KNN
    if ( missing(Threshold_KNN) || !is.numeric(Threshold_KNN) || 
           any(!is.finite(Threshold_KNN)) || any(Threshold_KNN<=0)){
      stop("No Threshold_KNN supplied.")
    }
    #4,5 max_scour
    if (missing(max_scour) || missing(min_scour) || 
          !is.numeric(max_scour) || !is.numeric(min_scour) || 
          any(!is.finite(max_scour)) || any(!is.finite(min_scour)) ||
          any(max_scour<=min_scour)){
      stop("The scour is invalidly specified.")
    }
    #6 PopSize
    if (!is.numeric(PopSize) || any(!is.finite(PopSize)) ||
          any(PopSize<=0)){
      stop("No PopSize specified.")
    }
    #7 MaxIteration
    if (!is.numeric(MaxIteration) || any(!is.finite(MaxIteration)) ||
          any(MaxIteration<=0)){
      stop("No MaxIteration specified.")
    }
    #8 CrossPercent
    if (!is.numeric(CrossPercent) || any(CrossPercent<=0) ||
          any(CrossPercent>=100)){
      stop("The CrossPercent is invalidly specified.")
    }
    #9 MutatPercent
    if (!is.numeric(MutatPercent) || any(MutatPercent<=0) ||
          any(MutatPercent>=100)){
      stop("The MutatPercent is invalidly specified.")
    }
    #------------------------------End conditions
    
    MAE_Pearson  <- c(rep(NaN,length(active_users))) 
    MAE_NewKNN   <- c(rep(NaN,length(active_users)))
    MAE_Genetic  <- c(rep(NaN,length(active_users)))
    
    time_Pearson <- c(rep(NaN,length(active_users))) 
    time_NewKNN  <- c(rep(NaN,length(active_users)))
    time_Genetic <- c(rep(NaN,length(active_users)))
    
    MAE_GA <- list(rep(NaN,MaxIteration))
    
    for(ac in 1:length(active_users)){##-------------------------ac loop
      
      active_user <- active_users[ac]
      
      #-------------------------------------------------------------Not NaN Items###4
      nnn<-0;
      for (i in 1:dim(ratings)[1])
      {  if(!is.na(ratings[i,active_user])) {nnn<-nnn+1}  }
      
      notNaNItem<-c(rep(NaN,nnn))
      
      nnn<-0;
      for (i in 1:dim(ratings)[1])
      {  if(!is.na(ratings[i,active_user])) { nnn<-nnn+1 ; notNaNItem[nnn]<-i }  }
      
      #---------------------------------------------------------- End Not NaN Items
      
      object_Pearson <- Pearson(ratings, active_user, Threshold_KNN)
      
      object_NewKNN  <- NewKNN(ratings, active_user, Threshold_KNN, 
                               max_scour, min_scour)
      object_Genetic <- Genetic(ratings, active_user, object_NewKNN$near_user,
                                Threshold_KNN, max_scour, min_scour,
                                PopSize, MaxIteration, CrossPercent, MutatPercent)
      
      time_Pearson[ac]  <- object_Pearson$time_Pearson
      time_NewKNN[ac]   <- object_NewKNN$time_NewKNN
      time_Genetic[ac]  <- object_Genetic$time_Genetic
      
      MAE_Pearson[ac] <-mean(abs(object_Pearson$pre_Pearson[notNaNItem]-
                                   ratings[notNaNItem,active_user])) 
      MAE_NewKNN[ac]  <- mean(abs(object_NewKNN$pre_NewKNN[notNaNItem]-
                                    ratings[notNaNItem,active_user]))
      MAE_Genetic[ac] <- object_Genetic$save_MAE_GA[MaxIteration]
      
      MAE_GA[[ac]] <- object_Genetic$save_MAE_GA
      
      cat("====================== user",active_user, "======================",
          "\n","\n")    
      cat("              MAE_Pearson =", MAE_Pearson[ac],                       "\n")
      cat("               MAE_NewKNN =", MAE_NewKNN[ac],                        "\n")
      cat("                   MAE_GA =", MAE_Genetic[ac],                       "\n")
      cat('----------------------------------------------------',               "\n")
      cat("              sim_Pearson =", object_Pearson$sim_Pearson,            "\n")
      cat("               sim_NewKNN =", object_NewKNN$sim_NewKNN,              "\n")
      cat("                   sim_GA =", object_Genetic$sim_GA,                 "\n")
      cat('----------------------------------------------------',               "\n")
      cat("             real ratings =", ratings[notNaNItem,active_user],       "\n")                               
      cat("Predicted ratings_Pearson =", object_Pearson$pre_Pearson[notNaNItem],"\n")
      cat(" Predicted ratings_NewKNN =", object_NewKNN$pre_NewKNN[notNaNItem],  "\n")
      cat("     Predicted ratings_GA =", object_Genetic$pre_GA[notNaNItem],     "\n")
      cat('----------------------------------------------------',               "\n")
      cat("  predicted items_Pearson =", head(object_Pearson$item_Pearson),     "\n")
      cat("   predicted items_NewKNN =", head(object_NewKNN$item_NewKNN),       "\n") 
      cat("       predicted items_GA =", head(object_Genetic$item_GA),          "\n")
      cat('----------------------------------------------------',               "\n")                      
      cat("        neighbors_Pearson =", object_Pearson$near_user_Pearson,      "\n")
      cat("         neighbors_NewKNN =", object_NewKNN$near_user,               "\n")
      cat("             neighbors_GA =", object_NewKNN$near_user,               "\n")
      cat('----------------------------------------------------',               "\n")
      cat("             Time_Pearson =", time_Pearson[ac],                      "\n")
      cat("              Time_NewKNN =", time_NewKNN[ac],                       "\n")
      cat("             Time_Genetic =", time_Genetic[ac],                 "\n","\n")
      
    }#---------end ac  
    obj_Results <-  list(    call = match.call(),  
                             MAE_Pearson  = MAE_Pearson,
                             MAE_NewKNN   = MAE_NewKNN,
                             MAE_Genetic  = MAE_Genetic,
                             MAE_GA       = MAE_GA,
                             time_Pearson = time_Pearson,
                             time_NewKNN  = time_NewKNN,
                             time_Genetic = time_Genetic )
    
    
    class(obj_Results) <- "Results"
    return(obj_Results)
  }