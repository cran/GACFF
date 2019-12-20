#================================================================================
#                          5- NewKNN function
#================================================================================
NewKNN <- 
  function(ratings, active_user, Threshold_KNN, max_scour, min_scour){
    
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
    if (missing(Threshold_KNN) || !is.numeric(Threshold_KNN) || 
          any(!is.finite(Threshold_KNN) || any(Threshold_KNN<=0))){
      stop("No Threshold_KNN supplied.")
    }
    #4,5 max_scour 
    if (     missing(max_scour) || missing(min_scour) || 
               !is.numeric(max_scour) || !is.numeric(min_scour) || 
               any(!is.finite(max_scour)) || any(!is.finite(min_scour)) ||
               any(max_scour<=min_scour)){
      stop("The scour is invalidly specified.")
    }
    #------------------------------End conditions
    
    time_NewKNN_start<-Sys.time()
    #--------------------------------------------------------------Not NaN Items###2
    nnn<-0;
    for (i in 1:dim(ratings)[1])
    {  if(!is.na(ratings[i,active_user])) {nnn<-nnn+1}  }
    
    notNaNItem<-c(rep(NaN,nnn))
    
    nnn<-0;
    for (i in 1:dim(ratings)[1])
    {  if(!is.na(ratings[i,active_user])) { nnn<-nnn+1 ; notNaNItem[nnn]<-i }  }
    
    #------------------------------------------------------------ End Not NaN Items
    
    users_mu        <- colMeans(ratings, na.rm=T)#####
    
    pre_NewKNN <- c(rep(NaN, dim(ratings)[1]))
    
    #------------------------------------------------------------------combination
    
    b<-matrix(c(rep(0,5)),ncol=5)#-------2 users, 2 items, 1 dif
    
    for (i in 1:length(notNaNItem))
    {
      ff<-0
      for (j in 1:dim(ratings)[2])
      {
        if(!is.na(ratings[notNaNItem[i],j]))
        {ff<-ff+1}
      } 
      sp_user<-c(rep(0,ff))
      
      sp_item<-c(rep(0,ff))
      
      ff<-0
      for (j in 1:dim(ratings)[2])
      {
        if(!is.na(ratings[notNaNItem[i],j]))
        {
          ff<-ff+1
          sp_user[ff]<-j
          sp_item[ff]<-ratings[notNaNItem[i],j]
        }
      }
      if(ff>1)
      {
        c_user<- t(combn(sp_user[1:ff],2))
        c_item<- t(combn(sp_item[1:ff],2))
        c_dis <- t(t(abs(c_item[,1]-c_item[,2])))
        abind <- cbind(c_user, c_item, c_dis)   
        b<-rbind(b, abind)
      }
    }
    
    b<-b[-1,]
    
    t2<-0 
    for (i in 1:dim(b)[1])
    {
      if((b[i,1]==active_user)||(b[i,2]==active_user))
      {
        t2<-t2+1
        b[t2,]<-b[i,]
        if(b[t2,1]==active_user) { b[t2,1]<-b[t2,2] ; b[t2,2]<-active_user }
      }
    }
    
    combination <-b[1:t2,]
    #---------------------------------------------------------------End combination
    
    #-------------------------------------------------Sort combination based on dif
    
    sort_combination<-sort(combination[,5],index.return = TRUE)
    
    sort_dif<-combination[sort_combination$ix,]
    
    for (i in 1:(dim(sort_dif)[1]-1))
    {
      s1<-0
      for (j in (i+1):dim(sort_dif)[1])
      {
        if(sort_dif[i,1]==sort_dif[j,1])
        {
          sort_dif[i,5]<-sort_dif[i,5]+sort_dif[j,5]
          sort_dif[j,]<-0
          s1<-s1+1
        }
      }
      
      if(s1!=0) { sort_dif[i,5]<-sort_dif[i,5]/(s1+1) }
    }
    #--------------------------------------------End sort combination based on dif
    
    #----------------------------------------------------------------determine KNN
    KNN<-0
    
    for (i in 1:dim(sort_dif)[1])
    { if(sort_dif[i,1]!=0) { KNN<-KNN+1 ; sort_dif[KNN,]<-sort_dif[i,] }  }
    
    if(KNN<1) {KNN<-1}
    
    if(KNN>Threshold_KNN) { KNN<-Threshold_KNN }
    #------------------------------------------------------------End determine KNN
    
    #-------------------------------------------------------------------Sim NewKNN
    
    diff<-c(rep(0,KNN))
    
    diff[1:KNN]<-sort_dif[1:KNN,5]
    
    sim_NewKNN<-c(rep(NaN,KNN))
    
    sum_ratings<-sum(1:max_scour)
    
    for (i in 1:KNN) { sim_NewKNN[i]<-(max_scour-diff[i])/sum_ratings }
    
    #-------------------------------------------------------------- End sim NewKNN
    
    near_user<-c(rep(0,KNN))
    
    near_user[1:KNN]<-sort_dif[1:KNN,1]
    
    AVE_near<-c(rep(0,KNN))
    
    for (i in 1:KNN) { AVE_near[i]<-mean(ratings[,near_user[i]],na.rm=T) }
    
    pre_NewKNN<-Prediction(ratings, active_user, near_user, sim_NewKNN, KNN)
    
    item_NewKNN<-ItemSelect(ratings, active_user, pre_NewKNN)
    
    time_NewKNN<-Sys.time()-time_NewKNN_start
    
    obj_NewKNN <- list(    call = match.call(),
                           sim_NewKNN = sim_NewKNN,
                           pre_NewKNN = pre_NewKNN,
                           item_NewKNN = item_NewKNN,
                           near_user = near_user,
                           time_NewKNN = time_NewKNN)
    
    class(obj_NewKNN) <- "NewKNN"
    return(obj_NewKNN)
  }