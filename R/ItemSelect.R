#================================================================================
#                        4- ItemSelect function
#================================================================================

ItemSelect <-
  function(ratings, active_user, pre_x){
    
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
    #3 pre_x
    if (missing(pre_x) || !is.vector(pre_x)){
      stop("No pre_x supplied.")
    }
    #------------------------------End conditions
    
    #--------------------------------------------------------------Not NaN Items###
    nnn<-0;
    for (i in 1:dim(ratings)[1])
    {  if(!is.na(ratings[i,active_user])) {nnn<-nnn+1}  }
    
    notNaNItem<-c(rep(NaN,nnn))
    
    nnn<-0;
    for (i in 1:dim(ratings)[1])
    {  if(!is.na(ratings[i,active_user])) { nnn<-nnn+1 ; notNaNItem[nnn]<-i }  }
    
    #------------------------------------------------------------ End Not NaN Items
    
    pre_x[notNaNItem]<-NaN
    
    sort_pre_x<-sort(pre_x,na.last=TRUE,decreasing=TRUE,index.return = TRUE)
    
    i<-1
    while(!is.na(sort_pre_x$x[i])){i<-i+1}
    
    i<-i-1
    
    item_x<-c(rep(0,i))    
    
    if(i!=0)
    {
      for(j in 1:i)
      {item_x[j]<-sort_pre_x$ix[j]}
      
    }else{
      
      item_x<-NaN
      cat("Is not any Item to recommend for this user in method","\n")
    }
    return(item_x)
  }