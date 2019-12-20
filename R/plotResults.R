#==========================================================================
#                       9- plot function
#==========================================================================
plotResults <- 
  function(active_users, obj_Results, xlab = "Iteration", ylab = "MAE",
           main = "MAE (New KNN+GA) in CF Recommender Systems", ...){
    
    #---------------------------------conditions
    #1 active_users
    if (missing(active_users) || !is.numeric(active_users) ||
          !is.vector(active_users)){
      stop("No active_users supplied.")
    }
    #2 obj_Results
    if (!is.object(obj_Results)){
      stop("No obj_Results specified.")
    }
    #3
    if(!is.character(xlab)){
      stop("No xlab supplied.")
    }
    #4
    if(!is.character(ylab)){
      stop("No ylab supplied.")
    }
    #5
    if(!is.character(main)){
      stop("No main supplied.")
    }
    #------------------------------End conditions
    
    color <- c(1:length(active_users))
    
    MaxIteration <- length(obj_Results$MAE_GA[[1]])
    
    plot.new()
    
    for (ac in 1:length(active_users)){
      
      opar<-par(new=TRUE,no.readonly =TRUE)
      on.exit(par(opar))
      plot(1:MaxIteration, obj_Results$MAE_GA[[ac]], 
           xlim=c(0,MaxIteration), ylim=c(0,2.5), xlab = xlab, 
           ylab = ylab, main = main, type = "l", col = color[ac])
      
      lines(MaxIteration, obj_Results$MAE_Pearson[ac] ,type="p",
            pch=17 ,bg=color[ac],col=color[ac])
      
      lines(MaxIteration, obj_Results$MAE_NewKNN[ac], type="p",
            pch=19 ,bg=color[ac],col=color[ac])
    }
    
    legend("topleft",c('Genetic','Pearson','NewKNN'),col=c(1,1,1),
           ncol=3, lty=c(1,NaN,NaN),pch=c(NaN,17,19))
  }