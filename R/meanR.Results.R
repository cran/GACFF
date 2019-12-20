#' @export
#==========================================================================
#                       8- meanR.KNNGAsim function
#==========================================================================
meanR.Results <-
  function(obj_Results){
    
    #---------------------------------conditions
    #1
    if (missing(obj_Results) ||!is.object(obj_Results)){
      stop("No obj_Results specified.")
    }
    #------------------------------End conditions
    
    mean_MAE_Pearson    <- mean(obj_Results$MAE_Pearson, na.rm = T)
    mean_MAE_NewKNN     <- mean(obj_Results$MAE_NewKNN,  na.rm = T)
    mean_MAE_Genetic    <- mean(obj_Results$MAE_Genetic, na.rm = T)
    
    diff_MAE_GA_Pearson <- mean_MAE_Pearson - mean_MAE_Genetic
    
    mean_Time_Pearson   <- mean(obj_Results$time_Pearson, na.rm = T)
    mean_Time_NewKNN    <- mean(obj_Results$time_NewKNN,  na.rm = T)
    mean_Time_Genetic   <- mean(obj_Results$time_Genetic, na.rm = T)
    
    cat("====================================================", "\n")
    cat("--------------------------------------------mean MAE", "\n")
    cat("         mean MAE Pearson =", mean_MAE_Pearson,        "\n")
    cat("          mean MAE NewKNN =", mean_MAE_NewKNN,         "\n")
    cat("              mean MAE GA =", mean_MAE_Genetic,        "\n")
    cat("-------------------------------------------------dif", "\n")
    cat("The difference of MAE in GA and Pearson methods =", 
        diff_MAE_GA_Pearson, "\n")
    cat("-------------------------------------------mean Time", "\n")
    cat("        mean Time Pearson =", mean_Time_Pearson,       "\n")
    cat("         mean Time NewKNN =", mean_Time_NewKNN,        "\n")
    cat("             mean Time GA =", mean_Time_Genetic,       "\n")
    
    obj_meanR.Results <-  list(call = match.call(),  
                              mean_MAE_Pearson = mean_MAE_Pearson,
                              mean_MAE_NewKNN = mean_MAE_NewKNN,
                              mean_MAE_Genetic = mean_MAE_Genetic,
                              diff_MAE_GA_Pearson = diff_MAE_GA_Pearson,
                              mean_Time_Pearson = mean_Time_Pearson,
                              mean_Time_NewKNN = mean_Time_NewKNN,
                              mean_Time_GA = mean_Time_Genetic)
    
    class(obj_meanR.Results) <- "meanR.Results"
    return(obj_meanR.Results)
  }