\name{meanR.Results}
\alias{meanR.Results}
\title{
Average of results for all active users.
}
\description{
Average of MAE and elapsed time for all active users.
}
\usage{
meanR.Results(obj_Results)
}

\arguments{
  \item{obj_Results}{An object of class \code{"Results"}.
  }
}
\details{
Due to the difference in the results of each active user, the average of all active users is calculated.
} 
\value{
An object of class \code{"meanR.Results"}, a list with components:
  \item{call}{
  The call used.
  }
  \item{mean_MAE_Pearson}{
  Average of MAE obtained from the \code{"Pearson"} method for all active users.
  }
  \item{mean_MAE_NewKNN}{
  Average of MAE obtained from the \code{"NewKNN"} method for all active users.
  }
  \item{mean_MAE_Genetic}{
  Average of MAE obtained from the \code{"Genetic"} method for all active users.
  }
  \item{diff_MAE_GA_Pearson}{
  The difference of MAE in the \code{"Pearson"} method and \code{"Genetic"} algorithm.
  }
  \item{mean_Time_Pearson}{
  Average of the elapsed time of the \code{"Pearson"} method for all active users.
  }
  \item{mean_Time_NewKNN}{
  Average of the elapsed time of the \code{"NewKNN"} method for all active users.
  }
  \item{mean_Time_GA}{
  Average of the elapsed time of the \code{"Genetic"} method for all active users.
  }
}
\examples{
ratings <- matrix(c(  2,    5,  NaN,  NaN,  NaN,    4,
                    NaN,  NaN,  NaN,    1,  NaN,    5,
                    NaN,    4,    5,  NaN,    4,  NaN,
                      4,  NaN,  NaN,    5,  NaN,  NaN,
                      5,  NaN,    2,  NaN,  NaN,  NaN,
                    NaN,    1,  NaN,    4,    2,  NaN),nrow=6,byrow=TRUE)

active_users <- c(1:dim(ratings)[2])

Results.out <- Results(ratings, active_users, Threshold_KNN=4, max_scour=5, 
                     min_scour=1, PopSize=100, MaxIteration=50, 
                     CrossPercent=70, MutatPercent=20)
                     
meanR.Results.out <- meanR.Results (obj_Results=Results.out)
}

