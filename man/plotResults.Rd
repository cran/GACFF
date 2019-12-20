\name{plotResults}
\alias{plotResults}
\title{
Methods for Results objects.
}
\description{
Provide standard methods for manipulating Results objects.
}
\usage{
plotResults (active_users, obj_Results, 
                       xlab = "Iteration", ylab = "MAE",
           main = "MAE (New KNN+GA) in CF Recommender Systems", ...)
}  
\arguments{
  \item{active_users}{
  A vector of all active users id.
  }
  \item{obj_Results}{
  An object of class \code{"Results"}.
  }
  \item{
  xlab, ylab, main}{Graphics parameters.
  }
  \item{\dots}{
  Additional arguments passed on to the method.
  }
}
\details{
Methods for standard generic functions when dealing with objects of
  class \code{"Results"}
}
\value{
a plot of the history of the process is produced
  with a \code{NULL} return value.
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

plotResults(active_users, Results.out, xlab = "Iteration", ylab = "MAE",
                main = "MAE (New KNN+GA) in CF Recommender Systems" )
}


