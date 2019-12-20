\name{Results}
\alias{Results}
\title{
Results of all active users. 
}
\description{
comparison of three methods (Genetic, NewKNN, Pearson) about MAE, elapsed time and predicted items.
}
\usage{
Results(ratings, active_users, Threshold_KNN, max_scour, min_scour,
        PopSize=100, MaxIteration=50, CrossPercent=70, MutatPercent=20)
}

\arguments{
  \item{ratings}{
  A rating matrix whose rows are items and columns are users.
  }
  \item{active_users}{
  A vector of all active users id.
  }
  \item{Threshold_KNN}{
  Maximum number of neighbors.
  }
  \item{max_scour}{
  The maximum range of ratings.
  }
  \item{min_scour}{
  The minimum range of ratings.
  }
  \item{PopSize}{
  Population size (Number of chromosomes) in \code{Genetic} algorithm.
  }
  \item{MaxIteration}{
  Number of iterations in \code{Genetic} algorithm.
  }
  \item{CrossPercent}{
  Percentage of the \code{Genetic} algorithm population that participates in the Single-point 
  crossover operator to generate new offspring.
  }
  \item{MutatPercent}{
  Percentage of the \code{Genetic} algorithm population that participates in the mutation.
  }
}
\details{
MAE is the average of the difference between real ratings of the active user and predicted ratings obtained from a method.
}
\value{
An object of class \code{"Results"}, a list with components:
  \item{call}{
  The call used.
  }
  \item{MAE_Pearson}{
  MAE obtained from the \code{"Pearson"} method.
  }
  \item{MAE_NewKNN}{
  MAE obtained from the \code{"NewKNN"} method.
  }
  \item{MAE_Genetic}{
  MAE obtained from the \code{"Genetic"} method.
  }
  \item{MAE_GA}{
  A vector of MAE in every \code{"Genetic"} iteration.
  }
  \item{time_Pearson}{
  Elapsed time of the \code{"Pearson"} method. 
  }
  \item{time_NewKNN}{
  Elapsed time of the \code{"NewKNN"} method.
  }
  \item{time_Genetic}{
  Elapsed time of the \code{"Genetic"} method.
  }
}
\references{
Salehi, M. (2014). \emph{Latent feature based recommender system for learning materials using genetic algorithm}. Information Systems & Telecommunication, vol. 137.
}

\note{
MAE is abbreviate of Mean Absolute Error.
}

\seealso{
\code{\link{Genetic}},
\code{\link{NewKNN}}, 
\code{\link{Pearson}}.
}
\examples{
ratings<-matrix(c(  2,    5,  NaN,  NaN,  NaN,    4,
                  NaN,  NaN,  NaN,    1,  NaN,    5,
                  NaN,    4,    5,  NaN,    4,  NaN,
                    4,  NaN,  NaN,    5,  NaN,  NaN,
                    5,  NaN,    2,  NaN,  NaN,  NaN,
                  NaN,    1,  NaN,    4,    2,  NaN),nrow=6,byrow=TRUE)

active_users <- c(1:dim(ratings)[2])

Results.out <- Results(ratings, active_users, Threshold_KNN=4, max_scour=5, 
                     min_scour=1, PopSize=100, MaxIteration=50, 
                     CrossPercent=70, MutatPercent=20)
}




