\name{NewKNN}
\alias{NewKNN}
\title{
Nearest Neighbors.
}
\description{
Determining of nearest neighbors and their id to determine the number of genes in a chromosome.
}
\usage{
NewKNN(ratings, active_user, Threshold_KNN, max_scour, min_scour)
}
\arguments{
  \item{ratings}{
  A rating matrix whose rows are items and columns are users.
  }
  \item{active_user}{
  The id of an active user as an integer greater than zero (for example active_user<-6).
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
}
\details{
The number of neighbors for the active user determines the number of genes in the chromosome of the genetic algorithm. The fitness function is MAE which by being minimized, the similarity of the neighbor users is optimized within the processes of the genetic algorithm. The following equation is used to determine the starting points of the genetic algorithm, which are essentially approximation similarities. Using these starting points, the genetic algorithm converges faster.

\deqn{sim_dif = (max rating - dif)/sum(ratings)}
\deqn{range of dif: [min rating-1,\dots, max rating-1]}

\emph{dif} is the difference in the existing ratings. For example, for a difference of 0.5, the approximate similarity is \eqn{4.5/15} and for a difference of 0, the similarity is \eqn{5/15}. 
In this method, the number of neighbors varies for each active user, so the problem of predetermining it is solved.

The steps of this function are:

1) The rating matrix is assigned to the form of the Item-user matrix (Items in rows and users in one column).

2) The users rating differences of each item are calculated for each pair of related users.

3) For each user, the related pairwise are separated from all rows in one column.

4) If a pairwise is repeated several times, the average values of the differences are calculated. The number of neighbor users is different for each active user.

5) The rating differences are sorted in ascending order.

6) Neighbor users are selected based on lower rating differences. If the threshold for the difference is already specified, the out-of-area relationships are eliminated.
}
\value{
An object of class \code{"NewKNN"}, a list with components:
  \item{call}{
  The call used.
  }
  \item{sim_NewKNN}{
  The similarities between near users and the active user that have obtained from the \code{"NewKNN"} method.
  }
  \item{pre_NewKNN}{
  The predicted ratings for the active user by the \code{NewKNN} method.
  }
  \item{item_NewKNN}{
  A set of recommended items id, obtained from the \code{NewKNN} method.
  }
  \item{near_user}{
  Neighbors of the active user by the \code{NewKNN} method orderly.
  }
  \item{time_NewKNN}{
  The elapsed time in \code{NewKNN} method.
  }
}
\references{
Koohi, H. and Kiani, K. (2017). \emph{A new method to find neighbor users that improves the performance of Collaborative Filtering}. Expert Systems with Applications, vol. 83, pp.30-39.
}
\author{
Farimah Houshmand Nanehkaran

Maintainer: Farimah Houshmand Nanehkaran <hoshmandcomputer@gmail.com>
}
\examples{
ratings <- matrix(c(  2,    5,  NaN,  NaN,  NaN,    4,
                    NaN,  NaN,  NaN,    1,  NaN,    5,
                    NaN,    4,    5,  NaN,    4,  NaN,
                      4,  NaN,  NaN,    5,  NaN,  NaN,
                      5,  NaN,    2,  NaN,  NaN,  NaN,
                    NaN,    1,  NaN,    4,    2,  NaN),nrow=6,byrow=TRUE)
                    
NewKNN.out  <- NewKNN (ratings, active_user=6, Threshold_KNN=4,
                       max_scour=5, min_scour=1)
}


