\name{Genetic}
\alias{Genetic}
\title{
The genetic algorithm for finding similarities between users.
}
\description{
Finding users' similarity by continuous genetic algorithm directly.
}
\usage{
Genetic(ratings, active_user, near_user, Threshold_KNN, max_scour, min_scour,
         PopSize=100, MaxIteration=50, CrossPercent=70, MutatPercent=20)
}
\arguments{
  \item{ratings}{
  A rating matrix whose rows are items and columns are users.
  }
  \item{active_user}{
  The id of an active user as an integer greater than zero (for example active_user<-6).
  }
  \item{near_user}{
  The number of neighbor users that obtained from \code{"NewKNN"} for the active user.
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
The fitness function of the genetic algorithm determines the optimality of the neighbor's similarity to the active user. The fitness function is considered the MAE of the RS. The MAE is obtained by comparing the real ratings of users with the predicted ratings that are calculated according to the similarity obtained by the \code{Genetic} algorithm.

The steps of the \code{Genetic} algorithm are:

Selection. Selection is based on elitism. Using this operator, the best member of each population survives and will be present in the next population. In other words, the member with the highest match will automatically be transferred to the new population (elitist \eqn{selection=10\%} of the best individuals from each generation). The application of elitism in the genetic algorithm usually improves its efficiency.

Crossover. Single-point crossover technique is used. The crossover operator is used to produce children. A weight coefficient (crossover probability) of between 0 and 1 (0.8) is considered equal to the length of the parent, and by using follow formulas, two new chromosomes or two children are created.

\deqn{y1=\alpha * x1 + (1-\alpha) * x2}
\deqn{y2=\alpha * x2 + (1-\alpha) * x1}

\eqn{x1} and \eqn{x2} are decimal values that represent the parent chromosome. \eqn{\alpha} is the weighting factor and, \eqn{y1} and \eqn{y2} are the children's chromosomes resulting from the parent compound.

Mutation. Single-point mutation technique is used to introduce diversity. The mutation probability is 0.02. The Gaussian mutation operator is implemented. First, a chromosome is randomly selected from the population, and then one or more of its components is changed according to the Gaussian function using follow formula.

\deqn{y1= x1 + r1 * N(0,1)}
\eqn{x1} is the similarity value that represents the parent chromosome, \eqn{r1} is a random number in the range of 0 and 1 (0.02). N(0,1) is a random number distributed by using the Gaussian distribution.

The genetic algorithm stops when an individual in the population has a fitness value less than a constant value (for example 0.5).
}
\value{
An object of class \code{"Genetic"}, a list with components:
  \item{call}{
  The call used.
  }
  \item{sim_GA}{
  Similarity obtained from the \code{Genetic} algorithm as much as the number of neighbors.
  }
  \item{pre_GA}{
  Predicted active user ratings for all items.
  }
  \item{item_GA}{
  A set of best-predicted items for the active user.
  }
  \item{save_MAE_GA}{
  A set of MAEs obtained from each iteration of the \code{Genetic} algorithm.
  }
  \item{time_Genetic}{
  The elapsed time of \code{Genetic} algorithm.
  }
}
\references{
Bobadilla, J., Ortega, F., Hernando, A. and Alcala, J. (2011). \emph{Improving collaborative filtering recommender system results and performance using genetic algorithms}. Knowledge-based systems, vol. 24, no. 8, pp. 1310-1316.
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
                      
Genetic.out <- Genetic (ratings, active_user=6, 
                        near_user=NewKNN.out$near_user, 
                        Threshold_KNN=4, max_scour=5, min_scour=1,
                        PopSize=100, MaxIteration=50, CrossPercent=70, 
                        MutatPercent=20)
}
\keyword{ Optimize }