\name{Prediction}
\alias{Prediction}
\title{
prediction function
}
\description{
Obtaining the ratings of items that not seen by the active user.
}
\usage{
Prediction (ratings, active_user, near_user, sim_x, KNN)
}
\arguments{
  \item{ratings}{
  A rating matrix whose rows are items and columns are users.
  }
  \item{active_user}{
  The id of an active user as an integer greater than zero (for example active_user<-6).
  }
  \item{near_user}{
  Neighbor users.
  }
  \item{sim_x}{
  Similarity of neighbor users obtained from Similarity function.
  }
  \item{KNN}{
  The number of neighbor users that obtained for the active user from function or manually.
  }
}
\details{
The prediction formula is:
\deqn{(p_x)^i=\bar r_x + ((\sum_(n\in near users)([sim(u_x, u_n).((r_n)^i - (\bar r)_n)]))/(\sum_(n\in near users)(|sim(u_x, u_n)|)))}

where \eqn{(P_x)^i} is the prediction of the user x to an item i. \eqn{(\bar r)_x} is the average ratings of the user x and \eqn{\bar r_n} is the average ratings of neighbors.
}
\value{
  \item{pre_y}{
  A set of predicted ratings for all items of the active user.
  }
}
\references{
Moses, J.S. and Babu, L.D. (2018). \emph{Evaluating Prediction Accuracy, Developmental Challenges, and Issues of Recommender Systems}. International Journal of Web Portals (IJWP), vol. 10, no. 2, pp. 61-79.

Singh, P., Ahuja, S. and Jain, S. (2019). \emph{Latest Trends in Recommender Systems 2017}. In Advances in Data and Information Sciences, pp. 197-210. Springer, Singapore.
}
\examples{
ratings <- matrix(c(  2,    5,  NaN,  NaN,  NaN,    4,
                    NaN,  NaN,  NaN,    1,  NaN,    5,
                    NaN,    4,    5,  NaN,    4,  NaN,
                      4,  NaN,  NaN,    5,  NaN,  NaN,
                      5,  NaN,    2,  NaN,  NaN,  NaN,
                    NaN,    1,  NaN,    4,    2,  NaN),nrow=6,byrow=TRUE)
                    
Pearson.out  <- Pearson (ratings, active_user=6, Threshold_KNN=4)

predict <- Prediction (ratings, active_user=6, 
                       near_user=Pearson.out$near_user_Pearson, 
                       sim_x=Pearson.out$sim_Pearson, 
                       KNN=length(Pearson.out$sim_Pearson))
}



