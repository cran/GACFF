\name{Pearson}
\alias{Pearson}
\title{
Pearson method
}
\description{
The Pearson method is the most well-known method for finding users' similarity, so to compare the genetic-based method, the Pearson method has been implemented in this package.
}
\usage{
Pearson(ratings, active_user, Threshold_KNN)
}
\arguments{
  \item{ratings}{
  A rating matrix whose rows are items and columns are users.
  }
  \item{active_user}{
  The id of an active user as an integer greater than zero (for example active_user<-6).
  }
  \item{Threshold_KNN}{
  Maximum number of neighbor users.
  }
}
\details{
Pearson Correlation Coefficient (PCC) is the similarity measure for Collaborative filtering recommender system, to evaluate how much two users are correlated [3]. 
}
\value{
An object of class \code{"Pearson"}, a list with components:
  \item{call}{
  The call used.
  }
  \item{sim_Pearson}{
  The similarity of the Pearson method.
  }
  \item{pre_Pearson}{
  The prediction of the Pearson method.
  }
  \item{item_Pearson}{
  A list of recommended items by the Pearson method.
  }
  \item{near_user_Pearson}{
  Neighbors of active user in the Pearson method orderly.
  }
  \item{time_Pearson}{
  The elapsed time of the Pearson method.
  }
}
\references{  
[1] Bobadilla, J., Ortega, F., Hernando, A. and Alcala, J. (2011). \emph{Improving collaborative filtering recommender system results and performance using genetic algorithms}. Knowledge-based systems, vol. 24, no. 8, pp. 1310-1316.

[2] Lu, J., Wu, D., Mao, M., Wang W. and Zhang, G. (2015). \emph{Recommender system application developments: a survey}. Decision Support Systems, vol. 74, pp. 12-32.

[3] Sheugh, L. and Alizadeh, S.H. (2015). \emph{A note on pearson correlation coefficient as a metric of similarity in recommender system}. In 2015 AI & Robotics (IRANOPEN) (pp. 1-6). IEEE.
}
\examples{
ratings <- matrix(c(  2,    5,  NaN,  NaN,  NaN,    4,
                    NaN,  NaN,  NaN,    1,  NaN,    5,
                    NaN,    4,    5,  NaN,    4,  NaN,
                      4,  NaN,  NaN,    5,  NaN,  NaN,
                      5,  NaN,    2,  NaN,  NaN,  NaN,
                    NaN,    1,  NaN,    4,    2,  NaN),nrow=6,byrow=TRUE)

Pearson.out  <- Pearson (ratings, active_user=6, Threshold_KNN=4)
}

