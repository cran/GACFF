\name{ItemSelect}
\alias{ItemSelect}
\title{
A set of Items id for recommending to an active user.
}
\description{
Selecting the best items to recommend.
}
\usage{
ItemSelect(ratings, active_user, pre_x)
}
\arguments{
  \item{ratings}{
  A rating matrix whose rows are items and columns are users.
  }
  \item{active_user}{
  The id of an active user as an integer greater than zero (for example active_user<-6).
  }
  \item{pre_x}{
  A set of predicted ratings for all items not rated by the active user.
  }
}
\details{
Items selecting and their order depends on the method (Pearson, NewKNN, Genetic).
}
\value{
  \item{item_x}{
  A set of item identifiers recommended to the active user.
  }
}
\references{
Nilashi, M., Ibrahim, O. and Bagherifard, K. (2018). \emph{A recommender system based on collaborative filtering using ontology and dimensionality reduction techniques}. Expert Systems with Applications, vol. 92, pp. 507-520.
}
\examples{
ratings <- matrix(c(  2,    5,  NaN,  NaN,  NaN,    4,
                    NaN,  NaN,  NaN,    1,  NaN,    5,
                    NaN,    4,    5,  NaN,    4,  NaN,
                      4,  NaN,  NaN,    5,  NaN,  NaN,
                      5,  NaN,    2,  NaN,  NaN,  NaN,
                    NaN,    1,  NaN,    4,    2,  NaN),nrow=6,byrow=TRUE)
                    
Pearson.out  <- Pearson (ratings, active_user=6, Threshold_KNN=4)

predict      <-Prediction (ratings, active_user=6, 
                      near_user=Pearson.out$near_user_Pearson, 
                      sim_x=Pearson.out$sim_Pearson, 
                      KNN=length(Pearson.out$sim_Pearson))
                      
ItemSelect (ratings, active_user=6, pre_x=predict)
}


