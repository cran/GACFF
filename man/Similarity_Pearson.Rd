\name{Similarity_Pearson}
\alias{Similarity_Pearson}
\title{
Similarity between users in Pearson method.
}
\description{
Finding the similarity with Pearson's method.
}
\usage{
Similarity_Pearson(ratings, active_user, near_user)
}
\arguments{
  \item{ratings}{
  A rating matrix whose rows are items and columns are users.
  }
  \item{active_user}{
  The id of an active user as an integer greater than zero (for example active_user<-6).
  }
  \item{near_user}{
  A vector of neighbor users that in the Pearson method are all users.
  }
}
\details{
Obtaining the Pearson similarity through the cor command from state package does not produce the desired response.
}
\value{
  \item{sim_ac}{
  Pearson's similarity between the active user and all users.
  }
}
\references{
Karabadji, N.E.I., Beldjoudi, S., Seridi, H., Aridhi, S. and Dhifli, W. (2018). \emph{Improving memory-based user collaborative filtering with evolutionary multi-objective optimization}. Expert Systems with Applications, vol. 98, pp.153-165.
}
\examples{
ratings <- matrix(c(  2,    5,  NaN,  NaN,  NaN,    4,
                    NaN,  NaN,  NaN,    1,  NaN,    5,
                    NaN,    4,    5,  NaN,    4,  NaN,
                      4,  NaN,  NaN,    5,  NaN,  NaN,
                      5,  NaN,    2,  NaN,  NaN,  NaN,
                    NaN,    1,  NaN,    4,    2,  NaN),nrow=6,byrow=TRUE)	

sim.Pearson  <- Similarity_Pearson (ratings, active_user=6, 
                                   near_user=c(1:dim(ratings)[2]))
}
