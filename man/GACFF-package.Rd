\name{GACFF-package}
\alias{GACFF-package}
\docType{package}
\title{
\packageTitle{GACFF}
}
\description{
\packageDescription{GACFF}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{GACFF}
\packageIndices{GACFF}
Genetic-based recommender systems.

Finding the Nearest Neighbors and Using Them in the Genetic-Based Collaborative Filtering Recommender System.
}
\author{
\packageAuthor{GACFF}

Maintainer: \packageMaintainer{GACFF}
}
\references{
Bobadilla, J., Ortega, F., Hernando, A. and Alcalá, J. (2011). \emph{Improving collaborative filtering recommender system results and performance using genetic algorithms}. Knowledge-based systems, vol. 24, no. 8, pp. 1310-1316.

Ben-Shimon, D., Rokach, L. and Shapira, B. (2016). \emph{An ensemble method for top-N recommendations from the SVD}. Expert Systems with Applications, vol. 64, pp.84-92.

Kang, Z., Peng, C. and Cheng, Q. (2016). \emph{Top-n recommender system via matrix completion}. In Thirtieth AAAI Conference on Artificial Intelligence.

Qian, Y., Zhang, Y., Ma, X., Yu, H. and Peng, L. (2019). \emph{EARS: Emotion-aware recommender system based on hybrid information fusion}. Information Fusion, vol. 46, pp.141-146.

Xia, B., Li, T., Li, Q. and Zhang, H. (2018). \emph{Noise-tolerance matrix completion for location recommendation}. Data Mining and Knowledge Discovery, vol. 32, no. 1, pp.1-24.
}
\keyword{Optimize}
\examples{
ratings <- matrix(c(  2,    5,  NaN,  NaN,  NaN,    4,
                    NaN,  NaN,  NaN,    1,  NaN,    5,
                    NaN,    4,    5,  NaN,    4,  NaN,
                      4,  NaN,  NaN,    5,  NaN,  NaN,
                      5,  NaN,    2,  NaN,  NaN,  NaN,
                    NaN,    1,  NaN,    4,    2,  NaN),nrow=6,byrow=TRUE)

active_users <- c(1:dim(ratings)[2])
##1
sim.Pearson  <- Similarity_Pearson (ratings, active_user=6, 
                                   near_user=c(1:dim(ratings)[2]))
##2
Pearson.out  <- Pearson (ratings, active_user=6, Threshold_KNN=4)
##3
predict      <-Prediction (ratings, active_user=6, 
                      near_user=Pearson.out$near_user_Pearson, 
                      sim_x=Pearson.out$sim_Pearson, 
                      KNN=length(Pearson.out$sim_Pearson))
##4
ItemSelect (ratings, active_user=6, pre_x=predict)
##5
NewKNN.out  <- NewKNN (ratings, active_user=6, Threshold_KNN=4,
                      max_scour=5, min_scour=1)
##6
Genetic.out <- Genetic (ratings, active_user=6, 
                        near_user=NewKNN.out$near_user, 
                        Threshold_KNN=4, max_scour=5, min_scour=1,
                        PopSize=100, MaxIteration=50, CrossPercent=70, 
                        MutatPercent=20)
##7
Results.out <- Results(ratings, active_users, Threshold_KNN=4, max_scour=5, 
                     min_scour=1, PopSize=100, MaxIteration=50, 
                     CrossPercent=70, MutatPercent=20)
##8
meanR.Results.out <- meanR.Results (obj_Results=Results.out)
##9
plotResults(active_users, Results.out, xlab = "Iteration", ylab = "MAE",
                main = "MAE (New KNN+GA) in CF Recommender Systems" )
}