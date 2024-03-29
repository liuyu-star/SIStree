% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/node_split.R
\name{Split}
\alias{Split}
\title{Choose the optimal splitting variables and splitting values.}
\usage{
Split(
  x,
  y,
  criteria = c("DCor", "Pearson", "CvM", "MI", "HHG", "HSIC", "Gini")[1],
  MinLeaf = 10
)
}
\arguments{
\item{x}{If \code{criterion="Gini"} x is an n by p numeric matrix, otherwise it is a vector of length n.}

\item{y}{A response vector of length n.}

\item{criteria}{The independence measurement criteria used for splitting the nodes. We provided seven criteria, and the default being distance correlation coefficient (criteria='DCor').
The following seven criterions to select the splitting variable first, and then use this function to select the splitting value.\itemize{
\item{'DCor': Distance correlation coefficient.}
\item{'Pearson': Pearson correlation coefficient.}
\item{'CvM': Cramér's V correlation coefficient}
\item{'MI': Mutual information}
\item{'HHG': HHG independence criterion}
\item{'HSIC': Hilbert-Schmidt independence criterion}
\item{"Gini": Use gini impurity index to choose both the splitting variable and the splitting value corresponding to y is categorical responses.}
}}

\item{MinLeaf}{Minimum sample size for leaf nodes (Default 10).}
}
\value{
The splitting variable and the splitting value corresponding. If \code{criterion="Gini"} returns all results, otherwise returns \code{BestCutVal}.
\itemize{
\item{\code{BestCutVar}: Splitting variable.}
\item{\code{BestCutVal}: Splitting value.}
\item{\code{varGini}: Gini impurity index for each variable.}
}
}
\description{
the optimal splitting variable and splitting value are chosen by one of seven independence measurement criteria.
}
\examples{
\dontrun{
#simulation data with continuous response.
X = matrix(rnorm(100*1000), 100, 1000)
y = X[,1] + X[,2]^2 + 2*X[,4]*X[,5] + rnorm(100)

#if criteria is "MI","CvM" or "Pearson".
(varcor= MI(X, y))
(cutvar=which.max(varcor))
(cutval <- Split(X[, cutvar], y,criteria='MI',MinLeaf=10))

#if criteria is 'DCor','HHG' or 'HSIC'.
(varcor=apply(X, 2, DCor, y))
(cutvar=which.max(varcor))
(cutval <- Split(X[, cutvar], y,criteria='DCor',MinLeaf=10))


#real data with categorical response.
#if criteria is 'Gini'.
data(breast_cancer)
gini <- Split(x= as.matrix(breast_cancer[, -c(1,2)]),y=breast_cancer[, 2],
criteria='Gini',MinLeaf=10)
(varcor=gini$varGini)
(cutvar=gini$BestCutVar)
(cutval=gini$BestCutVal)
}
}
\seealso{
\code{\link{SISforest}} \code{\link{SIStree}}
}
\author{
Yu Liu and Zhibo Cai
}
\keyword{split}
