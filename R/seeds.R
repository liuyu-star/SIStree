#' seeds Data Set
#'
#' Measurements of geometrical properties of kernels belonging to three different varieties of wheat.
#' A soft X-ray technique and GRAINS package were used to construct all seven, real-valued attributes.
#'
#' The variables listed below, from left to right, are:
#' \itemize{
#' \item area A
#' \item perimeter P
#' \item compactness C = 4*pi*A/P^2
#' \item length of kernel
#' \item width of kernel
#' \item asymmetry coefficient
#' \item length of kernel groove
#' \item varieties of wheat (1, 2, 3 for Kama, Rosa and Canadian respectively)
#' }
#'
#' @docType data
#' @keywords datasets internal
#' @format A data frame with 209 rows and 7 covariate variables and 1 response variable.
#' @source \url{https://archive.ics.uci.edu/ml/datasets/seeds}
#' @references M. Charytanowicz, J. Niewczas, P. Kulczycki, P.A. Kowalski, S. Lukasik, S. Zak, 'A Complete Gradient Clustering Algorithm for Features Analysis of X-ray Images', in: Information Technologies in Biomedicine, Ewa Pietka, Jacek Kawa (eds.), Springer-Verlag, Berlin-Heidelberg, 2010, pp. 15-24.
#' @name seeds
#'
#' @seealso  \code{\link{body_fat}} \code{\link{breast_cancer}}
#' @examples
#' data(seeds)
#' sis <- SIStree(as.factor(varieties_of_wheat) ~ .,seeds, criteria='Gini')
#' #show variables ranked by SIStree
#' sis$VarImp$rank.SIStree
NULL
