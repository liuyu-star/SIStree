#' Body Fat Prediction Dataset
#'
#' Lists estimates of the percentage of body fat determined by underwater
#' weighing and various body circumference measurements for 252 men.
#' Accurate measurement of body fat is inconvenient/costly and it is desirable to have easy methods of estimating body fat that are not inconvenient/costly.
#'
#' The variables listed below, from left to right, are:
#' \itemize{
#' \item Density determined from underwater weighing
#' \item Age (years)
#' \item Weight (lbs)
#' \item Height (inches)
#' \item Neck circumference (cm)
#' \item Chest circumference (cm)
#' \item Abdomen 2 circumference (cm)
#' \item Hip circumference (cm)
#' \item Thigh circumference (cm)
#' \item Knee circumference (cm)
#' \item Ankle circumference (cm)
#' \item Biceps (extended) circumference (cm)
#' \item Forearm circumference (cm)
#' \item Wrist circumference (cm)
#' }
#'
#' @docType data
#' @keywords datasets internal
#' @format A data frame with 252 rows and 15 covariate variables and 1 response variable
#' @source \url{https://www.kaggle.com/datasets/fedesoriano/body-fat-prediction-dataset}
#' @references Bailey, Covert (1994). Smart Exercise: Burning Fat, Getting Fit, Houghton-Mifflin Co., Boston, pp. 179-186.
#' @name body_fat
#'
#' @seealso  \code{\link{breast_cancer}} \code{\link{seeds}}
#' @examples
#'
#' data(body_fat)
#' sis <- SIStree(Density ~ .,body_fat, criteria='DCor')
#' #show variables ranked by SIStree
#' sis$VarImp$rank.SIStree
#'
NULL
