#library(dHSIC)
#library(dcov)
#library(HHG)

#' @import dHSIC
#' @import HHG
#' @importFrom stats quantile dist sd cor


#' @keywords internal
#' @noRd
Pearson<-function(X,Y)
{
    pea = as.numeric(cor(X, Y)^2)
    pea[is.na(pea)] <- 0
    return(pea)
}

#' @keywords internal
#' @noRd
HHG <- function(x, y)
{
  Dy = as.matrix(dist(y))
  Dx = as.matrix(dist(x))
  n = length(y)
  hhg = hhg.test(Dy, Dx, nr.perm = 0)$sum.chisq /n/(n-2)/(n-3)
  return(hhg)
}

#' @keywords internal
#' @noRd
CvM<-function(X,Y){
  #INPUT
  #X: an n x p matrix
  #Y: an n x 1 vector
  #OUTPUT
  #CvM correlation
  #mbkr = int {corr(I(X <= x), I(Y <= y))}^2 dF(x)dF(y)

  n = length(Y)
  X = matrix(X, nrow=n)
  Y = matrix(Y, ncol=1)
  n=dim(X)[1]
  p=dim(X)[2]
  mbkr = rep(0,p)

  m = floor(n^0.8)
  Qpoints = (2:(m-1))/(m+1)
  m = length(Qpoints)
  yknots = quantile(Y, Qpoints)

  yknots = Y; m=n
  if (n > 2)
  {
    YY = matrix(Y, n, m) <= matrix(yknots, n, m, byrow=TRUE)
    Ymean = colMeans(YY)
    YY=  YY - matrix(Ymean, n, m, byrow=TRUE)
    YY = YY/matrix(sqrt(colSums(YY^2))+1.0e-10, n, m, byrow=TRUE)
    YY = t(YY)

    for (k in 1:p){
      xknots = quantile(X[,k], Qpoints)
      xknots = X[,k]
      Xk =  matrix(X[,k], n, m) <= matrix(xknots, n, m, byrow=TRUE)
      Xmean = colMeans(Xk)
      Xk = Xk- matrix(Xmean, n, m, byrow=TRUE)
      Xk = Xk/matrix(sqrt(colSums(Xk^2))+1.0e-10, n, m, byrow=TRUE)
      mbkr[k] = mean((YY%*%Xk)^2)
    }
  }
  return(mbkr)
}

#' @keywords internal
#' @noRd
DCor <- function(x, y)
{
  d = fastdcor(x, y)
  d = max(c(d, fastdcor(abs(x-mean(x)), y)))
  return(d)
}


#' @keywords internal
#' @noRd
MI <- function(X, Y)
{
  Y = scale(Y)
  X = as.matrix(scale(X))
  n = length(Y)
  p = ncol(X)

  Y = ((rank(Y)+1)/(n+2))
  h = 2*sd(Y)/n^0.2

  dy = as.matrix(dist(Y/h))^2
  #  fy = exp(-Y^2/2)
  fy = rowMeans(exp(-dy/2))

  mi = rep(0, p)
  for (ip in 1:p)
  {
    xi = X[,ip]

    xi = ((rank(xi)+1)/(n+2))

    dx = as.matrix(dist(xi/h))^2
    #    fx = exp(-xi^2/2)
    fx = rowMeans(exp(-dx/2))

    fxy = rowMeans(exp(-(dx+dy)/2))

    mi[ip] = mean(log(fxy/(fx+1.0e-10)/(fy+1.0e-10)))
  }

  return(mi)
}

#' @keywords internal
#' @noRd
HSIC=function(x, y){
  dhsic(y, x)$dHSIC
}

