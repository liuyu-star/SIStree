#' Sure Independence Screening for Tree Structures
#'
#' Sure Independence Screening Tree (SIStree) by partitioning the sample into subsets consecutively, leading to a tree structure of sub-samples. While the splitting values are chosen by one of six independence measurement criteria.
#'
#' @param formula Object of class \code{formula} with a response describing the model to fit. If this is a data frame, it is taken as the model frame. (see \code{\link{model.frame}})
#' @param data Training data of class \code{data.frame} containing variables named in the formula. If \code{data} is missing it is obtained from the current environment by \code{formula}.
#' @param X An n by d numeric matrix (preferable) or data frame.
#' @param y A response vector of length n.
#' @param criteria The independence measurement criteria used for splitting the nodes. We provided seven criteria (default criteria='DCor'), for details see \code{\link{Split}}.
#' @param MaxDepth The maximum depth of the tree (default \code{Inf}).
#' @param numNode Number of nodes that can be used by the tree (default \code{Inf}).
#' @param MinLeaf Minimum sample size for leaf nodes (Default 10).
#' @param subset An index vector indicating which rows should be used. (NOTE: If given, this argument must be named.)
#' @param weights Vector of non-negative observational weights; fractional weights are allowed (default NULL).
#' @param na.action A function to specify the action to be taken if NAs are found. (NOTE: If given, this argument must be named.)
#' @param Xscale Predictor standardization methods. " Min-max" (default), "Quantile", "No" denote Min-max transformation, Quantile transformation and No transformation respectively.
#' @param ... Optional parameters to be passed to the low level function.
#'
#' @return An object of class SIStree containing a list of components::
#' \itemize{
#' \item{\code{call}: The original call to SIStree.}
#' \item{\code{terms}: An object of class \code{c("terms", "formula")} (see \code{\link{terms.object}}) summarizing the formula. Used by various methods, but typically not of direct relevance to users.}
#' \item{\code{data}: The list of data related parameters used to build the tree.}
#' \item{\code{tree}: The list of tree related parameters used to build the tree.}
#' \item{\code{structure}: A set of tree structure data records.
#' \itemize{
#' \item{\code{nodeXIndx}: Training samples used for each node.}
#' \item{\code{nodeCutVar}: Splitting variables for each node.}
#' \item{\code{nodeCutValue}: Splitting Values for each node.}
#' \item{\code{childNode}: The number of child nodes after each splitting.}
#' \item{\code{nodeDepth}: The depth of the tree where each node is located.}
#' }}
#' \item{\code{VarImp}: Variable screening results for tree structure.
#' \itemize{
#' \item{\code{rank.SIS}: Variable ranking results for sure independence screening.}
#' \item{\code{rank.SIStree}: Variable ranking results for sure independence screening tree.}
#' \item{\code{rank.SIStree.layer}: Variable ranking results in each layer for sure independence screening tree.}
#' \item{\code{SIStree}: The correlation coefficients of each variable with the response for sure independence screening tree.}
#' \item{\code{SIStree.layer}: The correlation coefficients of each variable with the response in each layer for sure independence screening tree.}
#' \item{\code{splitVar}: The splitting variables for each splitting node.}
#' }}
#' }
#'
#' @seealso \code{\link{SISforest}}
#'
#' @author Yu Liu and Zhibo Cai
#' @keywords tree
#'
#' @examples
#' #simulation data with continuous response.
#' X = matrix(rnorm(100*1000), 100, 1000)
#' y = X[,1] + X[,2]^2 + 2*X[,4]*X[,5] + rnorm(100)
#'
#' sis = SIStree(X, y)
#' # show the first 20 variables ranked by SIS
#' sis$VarImp$rank.SIS[1:20]
#' # show the first 20 variables ranked by SIStree
#' sis$VarImp$rank.SIStree[1:20]
#'
#'
#' #real data with categorical response.
#' data(breast_cancer)
#'
#' sis <- SIStree(as.factor(diagnosis) ~ ., breast_cancer[,-1], criteria='Gini')
#' #show variables ranked by SIStree
#' sis$VarImp$rank.SIStree
#'
#' @import Rcpp
#' @importFrom stats model.frame model.extract model.matrix na.fail median
#' @export
SIStree <- function(X, ...) {
  UseMethod("SIStree")
  # formula X
}

#' @rdname SIStree
#' @method SIStree formula
#' @aliases SIStree.formula
#' @export
SIStree.formula <- function(formula, data = NULL, criteria=c('DCor',"Pearson", "CvM","MI","HHG","HSIC","Gini")[1],
                            MaxDepth = Inf, numNode = Inf, MinLeaf = 10, subset = NULL, weights = NULL, na.action = na.fail,
                            Xscale = "Min-max", ...) {
  Call <- match.call()
  indx <- match(c("formula", "data", "subset", "na.action"), names(Call), nomatch = 0L) # , "weights"
  # formula=X
  if (indx[[1]] == 0) {
    stop("A 'formula' or 'X', 'y' argument is required")
  } else if (indx[[2]] == 0) {
    # stop("a 'data' argument is required")
    # data <- environment(formula)
    X <- eval(formula[[3]]) # ,envir =.BaseNamespaceEnv)
    y <- eval(formula[[2]]) # ,envir =.BaseNamespaceEnv)
    if (sum(match(class(X), c("data.frame", "matrix"), nomatch = 0L)) == 0) {
      stop("argument 'X' can only be the classes 'data.frame' or 'matrix'")
    }
    if (ncol(X) == 1) {
      stop("argument 'X' dimension must exceed 1")
    }

    if (is.null(colnames(X))) {
      colnames(X) <- paste0("X", seq_len(ncol(X)))
    }
    data <- data.frame(y, X)
    # varName <- colnames(X)
    yname <- ls(envir = .GlobalEnv)
    colnames(data) <- c(as.character(formula)[2], colnames(X))
    formula <- as.formula(paste0(as.character(formula)[2], "~."))
    Call$formula <- formula
    Call$data <- quote(data)
  } else {
    if (sum(match(class(data), c("data.frame"), nomatch = 0L)) == 0) {
      stop("argument 'data' can only be the classe 'data.frame'")
    }
    if (ncol(data) == 2) {
      stop("The predictor dimension of argument 'data' must exceed 1.")
    }

    # varName <- setdiff(colnames(data), as.character(formula)[2])
    # X <- data[, varName]
    # y <- data[, as.character(formula)[2]]
    # Call$data <- quote(data)
    yname <- colnames(data)
    data <- model.frame(formula, data, drop.unused.levels = TRUE)
    y <- data[, 1]
    X <- data[, -1]
    Call$data <- quote(data)
  }

  varName <- colnames(X)
  yname <- names(unlist(sapply(yname, function(x) grep(x, as.character(formula)[2]))))
  yname <- yname[which.max(nchar(yname))]
  if (yname != as.character(formula)[2]) {
    varName <- c(yname, varName)
  }
  #if(split=="RotMatRF"&&is.null(paramList$numProj)){paramList$numProj <- ncol(X)}

  SIStree.compute(
    formula, Call, varName, X, y, criteria,MaxDepth, numNode, MinLeaf,
    subset, weights, na.action, Xscale)

}


#' @rdname SIStree
#' @method SIStree default
#' @aliases SIStree.default
#' @export
SIStree.default <- function(X, y, criteria=c('DCor',"Pearson", "CvM","MI","HHG","HSIC","Gini")[1],
                            MaxDepth = Inf, numNode = Inf, MinLeaf = 10, subset = NULL, weights = NULL, na.action = na.fail,
                            Xscale = "Min-max", ...){
  Call <- match.call()
  indx <- match(c("X", "y", "subset", "na.action"), names(Call), nomatch = 0L) # , "weights"
  if (indx[[1]] == 0 || indx[[2]] == 0) {
    stop("A 'formula' or 'X', 'y' argument is required")
  } else {
    if (sum(match(class(X), c("data.frame", "matrix"), nomatch = 0L)) == 0) {
      stop("argument 'X' can only be the classes 'data.frame' or 'matrix'")
    }
    if (ncol(X) == 1) {
      stop("argument 'X' dimension must exceed 1")
    }

    if (is.null(colnames(X))) {
      colnames(X) <- paste0("X", seq_len(ncol(X)))
    }
    data <- data.frame(y = y, X)
    varName <- colnames(X)

    formula <- y~.
    Call$formula <- formula
    Call$data <- quote(data)
    Call$X <- NULL
    Call$y <- NULL
  }
  #if(split=="RotMatRF"&&is.null(paramList$numProj)){paramList$numProj <- ncol(X)}

  SIStree.compute(
    formula, Call, varName, X, y, criteria,MaxDepth, numNode, MinLeaf,
    subset, weights, na.action, Xscale
    )

}

#' @keywords internal
#' @noRd
SIStree.compute <- function(formula, Call, varName, X, y, criteria,MaxDepth, numNode, MinLeaf,
                            subset, weights, na.action, Xscale) {
  if (is.factor(y) && (criteria %in%c("CvM","MI"))) {
    stop(paste0("When ", formula[[2]], " is a factor type, 'criteria' cannot take 'CvM' and 'MI'."))
  }
  if (!is.factor(y) && (criteria =="Gini")) {
    stop(paste0("When criteria = 'Gini' ", formula[[2]], " must be a factor type."))
  }

  MinLeaf <- (MinLeaf == 1) + MinLeaf
  # if (MinLeaf == 5) {
  #  MinLeaf <- ifelse(split == "mse", 10, 5)
  # }

  n <- length(y)
  p <- ncol(X)
  yname <- NULL
  if (length(varName) > p) {
    yname <- varName[1]
    varName <- varName[-1]
  }

  if (is.factor(y)) {
    Levels <- levels(y)
    y <- as.integer(y)

    numLabels <- length(Levels)
    if (numLabels == 1) {
      stop("the number of factor levels of categorical response must be greater than one")
    }
  } else {
    y <- c(y)
    numLabels <- 0
  }

  if (!is.numeric(X)){
    X=apply(X, 2, as.numeric)
  }
  X <- as.matrix(X)
  colnames(X) <- varName


  # address na values.
  data <- data.frame(y, X)
  if (any(is.na(as.list(data)))) {
    warning("NA values exist in data frame")
  }

  Call0 <- Call
  colnames(data) <- c(as.character(formula)[2], varName)
  if (!is.null(yname)) {
    colnames(data)[1] <- yname
    temp <- model.frame(formula, data, drop.unused.levels = TRUE)
    Terms <- attr(temp, "terms")

    colnames(data)[1] <- "y" # as.character(formula)[2]
    Call0$formula[[2]] <- quote(y)
  }

  indx <- match(c("formula", "data", "subset", "na.action"), names(Call0), nomatch = 0L)
  temp <- Call0[c(1L, indx)]
  temp[[1L]] <- quote(stats::model.frame)
  temp$drop.unused.levels <- TRUE
  temp <- eval(temp) # , parent.frame())
  Terms0 <- attr(temp, "terms")
  if (is.null(yname)) {
    Terms <- Terms0
    Call <- Call0
  }

  # data=model.frame(formula, data, drop.unused.levels = TRUE)
  # y <- data[,1]
  # X <- data[,-1]
  y <- c(model.extract(temp, "response"))
  X <- model.matrix(Terms0, temp)
  int <- match("(Intercept)", dimnames(X)[[2]], nomatch = 0)
  if (int > 0) {
    X <- X[, -int, drop = FALSE]
  }
  n <- length(y)
  p <- ncol(X)

  rm(data)


  # weights=c(weights,paramList$weights)
  if (!is.null(subset)) {
    weights <- weights[subset]
  }
  if (!is.null(weights)) {
    X <- X * matrix(weights, n, p)
  }

  # Variable scaling.
  minCol <- NULL
  maxminCol <- NULL
  if (Xscale != "No") {
    if (Xscale == "Min-max") {
      minCol <- apply(X, 2, min)
      maxminCol <- apply(X, 2, function(x) {
        max(x) - min(x)
      })
    }
    if (Xscale == "Quantile") {
      minCol <- apply(X, 2, quantile, 0.05)
      maxminCol <- apply(X, 2, function(x) {
        quantile(x, 0.95) - quantile(x, 0.05)
      })
    }
    maxminCol<-maxminCol+1e-06
    X <- (X - matrix(minCol, n, p, byrow = T)) / matrix(maxminCol, n, p, byrow = T)
  }


  #if (is.infinite(MaxDepth)) {
    numNode <- min(numNode, sum(2^(0:ceiling(log2(n / MinLeaf)))))
  #} else {
    #MaxDepth <- min(MaxDepth, ceiling(log2(n / MinLeaf)), n / MinLeaf - 1)
  #  numNode <- min(numNode, sum(2^(0:MaxDepth)))
  #}

  max_depth <- ceiling(max(log2(n / MinLeaf), n / MinLeaf - 1))

  nodeXIndx <- vector("list", numNode + 1)
  nodeXIndx[[1]] <- 1:n
  nodeDepth <- rep(1, numNode)
  nodeCutValue <- rep(0, numNode)
  nodeCutVar <- nodeCutValue
  childNode <- nodeCutValue
  #nodeSize <- nodeCutValue
  #nodeVarCor <- matrix(0, numNode, p)
  cutVarCor=matrix(c(1,0.0), numNode, 2,byrow = TRUE)
  layerVarCor=matrix(0, max_depth, p)

  if(criteria != "Gini")
    criteriaFun <- match.fun(criteria, descend = TRUE)


  # start create SIS-tree.
  ##################################Start#######################################
  currentNode <- 1
  freeNode <- 2
  while (!is.null(nodeXIndx[[currentNode]])) {
    Indx=nodeXIndx[[currentNode]]

    #varcor=rep(0,p)
    #sp=sample.int(p,mtry)
    if(criteria == "Gini"){
      gini=gini_split(X[Indx, ,drop=F], y[Indx], MinLeaf, numLabels)
      varcor=gini$varGini
      cutvar=gini$BestCutVar
      cutval=gini$BestCutVal
    }else{
      if(criteria%in%c("MI","CvM","Pearson")){
        #varcor[sp]= splitFun(X[Indx,sp,drop=F], y[Indx])
        varcor= criteriaFun(X[Indx,,drop=F], y[Indx])
      }else{
        varcor=apply(X[Indx,,drop=F], 2, criteriaFun, y[Indx])
        #varcor=fastdcor3d(X[nodeXIndx[[currentNode]],],y[nodeXIndx[[currentNode]]],Xabs = TRUE)# dcor2d
      }
      cutvar=which.max(varcor)
    }


    # using the maximum value for the splitting variable
    varcor[cutVarCor[currentNode,1]]=max(varcor[cutVarCor[currentNode,1]],cutVarCor[currentNode,2])
    nodesize=length(Indx)
    #nodeVarCor[currentNode,]=varcor
    #nodeSize[currentNode] <- nodesize
    depth=nodeDepth[currentNode]

    layerVarCor[depth,]=layerVarCor[depth,]+varcor*nodesize/n
    #(length(unique(y[Indx])) == 1) ||
    if ((nodesize <= (3 * MinLeaf)) ||
        (depth >= MaxDepth) ||
        (freeNode >= numNode)||
        (length(unique(varcor))==1)) {#
      #nodeXIndx[currentNode] <- NA
      layerVarCor[-seq(depth),]=layerVarCor[-seq(depth),]+matrix(varcor*nodesize/n,max_depth-depth,p,byrow = TRUE)
      currentNode <- currentNode + 1
      next
    }


    if (criteria != "Gini") {
      cutval <- Split(X[Indx, cutvar], y[Indx],criteria,MinLeaf)
    }
    Lindex <- which(X[Indx, cutvar] <= cutval)
    Rindex=setdiff(seq(nodesize), Lindex)
    #if (min(Lindex,Rindex) <= MinLeaf){
    #  layerVarCor[-seq(depth),]=layerVarCor[-seq(depth),]+matrix(varcor*nodesize/n,max_depth-depth,p,byrow = TRUE)
    #  currentNode <- currentNode + 1
     # next
    #}

    #TF <- min(length(Lindex), length(Indx) - length(Lindex)) <= MinLeaf
    nodeCutVar[currentNode]=cutvar
    nodeCutValue[currentNode] <- cutval
    childNode[currentNode] <- freeNode

    Lindex <- which(X[Indx, cutvar] <= cutval)
    Rindex=setdiff(seq(nodesize), Lindex)
    nodeXIndx[[freeNode]] <- Indx[Lindex]
    nodeXIndx[[freeNode + 1]] <- Indx[Rindex]
    nodeDepth[freeNode + c(0, 1)] <- depth + 1
    cutVarCor[freeNode + c(0, 1),]<-c(cutvar,cutvar,varcor[cutvar],varcor[cutvar])

    #nodeXIndx[currentNode] <- NA
    #varcor0=varcor
    freeNode <- freeNode + 2
    currentNode <- currentNode + 1
  }
  ###############################End############################################

  nodeDepth <- nodeDepth[1:(currentNode - 1)]
  Layer=paste0("Layer.",nodeDepth)
  #nodeSize <- nodeSize[1:(currentNode - 1)]
  nodeCutVar <- nodeCutVar[1:(currentNode - 1)]
  nodeCutValue <- nodeCutValue[1:(currentNode - 1)]
  nodeXIndx=nodeXIndx[1:(currentNode - 1)]
  #nodeVarCor=nodeVarCor[1:(currentNode - 1),,drop=F]
  #names(nodeSize)=Layer
  names(nodeCutVar)=Layer
  names(nodeCutValue)=Layer

  rowname=c()
  numDepth=table(nodeDepth)
  for (d in seq_along(numDepth)) {
    rowname= c(rowname,paste(d,seq(numDepth[d]),sep = "."))
  }
  names(nodeXIndx)=rowname
  #rownames(nodeVarCor)=rowname
  #colnames(nodeVarCor)=varName

  #nodesize=vapply(nodeXIndx,length,n)
  #nodesize[nodeCutVar!=0]=0
  #treeVarCor=c(t(nodesize/n)%*%nodeVarCor)
  #names(treeVarCor)=varName
  #order.treeVar=order(treeVarCor,decreasing = TRUE)

  #NodeSize=vapply(nodeXIndx,length,n)
  #layerVarCor0<-aggregate(nodeVarCor*matrix(nodeSize/n,currentNode - 1,p), by = list(nodeDepth), sum)[, -1]
  layerVarCor=layerVarCor[seq(max(nodeDepth)),,drop=F]
  layerVarCor0=layerVarCor
  layerVarCor<-sapply(seq_along(numDepth), function(d) colMeans(layerVarCor[seq(d),,drop=F]))

  order.layerVar= t(apply(layerVarCor,2,order,decreasing = TRUE))
  rownames(order.layerVar)=paste0("Layer.",seq_along(numDepth))
  ## averaged dcor
  treeVarCor.ave=rowMeans(layerVarCor)
  names(treeVarCor.ave)=varName
  order.treeVar.ave=order(treeVarCor.ave,decreasing = TRUE)


  Tree <- list(call = Call, terms = Terms)
  Tree$data <- list(
    subset = subset, weights = weights, na.action = na.action, n = n, p = p, varName = varName,
    Xscale = Xscale, minCol = minCol, maxminCol = maxminCol
  )
  Tree$tree <- list(criteria = criteria, MaxDepth = MaxDepth, MinLeaf = MinLeaf, numNode = numNode)
  Tree$structure <- list(
    #nodeVarCor=nodeVarCor,
    nodeXIndx=nodeXIndx,
    #nodeSize=nodeSize,
    nodeCutVar=nodeCutVar,
    nodeCutValue = nodeCutValue,
    nodeDepth = nodeDepth,
    childNode = childNode[1:(currentNode - 1)]
    )
  Tree$VarImp=list(rank.SIS =order.layerVar[1,],
                     #rank.SIStree=order.treeVar,
                     rank.SIStree=order.treeVar.ave,
                     rank.SIStree.layer=order.layerVar,
                     #SIStree=treeVarCor,
                     SIStree=treeVarCor.ave,
                     SIStree.layer=layerVarCor0,
                     splitVar=nodeCutVar[nodeCutVar!=0]
                   )

  class(Tree) <- append(class(Tree), "SIStree")
  return(Tree)
}

