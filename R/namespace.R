
#' @useDynLib gRbase

## Vanilla R imports (and exports)
## -------------------------------

#' @importFrom stats as.formula cov cov2cor delete.response deviance
#'     formula runif terms xtabs simulate xtabs runif terms addmargins
#'     as.formula cov.wt fitted formula ftable getCall logLik loglin
#'     na.omit pchisq pf pnorm r2dtable terms update update.formula
#' 
#' @importFrom utils combn str
#'
#' @importMethodsFrom stats4 plot
#' @exportMethod plot
#' 
## To make available in vignette 
#' @importFrom magrittr   "%>%"
#' @export "%>%" 

## Miscellaneous
## -------------
#' @importFrom Rcpp evalCpp
#'
#' @import methods
#'
#' @importFrom igraph igraph.to.graphNEL igraph.from.graphNEL
#'     get.adjacency V "V<-" E "E<-" is.directed layout.lgl
#'     layout.graphopt plot.igraph graph.adjacency is.dag


## Bioconductor imports/exports
## ----------------------------

#' @importClassesFrom graph graphNEL
#' @importFrom graph edges nodes

#' @importMethodsFrom Rgraphviz plot

#' @importFrom methods as new setOldClass
#'
#' @importMethodsFrom Matrix t isSymmetric diag
#' @importFrom Matrix Matrix

#' @export solveSPD MAT2ftM_ symMAT2ftM_

.dumfunction_afterimportFrom <- function(){}

