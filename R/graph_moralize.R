#######################################################################
#' @title Moralize a directed acyclic graph
#' @description Moralize a directed acyclic graph which means marrying
#'     parents and dropping directions.
#' @name graph-moralize
#######################################################################
#' @aliases moralize moralize.default moralizeMAT
#' @param object A directed acyclic graph represented either as a
#'     \code{graphNEL} object, an \code{igraph}, a (dense)
#'     \code{matrix}, a (sparse) \code{dgCMatrix}.
#' @param result The representation of the moralized graph.  When NULL
#'     the representation will be the same as the input object.
#' @param \dots Additional arguments, currently not used
#' @return A moralized graph represented either as a \code{graphNEL}, a
#'     dense \code{matrix} or a sparse \code{dgCMatrix}.
#' @note The workhorse is the \code{moralizeMAT} function.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{mcs}}, \code{\link{junction_tree}}, \code{\link{rip}},
#'     \code{\link{ug}}, \code{\link{dag}}
#' @keywords utilities
#' @examples
#' 
#' daG <- dag(~me+ve,~me+al,~ve+al,~al+an,~al+st,~an+st)
#' moralize(daG)
#' 
#' daG <- dag(~me+ve,~me+al,~ve+al,~al+an,~al+st,~an+st, result="matrix")
#' moralizeMAT(daG)
#' 
#' if (require(igraph)){
#' M <- matrix(c(1,2,3,3), nrow=2)
#' G <- graph.edgelist(M)
#' G
#' V(G)$name
#' moralize(G)
#' }
#' 
#' @export moralize
moralize <- function(object,...){
  UseMethod("moralize")
}

## FIXME moralize Is it checked that the object is dag? I doubt.

#' @export
#' @rdname graph-moralize
moralize.default <- function(object, result=NULL, ...) {
    
    graph_class <- c("graphNEL", "igraph", "matrix", "dgCMatrix")
    chk <- inherits(object, graph_class, which=TRUE)
    if (!any(chk)) stop("Invalid class of 'object'\n")
    cls <- graph_class[which(chk > 0)]
    
    if (is.null(result))
        result <- cls
    
    switch(cls,
           ## "graphNEL" ={
           ##     m <- as(object, "matrix") ## FIXME check this graphNEL2M( object )
           ##     if (!is_dagMAT(m))
           ##         stop("Graph must be directed")
           ##     m <- moralizeMAT(m)
           ## },
           "dgCMatrix"=,
           "matrix"   ={
               if (!is_dagMAT(object))
                   stop("Graph must be directed")
               m <- moralizeMAT(object)
           },
           "igraph"   ={
               if (!igraph::is.directed( object ))
                   stop("Graph must be directed")
               if (is.null(igraph::V(object)$name))
                   igraph::V(object)$name <- igraph::V(object)
               m <- moralizeMAT(igraph::get.adjacency(object))
           })
    as(m, result)
}

#' @export
moralizeMAT <- moralizeMAT


