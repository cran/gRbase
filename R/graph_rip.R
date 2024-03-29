#############################################################################
#' @title Create RIP ordering of the cliques of  an undirected graph;
#'     create junction tree.
#' 
#' @description A RIP (running intersection property) ordering of the
#'     cliques is also called a perfect ordering. If the graph is not
#'     chordal, then no such ordering exists.
#' 
#' @name graph-rip
#############################################################################

#### Based on Algorithm 4.11 in Steffen et all (the yellow book)
####
#### Known issues: Should check that amat specifies TUG;
#### possibly controlled by forceCheck argument

#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' 
#' @details The RIP ordering of the cliques of a decomposable
#'     (i.e. chordal) graph is obtained by first ordering the
#'     variables linearly with maximum cardinality search (by
#'     \code{mcs}). The root argument is transfered to \code{mcs} as a
#'     way of controlling which clique will be the first in the RIP
#'     ordering.  The \code{junction_tree()} (and
#'     \code{junction_tree()}) (for "junction tree") is just a wrapper
#'     for a call of \code{triangulate()} followed by a call of
#'     \code{rip()}.
#'
#' @param object An undirected graph represented either as a
#'     \code{graphNEL} object, an \code{igraph}, a (dense)
#'     \code{matrix}, a (sparse) \code{dgCMatrix}.
#' @param root A vector of variables. The first variable in the
#'     perfect ordering will be the first variable on 'root'. The
#'     ordering of the variables given in 'root' will be followed as
#'     far as possible.
#' @param nLevels Typically, the number of levels of the variables
#'     (nodes) when these are discrete. Used in determining the
#'     triangulation using a "minimum clique weight heuristic". See
#'     section 'details'.
#' @param amat Adjacency matrix
#' @param ... Additional arguments; currently not used
#' @return \code{rip} returns a list (an object of class
#'     \code{ripOrder}. A print method exists for such objects.)
#' 
#' @note The workhorse is the \code{ripMAT()} function. The
#'     \code{nLevels} argument to the \code{rip} functions has no
#'     meaning.
#' 
#' @seealso \code{\link{mcs}}, \code{\link{triangulate}},
#'     \code{\link{moralize}}, \code{\link{ug}}, \code{\link{dag}}
#'
#' @keywords utilities
#' @examples
#' 
#' ## graphNEL
#' uG <- ug(~me:ve + me:al + ve:al + al:an + al:st + an:st)
#' mcs(uG)
#' rip(uG)
#' junction_tree(uG)
#' 
#' ## Adjacency matrix
#' uG <- ug(~me:ve:al + al:an:st, result="matrix")
#' mcs(uG)
#' rip(uG)
#' junction_tree(uG)
#' 
#' ## Sparse adjacency matrix
#' uG <- ug(c("me", "ve", "al"), c("al", "an", "st"), result="dgCMatrix")
#' mcs(uG)
#' rip(uG)
#' junction_tree(uG)
#' 
#' ## Non--decomposable graph
#' uG <- ug(~1:2 + 2:3 + 3:4 + 4:5 + 5:1)
#' mcs(uG)
#' rip(uG)
#' junction_tree(uG)
#' 
#' 

#' @export
#' @rdname graph-rip
rip <- function(object, ...){
  UseMethod("rip")
}

#' @export
#' @rdname graph-rip
rip.default <- function(object, root=NULL, nLevels=NULL, ...){

    if (!inherits(object, c("graphNEL", "igraph", "matrix", "dgCMatrix")))
        stop("Object of correct type\n")

    if (!is_ug(object))
        stop("Graph must be undirected\n")

    ripMAT(as(object, "matrix"), root=root, nLevels=nLevels)
}

#' @export
#' @rdname graph-rip
ripMAT <- function(amat, root=NULL, nLevels=rep(2, ncol(amat))) {

    ##cat("ripMAT\n")
    
    ## mcs.idx: The enumeration of nodes in vn
    ## so 1,2,8 ... means that nodes 1,2,8... come first in the elimination
    ## mcs.vn: corresponding ordering of nodes

    mcs.vn <- mcsMAT(amat, root=root)
    if (length(mcs.vn) == 0)
        return(NULL)
    
    cq.vn   <- max_cliqueMAT(amat) ##[[1]]
    ncq     <- length(cq.vn)
    vn      <- colnames(amat)
    
  if (ncq > 1) {
      ## cq.idx       <- lapplyV2I(cq.vn, vn)
      cq.mcs.idx   <- lapplyV2I(cq.vn, mcs.vn)
      max.no       <- sapply(cq.mcs.idx, function(cc) max(cc))
      ## max.no: The highest number assigned to a clique by MCS
      cq.mcs.idx   <- cq.mcs.idx[ order(max.no) ]
      ## cq.mcs.idx: Order the cliques by the largest number
      cq.mcs.vn    <- lapplyI2V( cq.mcs.idx, mcs.vn )
      
      sp.idx      <- vector("list", ncq)
      sp.idx[[1]] <- integer(0)
      pa.idx      <- rep.int(0L, ncq)
      
      ## Use: cq.mcs.idx
      for (ii in 2:ncq){
          paset  <- unlist(cq.mcs.idx[ 1:(ii-1L) ])
          isect  <- intersectPrim( cq.mcs.idx[[ii]], paset )
          sp.idx[[ii]] <- isect
          if (length(isect)){
              for (kk in (ii - 1):1) {
                  ##if (subsetof( isect, cq.mcs.idx[[kk]]) ){
                  if (is_subsetof(isect, cq.mcs.idx[[kk]])){
                      pa.idx[ii] <- kk
                      break()
                  }
              }
          }
      }
      nodes <- mcs.vn
      sp.vn <- lapplyI2V(sp.idx, mcs.vn)
      child <- match(seq_along(cq.mcs.idx), pa.idx)
      host  <- rep.int(0L, length(nodes))
      ll    <- lapply(cq.mcs.vn, match, nodes)
      for (ii in seq_along(ll)){
          host[ll[[ii]]] <- ii
      }
  } else { ## The graph has only one clique!
      cq.mcs.vn <- list(vn)
      nodes  <- vn
      pa.idx <- 0L
      sp.vn  <- list(character(0))
      child  <- NA
      host  <- rep.int(1L, length(nodes))
  }

    .getChildList <- function(parents) {
        vv  <- 1:length(parents)
        ft  <- unname(cbind(parents, vv))
        ft  <- ft[ft[, 1] != 0, , drop = FALSE]
        vch <- lapply(vv, function(ff) ft[ft[,1]==ff,2])
        names(vch) <- vv
        vch
    }
    ## FIXME: Create ftM2vch and ftM2vpar from the above
    
  rip3 <-
    structure(list(nodes       = nodes,
                   cliques     = cq.mcs.vn,
                   separators  = sp.vn,
                   parents     = pa.idx,
                   children    = child,
                   host        = host,
                   nLevels     = nLevels,
                   #createGraph = .createJTreeGraph,
                   childList   = .getChildList(pa.idx)
                   ),
              class="ripOrder")

  rip3
}

## @rdname graph-rip
## @param x Object to be printet or plottet
#' @export
print.ripOrder <- function(x, ...){
  idx <- 1:length(x$cliques)
  cat("cliques\n")
  mapply(function(xx,ii) cat(" ",ii,":",paste(xx, collapse=' '),"\n"),
         x$cliques, idx)

  cat("separators\n")
  mapply(function(xx,ii) cat(" ",ii,":",paste(xx, collapse=' '),"\n"),
         x$separators, idx)

  cat("parents\n")
  mapply(function(xx,ii) cat(" ",ii,":",paste(xx, collapse=' '),"\n"),
         x$pa, idx)

#  cat("Children\n")
#  mapply(function(xx,ii) cat(" ",ii,paste(xx, collapse=' '),"\n"), x$ch, idx)
}


## @rdname graph-rip
#' @method plot ripOrder
#' @export
plot.ripOrder <- function(x, ...){
    g <- .rip2ug(x)
    ## FIXME use igraph
    ## Rgraphviz::plot(g)
}


## .rip2ug <- function(rp_){
##     if (length(rp_$cliques) > 1){
##         ft <-cbind(rp_$parents, 1:length(rp_$parents))
##         ft <- ft[ft[, 1] != 0, , drop=FALSE]
##         V <- seq_along(rp_$parents)
##         if (nrow(ft) == 0){
##             jt <- new("graphNEL", nodes = as.character(V), edgemode = "undirected") ## FIXME
##         } else {
##             jt <- graph::ftM2graphNEL(ft, V=as.character(V), edgemode="undirected") ## FIXME
##         }
##     } else {
##         jt <- new("graphNEL", nodes = "1", edgemode = "undirected") ## FIXME
##     }
##     jt
## }


.rip2ug <- function(rp) {
    ft <- cbind(rp$parents, 1:length(rp$parents)) ## n x 2
    ft <- ft[ft[, 1] != 0, , drop=FALSE]

    vn <- seq_along(rp$parents)
    jt <- make_empty_graph(directed=FALSE)
    
    if (length(rp$cliques) == 1) {
        jt <- add_vertices(jt, 1) 
    } else {
        jt <- add_vertices(jt, length(vn))
        if (nrow(ft) > 0){
            jt <- add_edges(jt, ft)
        }
    }
    jt
}



## .rip2dag <- function (rip) {
##   if (length(rip$cliques) > 1) {
##     ft <- cbind(rip$parents, 1:length(rip$parents))
##     ft <- ft[ft[, 1] != 0, , drop = FALSE]
##     V  <- seq_along(rip$parents)
##     if (nrow(ft) == 0) {
##       jt <- new("graphNEL", nodes = as.character(V), edgemode = "directed") ## FIXME
##     } else {
##       jt <- graph::ftM2graphNEL(ft, V = as.character(V), edgemode = "directed") ## FIXME
##     }
##   } else {
##     jt <- new("graphNEL", nodes = "1", edgemode = "directed") ## FIXME
##   }
##   return(jt)
## }



#' @export
#' @rdname graph-rip
junction_tree <- function(object, ...){
  UseMethod("junction_tree")
}

#' @export
#' @rdname graph-rip
junction_tree.default <-  function(object, nLevels = NULL, ...){

    if (!inherits(object, c("graphNEL", "igraph", "matrix", "dgCMatrix")))
        stop("Object of correct type\n")

    junction_treeMAT(as(object, "matrix"), nLevels=nLevels, ...)
}

#' @export
#' @rdname graph-rip
junction_treeMAT <- function(amat, nLevels=rep(2, ncol(amat)), ...){
  tug_mat  <- triangulateMAT(amat, nLevels=nLevels, result="dgCMatrix", ...)
  ripMAT(tug_mat, nLevels=nLevels)
}



#' @rdname graph-rip
#' @section Synonymous functions:
#'
#' For backward compatibility with downstream packages we have the
#' following synonymous functions:
#'
#' * jTree = junction_tree (Used in rags2ridges)
#'
#' * junctionTree = junction_tree
#' 
#' @aliases junctionTree jTree

#' @export
jTree <- junction_tree

#' @export
junctionTree <- junction_tree


## #' @export
## jTree <- function(object, ...){
##   UseMethod("jTree")
## }

## #' @export
## jTree.default  <- junction_tree.default

