% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph_utilities.R
\name{graph-edgeList}
\alias{graph-edgeList}
\alias{edgeList}
\alias{edgeListMAT}
\alias{nonEdgeList}
\alias{nonEdgeListMAT}
\title{Find edges in a graph and edges not in a graph.}
\usage{
edgeList(object, matrix = FALSE)

edgeListMAT(adjmat, matrix = FALSE)

nonEdgeList(object, matrix = FALSE)

nonEdgeListMAT(adjmat, matrix = FALSE)
}
\arguments{
\item{object}{A \code{graphNEL} object, an \code{igraph} object, a dense
matrix or a sparse \code{dgCMatrix} (the two latter representing an
adjacency matrix).}

\item{matrix}{If TRUE the result is a matrix; otherwise the result
is a list.}

\item{adjmat}{An adjacency matrix.}
}
\description{
Returns the edges of a graph (or edges not in a graph)
where the graph can be either a \code{graphNEL} object, an \code{igraph}
object or an adjacency matrix.
}
\examples{
## A graph with edges
g  <- ug(~a:b + b:c + c:d)
gm <- as(g, "matrix")
edgeList(g)
edgeList(gm)
edgeListMAT(gm)
edgeList(g, matrix=TRUE)
edgeList(gm, matrix=TRUE)
edgeListMAT(gm, matrix=TRUE)
nonEdgeList(g)
nonEdgeList(gm)
nonEdgeListMAT(gm)
## A graph without edges
g  <- ug(~a + b + c)
gm <- as(g, "matrix")
edgeList(g)
edgeList(gm)
edgeListMAT(gm)
edgeList(g, matrix=TRUE)
edgeList(gm, matrix=TRUE)
edgeListMAT(gm, matrix=TRUE)
nonEdgeList(g)
nonEdgeList(gm)
nonEdgeListMAT(gm)

}
