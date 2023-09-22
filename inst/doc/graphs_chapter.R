## ----include=FALSE--------------------------------------------------------------------------------
library(knitr)
opts_chunk$set(
fig.path='fig/GRAPH', tidy=FALSE
)

## ----echo=F-------------------------------------------------------------------
options("width"=80)
library(igraph)
ps.options(family="serif")

## -----------------------------------------------------------------------------
library(gRbase)
ug0 <- gRbase::ug(~a:b, ~b:c:d, ~e)
ug0 <- gRbase::ug(~a:b + b:c:d + e)
ug0 <- gRbase::ug(~a*b + b*c*d + e)
ug0 <- gRbase::ug(c("a", "b"), c("b", "c", "d"), "e")
ug0

## -----------------------------------------------------------------------------
plot(ug0)

## -----------------------------------------------------------------------------
myplot <- function(x, layout=layout.fruchterman.reingold(x), ...) {
  V(x)$size <- 30
  V(x)$label.cex <- 3
  plot(x, layout=layout, ...)
  return(invisible())
}

## -----------------------------------------------------------------------------
myplot(ug0)

## -----------------------------------------------------------------------------
ug0i <- gRbase::ug(~a:b + b:c:d + e, result="matrix")
ug0i

## -----------------------------------------------------------------------------
as(ug0, "matrix")
as(ug0, "dgCMatrix")
as(ug0i, "igraph")

## ----eval=T-------------------------------------------------------------------
## Using gRbase
ug0a <- gRbase::addEdge("a", "c", ug0)
ug0a <- gRbase::removeEdge("c", "d", ug0)

## -----------------------------------------------------------------------------
## Using igraph
ug0a <- igraph::add_edges(ug0, c("a", "c"))
ug0a <- igraph::delete_edges(ug0, c("c|d"))

## ----eval=T-------------------------------------------------------------------
## Using gRbase
gRbase::nodes(ug0)           
gRbase::edges(ug0) |> str()

## -----------------------------------------------------------------------------
## Using igraph
igraph::V(ug0)
igraph::V(ug0) |> attr("names")
igraph::E(ug0)
igraph::E(ug0) |> attr("vnames")

## -----------------------------------------------------------------------------
gRbase::maxClique(ug0) ## |> str() 
gRbase::get_cliques(ug0) |> str()
## Using igraph
igraph::max_cliques(ug0) |>
    lapply(function(x) attr(x, "names"))  |> str()

## -----------------------------------------------------------------------------
gRbase::separates("a", "d", c("b", "c"), ug0)

## -----------------------------------------------------------------------------
ug1 <- gRbase::subGraph(c("b", "c", "d", "e"), ug0)

## -----------------------------------------------------------------------------
ug12 <- igraph::subgraph(ug0, c("b", "c", "d", "e"))

## -----------------------------------------------------------------------------
par(mfrow=c(1,2), mar=c(0,0,0,0))
myplot(ug1); myplot(ug12)

## -----------------------------------------------------------------------------
gRbase::adj(ug0, "c")
gRbase::closure("c", ug0)

## -----------------------------------------------------------------------------
dag0 <- gRbase::dag(~a, ~b*a,  ~c*a*b, ~d*c*e, ~e*a, ~g*f)
dag0 <- gRbase::dag(~a + b*a + c*a*b + d*c*e + e*a + g*f)
dag0 <- gRbase::dag(~a + b|a + c|a*b + d|c*e + e|a + g|f)
dag0 <- gRbase::dag("a", c("b", "a"), c("c", "a", "b"), c("d", "c", "e"), 
            c("e", "a"), c("g", "f"))
dag0

## ----echo=T-------------------------------------------------------------------
myplot(dag0)

## -----------------------------------------------------------------------------
gRbase::nodes(dag0)
gRbase::edges(dag0) |> str()

## -----------------------------------------------------------------------------
edgeList(dag0) |> str()

## -----------------------------------------------------------------------------
vpardag0 <- gRbase::vpar(dag0)
vpardag0 |> str()
vpardag0$c

## -----------------------------------------------------------------------------
gRbase::parents("d", dag0)
gRbase::children("c", dag0)
gRbase::ancestralSet(c("b", "e"), dag0)
ag <- gRbase::ancestralGraph(c("b", "e"), dag0)
myplot(ag)

## -----------------------------------------------------------------------------
dag0m <- gRbase::moralize(dag0)
myplot(dag0m)

## -----------------------------------------------------------------------------
adjm <- matrix(c(0, 1, 1, 1,
                 1, 0, 0, 1,
                 1, 0, 0, 1,
                 0, 1, 0, 0), byrow=TRUE, nrow=4)
rownames(adjm) <- colnames(adjm) <- letters[1:4]
adjm

## -----------------------------------------------------------------------------
gG1 <- gG2 <- as(adjm, "igraph")
lay <- layout.fruchterman.reingold(gG1)
E(gG2)$arrow.mode <- c(2,0)[1+is.mutual(gG2)]

## -----------------------------------------------------------------------------
par(mfrow=c(1,2), mar=c(0,0,0,0))
myplot(gG1, layout=lay); myplot(gG2, layout=lay)

## -----------------------------------------------------------------------------
d1 <- matrix(0, 11, 11)
d1[1,2] <- d1[2,1] <- d1[1,3] <- d1[3,1] <- d1[2,4] <- d1[4,2] <- 
  d1[5,6] <- d1[6,5] <- 1
d1[9,10] <- d1[10,9] <- d1[7,8] <- d1[8,7] <- d1[3,5] <- 
  d1[5,10] <- d1[4,6] <- d1[4,7] <- 1
d1[6,11] <- d1[7,11] <- 1
rownames(d1) <- colnames(d1) <- letters[1:11]
cG1 <- as(d1, "igraph")
E(cG1)$arrow.mode <- c(2,0)[1+is.mutual(cG1)]
myplot(cG1, layout=layout.fruchterman.reingold)

## -----------------------------------------------------------------------------
myplot(ug0)

## -----------------------------------------------------------------------------
gRbase::separates("a", "d", "b", ug0) 

## -----------------------------------------------------------------------------
gRbase::separates("a", "d", character(0), ug0)

## ----eval=T-------------------------------------------------------------------
d_separates <- function(a, b, c, dag_) {
    ##ag <- ancestralGraph(union(union(a, b), c), dag_)
    ag <- ancestralGraph(c(a, b, c), dag_)
    separates(a, b, c, moralize(ag))
}
d_separates("c", "e", "a", dag0)    

## -----------------------------------------------------------------------------
gRbase::is.simplicial("b", ug0)
gRbase::simplicialNodes(ug0)

## -----------------------------------------------------------------------------
gRbase::connComp(ug0) |> str()
## Using igraph
igraph::components(ug0) |> str()

## ----eval=T-------------------------------------------------------------------
gRbase::is.triangulated(ug0)

## -----------------------------------------------------------------------------
igraph::is_chordal(ug0)

## -----------------------------------------------------------------------------
gRbase::is.decomposition("a", "d", c("b", "c"), ug0) 

## -----------------------------------------------------------------------------
myplot(ug0)

## -----------------------------------------------------------------------------
gRbase::mcs(ug0)

## -----------------------------------------------------------------------------
igraph::max_cardinality(ug0)
igraph::max_cardinality(ug0)$alpham1 |> attr("names")

## -----------------------------------------------------------------------------
gRbase::mcs(ug0, root=c("d", "c", "a"))

## -----------------------------------------------------------------------------
gRbase::rip(ug0)

## -----------------------------------------------------------------------------
ug2 <- gRbase::ug(~a:b:c + c:d + d:e + a:e)
ug2 <- gRbase::ug(~a:b:c + c:d + d:e + e:f + a:f)

gRbase::is.triangulated(ug2)
igraph::is_chordal(ug2)  |> str()
myplot(ug2)

## -----------------------------------------------------------------------------
ug3 <- gRbase::triangulate(ug2)
gRbase::is.triangulated(ug3)

## -----------------------------------------------------------------------------
zzz <- igraph::is_chordal(ug2, fillin=TRUE, newgraph=TRUE)
V(ug2)[zzz$fillin]
ug32 <- zzz$newgraph

## -----------------------------------------------------------------------------
par(mfrow=c(1,3), mar=c(0,0,0,0))
lay <- layout.fruchterman.reingold(ug2) 
myplot(ug2, layout=lay);
myplot(ug3, layout=lay);
myplot(ug32, layout=lay)

## ----eval=F-------------------------------------------------------------------
#  adj(moralize(dag0), "e")

## -----------------------------------------------------------------------------
ug4 <- graph.formula(a -- b:c, c--b:d, e -- a:d) 
ug4
myplot(ug4)

## ----eval=T-------------------------------------------------------------------
ug4.2 <- graph.empty(n=5, directed=FALSE)
V(ug4.2)$name <- V(ug4.2)$label <- letters[1:5]
ug4.2 <- add.edges(ug4.2, 1+c(0,1, 0,2, 0,4, 1,2, 2,3, 3,4))
ug4.2

## ----eval=F-------------------------------------------------------------------
#  myplot(ug4, layout=layout.graphopt)

## -----------------------------------------------------------------------------
ug4$layout   <- layout.graphopt(ug4)
V(ug4)$label <- V(ug4)$name
V(ug4)$color <- "red"
V(ug4)[1]$color <- "green"
V(ug4)$size <- 40
V(ug4)$label.cex <- 3
plot(ug4)

## -----------------------------------------------------------------------------
ug5 <- set.vertex.attribute(ug4, "discrete", value=c(T, T, F, F, T))
V(ug5)[discrete]$color <- "green"
V(ug5)[!discrete]$color <- "red"
plot(ug5)

## ----eval=F-------------------------------------------------------------------
#  xy <- tkplot.getcoords(2)
#  plot(g, layout=xy)

## -----------------------------------------------------------------------------
layout.fruchterman.reingold(ug4)

## -----------------------------------------------------------------------------
ug4$layout <- layout.fruchterman.reingold

## -----------------------------------------------------------------------------
ug4$layout <- layout.fruchterman.reingold(ug4)

## ----samelay, include=T, eval=T-----------------------------------------------
ug5 <- gRbase::ug(~A*B*C + B*C*D + D*E)
ug6 <- gRbase::ug(~A*B + B*C + C*D + D*E) 
lay.fr <- layout.fruchterman.reingold(ug5)
ug6$layout       <- ug5$layout       <- lay.fr
V(ug5)$size      <- V(ug6)$size      <- 50
V(ug5)$label.cex <- V(ug6)$label.cex <- 3
par(mfrow=c(1,2), mar=c(0,0,0,0))
plot(ug5); plot(ug6)

## ----eval=T-------------------------------------------------------------------
em1 <- matrix(c(0, 1, 1, 0,
                0, 0, 0, 1,
                1, 0, 0, 1,
                0, 1, 0, 0), nrow=4, byrow=TRUE)
iG  <- graph.adjacency(em1) 
V(iG)$shape <- c("circle", "square", "circle", "square")  
V(iG)$color <- rep(c("red", "green"), 2)
V(iG)$label <- c("A", "B", "C", "D")
E(iG)$arrow.mode <- c(2,0)[1 + is.mutual(iG)]
E(iG)$color  <- rep(c("blue", "black"), 3)
E(iG)$curved <- c(T, F, F, F, F, F) 
iG$layout    <- layout.graphopt(iG)
myplot(iG)

## -----------------------------------------------------------------------------
args(querygraph)

## ----eval=T-------------------------------------------------------------------
ug_ <- gRbase::ug(~a:b + b:c:d + e)
gRbase::separates("a", "d", c("b", "c"), ug_)                   
gRbase::querygraph(ug_, "separates", "a", "d", c("b", "c"))
gRbase::qgraph(ug_, "separates", "a", "d", c("b", "c")) 

