##
## Update 2016 in Rennes, France
## Needed to get compileCPT to work with proper array input in gRain
## (strange that nobody has noticed that it fails before)
##

#' @title Utilities for data handling
#' @name data_handling
#' @param x Data, typically a dataframe.
#' @rdname data_handling
#' @export
valueLabels         <- function(x) UseMethod("valueLabels")
#' @rdname data_handling
#' @export
valueLabels.default  <- function(x) attr(x,"dimnames")
#' @rdname data_handling
#' @export
varNames         <- function(x)UseMethod("varNames")
#' @rdname data_handling
#' @export
varNames.default <- function(x) names(attr(x,"dimnames"))
#' @rdname data_handling
#' @export
nLevels         <- function(x)UseMethod("nLevels")
#' @rdname data_handling
#' @export
nLevels.default <- function(x) dim(x)


## #' @export
## valueLabels.gmData  <- function(x) attr(x,"valueLabels")

## #' @export
## varNames.gmData <- function(x)as.vector(x$varNames)




## ## #################################################################
## ##
## ## Add and drop edges
## ##
## ## #################################################################

## #dropEdge <- function(object, name.1, name.2) UseMethod("dropEdge")
## #' @export
## dropEdge.gModel <-
##           function(object,name.1,name.2) {

##             ## cat("Drop:",name.1,name.2,"\n",sep=" ")
##             ## edit hllm formula
##             form <- formula(object)
##             listform <- readf(form[2])
##             new.form <- .delete.edge(listform,c(name.1,name.2))

##             form <- paste("~",showf(new.form))
##             formula(object) <- as.formula(form)

##             if (inherits(object,"gRfit"))
##               object <- fit(object)

##             return(object)
##           }


## #addEdge <- function(object, name.1, name.2) UseMethod("addEdge")
## #' @export
## addEdge.gModel <-
##           function(object,name.1,name.2) {

##             new.object <- object
##             ## edit hllm formula
##             form <- formula(object)
##             listform <- readf(form[2])
##             new.form <- .add.edge(listform,c(name.1,name.2))
##             form <- paste("~",showf(new.form))
##             formula(new.object) <- as.formula(form)

##             if (inherits(new.object,"gRfit"))
##               new.object <- fit(new.object)

##             return(new.object)
##           }


## ggm <- function(formula=~.^1, gmData, marginal){
##   value <- processFormula(formula,gmData, marginal,"Continuous")
##   value$gmData <- gmData
##   class(value) <- c("ggm","gModel")
##   return(value)
## }


## #' @export
## fit.ggm <- function(object, ...) {
##   Ydf  <- observations(object$gmData)
##   nobs <- nrow(Ydf)
##   gc <- object$numformula
##   Ymat <- as.matrix(Ydf)
##   Smat   <- cov(Ymat)*(nobs-1)/nobs
##   ipsFit <- ips(gc,Smat)
##   fit      <- outfun( ipsFit$MLE, Smat,nrow(Ydf))
##   fit$n    <- nobs
##   fit$mean <- apply(Ymat,2,mean)
##   fit$df   <- length(which(fit$part==0))/2
##   fit$iterations <- ipsFit$iterations
##   value<-object
##   value$fit <- fit
##   class(value) <- c("gRfit", "ggm", class(object))
##   return(value)
## }

## ## Partial correlation matrix ##
## ## computes partial correlation matrix for covariance matrix ##
## partial.corr.matrix <- function(S){
##   A <- solve(S)
##   temp <- diag(1/sqrt(diag(A)))
##   temp <- zapsmall(-temp%*%A%*%temp)
##   diag(temp) <- 1
##   return(temp)
## }

## ## Output function ##
## outfun <- function(Sigma, S, n){
##     .ell <- function(Sigma, S, n){
        
##         shdet <- function(Sigma){
##             prod(eigen(Sigma)[[1]])
##         }
##         p <- dim(S)[1]
##         const <- -n * p/2 * log(2 * pi)
##         const - n/2 * log(shdet(Sigma)) - n/2 * sum(diag( solve(Sigma) %*% S )) 
##     }
    
##     return(list(Sigma=round(Sigma,3),
##                 eigenvalues=eigen(Sigma)[[1]],
##                 correlation=cov2cor(Sigma),###corr.matrix(Sigma),
##                 partial.correlations=partial.corr.matrix(Sigma),
##                 loglik=.ell(Sigma,S,n)))
## }




## ## UNDIRECTED GRAPHS ###

## ## cliques must be a list with all cliques ##
## ## the components of this list must be ##
## ## vectors enumerating the vertices in a clique ##
## ips <- function(cliques, S){
##   if(!is.matrix(S)){
##     return("Second argument is not a matrix!")
##   }
##   if(dim(S)[1]!=dim(S)[2]){
##     return("Second argument is not a square matrix!")
##   }
##   if(min(eigen(S)[[1]])<=0){
##     return("Second argument is not a positive definite matrix!")
##   }
##   start <- diag(diag(S)) # starting value
##   p <- dim(S)[1] # dimensionality
##   K <- solve(start)
##   i <- 0
##   if(length(cliques)==1){
##     return(list(MLE=S, iterations=1))
##   }
##   my.complement <- function(C) return(setdiff(1:p,C))
##   cliques.complements <- lapply(cliques, my.complement)
##   repeat{
##     K.old <- K
##     i <- i+1
##     for(j in 1:length(cliques)){
##       C <- cliques[[j]]
##       notC <- cliques.complements[[j]]
##       K[C,C] <- solve( S[C,C] ) +
##         K[C,notC]%*%solve(K[notC,notC])%*%K[notC,C]
##     }
##     if(sum(abs(K.old-K)) < 1e-10) break
##   }
##   return(list(MLE=solve(K), iterations=i))
## }

## globalVariables(c("rawdata", "loglm.formula"))


### Some generic functions




### ' @export

## nLevels.gmData  <- function(x)structure(as.vector(x$nLevels), .Names=varNames(x))


## #' @export
## "varNames<-" <- function(tmp,value) UseMethod("varNames<-")






## nLevels.array      <- function(x) dim(x)
## nLevels.parray     <- function(x) dim(x)

##varNames.array     <- function(x) names(attr(x,"dimnames"))
##valueLabels.array  <- function(x) attr(x,"dimnames")

##varNames.parray    <- function(x) names(attr(x,"dimnames"))
##valueLabels.parray <- function(x) attr(x,"dimnames")

##
## END of French update
##

## #' @export
## "latent.gmData" <- function(x){attr(x,"latent")}
## #' @export
## "latent" <- function(x) UseMethod("latent")
## #' @export
## "latent<-.gmData" <- function(tmp,value){attr(tmp,"latent")<-value; return(tmp)}
## #' @export
## "latent<-" <- function(tmp,value) UseMethod("latent<-")

## #' @export
## "valueLabels<-.gmData"<- function(tmp,value){attr(tmp,"valueLabels")<-value; return(tmp)}
## #' @export
## "valueLabels<-"       <- function(tmp,value) UseMethod("valueLabels<-")

## #' @export
## observations    <- function(x) UseMethod("observations")
## #' @export
## obs             <- function(x) UseMethod("observations")

## #' @export
## observations.gmData <- function(x) attr(x,"observations")

## #' @export
## "observations<-.gmData"<- function(tmp,value){attr(tmp,"observations")<-value; return(tmp)}

## #' @export
## "observations<-"       <- function(tmp,value)UseMethod("observations<-")

## "description.gmData" <- function(x){attr(x,"description")}
## "description" <- function(x) UseMethod("description")

## #' @export
## "description<-.gmData" <- function(tmp,value){attr(tmp,"description")<-value; return(tmp)}

## #' @export
## "description<-" <- function(tmp,value) UseMethod("description<-")


## #' @export
## "varTypes.gmData" <- function(x){structure(x$varTypes, .Names=varNames(x))}

## #' @export
## "varTypes" <- function(x) UseMethod("varTypes")

## ## #' @export
## ## "varTypes<-.gmData" <- function(tmp,value){ tmp$varTypes <-value; return(tmp)}

## #' @export
## "varTypes<-" <- function(tmp,value) UseMethod("varTypes<-")

## #' @export
## "varNames<-.gmData" <- function(tmp,value){ tmp$varNames <-value; return(tmp)}

## #' @export
## "nLevels<-.gmData" <- function(tmp,value){ tmp$nLevels <-value; return(tmp)}

## #' @export
## "nLevels<-" <- function(tmp,value) UseMethod("nLevels<-")

## #' @export
## shortNames.gmData <- function(x)structure(as.vector(x$shortNames), .Names=varNames(x))

## #' @export
## shortNames <- function(x)UseMethod("shortNames")

## #' @export
## "shortNames<-.gmData" <- function(tmp,value){ tmp$shortNames <-value; return(tmp)}

## #' @export
## "shortNames<-" <- function(tmp,value) UseMethod("shortNames<-")

## #' @export
## dataOrigin.gmData   <- function(x) attr(x,"dataOrigin")[1]

## #' @export
## dataOrigin   <- function(x)UseMethod("dataOrigin")

## #' @export
## "ordinal"           <- function(tmp) UseMethod("ordinal")

## #' @export
## "ordinal<-"         <- function(tmp,value) UseMethod("ordinal<-")

## #' @export
## "ordinal.gmData" <- function(tmp)attr(tmp,"ordinal")

## #' @export
## "ordinal<-.gmData" <- function(tmp,value){
##   varTypes(tmp)[match(value, varNames(tmp))]<-"Ordinal"
##   return(tmp)}

## #' @export
## "nominal"           <- function(tmp) UseMethod("nominal")

## #' @export
## "nominal<-"         <- function(tmp,value) UseMethod("nominal<-")

## #' @export
## "nominal.gmData" <- function(tmp){
##   varNames(tmp)["Discrete"==varTypes(tmp)]
## }

## #' @export
## "nominal<-.gmData" <- function(tmp,value){
##   varTypes(tmp)[match(value, varNames(tmp))]<-"Discrete"
##   return(tmp)}






####################################################################
#' @export
## as.gmData       <- function(from) UseMethod("as.gmData")
####################################################################

## #' @export
## print.gmData  <- function(x, ...){
##   xx<-attr(x,"description")
##   if (!is.null(xx))
##     cat("Description:", xx, "\n")
##   print.data.frame(x);
##   ##cat("Data origin:     ", .dataOrigin(x),"\n")
##   if (!is.null(latent(x)))
##     cat ("Latent variables:", paste(latent(x),collapse=' '), "\n")
##   if (!is.null(valueLabels(x)))
##   cat("To see the values of the factors use the 'valueLabels' function\n")
##   if (!is.null(observations(x)))
##   cat("To see the data use the 'observations' function\n")
##   return(invisible(x))
## }

## # summary.gmData  <- function(object, ...){
## #   print(table(object$varTypes))
## #   if (!is.null(observations(object))) {
## #     cat("\nObservation summary:\n")
## #     print(summary(obs(object)))
## #   }
## #   invisible(object)
## # }


## #' @export
## summary.gmData <- function(object, ...){
##   print(object)
##   mapply(function(xx,ll){
##     cat("Factor:", ll, "\n Levels:", paste(xx,sep=' '),"\n")
##   }, valueLabels(object),names(valueLabels(object)))
##   return(invisible(object))
## }






#### ##############################################################

# newgmData <- function(varNames,
#                    varTypes=rep(validVarTypes()[1],length(varNames)),
#                    nLevels=NA,
#                    latent=NA,
#                    valueLabels=NULL,
#                    observations=NULL,
#                    description=NA,
#                    shortNames=c(letters,LETTERS)
#                    ){
#   value <- data.frame(varNames, abbreviate(varNames,1),row.names=NULL)

#   names(value) <- c("varNames","shortNames")
#   value$varTypes <- factor(varTypes,levels=validVarTypes())
#   value$nLevels  <- nLevels

#   obsclass <- class(observations)
#   class(value) <- c("gmData","data.frame")


#   attr(value,"valueLabels")    <- valueLabels
#   attr(value,"latent")         <- latent
#   attr(value,"description")    <- description
#   attr(value,"observations")   <- observations
#   ##switch(class(data),
#   switch(obsclass,
#          "table"=     { attr(value,"dataOrigin")     <- "table"      },
#          "data.frame"={ attr(value,"dataOrigin")     <- "data.frame" },
#          NULL=        { attr(value,"dataOrigin")     <- "table"      })
#   return(value)
# }


## #' @export
## newgmData <- function (varNames,
##                        varTypes = rep(validVarTypes()[1], length(varNames)),
##                        nLevels  = NULL,
##                        latent   = NULL,
##                        valueLabels  = NULL,
##                        observations = NULL,
##                        description  = NULL,
##                        shortNames   = NULL)
## {

##   cl <- match.call()

##   .is.subset <- function(x,y){
##     setequal(intersect(x,y),x)
##   }

##   .simpleCap <- function(x) {
##     s <- strsplit(x, " ")[[1]]
##     paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")
##   }

##   ## Find good short names...
##   ##
##   if (is.null(shortNames)){
##     nam <- varNames
##     nama  <- abbreviate(nam,1)
##     nc    <- nchar(nama)
##       rest  <- setdiff(c(letters,LETTERS), nama[nc==1])
##     if (length(which(nc>1)) <= length(rest))
##       nama[nc>1]<- rest[1:length(which(nc>1))]
##   } else {
##     nama <- shortNames
##   }

##   value <- data.frame(varNames, nama, row.names = NULL)
##   names(value) <- c("varNames", "shortNames")

##   ## Deal with abbreviated varTypes
##   ##
##   varTypes <- sapply(varTypes, .simpleCap)
##   varTypes <- sapply(varTypes, match.arg, choices=validVarTypes(), several.ok = FALSE)

##   value$varTypes <- factor(varTypes, levels = validVarTypes())
##   discidx        <- which("Discrete"==varTypes | "Ordinal"==varTypes)
##   aa             <- rep(NA, length(varNames))

##   ## If valueLabels=c(1,2,3) turn into list(c(1,2,3))
##   ##
##   if (!is.null(valueLabels) & !is.list(valueLabels))
##     valueLabels <- list(valueLabels)

##   if (is.null(nLevels) & is.null(valueLabels)){
##     ## If neither nLevels or valueLabels are given; make all
##     ## categorical variables binary
##     ##
##     aa[discidx]   <-  2
##     nLevels <- aa
##   }

##   if (!is.null(valueLabels)){
##     ## If valueLabels are given, infer nLevels from these; recycle if necessary...
##     ##
##     if (!.is.subset(varNames[discidx], names(valueLabels))){
##       vl <- rep(valueLabels, length(discidx))
##       valueLabels <- vl[1:length(discidx)]
##       names(valueLabels) <- varNames[discidx]
##     }
##     uu            <- valueLabels[varNames[discidx]]
##     uu            <- sapply(uu,length)
##     aa[discidx]   <- uu
##     value$nLevels <- unlist(aa)
##   } else {
##     ## Use nLevels as given; recycle if necessary
##     ## Infer valueLabels from these
##     v <- nLevels[discidx]
##     v <- v[!is.na(v)]
##     if (length(v)==0)
##       v <- 2
##     v <- rep(v, length(discidx))
##     v <- v[discidx]
##     aa[discidx]   <-  v
##     value$nLevels <- unlist(aa)
##     uu <- varNames[discidx]
##     ##v  <<- v

##     valueLabels <- mapply(function(nn,vv){paste(nn,1:vv,sep='')},uu,v, SIMPLIFY=FALSE)

##   }


##   class(value) <- c("gmData", "data.frame")
##   attr(value, "valueLabels")  <- valueLabels
##   attr(value, "latent")       <- latent
##   attr(value, "description")  <- description
##   attr(value, "observations") <- observations
##   attr(value, "dataOrigin")   <- class(observations)

##   obsclass <- class(observations)

##   if (is.null(obsclass)){
##     attr(value, "dataOrigin") <- NULL
##   } else {
##     if(is.element("table", obsclass))
##       attr(value, "dataOrigin") <- c("table",setdiff(obsclass, "table"))
##     else{
##       if(is.element("data.frame", obsclass))
##         attr(value, "dataOrigin") <- c("data.frame", setdiff(obsclass, "data.frame"))
##       else
##         attr(value, "dataOrigin") <- "other"
##     }
##   }



##   return(value)
## }







#     switch(class(observations),
#     table = {
#         attr(value, "dataOrigin") <- "table"
#     }, data.frame = {
#         attr(value, "dataOrigin") <- "data.frame"
#     }, "NULL" = {
#         attr(value, "dataOrigin") <- "table"
#     })

## #' @export
## validVarTypes <- function() {c("Discrete","Ordinal","Continuous")}


## ## ####################################################################
## ## Convert data.frame into gmData

## #' @export
## as.gmData.data.frame <- function(from){
##   fact   <- unlist(lapply(1:ncol(from), function(j)
##                           is.factor(from[,j])))
##   Types <- rep(validVarTypes()[3],length(fact))
##   Types[fact] <- validVarTypes()[1]

##   levels <- unlist(lapply(1:ncol(from),
##                           function(j)
##                           {
##                             if(is.factor(from[,j]))
##                               length(levels(from[,j]))
##                             else NA}
##                           )
##                    )

##   if (length(which(fact))>0){
##     vallabels <- list()
##     for (j in which(fact)){
##       vallabels <- c(vallabels, list(levels(from[,j])))
##     }
##     names(vallabels) <- names(from[which(fact)])
##   } else {
##     vallabels <- list()
##   }

##   newgmData(
##       varNames=names(from),
##       varTypes=Types,
##       nLevels=levels,
##       valueLabels=vallabels,
##       observations=from
##  )
## }



## ## ####################################################################
## ## Convert table into gmData

## #' @export
## as.gmData.table <- function(from){
##   counts <- as.vector(from)
##   dn     <- dimnames(from)
##   name   <- names(lapply(dn,function(x)names(x)))
##   dim    <- unlist(lapply(dn,length))
##   newgmData(
##          varNames=name,
##          varTypes=rep("Discrete",length(name)),
##          nLevels=dim,
##          valueLabels=dn,
##          observations=from
##          )
## }


## ## ####################################################################
## ## Convert array into gmData

## #' @export
## as.gmData.array <- function(from){
##   res <- as.gmData(as.table(from))
##   observations(res) <- from
##   res
## }

## ##                              -*- Mode: Ess -*-
## ##gModel.R ---
## ##Author          : Claus Dethlefsen
## ##Created On      : Mon May 02 09:35:24 2005
## ##Last Modified By:
## ##Last Modified On:
## ##Update Count    : 0
## ##Status          : Unknown, Use with caution!
## ##


## #' @export
## gModel <- function(formula, gmData){
##   value <- list(formula=formula, gmData=gmData)
##   class(value) <- "gModel"
##   return(value)
## }

## #' @export
## "formula<-.gModel" <- function(tmp,value){tmp$formula<-value; return(tmp)}

## #' @export
## "formula<-" <- function(tmp,value) UseMethod("formula<-")

## #' @export
## "gmData.gModel" <- function(x){x$gmData}

## #' @export
## "gmData" <- function(x) UseMethod("gmData")

## #' @export
## "gmData<-.gModel" <- function(tmp,value){tmp$gmData<-value; return(tmp)}

## #' @export
## "gmData<-" <- function(tmp,value) UseMethod("gmData<-")

## #' @export
## print.gModel <- function(x, ...){
##   cat("Model information (gRbase)\n")
##   cat(" Class:   ", paste(class(x),collapse=' <- '),"\n")
##   cat(" Formula: ", paste(paste(x$formula),collapse=''), "\n")
## }



## "getFit" <- function(x) UseMethod("getFit")

## "getFit.gRfit" <- function(x){x$fit}

## "getFit<-" <- function(tmp,value) UseMethod("getFit<-")

## "getFit<-.gRfit" <- function(tmp,value){ tmp$fit <-value; return(tmp)}

## print.gRfit <- function(x, ...){
##   print.gModel(x)
##   cat("Fit information (gRbase)\n")
##   cat("   logL", deviance(getFit(x)), "df", x$fit$df,"\n")
## }

## summary.gRfit <- function(object,...)
##   summary(getFit(object))



## hllm <- function(formula = ~.^1,  gmData, marginal){
##   stop("function 'hllm' from gRbase is defunct. Please use the gRim package for hierarchical log-linear models.")
## }

## #' @export
## fit.hllm <- function(object,engine="loglm",...){
##   stop("function 'fit.hllm' from gRbase is defunct. Please use the gRim package for hierarchical log-linear models.")
## }

## #' @export
## stepwise.hllm <-    function (object, ...)  {
##   stop("function 'stepwise.hllm' from gRbase is defunct. Please use the gRim package for hierarchical log-linear models.")
## }


## update.gModel <- function(object, addedge=NULL, dropedge=NULL, ...){

##   if (!is.null(addedge))
##     object <- addEdge.gModel(object, addedge[1], addedge[2])

##   if (!is.null(dropedge))
##     object <- dropEdge.gModel(object, dropedge[1], dropedge[2])

##   return(object)


## }




