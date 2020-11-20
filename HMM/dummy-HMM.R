setClass("dummyResponse", contains="response")

setGeneric("dummyResponse", function(y, pstart = NULL, fixed = NULL, ...) standardGeneric("dummyResponse"))

setMethod("dummyResponse", 
          signature(y="ANY"), 
          function(y,pstart=NULL,fixed=NULL, ...) {
            y <- matrix(y,length(y))
            x <- matrix(1)
            parameters <- list()
            npar <- 0
            mod <- new("dummyResponse",parameters=parameters,fixed=logical(0),x=x,y=y,npar=npar)
            mod
          }
)

setMethod("show","dummyResponse",
          function(object) {
            cat("Dummy for fixed likelihood Model \n")
          }
)

setMethod("dens","dummyResponse",
          function(object,log=FALSE) {
            if(log) log(as.numeric(object@y)) else as.numeric(object@y)
          }
)

setMethod("getpars","dummyResponse",
          function(object,which="pars",...) {
            switch(which,
                   "pars" = {
                     pars <- numeric(0)
                   },
                   "fixed" = {
                     pars <- logical(0)
                   }
            )
            return(pars)
          }
)

setMethod("setpars","dummyResponse",
          function(object, values, which="pars", ...) {
            npar <- npar(object)
            if(length(values)!=npar) stop("length of 'values' must be",npar)
            # determine whether parameters or fixed constraints are being set
            nms <- ""
            switch(which,
                   "pars"= {
                   },
                   "fixed" = {
                   }
            )
            names(object@parameters) <- nms
            return(object)
          }
)

setMethod("fit","dummyResponse",
          function(object,w) {
            return(object)
          }
)

setMethod("predict","dummyResponse", 
          function(object) {
            ret <- object@y
            return(ret)
          }
)