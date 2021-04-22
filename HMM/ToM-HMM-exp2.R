library(depmixS4)

setClass(
  "ToM",
  representation(
    act = "numeric",
    opp_act = "numeric",
    pred = "array", # nT * nA * nS 
    n_act = "numeric", # for each trial
    opponent_id = "numeric",
    game_id = "numeric",
    nS = "numeric",
    nA = "numeric", # max n(act)
    nO = "numeric",
    ntimes = "numeric"
  ),
  prototype(
    act = 1,
    opp_act = 1,
    pred = array(1.0, dim=c(1,1,1)),
    n_act = 1,
    opponent_id = 1,
    game_id = 1,
    nS = 1,
    nA = 1,
    nO = 1,
    ntimes = 1
  ),
  contains = "response"
)

setGeneric("ToM", function(act,
                           opp_act,
                           pred,
                           n_act,
                           opponent_id,
                           game_id,
                           data = NULL,
                           ntimes = NULL,
                           pstart = NULL,
                           fixed = NULL,
                           ...)
  standardGeneric("ToM"))

setMethod("ToM",
          signature(
            act = "numeric",
            opp_act = "numeric",
            pred = "array",
            n_act = "numeric",
            opponent_id = "numeric",
            game_id = "numeric",
            ntimes = "numeric"
          ),
          function(act,
                   opp_act,
                   pred,
                   n_act,
                   opponent_id,
                   game_id,
                   data = NULL,
                   ntimes = NULL,
                   pstart = NULL,
                   fixed = NULL,
                   prob = TRUE,
                   na.action = "na.pass",
                   ...) {
            if (!all.equal(length(act), length(opp_act), length(n_act), dim(pred)[1], length(opponent_id)))
              stop("unequal length of data")
            
            nT <- length(act)
            nS <- dim(pred)[3]
            nA <- dim(pred)[2]
            nO <- max(opponent_id)
        
            if (!is.null(ntimes)) {
              if (sum(ntimes) != nT)
                stop("invalid ntimes; sum of ntimes should equal total number of observations")
            } else {
              ntimes = nT
            }
            
            parameters <- list(alpha = .1)
            constr <- NULL
            
            npar <- length(unlist(parameters))
            
            if (is.null(fixed))
              fixed <- as.logical(rep(0, npar))
            if (!is.null(pstart)) {
              if (length(pstart) != npar)
                stop("length of 'pstart' must be", npar)
              parameters$alpha <- pstart[1]
            } else {
              # do nothing
            }
            mod <-
              new(
                "ToM",
                parameters = parameters,
                fixed = fixed,
                x = matrix(1),
                y = matrix(1),
                act = act,
                opp_act = opp_act,
                pred = pred,
                n_act = n_act,
                opponent_id = opponent_id,
                game_id = game_id,
                nS = nS,
                nA = nA,
                nO = nO,
                npar = npar,
                ntimes = ntimes,
                constr = constr
              )
            mod
          })

setMethod("setpars", "ToM",
          function(object,
                   values,
                   which = "pars",
                   prob = FALSE,
                   ...) {
            npar <- npar(object)
            if (length(values) != npar)
              stop("length of 'values' must be", npar)
            # determine whether parameters or fixed constraints are being set
            nms <- attributes(object@parameters)
            if (length(values) == 0)
              return(object) # nothing to set;
            switch(which,
                   "pars" = {
                     object@parameters$alpha <- values[1]
                   },
                   "fixed" = {
                     object@fixed <- as.logical(values)
                   })
            # attributes(object@parameters) <- nms
            return(object)
          })

setMethod("getpars", "ToM",
          function(object, which = "pars", ...) {
            switch(which,
                   "pars" = {
                     pars <- unlist(object@parameters)
                   },
                   "fixed" = {
                     pars <- object@fixed
                   })
            return(pars)
          })

setMethod("fit", "ToM",
          function(object, w) {
            if (missing(w))
              w <- NULL
            pars <- object@parameters
            # start <- c(gtools::logit(pars$alpha))
            #fit <- optim(start,
            #        fn = ToM.logLik,
            #        object = object,
            #        w = w)
            #pars$alpha <- gtools::inv.logit(fit$par[1])
            pars$alpha <- optimise(ToM.logLik, interval=c(.5,1), object = object, w = w)$minimum
            object <- setpars(object, unlist(pars))
            object
          })

ToM.logLik <- function(par, object, w) {
  #obj <-
  #  setpars(object, c(gtools::inv.logit(par[1])))
  obj <- setpars(object, par[1])
  if (!is.null(w))
    return(-2 * sum(w * dens(obj, log = TRUE)))
  else
    return(-2 * sum(dens(obj, log = TRUE)))
}

setMethod("dens", "ToM",
          function(object, log = FALSE) {
            pred <- depmixS4::predict(object)
            p <- pred[cbind(1:nrow(pred), object@act)]
            if (log)
              p <- log(p)
            p[is.na(p)] <- 0
            p
          })

setMethod("logLik", "ToM",
          function(object) {
            sum(dens(object, log = TRUE))
          })

setMethod("predict", "ToM",
          function(object) {
            lt <- length(object@ntimes)
            et <- cumsum(object@ntimes)
            bt <- c(1, et[-lt] + 1)
            
            nT <- et[lt]
            predict <- matrix(0.0, nrow=nT, ncol=object@nA)
            
            post <- lik <- matrix(0.0, nrow=nT, ncol=object@nS)
            for(i in 1:object@nS) {
              lik[,i] <- object@pred[,,i][cbind(1:nT,object@opp_act)]
            }
            lik <- object@parameters$alpha*lik + (1-object@parameters$alpha)*(1/object@n_act)
            
            prior <- rep(1,object@nS) # prior alpha for dirichlet on p(level)
            prior <- prior/sum(prior)
            for (id in 1:lt) {
              #lik <- object@parameters$alpha*lik[bt[id]:et[id],] + (1-object@parameters$alpha)*(1/object@n_act)
              
              for(o_id in unique(object@opponent_id)) {
                for(s in 1:object@nS) {
                  lik[(bt[id]:et[id])[object@opponent_id[bt[id]:et[id]] == o_id],s] <- dplyr::lag(log(prior[s]) + cumsum(log(lik[(bt[id]:et[id])[object@opponent_id[bt[id]:et[id]] == o_id],s])),default=log(prior[s]))
                }
                min <- apply(lik[(bt[id]:et[id])[object@opponent_id[bt[id]:et[id]] == o_id],],1,min)
                lik[(bt[id]:et[id])[object@opponent_id[bt[id]:et[id]] == o_id],] <- exp(lik[(bt[id]:et[id])[object@opponent_id[bt[id]:et[id]] == o_id],] - min)
                lik[(bt[id]:et[id])[object@opponent_id[bt[id]:et[id]] == o_id],] <- lik[(bt[id]:et[id])[object@opponent_id[bt[id]:et[id]] == o_id],]/rowSums(lik[(bt[id]:et[id])[object@opponent_id[bt[id]:et[id]] == o_id],])
                post[(bt[id]:et[id])[object@opponent_id[bt[id]:et[id]] == o_id],] <- lik[(bt[id]:et[id])[object@opponent_id[bt[id]:et[id]] == o_id],] # prior predictive
              }
            }
            for(a in 1:object@nA) {
              predict[,a] <- rowSums(post*(object@parameters$alpha*object@pred[,a,] + (1-object@parameters$alpha)*(1/object@n_act)))
            }
            ### THIS IS A HACK TO GET BEST RESPONSE; SHOULD BE MORE GENERAL SOLUTION
            predict[object@game_id != 3,] <- predict[object@game_id != 3,c(3,1,2)]
            predict[object@game_id == 3,] <- cbind(.5*predict[object@game_id == 3,2] +.5* predict[object@game_id == 3,3],
                                                   .5*predict[object@game_id == 3,1] +.5* predict[object@game_id == 3,3],
                                                   .5*predict[object@game_id == 3,1] +.5* predict[object@game_id == 3,2])
            return(predict)
          })
