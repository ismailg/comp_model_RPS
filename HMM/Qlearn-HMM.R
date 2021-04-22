library(depmixS4)

setClass(
  "Qlearn",
  representation(
    reward = "numeric",
    state = "numeric",
    act = "numeric",
    act_mask = "matrix",
    nS = "numeric",
    nA = "numeric",
    ntimes = "numeric"
  ),
  prototype(
    reward = 1,
    state = 1,
    act = 1,
    act_mask = matrix(1),
    nS = 1,
    nA = 1,
    ntimes = 1
  ),
  contains = "response"
)

setGeneric("Qlearn", function(reward,
                              state,
                              act,
                              act_mask,
                              data = NULL,
                              ntimes = NULL,
                              pstart = NULL,
                              fixed = NULL,
                              ...)
  standardGeneric("Qlearn"))

setMethod("Qlearn",
          signature(
            reward = "numeric",
            state = "numeric",
            act = "numeric",
            act_mask = "matrix",
            ntimes = "numeric"
          ),
          function(reward,
                   state,
                   act,
                   act_mask,
                   data = NULL,
                   ntimes = NULL,
                   pstart = NULL,
                   fixed = NULL,
                   prob = TRUE,
                   na.action = "na.pass",
                   ...) {
            if (!all.equal(length(reward), length(state), length(act), nrow(act_mask)))
              stop("unequal length of data")
            
            nT <- length(reward)
            nS <- max(state, na.rm = TRUE)
            nA <- max(act)
            
            Q <- array(0.0, dim = c(nT, nS, nA))
            
            if (!is.null(ntimes)) {
              if (sum(ntimes) != nT)
                stop("invalid ntimes; sum of ntimes should equal total number of observations")
            } else {
              ntimes = nT
            }
            
            parameters <- list(alpha = .1, beta = 1)
            constr <- NULL
            
            npar <- length(unlist(parameters))
            
            if (is.null(fixed))
              fixed <- as.logical(rep(0, npar))
            if (!is.null(pstart)) {
              if (length(pstart) != npar)
                stop("length of 'pstart' must be", npar)
              parameters$alpha <- pstart[1]
              parameters$beta <- pstart[2]
            } else {
              # do nothing
            }
            mod <-
              new(
                "Qlearn",
                #formula = ~ 1,
                parameters = parameters,
                fixed = fixed,
                x = matrix(1),
                y = matrix(1),
                reward = reward,
                state = state,
                act = act,
                act_mask = act_mask,
                nS = nS,
                nA = nA,
                npar = npar,
                ntimes = ntimes,
                constr = constr
              )
            mod
          })

setMethod("setpars", "Qlearn",
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
                     object@parameters$beta <- values[2]
                   },
                   "fixed" = {
                     object@fixed <- as.logical(values)
                   })
            # attributes(object@parameters) <- nms
            return(object)
          })

setMethod("getpars", "Qlearn",
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

setMethod("fit", "Qlearn",
          function(object, w) {
            if (missing(w))
              w <- NULL
            pars <- object@parameters
            start <-
              c(gtools::logit(pars$alpha),
                gtools::logit(pars$beta, max = 10))
            fit <-
              optim(start,
                    fn = Qlearn.logLik,
                    object = object,
                    w = w)
            pars$alpha <- gtools::inv.logit(fit$par[1])
            pars$beta <- gtools::inv.logit(fit$par[2])
            object <- setpars(object, unlist(pars))
            object
          })

Qlearn.logLik <- function(par, object, w) {
  obj <-
    setpars(object, c(gtools::inv.logit(par[1]), gtools::inv.logit(par[2], max =
                                                                     10)))
  if (!is.null(w))
    return(-2 * sum(w * dens(obj, log = TRUE)))
  else
    return(-2 * sum(dens(obj, log = TRUE)))
}

setMethod("dens", "Qlearn",
          function(object, log = FALSE) {
            pred <- depmixS4::predict(object)
            p <- pred[cbind(1:nrow(pred), object@act)]
            if (log)
              p <- log(p)
            p[is.na(p)] <- 0
            p
          })

setMethod("logLik", "Qlearn",
          function(object) {
            sum(dens(object, log = TRUE))
          })

setMethod("predict", "Qlearn",
          function(object) {
            lt <- length(object@ntimes)
            et <- cumsum(object@ntimes)
            bt <- c(1, et[-lt] + 1)
            
            nT <- et[lt]
            pred <- matrix(0.0, nrow=nT, ncol=object@nA)
            
            for (id in 1:lt) {
              Q <- array(0.0, dim = c(object@nA, object@nS))
              for (t in bt[id]:et[id]) {
                # prob of action
                if (is.na(object@state[t])) {
                  pred[t, as.logical(object@act_mask[t, ])] <- 1 / sum(object@act_mask[t, ])
                } else {
                  pred[t, as.logical(object@act_mask[t, ])] <-
                    exp(Q[as.logical(object@act_mask[t, ]), object@state[t]] / object@parameters$beta) /
                    sum(exp(Q[as.logical(object@act_mask[t, ]), object@state[t]] / object@parameters$beta))
                }
                # update Q
                if (!is.na(object@state[t])) {
                  Q[object@act[t], object@state[t]] <-
                    Q[object@act[t], object@state[t]] + object@parameters$alpha * (object@reward[t] - Q[object@act[t], object@state[t]])
                }
              }
            }
            return(pred)
          })
