#' @include allGenerics.R

setClassUnion("canFit", c("Classing", "Scorecard"))

#' @export
setMethod("classing", "Classing", function(object) {
  return(object)
})

#' @export
setMethod("classing", "Scorecard", function(object) {
  return(object@classing)
})

## if both are missing then it usese the classing's data and target
#' @export
setMethod("fit", signature = c(object="canFit", x="missing", y="missing"),
  function(object, x, y, ...) {
    classing <- classing(object)
    x <- as.data.frame(classing)
    callGeneric(object=object, x=x, y=classing@y, ...)
  })

#' @export
setMethod("fit", signature = c(object="Classing", x="data.frame", y="numeric"),
  function(object, x, y, fixed=FALSE, nfolds=3, lower.limits=0, upper.limits=3,
           family="binomial", alpha=1, ...) {

    stopifnot(NROW(x) == NROW(y))

    ## get vector of keep vars
    k <- !dropped(object)
    woe <- data.matrix(.predict(object[k], x=x, type="woe"))

    ## set the penalty factor for fixed vars
    im <- inmodel(object)
    pf <- rep(1, length(object))
    if (fixed) pf[im] <- 0

    fit <- glmnet::cv.glmnet(woe, y, nfolds=nfolds, lower.limits=lower.limits,
                             upper.limits=upper.limits, family=family,
                             alpha=alpha, penalty.factor = pf[k], ...)

    coefs <- coef(fit, s="lambda.min")[,1]
    coefs <- coefs[coefs != 0]
    contributions <- .contributions(woe[,names(coefs)[-1]], coefs, y)

    ## flag vars as in the model
    inmodel(object) <- FALSE
    inmodel(object[names(coefs)[-1]]) <- TRUE

    ## flag the new vars
    new(object) <- FALSE
    new(object[which(!im & inmodel(object))]) <- TRUE

    ## calculate performance metrics
    ks <- .ks(woe[,names(coefs)[-1]] %*% coefs[-1] + coefs[1], y)

    new("Scorecard", fit=fit, classing=object, y=y, coef=coefs,
        contribution=contributions, performance=ks)
  })

#' @export
setMethod("fit", signature = c("Scorecard", "data.frame", "numeric"),
  function(object, x, y, fixed = FALSE, ...) {
    stopifnot(NROW(x) == NROW(y))
    classing <- object@classing
    callGeneric(object=classing, x=x, y=y, fixed=fixed, ...)
  })

#' @export
setMethod("fit", signature = c("canFit", "ANY", "ANY"),
  function(object, data, y, ...) {
    stop("Must provide data and target to fit")
  })


## segmented classing, with factor provided
#' @export
setMethod("fit", signature = c("Segmented-Classing", "data.frame", "numeric", "factor"),
  function(object, x, y, seg, ...) {

    ## check that classing levels are in the same order as classings
    ord <- match(levels(seg), names(object@classings))

    # TODO: add error message here
    stopifnot(all(!is.na(ord)))

    xs <- split(x, seg)
    ys <- split(y, seg)

    mods <- mapply(fit, object@classings[ord], xs, ys, ...)

    score <- do.call(c, lapply(mods, predict))
    y     <- do.call(c, lapply(mods, function(z) z@classing@y))
    perf  <- .ks(-score, y)

    new("Segmented-Scorecard", scorecards=mods, segmentor=object@segmentor,
        performance=perf)

  })

#' @export
setMethod("fit", signature = c(object="Segmented-Classing", x="missing",
                               y="missing", seg="missing"),
  function(object, x, y, seg, ...) {
    mods <- lapply(object@classings, fit, ...)

    ## loop over all scorecards and get the prediction and response
    score <- do.call(c, lapply(mods, predict))
    y     <- do.call(c, lapply(mods, function(z) z@classing@y))
    perf  <- .ks(-score, y)

    new("Segmented-Scorecard", scorecards=mods, segmentor=object@segmentor,
        performance=perf)
  })

#' @export
setMethod("fit", signature = "Segmented-Scorecard",
  function(object, x, y, seg, ...) {

    ## create a segmented-classing
    classings <- lapply(object@scorecards, slot, "classing")
    classings <- new("Segmented-Classing", classings=classings,
                     segmentor=object@segmentor)

    callGeneric(classings)
  })




