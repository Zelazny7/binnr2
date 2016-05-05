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
    callGeneric(object=object, x=x, y=classing@y, w=classing@w, ...)
  })

#' @export
setMethod("fit", signature = c(object="Classing", x="data.frame", y="numeric"),
  function(object, x, y, w, fixed=FALSE, nfolds=3, lower.limits=0, upper.limits=3,
           family="binomial", alpha=1, ...) {

    stopifnot(NROW(x) == NROW(y))

    if (missing(w)) w <- rep(1, NROW(x))

    ## get vector of keep vars
    k <- !dropped(object)
    woe <- data.matrix(.predict(object[k], x=x, type="woe"))

    ## set the penalty factor for fixed vars
    im <- inmodel(object)
    pf <- rep(1, length(object))
    if (fixed) pf[im] <- 0

    fit <- glmnet::cv.glmnet(woe, y, weights=w, nfolds=nfolds,
                             lower.limits=lower.limits,
                             upper.limits=upper.limits, family=family,
                             alpha=alpha, penalty.factor = pf[k], ...)

    coefs <- coef(fit, s="lambda.min")[,1]
    coefs <- coefs[coefs != 0]
    contributions <- .contributions(woe[,names(coefs)[-1]], coefs, y, w)

    ## flag vars as in the model
    inmodel(object)[] <- FALSE
    inmodel(object)[names(coefs)[-1]] <- TRUE

    ## flag the new vars
    new.vars(object)[] <- FALSE
    new.vars(object)[which(!im & inmodel(object))] <- TRUE

    # browser()
    ## step two predictors
    ## Find at least the next 10 variables that would have come in
    steptwo(object)[] <- FALSE
    d <- fit$glmnet.fit$df
    idx <- which(d[which.min(fit$cvm)] + 10 < d)[1]
    if (is.na(idx)) idx <- length(d)
    lambda <- fit$lambda[min(idx, length(fit$lambda))]
    step2 <- coef(fit, s=lambda)[,1]
    step2 <- names(step2[step2 != 0])[-1]
    steptwo(object)[setdiff(step2, names(coefs)[-1])] <- TRUE

    ## Reorder the bins ##
    vbest  <- names(sort(contributions, decreasing = T))
    vstep2 <- setdiff(step2, names(coefs)[-1])
    vrest  <- setdiff(names(object), c(vbest, vstep2))

    ## drop non step 1 and step 2
    dropped(object)[vrest] <- TRUE

    ord <- c(vbest, vstep2, vrest)

    ## calculate performance metrics
    ks <- .ks(woe[,names(coefs)[-1]] %*% coefs[-1] + coefs[1], y, w)

    new("Scorecard", fit=fit, classing=object[ord], y=y, coef=coefs,
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
setMethod("fit", signature = c(object="Segmented-Classing", x="data.frame", y="numeric", seg="factor"),
  function(object, x, y, w, seg, ...) {

    if (missing(w)) w <- rep(1, NROW(x))

    ## check that classing levels are in the same order as classings
    ord <- match(levels(seg), names(object@classings))

    # TODO: add error message here
    stopifnot(all(!is.na(ord)))

    xs <- split(x, seg)
    ys <- split(y, seg)
    ws <- split(w, seg)

    mods <- mapply(fit, object@classings[ord], xs, ys, ws, ...)

    score <- do.call(c, lapply(mods, predict, type="score"))
    y     <- do.call(c, ys)
    w     <- do.call(c, ws)

    perf  <- .ks(-score, y, w)

    new("Segmented-Scorecard", scorecards=mods, segmentor=object@segmentor,
        performance=perf)

  })

#' @export
setMethod("fit", signature = c(object="Segmented-Classing", x="missing", y="missing", w="missing", seg="missing"),
  function(object, x, y, w, seg, ...) {

    mods <- lapply(object@classings, fit, ...)

    ## loop over all scorecards and get the prediction and response
    score <- do.call(c, lapply(mods, predict, type="score"))
    y     <- do.call(c, lapply(mods, function(z) z@classing@y))
    w     <- do.call(c, lapply(mods, function(z) z@classing@w))

    perf  <- .ks(-score, y, w)

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




