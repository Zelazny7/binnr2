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
setMethod("fit", signature = c("canFit", "missing", "missing"),
  function(object, data, y, ...) {
    classing <- classing(object)
    data <- as.data.frame(classing)
    callGeneric(object=classing, data=data, y=classing@y, ...)
  })

#' @export
setMethod("fit", signature = c("Classing", "data.frame", "numeric"),
  function(object, data, y, nfolds=3, lower.limits=0, upper.limits=3,
           family="binomial", alpha=1, ...) {
    stopifnot(NROW(data) == NROW(y))
    woe <- data.matrix(predict(object, x=data, type="woe"))
    fit <- glmnet::cv.glmnet(woe, y, nfolds=nfolds, lower.limits=lower.limits,
                             upper.limits=upper.limits, family=family,
                             alpha=alpha, ...)

    coefs <- coef(fit, s="lambda.min")[,1]
    coefs <- coefs[coefs != 0]
    contributions <- .contributions(woe[,names(coefs)[-1]], coefs, y)

    new("Scorecard", fit=fit, classing=object, y=y, coef=coefs,
        contribution=contributions)
  })

#' @export
setMethod("fit", signature = c("Scorecard", "data.frame", "numeric"),
  function(object, data, y, ...) {
    stopifnot(NROW(data) == NROW(y))
    classing <- object@classing
    callGeneric(object=classing, data=data, y=y, ...)
  })

#' @export
setMethod("fit", signature = c("canFit", "ANY", "ANY"),
  function(object, data, y, ...) {
    stop("Must provide data and target to fit")
  })

#' @export
setMethod("fit", signature = c("Segmented-Classing", "missing", "missing"),
  function(object, data, y, ...) {
    mods <- lapply(object@classings, fit)
    new("Segmented-Scorecard", object, scorecards=mods)
  })
