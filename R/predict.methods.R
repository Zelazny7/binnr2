#' @include allGenerics.R
#' @include Scorecard.class.R
#' @include segmented.class.R

setClassUnion("Predictable", c("Bin", "Classing", "Scorecard", "Segmented"))
setClassUnion("NullOrDF", c("NULL","data.frame"))


setAs("Segmented-Scorecard", "Segmented-Classing",
  function(from) {
    classings <- lapply(from@scorecards, slot, "classing")
    new("Segmented-Classing", classings=classings, segmentor=from@segmentor)
  })

setGeneric("predict")

#' @export
setMethod("predict", "Predictable", function(object, ...) {
  .predict(object, ...)
})

setMethod(".predict", signature = c("Bin", "missing"),
  function(object, x, type="woe", ...) {
    if (missing(coef)) coef <- 1
    if (missing(method)) method <- "min"
    callGeneric(object=object, x=object@x, type=type, coef=coef, method=method, ...)
  })

distance <- function(preds, coef, method="min") {
  ref <- switch(method,
                "max" = max(preds),
                "neutral" = 0,
                "min" = min(preds))
  # return the distances
  (ref - preds) * coef
}

setMethod(".predict", signature = c("Bin", "ValidBinType"),
  function(object, x, type="woe", ...) {
    if (missing(coef)) coef <- 1
    if (missing(method)) method <- "min"
    binned <- as.character(collapse(object, x))
    switch(type,
      "bins"  = binned,
      "woe"   = object@pred[binned],
      "rcs"   = object@rcs[binned],
      "dist"  = distance(object@pred[binned], coef, method))
  })

setMethod(".predict", signature = c(object="Classing", x="missing"),
  function(object, x, type="woe", ...) {
    callGeneric(object=object, x=as.data.frame(object), type=type, ...)
  })

setMethod(".predict", signature = c(object="Classing", x="NullOrDF"),
  function(object, x, type="woe", ...) {
    if (is.null(x)) return(x)

    ## check that all variables are found int he data.frame
    vars <- names(object@classing)
    missing.vars <- setdiff(vars, colnames(x))
    if (length(missing.vars) > 0)
      stop(sprintf("Classing vars missing from x: %s",
                   paste(missing.vars, collapse=', ')), .call=F)

    out  <- vector("list", length = length(vars))
    names(out) <- vars
    for (i in seq_along(vars)) {
      .progress(i, length(vars), "Predicting")
      out[[vars[i]]] <- .predict(object[[vars[i]]], x=x[,vars[i]], type=type)
    }
    cat("", sep="\n")

    data.frame(out, row.names=NULL)
  })

setMethod(".predict", signature = c("Scorecard", "missing"),
  function(object, x, type="score", seg, method="min", ...) {
    x <- as.data.frame(object@classing)
    callGeneric(object=object, x=x, type=type, method=method, ...)
  })

setMethod(".predict", signature = c("Scorecard", "data.frame"),
  function(object, x, type, method="min", ...) {
    if (type == "kfold") {
      # browser()
      ## get the best lambda
      i <- which.min(object@fit$cvm)
      p <- object@fit$fit.preval[,i]
      return(log(p/(1 - p)))
    }
    else if (type == "score") {
      woe <- data.matrix(.predict(object@classing[names(object@coef[-1])], x=x,
                         type="woe", ...))

      score <- woe %*% object@coef[-1] + object@coef[1]
      attr(score, "dimnames")[[1]] <- NULL
      return(score)
    }
    callGeneric(object@classing, x=x, type=type, method=method, ...)
  })


setMethod(".predict", signature = c("Segmented-Classing", x="missing"),
  function(object, x, type, seg, drop=FALSE, method="min", ...) {
    lapply(object@classings, function(classing) {
      k <- if (drop) !dropped(classing) else rep(TRUE, length(classing))
      predict(classing[k], type=type, drop=drop, method=method, ...)
    })
  })


setMethod(".predict", signature = c("Segmented-Classing", x="data.frame", seg="factor"),
  function(object, x, type, seg, drop=FALSE, method="min", ...) {
    ## check levels exist
    stopifnot(all(levels(seg) %in% levels(object@segmentor)))
    lv <- levels(seg)
    xs <- split(x, seg, drop=TRUE)

    mapply(function(classing, x, drop, method) {
      k <- if (drop) !dropped(classing) else rep(TRUE, length(classing))
      predict(classing[k], x, type=type, drop=drop, method=method, ...)
    }, object@classings[lv], xs[lv],
    MoreArgs = list(drop=drop, method=method), SIMPLIFY = FALSE)
  })

setMethod(".predict", signature = c("Segmented-Classing", x="data.frame", seg="missing"),
  function(object, x, type, seg, drop=FALSE, method="min", ...) {
    lapply(object@classings, function(classing) {
      k <- if (drop) !dropped(classing) else rep(TRUE, length(classing))
      predict(classing[k], x, type=type, drop=drop, method=method, ...)
    })
  })

setMethod(".predict", signature = c(object="Segmented", x="ANY", seg="ANY"),
  function(object, x, type="score", seg, ...) {
    stop("Invalid arguments passed for predicting on segmented object")
  })


setMethod(".predict", signature = c(object="Segmented-Scorecard", x="missing", seg="missing"),
  function(object, x, type="score", seg, drop=FALSE, method="min", simplify=TRUE, ...) {
    if (type %in% c("score", "kfold")) {
      out <- lapply(object@scorecards, .predict, type=type, ...)
      if (simplify) unsplit(out, object@segmentor) else out
    } else {
      callGeneric(as(object, "Segmented-Classing"), type=type, drop=drop,
                  method=method, ...)
    }
  })


setMethod(".predict", signature = c(object="Segmented-Scorecard", x="data.frame", seg="missing"),
  function(object, x, type="score", seg, drop=FALSE, method="min", ...) {
    if (type == "score") {
      lapply(object@scorecards, .predict, x=x, type=type, ...)
    } else {
      callGeneric(as(object, "Segmented-Classing"), x=x, type=type, drop=drop,
                  method=method, ...)
    }
  })


setMethod(".predict", signature = c(object="Segmented-Scorecard", x="data.frame", seg="factor"),
  function(object, x, type="score", seg, drop=FALSE, method="min", simplify=TRUE, ...) {
    if (type == "score") {

      ## blank out the levels of the factor that aren't in the segmentor
      lvls <- levels(seg)
      levels(seg)[!lvls %in% levels(object@segmentor)] <- NA

      # stopifnot(all(levels(seg) %in% levels(object@segmentor)))

      lv <- levels(seg)
      xs <- split(x, seg, drop=TRUE)

      out <- mapply(predict, object@scorecards[lv], xs[lv],
                    MoreArgs = c(type=type, list(...)), SIMPLIFY = F)
      if (simplify) unsplit(out, seg) else out
    } else {
      callGeneric(as(object, "Segmented-Classing"), x=x, type=type, seg=seg,
                  drop=drop, method=method, ...)
    }
  })

