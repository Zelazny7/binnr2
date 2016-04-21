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

#' @export
setMethod("predict", "Predictable", function(object, ...) {
  .predict(object, ...)
})

setMethod(".predict", signature = c("Bin", "missing"),
  function(object, x, type="woe", coef=1, method="min", ...) {
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
  function(object, x, type="woe", coef=1, method="min", ...) {
    if (missing(coef)) coef <- 1
    if (missing(method)) method <- "min"
    binned <- collapse(object, x)
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

    data.frame(out)
    # if (type == "bins") data.frame(out) else {
    #   out <- do.call(cbind, out)
    #   rownames(out) <- NULL
    #   out
    # }
  })

setMethod(".predict", signature = c("Scorecard", "missing"),
  function(object, x, type="score", seg, ...) {
    x <- as.data.frame(object@classing)
    callGeneric(object=object, x=x, type=type, ...)
  })

setMethod(".predict", signature = c("Scorecard", "data.frame"),
  function(object, x, type, ...) {
    if (type == "score") {
      woe <- data.matrix(.predict(object@classing[names(object@coef[-1])], x=x,
                         type="woe", ...))

      score <- woe %*% object@coef[-1] + object@coef[1]
      attr(score, "dimnames")[[1]] <- NULL
      return(score)
    }
    callGeneric(object@classing, x=x, type=type, ...)
  })

setMethod(".predict", signature = c("Segmented-Classing", "missing"),
  function(object, x, type, seg, ...) {
    value <- lapply(object@classings, predict, type=type)
    # x <- value[[1L]][rep(NA, length(object@segmentor)), , drop = FALSE]
    x <- do.call(rbind, value)
    split(x, object@segmentor, drop = TRUE) <- value
    rownames(x) <- NULL
    x
  })

setMethod(".predict", signature = c("Segmented-Classing", x="data.frame", seg="factor"),
  function(object, x, type, seg, ...) {
    ## check levels exist
    # browser()
    stopifnot(all(levels(seg) %in% levels(object@segmentor)))
    lv <- levels(seg)
    xs <- split(x, seg, drop=TRUE)
    value <- mapply(predict, object@classings[lv], xs[lv],
                  MoreArgs = c(type=type, list(...)), SIMPLIFY = F)

    # x <- value[[1L]][rep(NA, length(object@segmentor)), , drop = FALSE]
    x <- do.call(rbind, value)
    split(x, object@segmentor, drop = TRUE) <- value
    rownames(x) <- NULL
    x
  })

setMethod(".predict", signature = c(object="Segmented", x="ANY", seg="ANY"),
  function(object, x, type="score", seg, ...) {
    stop("Predicting segmented objects require data and segment be provided OR both missing")
  })

setMethod(".predict", signature = c(object="Segmented-Scorecard", x="missing", seg="missing"),
  function(object, x, type="score", seg, ...) {
    if (type == "score") {
      out <- lapply(object@scorecards, .predict, type=type, ...)
      unsplit(out, object@segmentor)
    } else {
      callGeneric(as(object, "Segmented-Classing"), type=type, ...)
    }
  })

setMethod(".predict", signature = c(object="Segmented-Scorecard", x="data.frame", seg="factor"),
  function(object, x, type="score", seg, ...) {
    if (type == "score") {
      out <- lapply(object@scorecards, .predict, x=x, type=type, ...)

      ## create matrix for indexing scores
      idx <- cbind(seq_along(seg), match(seg, names(out)))
      do.call(cbind, out)[idx]
    } else {
      callGeneric(as(object, "Segmented-Classing"), x=x, type=type, seg=seg, ...)
    }
  })

