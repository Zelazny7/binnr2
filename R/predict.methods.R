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
  function(object, x, type, seg, ...) {
    callGeneric(object=object, x=object@x, type, ...)
  })

setMethod(".predict", signature = c("Bin", "ValidBinType"),
  function(object, x, type="woe", ...) {
    binned <- collapse(object, x)
    switch(type,
      "bins"  = binned,
      "woe"   = object@pred[binned],
      "rcs"   = "Not implemented",
      "dist"  = "Not implemented")
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

    if (type == "bins") data.frame(out) else {
      out <- do.call(cbind, out)
      rownames(out) <- NULL
      out
    }
  })

setMethod(".predict", signature = c("Scorecard", "missing"),
  function(object, x, type="score", seg, ...) {
    x <- as.data.frame(object@classing)
    callGeneric(object=object, x=x, type=type, ...)
  })

setMethod(".predict", signature = c("Scorecard", "data.frame"),
  function(object, x, type, ...) {
    if (type == "score") {
      woe <- .predict(object@classing[names(object@coef[-1])], x=x,
                      type="woe", ...)

      score <- woe %*% object@coef[-1] + object@coef[1]
      attr(score, "dimnames")[[1]] <- NULL
      return(score)
    }
    callGeneric(object@classing, x=x, type=type, ...)
  })

setMethod(".predict", signature = c("Segmented-Classing", "missing"),
  function(object, x, type, seg, ...) {
    lapply(object@classings, predict, type=type)
  })

setMethod(".predict", signature = c("Segmented-Classing", x="data.frame", seg="factor"),
  function(object, x, type, seg, ...) {
    ## check levels exist
    stopifnot(all(levels(seg) %in% levels(object@segmentor)))
    lv <- levels(seg)
    xs <- split(x, seg, drop=TRUE)
    mapply(predict, object@classings[lv], xs[lv],
           MoreArgs = c(type=type, list(...)), SIMPLIFY = F)
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
      # if (missing(seg)) seg <- object@segmentor

      ## create matrix for indexing scores
      idx <- cbind(seq_along(seg), match(seg, names(out)))
      do.call(cbind, out)[idx]
    } else {
      callGeneric(as(object, "Segmented-Classing"), x=x, type=type, seg=seg, ...)
    }
  })

