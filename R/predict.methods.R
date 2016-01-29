#' @include allGenerics.R
#' @include Scorecard.class.R

#' @export
setMethod("predict", signature = c("Bin"),
  function(object, x, type=c("bins","woe","rcs","dist"), ...) {
    # browser()
    type <- match.arg(type)
    if (missing(x)) x <- object@x
    binned <- collapse(object, x)
    switch(
      type,
      "bins"  = binned,
      "woe"   = object@pred[binned],
      "rcs"   = "Not implemented",
      "dist"  = "Not implemented")
  })

#' @export
setMethod("predict", signature = c("Classing"),
  function(object, x, type="woe", ...) {
    ## if no new data is passed in, get it from the classing object
    if (missing(x)) x <- as.data.frame(object)

    stopifnot(is.data.frame(x))

    ## only predict vars that are common to both data and classing
    vars <- intersect(names(object@classing), colnames(x))
    if (length(vars) == 0)
      stop("No vars in common between classing and x", .call=F)

    out  <- vector("list", length = length(vars))
    names(out) <- vars
    for (i in seq_along(vars)) {
      .progress(i, length(vars), "Predicting")
      out[[vars[i]]] <- predict(object[[vars[i]]], x=x[,vars[i]], type=type)
    }
    cat("", sep="\n")

    if (type == "bins") data.frame(out) else {
      out <- do.call(cbind, out)
      rownames(out) <- NULL
      out
    }

  })

#' @export
setMethod("predict", signature = c("Scorecard"),
  function(object, x, type="score", ...) {
    if (type == "score") {
      woe <- predict(object@classing[names(object@coef[-1])], x=x, type="woe")
      score <- woe %*% object@coef[-1] + object@coef[1]
      attr(score, "dimnames")[[1]] <- NULL
      return(score)
    }
    callGeneric(object@classing, x=x, type=type, ...)
  })


### TODO: Add x to predict Generic function

#' @export
setMethod("predict", signature = c("Segmented-Classing"),
  function(object, x, seg, type="score", ...) {
    lapply(object@classings, predict, x=x, type=type, ...)
  })

#' @export
setMethod("predict", signature = c("Segmented-Scorecard"),
  function(object, x, seg, type="score", ...) {

    if (type == "score") {
      out <- lapply(object@scorecards, predict, x=x, type=type, ...)
      if (missing(seg)) seg <- object@segmentor

      ## create matrix for indexing scores
      idx <- cbind(seq_along(seg), match(seg, names(out)))
      do.call(cbind, out)[idx]
    } else {
      ## should call the segmented-classing predict function
      callNextMethod()
    }
  })

