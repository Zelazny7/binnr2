#' @include allGenerics.R
#' @include Scorecard.class.R
#' @include segmented.class.R

setMethod("predict", signature = c("Bin", "missing"),
  function(object, x, type=c("bins","woe","rcs","dist"), ...) {
    callGeneric(object=object, x=object@x, type=type)
  })


setMethod("predict", signature = c("Bin", "ValidBinType"),
  function(object, x, type=c("bins","woe","rcs","dist"), ...) {
    type <- match.arg(type)
    binned <- collapse(object, x)
    switch(type,
      "bins"  = binned,
      "woe"   = object@pred[binned],
      "rcs"   = "Not implemented",
      "dist"  = "Not implemented")
  })

setMethod("predict", signature = c(object="Classing", x="missing"),
  function(object, x, type="woe", ...) {
    callGeneric(object=object, x=as.data.frame(object), type=type, ...)
  })

setMethod("predict", signature = c(object="Classing", x="data.frame"),
  function(object, x, type="woe", ...) {
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
      out[[vars[i]]] <- predict(object[[vars[i]]], x=x[,vars[i]], type=type)
    }
    cat("", sep="\n")

    if (type == "bins") data.frame(out) else {
      out <- do.call(cbind, out)
      rownames(out) <- NULL
      out
    }

  })

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


setMethod("predict", signature = c("Segmented-Classing"),
  function(object, x, seg, type="score", ...) {
    mapply(predict, object@classings, split(x, seg),
           MoreArgs = list(type=type), SIMPLIFY = FALSE)
  })

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

