#' @include allGenerics.R
#' @include Scorecard.class.R

setMethod("as.data.frame", signature = c("Bin", "missing", "missing"),
  function(x, row.names = NULL, optional = FALSE, ...) {
    binned <- collapse(x)
    f <- !is.na(x@x)

    ## get the bivariate matrix
    out <- tapply(x@y, binned, .bv, Y=x@y, f=f, simplify = TRUE)
    out[sapply(out, is.null)] <- 0

    out <- do.call(rbind, out)
    out[is.infinite(out)] <- 0
    out <- cbind(out, Pred=x@pred[row.names(out)])
    row.names(out) <- paste(sprintf("%02d", 1:nrow(out)), row.names(out), sep = ". ")

    ## calculate totals
    Total <- apply(out, 2, sum, na.rm=T)
    Total[c(5:8,10)] <- 0
    Total[7] <- Total[2]/Total[1]

    out <- data.frame(rbind(out, Total=Total))
    colnames(out) <- c("N", "#1", "#0", "%N","%1","%0","P(1)","WoE","IV", "Pred")
    out
})

setMethod("as.data.frame", signature = c("Classing", "missing", "missing"),
 function(x, row.names = NULL, optional = FALSE, ...) {
   data <- lapply(x@classing, function(bin) bin@x)
   data.frame(data)
 })

setMethod("show", signature = "Bin",
  function(object) {
    df <- as.data.frame(object)
    iv <- df['Total', "IV"]
    cat(sprintf("Variable Report: %s IV: %0.3f ", object@name, iv), sep = '\n')
    print(df, digits=5)
  })

setMethod("show", signature = "Classing",
  function(object) {
    lvls <- c("Discrete", "Continuous")
    cnts <- table(factor(sapply(object@classing, class), levels=lvls))
    cat("Classing object\n")
    cat(sprintf("  |-- %3d Bins Total\n", sum(cnts)))
    cat(sprintf("  |-- %3d Discrete\n"  , cnts[1]))
    cat(sprintf("  |-- %3d Continuous\n", cnts[2]))
  })

setMethod("show", signature = "Scorecard",
  function(object) {
    cat(sprintf("Scorecard model with %d predictors", length(object@coef) - 1))
    show(object@classing)
  })

setMethod("collapse", signature = c("Continuous", "numeric"),
  function(object, x, ...) {
    f <- !is.na(x) & !(x %in% object@exceptions)
    bins <- cut(x[f], object@cuts, include.lowest = T, dig.lab=3)
    out <- factor(x, exclude=NULL,
                  levels=c(levels(bins),object@exceptions, NA))
    levels(out)[is.na(levels(out))] <- "Missing"
    out[f] <- bins
    out
  })

setMethod("collapse", signature = c("Discrete", "factor"),
  function(object, x, ...) {
    levels(x) <- c(unlist(object@map), NA)
    levels(x)[is.na(levels(x))] <- "Missing"
    x
  })

## set the pred slot in Bin using the WoE if values aren't passed
setMethod("Update", signature = c("Bin", "missing"),
  function(object, keep = TRUE) {

    old <- object@pred

    pred <- head(as.data.frame(object)$WoE, -1)
    names(pred) <- levels(collapse(object, if(is(object, "Discrete"))
      factor() else numeric()))
    object@pred <- pred

    object@pred["Missing"] <- 0
    if (keep) object@pred[names(old)] <- old
    # object@pred["Missing"] <- 0
    object
  })

#' @export
setMethod("predict", signature = c("Bin"),
  function(object, x, type=c("bins","woe","rcs","dist"), ...) {
    type <- match.arg(type)

    binned <- collapse(object, x=x)

    switch(
      type,
      "bins"  = binned,
      "woe"   = object@pred[binned],
      "rcs"   = "Not implemented",
      "dist"  = "Not implemented")
})

#' extract parts of a Classing
#'
#' @name [
#' @aliases [,Classing-method
#' @docType methods
#' @rdname extract-methods
#' @export
setMethod("[", c(x = "Classing", i = "ANY", j = "missing", drop = "ANY"),
  function(x, i, j, ..., drop = TRUE) {
    initialize(x, classing=x@classing[i])
  })

#' extract element of a Classing
#'
#' @name [[
#' @aliases [[,Classing-method
#' @docType methods
#' @rdname extract-methods
#' @export
setMethod("[[", signature = c(x="Classing", i="ANY", j="missing"),
  function(x, i, j, ...) {
    x@classing[[i]]
  })

setClassUnion("Combinable", c("Bin", "Classing"))

#' @export
setMethod("c", signature = c("Combinable"),
  function(x, ..., recursive = FALSE) {
    ## loop over items and process
    items <- lapply(list(x, ...), function(item) {
      if (is(item, "Bin")) {
        return(item)
      } else if(is(item, "Classing")) {
        return(item@classing)
      } else {
        stop("All items must be Bin or Classing objects")
      }
    })

    new("Classing", classing = unlist(items))
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
      progress <- paste(rep("=", (10*i/length(vars))), collapse="")
      cat(sprintf("\rPredicting : %-10s|", progress))
      out[[vars[i]]] <- predict(object[[vars[i]]], x=x[,vars[i]], type=type)
    }
    cat("", sep="\n")


    if (type == "bins") data.frame(out) else do.call(cbind, out)

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
