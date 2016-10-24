#' @include allGenerics.R
#' @include Scorecard.class.R

add_guides_to_rownames = function(rn) {
  rn <- ifelse(nchar(rn) > 40, paste0(strtrim(rn, 37), "..."), rn)
  paste(sprintf("%02d", seq_along(rn)), rn, sep = ". ")
}

setMethod("as.data.frame", signature = c("Bin", "missing", "missing"),
  function(x, row.names = NULL, optional = FALSE, ...) {

    binned <- collapse(x)
    f <- !is.na(x@x)

    ## pass the weighted sums once
    Y1 = sum((x@y == 1) * x@w)
    Y0 = sum((x@y == 0) * x@w)

    out <- t(mapply(.bv, split(x@y, binned), split(x@w, binned),
      MoreArgs = list(Y1=Y1, Y0=Y0, W=Y1+Y0, f=f), SIMPLIFY = TRUE))

    out[is.infinite(out) | is.nan(out)] <- 0
    row.names(out) <- add_guides_to_rownames(row.names(out))

    ## calculate totals
    Total <- colSums(out, na.rm=T)
    Total[c(5:8,10)] <- 0
    Total[7] <- Total[2]/Total[1]

    out <- data.frame(rbind(out, Total=Total), check.names = FALSE)

    x@cache <- out
    x@summary <- get.bin.summary(out, x)
    x
})

setMethod("as.data.frame", signature = c("Classing", "missing", "missing"),
 function(x, row.names = NULL, optional = FALSE, ...) {
   data <- lapply(x@classing, function(bin) bin@x)
   data.frame(data, check.names = FALSE)
 })

setMethod("as.data.frame", signature = c("Scorecard", "missing", "missing"),
  function(x, row.names = NULL, optional = FALSE, ...) {
    data.frame(
      row.names=names(x@coef[-1]),
      Contribution=x@contribution,
      Coefficients=x@coef[-1],
      check.names = FALSE)
  })


setMethod("collapse", signature = c("Bin", "missing"),
  function(object, x, ...) callGeneric(object, object@x))

fmt_numeric_cuts <- function(cuts) {
  l = format(cuts, trim=TRUE, nsmall=2, digits=2, big.mark=",",
    scientific = FALSE)

  fmt = sprintf("(%%%1$ds - %%%1$ds]", max(nchar(l))) ## get width of largest value

  sprintf(fmt, head(l,-1), tail(l, -1))
}

setMethod("collapse", signature = c("continuous", "numeric"),
  function(object, x, ...) {
    f = !is.na(x) & !(x %in% object@exceptions)

    lbls = fmt_numeric_cuts(object@cuts)

    bins = cut(x[f], object@cuts, include.lowest = T, labels = lbls)

    out  = factor(x, exclude=NULL, levels=c(levels(bins), object@exceptions, NA))

    levels(out)[is.na(levels(out))] <- "Missing"
    out[f] <- bins
    out
  })


setMethod("collapse", signature = c("Discrete", "factor"),
  function(object, x, ...) {
    # levels(x)[levels(x) == ""] <- "Missing"
    out <- x
    levels(out) <- unlist(object@map)[levels(out)]
    out <- addNA(out)
    levels(out)[is.na(levels(out))] <- "Missing"
    out[is.na(out)] <- "Missing"
    out
  })


## set the pred slot in Bin using the WoE if values aren't passed
setMethod("Update", signature = c("Bin"),
  function(object) {
    x <- as.data.frame(object)
    pred <- head(x@cache$WoE, -1)
    names(pred) <- levels(collapse(object, object@x[1]))
    pred["Missing"] <- 0
    x@cache$Pred[1:length(pred)] <- pred
    initialize(x, pred=pred)
  })


#' @export
setMethod("[", c(x = "Classing", i = "ANY", j = "missing", drop = "ANY"),
  function(x, i, j, ..., drop = TRUE) {
    # browser()
    initialize(x, classing=x@classing[i])
  })

#' @export
setMethod("[", c(x = "Scorecard", i = "ANY", j = "missing", drop = "ANY"),
  function(x, i, j, ..., drop = TRUE) {
    x@classing[i]
  })

#' @export
setMethod("[[", c(x = "Scorecard", i = "ANY", j = "missing"),
  function(x, i, j, ..., drop = TRUE) {
    x@classing[[i]]
  })

#' @export
setMethod("[<-", signature = c(x="Scorecard", i="ANY", j="missing", value="Classing"),
  function(x, i, j, ..., value) {
    x@classing[i] <- value
    x
  })

#' @export
setMethod("[[<-", signature = c(x="Scorecard", i="ANY", j="missing", value="Bin"),
  function(x, i, j, ..., value) {
    x@classing[[i]] <- value
    x
  })

#' @export
setMethod("[<-", signature = c(x="Classing", i="ANY", j="missing", value="Classing"),
  function(x, i, j, ..., value) {
    stopifnot(length(x@classing[i]) == length(value))
    x@classing[i] <- value@classing
    names(x@classing[i]) <- names(value@classing)
    x
  })

#' @export
setMethod("[[", signature = c(x="Classing", i="ANY", j="missing"),
  function(x, i, j, ...) {
    x@classing[[i]]
  })

#' @export
setMethod("[[", signature = c(x="Segmented-Classing", i="ANY", j="missing"),
  function(x, i, j, ...) {
    x@classings[[i]]
  })

#' @export
setMethod("[", signature = c(x="Segmented-Classing", i="ANY", j="missing"),
  function(x, i, j, ...) {
    initialize(x, classings=x@classings[i])
  })

#' @export
setMethod("[[", signature = c(x="Segmented-Scorecard", i="ANY", j="missing"),
  function(x, i, j, ...) {
    x@scorecards[[i]]
  })

#' @export
setMethod("[", signature = c(x="Segmented-Scorecard", i="ANY", j="missing"),
  function(x, i, j, ...) {
    initialize(x, scorecards=x@scorecards[i])
  })

#' @export
setMethod("[[<-", signature = c(x="Classing", i="ANY", j="missing", value="Bin"),
  function(x, i, j, ..., value) {
    x@classing[[i]] <- value
    x
  })

#' @export
setMethod("[[<-", signature = c(x="Segmented-Classing", i="ANY", j="missing", value="Classing"),
  function(x, i, j, ..., value) {
    x@classings[[i]] <- value
    x
  })

#' @export
setMethod("[[<-", signature = c(x="Segmented-Scorecard", i="ANY", j="missing", value="Scorecard"),
  function(x, i, j, ..., value) {
    x@scorecards[[i]] <- value
    x
  })

#' @export
setMethod("[<-", signature = c(x="Segmented-Scorecard", i="ANY", j="missing", value="Scorecard"),
  function(x, i, j, ..., value) {
    x@scorecards[i] <- value
    x
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

    new("Classing", classing = unlist(items), y=x@y, w=x@w)
  })


#' @export
setMethod("length", "Classing", function(x) length(x@classing))
