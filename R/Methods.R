#' @include allGenerics.R
#' @include Scorecard.class.R

setMethod("as.data.frame", signature = c("Bin", "missing", "missing"),
  function(x, row.names = NULL, optional = FALSE, ...) {
    binned <- collapse(x)
    f <- !is.na(x@x)

    ## get the bivariate matrix
    out <- mapply(.bv, split(x@y, binned), split(x@w, binned),
                  MoreArgs = list(Y=x@y, W=x@w, f=f), SIMPLIFY = FALSE)

    out[sapply(out, is.null)] <- 0

    out <- do.call(rbind, out)
    out[is.infinite(out) | is.nan(out)] <- 0
    out <- cbind(out, Pred=x@pred[row.names(out)])

    rn <- row.names(out)
    rn <- ifelse(nchar(rn) > 40, paste0(strtrim(rn, 37), "..."), rn)
    row.names(out) <- paste(sprintf("%02d", 1:nrow(out)), rn, sep = ". ")

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


setMethod("collapse", signature = c("continuous", "numeric"),
  function(object, x, ...) {
    f <- !is.na(x) & !(x %in% object@exceptions)

    l <- format(object@cuts, trim=TRUE, nsmall=2, digits=2, big.mark=",",
                scientific = FALSE)

    ## get width of largest value
    width <- max(nchar(l))

    fmt <- sprintf("(%%%1$ds - %%%1$ds]", width)

    lbls <- sprintf(fmt, head(l,-1), tail(l, -1))

    bins <- cut(x[f], object@cuts, include.lowest = T, labels = lbls)

    out <- factor(x, exclude=NULL, levels=c(levels(bins),object@exceptions, NA))

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
    pred <- head(as.data.frame(object)$WoE, -1)
    names(pred) <- levels(collapse(object, object@x[1]))
    pred["Missing"] <- 0
    initialize(object, pred=pred)
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
setMethod("[<-", signature = c(x="Scorecard", i="ANY", j="missing", value="Classing"),
  function(x, i, j, ..., value) {
    x@classing[i] <- value
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
    # browser()
    #seg <- factor(x@segmentor[x@segmentor %in% levels(x@segmentor)[idx]])
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
    # idx <- match(names(x@scorecards)[i], levels(x@segmentor))
    # seg <- factor(x@segmentor[x@segmentor %in% levels(x@segmentor)[idx]])
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
