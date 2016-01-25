setClassUnion("ValidBinType", c("numeric", "factor"))

## virtual class contained by all bins
setClass("Bin", slots = list(
  name   = "character",
  x      = "ValidBinType",
  y      = "numeric",
  woe    = "numeric",
  rcs    = "character",
  .cache = "environment"
), contains = "VIRTUAL")

# bin options class used by Continuous bins
setClass("Bin.opts", slots = list(
  min.iv  = "numeric",
  min.cnt = "numeric",
  min.res = "numeric",
  max.bin = "numeric",
  mono    = "numeric",
  exceptions = "numeric"
), contains  = "VIRTUAL")

# Continuous bins use cutpoints and have options controlling the discretization
#' @export
setClass("Continuous",
  slots= list(
    cuts   = "numeric"),
  contains = c("Bin", "Bin.opts"))

# discrete bins map collapsed levels to raw levls
setClass("Discrete",
  slots=list(
    map    = "list"),
  contains = "Bin")

setClass("Classing",
  slots = list(
    classing = "list",
    y        = "numeric"),
  validity = function(object) {
    if (all(sapply(object@classing, is, "Bin"))) TRUE
    else "All members of a Classing object must be Bin objects"
  })

setMethod("as.data.frame", signature = c("Bin"), definition = function(x, row.names = NULL, optional = FALSE, ...) {

  # check if cache has been updated
  if (!is.null(x@.cache$updated)) {
    if (!x@.cache$updated) {
      return(x@.cache$df)
    }
  }

  binned <- collapse(x)
  f <- !is.na(x@x)

  # counts and percents
  out <- tapply(x@y, binned, function(y) {
    N1 <- sum(y == 1)
    N0 <- sum(y == 0)
    P1 <- N1/sum(x@y[f] == 1)
    P0 <- N0/sum(x@y[f] == 0)
    WoE <- log(P1 / P0)
    IV = (P1 - P0) * WoE
    c(N=length(y), N1, N0, PN=length(y)/length(x@y), P1, P0, N1/length(y), WoE, IV)
  }, simplify=FALSE)

  out <- do.call(rbind, out)
  out[is.infinite(out)] <- 0
  row.names(out) <- paste(sprintf("%02d", 1:nrow(out)), row.names(out), sep = ". ")

  ## calculate totals
  Total <- apply(out, 2, sum, na.rm=T)
  Total[c(5:8)] <- 0
  Total[7] <- Total[2]/Total[1]

  out <- data.frame(rbind(out, Total=Total))
  colnames(out) <- c("N", "#1", "#0", "%N","%1","%0","P(1)","WoE","IV")

  ## store in cache
  x@.cache$df <- out
  x@.cache$updated <- FALSE
  out
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

setGeneric("collapse",
  function(object, x, ...) {
    if (missing(x)) x <- object@x
    callGeneric(object, x=x, ...)
  })

setMethod("collapse", signature = c("Continuous", "numeric"),
  function(object, x, ...) {
    f <- !is.na(x) & !(x %in% object@exceptions)
    bins <- cut(x[f], object@cuts, include.lowest = T, dig.lab=3)
    out <- factor(x, exclude=NULL,
                  levels=c(levels(bins),object@exceptions, NA))
    levels(out)[is.na(levels(out))] <- "Missing"
    out[f] <- bins
    object@.cache$levels <- levels(out)
    out
  })

setMethod("collapse", signature = c("Discrete", "factor"),
  function(object, x, ...) {
    levels(x) <- c(unlist(object@map), NA)
    levels(x)[is.na(levels(x))] <- "Missing"
    object@.cache$levels <- levels(x)
    x
  })

# #' @export
Bin <- function(x, y, min.iv=0.01, min.cnt=10, min.res=0, max.bin=10, mono=0,
                exceptions=numeric(0), name = "NONE", .cache = new.env(), ...) {
}

setGeneric("Bin", valueClass = c("Bin", "Classing"))

setMethod("Bin", signature = "numeric",
  function( x, y, min.iv=0.01, min.cnt=10, min.res=0, max.bin=10, mono=0,
            exceptions=numeric(0), name = "NONE", .cache = new.env(), ...) {

    ## discretize numeric vars and return the cut points
    # browser()
    f <- !is.na(x)
    cuts <- .Call('bin', as.double(x[f]), as.double(y[f]),
                  as.double(min.iv), as.integer(min.cnt),
                  as.integer(min.res), as.integer(max.bin),
                  as.integer(mono), as.double(exceptions))

    out <- new("Continuous", x=x, y=y, cuts=cuts, min.iv=min.iv,
               min.res=min.res, max.bin=max.bin, mono=mono,
               exceptions=exceptions, name = name, .cache=new.env())
    out
  })

setMethod("Bin", signature = "factor",
  function(x, y, name = "NONE", .cache = new.env(), ...) {
    ## create a mapping of raw values to collapsed values, store in "map"
    map <- as.list(levels(x))
    names(map) <- levels(x)
    out <- new("Discrete", x=x, y=y, map=map, name = name, .cache=new.env())
    out
  })

setClassUnion("cantBin", c("character","logical"))

setMethod("Bin", signature = "cantBin", function(x, y, ...) {NULL})

setMethod("Bin", signature = c("Continuous", "missing"),
  function(x, y, ...) {
    if (missing(min.iv))  min.iv  <- x@min.iv
    if (missing(min.cnt)) min.cnt <- x@min.cnt
    if (missing(min.res)) min.res <- x@min.res
    if (missing(max.bin)) max.bin <- x@max.bin
    if (missing(mono))    mono    <- x@mono
    if (missing(exceptions)) exceptions <- x@exceptions

    callGeneric(x=x@x, y=x@y, cuts=x@cuts, min.iv=min.iv,
                min.res=min.res, max.bin=max.bin, mono=mono,
                exceptions=exceptions, name=x@name)
  })

setMethod("Bin", signature = "data.frame",
  function(x, y, min.iv=0.01, min.cnt=10, min.res=0, max.bin=10, mono=0,
           exceptions=numeric(0), name = "NONE", ...) {

    cols <- colnames(x)
    classing <- list()

    dashes <- c('\\','|','/','-')

    for (i in seq_along(cols)) {
      vname <- cols[i]
      cat(sprintf("\rProgress: %s %6.2f%%", dashes[(i %% 4) + 1], (100*i/ncol(x))))
      classing[[vname]] <- callGeneric(
        x[,vname], y=y, min.iv=min.iv, min.cnt=min.cnt, min.res=min.res,
        max.bin=max.bin, mono=mono, exceptions=exceptions, name=vname)
    }

    new("Classing", classing=classing, y=y)
  })



#' @export
mono <- function(Object, val) {
  val <- if(val %in% c(-1,0,1,2)) val else 0
  Bin(x=Object, mono=val)
}

#' @export
reset <- function(Object) {
  Bin(x=Object@x, y=Object@y)
}

#' @export
exception <- function(Object, val) {
  stopifnot(is(Object, "Continuous"))
  stopifnot(is.numeric(val))
  Bin(x=Object, exceptions=val)
}

.update <- function(Bin, ...) {

}


setMethod("predict", signature = c("Bin"),
  function(object, ..., type=c("bins","woe","rcs","dist")) {
    type <- match.arg(type)

    binned <- collapse(object, ...)
    df  <- as.data.frame(object) ## checks the .cache automatically
    woe <-df$WoE
    names(woe) <- levels(binned)

    switch(
      type,
      "bins"  = binned,
      "woe"   = woe[binned],
      "rcs"   = "Not implemented",
      "dist"  = "Not implemented")
})





setMethod("[", signature = c("Classing"),
  function(x, i, j, ..., drop = TRUE) {
    new("Classing", classing=x@classing[i], y=x@y)
  })

setMethod("[[", signature = c("Classing"),
  function(x, i, j, ..., drop = TRUE) {
    x@classing[[i]]
  })

setClassUnion("Combinable", c("Bin", "Classing"))

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
