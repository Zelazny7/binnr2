## transform class has data, and exceptions
# setClassUnion("transformData", c("factor", "numeric"))

setClass("Bin.opts", slots = list(
  min.iv="numeric",
  min.cnt="numeric",
  min.res="numeric",
  max.bin="numeric",
  mono="numeric",
  exceptions="numeric"
), contains = "VIRTUAL")

setClass("Bin", slots = list(
  name = "character",
  y    = "numeric",
  woe  = "numeric"),
  contains = "VIRTUAL")

setClass("Continuous",
  slots= list(
    x      = "numeric",
    cuts   = "numeric",
    filter = "logical"),
  contains = c("Bin", "Bin.opts"))

setClass("Discrete",
  slots=list(
    x      = "factor",
    map    = "list",
    filter = "logical"),
  contains = "Bin")

setClass("Classing",
  slots=list(classing="list"),
  validity = function(object) {
    if (all(sapply(object@classing, is, "Bin"))) TRUE
    else "All members of a Classing object must be Bins"
  })

setClassUnion("BinType", c("Continuous", "Discrete"))

setMethod("as.data.frame", signature = c("Bin"), definition = function(x, row.names = NULL, optional = FALSE, ...) {
  binned <- collapse(x)

  f <- x@filter

  # counts and percents
  out <- tapply(x@y, binned, function(y) {
    N1 <- sum(y == 1)
    N0 <- sum(y == 0)
    P1 <- N1/sum(x@y[f] == 0)
    P0 <- N0/sum(x@y[f] == 1)
    WoE <- log(P1 / P0)
    IV = (P1 - P0) * WoE
    c(N=length(y), N1, N0, PN=N1/length(y), P1, P0, WoE, IV)
  }, simplify=FALSE)

  out <- do.call(rbind, out)
  out[is.infinite(out)] <- 0
  row.names(out) <- paste(sprintf("%02d", 1:nrow(out)),
                          sprintf("%20s  ||  ", row.names(out)), sep = ". ")

  ## calculate totals
  Total <- apply(out, 2, sum, na.rm=T)
  Total[4:6] <- NA

  out <- rbind(out, Total=Total)
  colnames(out) <- c("N", "#1", "#0", "%N","%1","%0","WoE","IV")
  out
})

setMethod("show", signature = "Bin", definition = function(object) {
  df <- as.data.frame(object)
  iv <- df['Total', iv]
  cat(sprintf("Variable Report: %s", object@name), sep = '\n')
  print(df, digits=4)

})

setMethod("show", signature = "Classing", definition = function(object) {
  lvls <- c("Discrete", "Continuous")
  cnts <- table(factor(sapply(object@classing, class), levels=lvls))
  cat("Classing object\n")
  cat(sprintf("|-- %3d Bins Total\n", sum(cnts)))
  cat(sprintf(" |-- %3d Discrete\n"  , cnts[1]))
  cat(sprintf(" |-- %3d Continuous\n", cnts[2]))
})

setGeneric("collapse", function(object) {})

setMethod("collapse", signature = "BinType", function(object) {
  print("what's up!?")
})

setMethod("collapse", signature = "Continuous", function(object) {
  f <- object@filter
  bins <- cut(object@x[f], object@cuts, include.lowest = T, dig.lab=3)
  x <- factor(object@x, exclude=NULL, levels=c(levels(bins), object@exceptions, NA))
  levels(x)[is.na(levels(x))] <- "Missing"
  x[f] <- bins
  x
})

setMethod("collapse", signature = "Discrete", function(object) {
  x <- object@x
  levels(x) <- c(unlist(object@map), NA)
  x
})

#' @export
Bin <- function(x, y, min.iv=0.01, min.cnt=10, min.res=0, max.bin=10, mono=0, exceptions=numeric(0), ...) {}

setGeneric("Bin", valueClass = c("Bin", "Classing"))

setMethod("Bin", signature = "numeric", definition = function(x, y, min.iv=0.01, min.cnt=10, min.res=0, max.bin=10, mono=0, exceptions=numeric(0), ...) {
  ## discretize numeric vars and return the cut points
  # browser()
  f <- !is.na(x)
  cuts <- .Call('bin', as.double(x[f]), as.double(y[f]),
                as.double(min.iv), as.integer(min.cnt),
                as.integer(min.res), as.integer(max.bin),
                as.integer(mono), as.double(exceptions))

  f <- !is.na(x) & !(x %in% exceptions)
  new("Continuous", x=x, y=y, cuts=cuts, filter=f, min.iv=min.iv, min.res=min.res,
      max.bin=max.bin, mono=mono, exceptions=exceptions, ...)
})

setMethod("Bin", signature = "factor", definition = function(x, y, ...) {
  ## create a mapping of raw values to collapsed values, store in "map"
  map <- as.list(levels(x))
  names(map) <- levels(x)
  new("Discrete", x=x, y=y, filter=!is.na(x), map=map, ...)
})

setClassUnion("cantBin", c("character","logical"))

setMethod("Bin", signature = "cantBin", definition = function(x, y, ...) {
  NULL
})

setMethod("Bin", signature = c("Continuous", "missing"), definition = function(x, y, ...) {
  if (missing(min.iv))  min.iv  <- x@min.iv
  if (missing(min.cnt)) min.cnt <- x@min.cnt
  if (missing(min.res)) min.res <- x@min.res
  if (missing(max.bin)) max.bin <- x@max.bin
  if (missing(mono))    mono    <- x@mono
  if (missing(exceptions)) exceptions <- x@exceptions

  # browser()
  callGeneric(x=x@x, y=x@y, cuts=x@cuts, filter=x@f, min.iv=min.iv,
              min.res=min.res, max.bin=max.bin, mono=mono,
              exceptions=exceptions, name=x@name)
})

setMethod("Bin", signature = "data.frame", definition = function(x, y, min.iv=0.01, min.cnt=10, min.res=0, max.bin=10, mono=0, exceptions=numeric(0), ...) {
  cols <- colnames(x)
  nc <- ncol(x)

  classing <- list()

  dashes <- c('\\','|','/','-')

  for (i in seq_along(cols)) {
    vname <- cols[i]
    cat(sprintf("\rProgress: %s %6.2f%%", dashes[(i %% 4) + 1], (100*i/nc)))
    classing[[vname]] <- callGeneric(
      x[,vname], y=y, min.iv=min.iv, min.cnt=min.cnt, min.res=min.res,
      max.bin=max.bin, mono=mono, exceptions=exceptions, name=vname)
  }

  new("Classing", classing=classing)
})

setMethod("-", signature = c("Bin", "numeric"), definition = function(e1, e2) {
  if (length(e2) <= 1) return(e1)
  e1@transform <- callGeneric(e1@transform, e2)
  e1
}, valueClass = "Bin")

setMethod("-", signature = c("Continuous", "numeric"), definition = function(e1, e2) {
  if (!all(diff(e2)==1)) return(e1)
  ## make sure the requested collapse levels are within the acceptable range
  e2 <- unique(pmax(pmin(tail(e2, -1), length(e1@cuts) - 1), 2))
  e1@cuts <- e1@cuts[-(e2)]
  e1
})

setMethod("+", signature = c("Continuous", "numeric"), definition = function(e1, e2) {
  a <- min(max(1, e2), length(e1@cuts) - 1)  # can't be smaller than 1
  z <- max(min(e2 + 1, length(e1@cuts) - 1), a) # or larger than max els
  if (a == z) return(e1)

  vals <- e1@x[e1@x > e1@cuts[a] & e1@x <= e1@cuts[z] & e1@filter]
  q <- unique(quantile(vals, seq(0, 1, 0.2))) # quintiles

  e1@cuts <- sort(c(e1@cuts[-z], q))
  e1
})

setMethod("+", signature = c("Discrete", "numeric"), definition = function(e1, e2) {
  f <- e1@map %in% e1@map[e2]
  e1@map[f] <- levels(e1@x)[f]
  e1
})

setMethod("-", signature = c("Discrete", "numeric"), definition = function(e1, e2) {
  ## create a local copy of the map
  ## which values were selected for collapse?
  f <- e1@map %in% names(e1@map)[e2]

  # collapse them with commas
  e1@map[f] <- paste(names(e1@map)[f], collapse=',')
  e1
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



