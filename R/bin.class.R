
## modular list of lists
make.meta <- function(drop=FALSE, inmodel=FALSE, nu=FALSE, steptwo=FALSE,
                      approved=FALSE, history=list()) {
  structure(
    list(
      drop     = drop,
      inmodel  = inmodel,
      nu       = nu,
      steptwo  = steptwo,
      approved = approved,
      history  = history),
    class="meta")
}

make.opts <- function(min.iv=0.001, min.cnt=25, min.res=5, max.bin=10,
                      mono=0, exceptions=numeric()) {
  structure(
  list(
    min.iv  = min.iv,
    min.cnt = min.cnt,
    min.res = min.res,
    max.bin = max.bin,
    mono    = mono,
    exceptions = exceptions),
  class="opts")
}

make.y <- function(y, family="bernoulli") {
  switch(family,
    "bernoulli" = {
      stopifnot(all(y %in% c(0,1)))
      structure(y, class=family)
    },

    { ## default
      stop("Performance family not yet implemented")
      NULL
    })
}

make.bin <- function(name=NULL, x, y, w, map, rcs=character(), pred=numeric(),
                     opts=make.opts(), meta=make.meta()) {
  structure(
  list(
    name = name,
    x    = x,
    y    = y,
    w    = w,
    map  = map,
    rcs  = rcs,
    pred = pred,
    opts = opts,
    meta = meta
  ),
  class="bin")
}

make.continuous <- function(bin=make.bin()) structure(bin, class=c("bin", "continuous"))

make.discrete <- function(bin=make.bin()) structure(bin, class=c("bin", "discrete"))

### Bin function
Bin <- function(x, y, w=NULL, name, min.iv=0.01, min.cnt=10, min.res=0, max.bin=10,
                mono=0, exceptions=numeric(0), ...) {
  UseMethod("Bin")
}

Bin.default <- function(x, y, w, name="NoName", min.iv=0.01, min.cnt=10, min.res=0, max.bin=10,
                        mono=0, exceptions=numeric(0), ...) {
  warning(sprintf("Variable %s not binned. Class: %s", name, class(x)), call. = FALSE)
  NULL
}

## refactor -- just use maps -- put in the main bin func?

Bin.numeric <- function(x, y, w, name, min.iv=0.01, min.cnt=10, min.res=0, max.bin=10,
                        mono=0, exceptions=numeric(0), ...) {

  if (all(is.na(x))) return(NULL)

  f <- !is.na(x)
  map <- .Call('bin', as.double(x[f]), as.double(y[f]), as.double(w[f]),
                as.double(min.iv), as.integer(min.cnt),
                as.integer(min.res), as.integer(max.bin),
                as.integer(mono), as.double(exceptions))

  bin <- make.continuous(
    make.bin(
      name = name,
      x = x,
      y = make.y(y),
      w = w,
      map = map,
      opts = make.opts(
        min.iv  = min.iv,
        min.cnt = min.cnt,
        min.res = min.res,
        max.bin = max.bin,
        mono    = mono,
        exceptions = exceptions),
      meta = make.meta(
        drop = all(is.infinite(map)))
      )
    )
  Update(bin)
}

Bin.factor <- function(x, y, w, name, min.iv=0.01, min.cnt=10, min.res=0, max.bin=10,
                       mono=0, exceptions=numeric(0), ...) {

  #browser()
  x <- factor(x, levels=levels(x)[!levels(x) == ""])
  x <- droplevels(x) # Strip NAs as a level
  if (all(is.na(x))) return(NULL)

  ## sort the factor levels by bad rate and bin as if numeric
  x <- factor(x, levels(x)[order(-tapply(y, x, mean))])

  b <- Bin(as.numeric(x), y, w, name=name, min.iv=min.iv, min.cnt=min.cnt,
           min.res=min.res, max.bin=max.bin, mono=0, exceptions=exceptions, ...)

  grps <- split(levels(x), cut(seq_along(levels(x)), b$map))

  ## take the numeric cut points and map back to original factor levels
  map <- grps[cut(seq_along(levels(x)), b$map)]
  map <- lapply(map, paste, collapse=",")
  names(map) <- levels(x)

  bin <- make.discrete(
    make.bin(
      name = name,
      x = x,
      y = make.y(y),
      w = w,
      map = map,
      opts = make.opts(
        min.iv  = min.iv,
        min.cnt = min.cnt,
        min.res = min.res,
        max.bin = max.bin,
        mono    = mono,
        exceptions = exceptions),
      meta = make.meta(
        drop=all(is.infinite(b$map)))
    )
  )

  Update(bin)
}

Bin.data.frame <- function(x, y, w=NULL, name=NULL, min.iv=0.01, min.cnt=10, min.res=0, max.bin=10,
               mono=0, exceptions=numeric(0), ...) {

  ## do some checks
  stopifnot(nrow(x) == length(y))
  if (is.null(w)) w <- rep(1, length(y))
  stopifnot(nrow(x) == length(w))

  v <- colnames(x)
  bins <- lapply(seq_along(x), function(i) {
    .progress(i, ncol(x), "Binning   ", v[i])
    Bin(x[,i], y, w, v[i], min.iv, min.cnt, min.res, max.bin, mono, exceptions, ...)
  })
  names(bins) <- v
  cat("", sep="\n")

  make.classing(bins, y, w)
}

collapse <- function(object, x, ...) {
  #browser()
  UseMethod("collapse")
}

collapse.bin <- function(object, x=NULL, ...) {
  #browser()
  if (is.null(x)) x <- object$x
  NextMethod("collapse", x=x)
}

collapse.continuous <- function(object, x, ...) {
  #browser()
  if(!is.numeric(x)) stop("Collapsing continuous bin requires numeric x argument")

  e <- object$opts$exceptions
  f <- !is.na(x) & !(x %in% e)
  l <- format(object$map, trim=TRUE, nsmall=2, digits=2, big.mark=",",
              scientific = FALSE)

  N <- max(nchar(l))
  fmt <- sprintf("(%%%ds - %%%ds]", N, N)
  lbls <- sprintf(fmt, head(l,-1), tail(l, -1))
  bins <- cut(x[f], object$map, include.lowest = T, labels = lbls)

  out <- factor(x, exclude=NULL, levels=c(levels(bins), e, NA))
  levels(out)[is.na(levels(out))] <- "Missing"
  out[f] <- bins
  out
}

collapse.discrete <- function(object, x, ...) {
  if(!is.factor(x)) stop("Collapsing discrete bin requires factor x argument")

  out <- x
  levels(out) <- unlist(object$map)[levels(out)]
  out <- addNA(out)
  levels(out)[is.na(levels(out))] <- "Missing"
  out[is.na(out)] <- "Missing"
  out

}


as.data.frame.bernoulli <- function(x, row.names=NULL, optional=FALSE, ..., bin=bin) {
  #browser()
  x <- bin
  binned <- collapse(x)
  f <- !is.na(x$x)

  ## get the bivariate matrix
  out <- mapply(.bv, split(x$y, binned), split(x$w, binned),
                MoreArgs = list(Y=x$y, W=x$w, f=f), SIMPLIFY = FALSE)

  out[sapply(out, is.null)] <- 0

  out <- do.call(rbind, out)
  out[is.infinite(out) | is.nan(out)] <- 0
  out <- cbind(out, Pred=x$pred[row.names(out)])

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
}

as.data.frame.bin <- function(x, row.names=NULL, optional=FALSE, ...) {
  as.data.frame(x$y, bin=x)
}

Update <- function(object) {
  pred <- head(as.data.frame(object)$WoE, -1)
  names(pred) <- levels(collapse(object, object$x[0]))
  object$pred <- pred
  object$pred["Missing"] <- 0
  object
}

print.meta <- function(x, ...) {
  fmt <- "\nDropped [%1s] | In Model [%1s] | New [%1s] | Step 2 [%1s] | Approved [%1s] \n"
  cat(
    sprintf(fmt,
      ifelse(x$drop, "y", "n"),
      ifelse(x$inmodel, "y", "n"),
      ifelse(x$nu, "y", "n"),
      ifelse(x$steptwo, "y", "n"),
      ifelse(x$approved, "y", "n")),
    sep = '\n')
}

print.bin <- function(x, ...) {

  cat(x$name)
  print(x$meta)

  df <- as.data.frame(x)
  # make the bin look pretty
  df <- cbind(
    lapply(df[,1:3], prettyNum, big.mark = ',', scientific=FALSE),
    format(df[,4:7] , digits=2, scientific=FALSE),
    format(df[,8:10], digits=5, scientific=FALSE))

  print(df)

}

print.classing <- function(x, ...) {
  lvls <- c("discrete", "continuous")
  cnts <- table(factor(sapply(x, class), levels=lvls))
  ndrop <- sum(get.meta.attr(x, "drop"))
  cat("Classing object\n")
  cat(sprintf("  |-- %3d Bins Total\n", sum(cnts)))
  cat(sprintf("  |-- %3d Discrete\n"  , cnts[1]))
  cat(sprintf("  |-- %3d Continuous\n", cnts[2]))
  cat(sprintf("  |-- %3d Dropped\n"   , ndrop))
}

make.classing <- function(bins, y, w=NULL, bookmark=character()) {
  if (!is.list(bins)) stop("bins argument must be a list")
  bins <- bins[!sapply(bins, is.null)] # drop null bins
  if (!all(sapply(bins, inherits, "bin"))) stop("List passed to classing must all be bins")

  if (is.null(w)) w <- rep(1, length(y))
  structure(
    bins,
    y = y,
    w = w,
    bookmark = bookmark,
    class = "classing")
}



as.data.frame.classing <- function(x, row.names = NULL, optional = FALSE, ...) {
  data.frame(lapply(x, '[[', 'x'), check.names=FALSE)
}

`[.classing` <- function(x, i, j, ..., drop = TRUE) {
  browser()
  make.classing(unclass(x)[i], y=attr(x, "y"), w = attr(x, "w"))
}

`[<-.classing` <- function(x, i, j, ..., value) {
  browser()
  stopifnot(length(i) == length(value))
  stopifnot(inherits(value, "classing"))
  x <- unclass(x)
  x[i] <- value
  names(x)[i] <- names(value)
  make.classing(x, y=attr(x, "y"), w = attr(x, "w"))
}

