#' @include allGenerics.R

setMethod("Bin", signature = c(x="ValidBinType", y="numeric", w="missing"),
  function(x, y, w, name, min.iv=0.01, min.cnt=10, min.res=0, max.bin=10, mono=0,
            exceptions=numeric(0), ...) {
      w <- rep(1, length(x))
      callGeneric(x=x, y=y, w=w, name=name, min.iv=min.iv, min.cnt=min.cnt,
                  min.res=min.res, max.bin=max.bin, mono=mono, exceptions=exceptions, ...)
  })

setMethod("Bin", signature = c(x="numeric", y="numeric", w="numeric"),
  function(x, y, w, name, min.iv=0.01, min.cnt=10, min.res=0, max.bin=10, mono=0,
            exceptions=numeric(0), ...) {
    ## discretize numeric vars and return the cut points
    f <- !is.na(x)
    cuts <- .Call('bin', as.double(x[f]), as.double(y[f]), as.double(w[f]),
                  as.double(min.iv), as.integer(min.cnt),
                  as.integer(min.res), as.integer(max.bin),
                  as.integer(mono), as.double(exceptions))

    out <- new("continuous", x=x, y=y, w=w, cuts=cuts, min.iv=min.iv,
               min.cnt=min.cnt, min.res=min.res, max.bin=max.bin, mono=mono,
               exceptions=exceptions, name = name)
    # get the binned levels and map to woe predictions
    Update(out)
  })

setMethod("Bin", signature = c(x="factor", y="numeric", w="numeric"),
  function(x, y, w, name = "NONE", min.iv=0.01, min.cnt=10, min.res=0, max.bin=10,
           mono=0, exceptions=numeric(0), ...) {
    ## create a mapping of raw values to collapsed values, store in "map"
    ## check for factor levels that create issues
    if (any(levels(x) == "", na.rm=TRUE)) {
      levels(x)[which(levels(x) == "")] <- "Missing"
      warning(
        sprintf("Factor levels \"\" replaced with \"Missing\" for %s", name),
        call.=F)
    }

    mono <- 0 # monotonic factors doesn't make sense

    ## sort the factor levels by bad rate and bin as if numeric
    x <- factor(x, levels(x)[order(-tapply(y, x, mean))], exclude=NULL)

    b <- Bin(as.numeric(x), y, w, name=name, min.iv=min.iv, min.cnt=min.cnt,
             min.res=min.res, max.bin=max.bin, mono=mono, exceptions=exceptions, ...)

    grps <- split(levels(x), cut(seq_along(levels(x)), b@cuts))

    ## take the numeric cut points and map back to original factor levels
    map <- grps[cut(seq_along(levels(x)), b@cuts)]
    map <- lapply(map, paste, collapse=",")
    names(map) <- levels(x)

    out <- new("Discrete", x=x, y=y, w=w, map=map, name=name)
    ## get the binned levels and map to woe predictions
    Update(out)
  })

setMethod("Bin", signature = "ANY", function(x, y, w, name="NONE", ...) {NULL})

setMethod("Bin", signature = "logical",
  function(x, y, w, name="NONE", ...) {
    warning(sprintf("Not Binned: %s -- All missing" , name), call.=F)
    NULL
})

setMethod("Bin", signature = "character",
  function(x, y, w, name="NONE", ...) {
    warning(sprintf("Not Binned: %s -- Character, hint: cast to factor",
                     name), call.=F)
    NULL
  })

setMethod("Bin", signature = c(x="Bin", y="missing"),
  function(x, y, w, ...) do.call(Bin, modifyList(slots.to.list(x), list(...))))

# setMethod("Bin", signature = c(x="Discrete", y="missing"),
#   function(x, y, w, ...) {browser(); Bin(x@x, x@y, name=x@name)})

setMethod("Bin", signature = c(x="data.frame", y="numeric", seg="missing"),
  function(x, y, w, seg, min.iv=0.01, min.cnt=10, min.res=0, max.bin=10, mono=0,
           exceptions=numeric(0)) {

    if (missing(w)) w <- rep(1, nrow(x))

    cols <- colnames(x)
    classing <- list()
    for (i in seq_along(cols)) {
      .progress(i, ncol(x), "Binning   ")
      classing[[cols[i]]] <- callGeneric(
        x[,cols[i]], y=y, w=w, min.iv=min.iv, min.cnt=min.cnt, min.res=min.res,
        max.bin=max.bin, mono=mono, exceptions=exceptions, name=cols[i])
    }
    cat("", sep="\n")
    new("Classing", classing=classing, y=y, w=w)
  })

setMethod("Bin", signature = c(x="data.frame", y="numeric", seg="factor"),
  function(x, y, w, seg, min.iv=0.01, min.cnt=10, min.res=0, max.bin=10, mono=0,
           exceptions=numeric(0)) {

    if (missing(w)) w <- rep(1, nrow(x))

    ys <- split(y, seg, drop=T)
    xs <- split(x, seg, drop=T)
    ws <- split(w, seg, drop=T)

    out <- mapply(Bin, xs, ys, ws, MoreArgs = list(
      min.iv=min.iv, min.cnt=min.cnt, min.res=min.res, max.bin=max.bin,
      mono=mono, exceptions=exceptions), SIMPLIFY = F)

    new("Segmented-Classing", segmentor=seg, classings=out)
  })

