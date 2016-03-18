#' @include allGenerics.R

setMethod("Bin", signature = "numeric",
  function( x, y, name, min.iv=0.01, min.cnt=10, min.res=0, max.bin=10, mono=0,
            exceptions=numeric(0), ...) {
    ## discretize numeric vars and return the cut points
    f <- !is.na(x)
    cuts <- .Call('bin', as.double(x[f]), as.double(y[f]),
                  as.double(min.iv), as.integer(min.cnt),
                  as.integer(min.res), as.integer(max.bin),
                  as.integer(mono), as.double(exceptions))

    out <- new("Continuous", x=x, y=y, cuts=cuts, min.iv=min.iv,
               min.cnt=min.cnt, min.res=min.res, max.bin=max.bin, mono=mono,
               exceptions=exceptions, name = name)
    # get the binned levels and map to woe predictions
    Update(out)
  })

setMethod("Bin", signature = "factor",
  function(x, y, name = "NONE", ...) {
    ## create a mapping of raw values to collapsed values, store in "map"
    ## check for factor levels that create issues
    if (any(levels(x) == "")) {
      levels(x)[which(levels(x) == "")] <- "Missing"
      warning(
        sprintf("Factor levels \"\" replaced with \"Missing\" for %s", name),
        call.=F)
    }

    # sort the factor levels by bad rate and bin as if numeric
    m <- order(tapply(y, x, mean))
    names(m) <- levels(x)
    b <- Bin(m[x], y, name = name, ...)
    grps <- tapply(m, cut(m, b@cuts), names)

    # take the numeric cut points and map back to original factor levels
    map <- grps[sapply(paste0("\\b", levels(x), "\\b"), grep, grps)]
    map <- lapply(map, paste, collapse=",")
    names(map) <- levels(x)

    out <- new("Discrete", x=x, y=y, map=map, name=name)
    # get the binned levels and map to woe predictions
    Update(out)
  })

setMethod("Bin", signature = "ANY", function(x, y, name="NONE", ...) {NULL})

setMethod("Bin", signature = "logical",
  function(x, y, name="NONE", ...) {
    warning(sprintf("Not Binned: %s -- All missing" , name), call.=F)
    NULL
})

setMethod("Bin", signature = "character",
  function(x, y, name="NONE", ...) {
    warning(sprintf("Not Binned: %s -- Character, hint: cast to factor",
                     name), call.=F)
    NULL
  })

setMethod("Bin", signature = c(x="Continuous", y="missing"),
  function(x, y, ...) do.call(Bin, modifyList(slots.to.list(x), list(...))))

setMethod("Bin", signature = c(x="Discrete", y="missing"),
  function(x, y, ...) Bin(x@x, x@y, name=x@name))

setMethod("Bin", signature = c(x="data.frame", y="numeric", seg="missing"),
  function(x, y, seg, min.iv=0.01, min.cnt=10, min.res=0, max.bin=10, mono=0,
           exceptions=numeric(0)) {

    cols <- colnames(x)
    classing <- list()
    for (i in seq_along(cols)) {
      .progress(i, ncol(x), "Binning   ")
      classing[[cols[i]]] <- callGeneric(
        x[,cols[i]], y=y, min.iv=min.iv, min.cnt=min.cnt, min.res=min.res,
        max.bin=max.bin, mono=mono, exceptions=exceptions, name=cols[i])
    }
    cat("", sep="\n")
    new("Classing", classing=classing, y=y)
  })

setMethod("Bin", signature = c(x="data.frame", y="numeric", seg="factor"),
  function(x, y, seg, min.iv=0.01, min.cnt=10, min.res=0, max.bin=10, mono=0,
           exceptions=numeric(0)) {
    ys <- split(y, seg, drop=T)
    xs <- split(x, seg, drop=T)

    out <- mapply(Bin, xs, ys, MoreArgs = list(
      min.iv=min.iv, min.cnt=min.cnt, min.res=min.res, max.bin=max.bin,
      mono=mono, exceptions=exceptions), SIMPLIFY = F)

    new("Segmented-Classing", segmentor=seg, classings=out)
  })

