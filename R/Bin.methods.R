#' @include allGenerics.R

setMethod("Bin", signature = "numeric",
  function( x, y, min.iv=0.01, min.cnt=10, min.res=0, max.bin=10, mono=0,
            exceptions=numeric(0), name = "NONE", ...) {
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
      warning("Factor levels \"\" replaced with \"Missing\"", call.=F)
    }
    ## Add NA to valid factor levels
    x <- addNA(x)
    levels(x)[is.na(levels(x))] <- "Missing"
    map <- as.list(levels(x))
    names(map) <- levels(x)
    out <- new("Discrete", x=x, y=y, map=map, name = name)
    # get the binned levels and map to woe predictions
    Update(out)
  })

setMethod("Bin", signature = "cantBin", function(x, y, ...) {NULL})

setMethod("Bin", signature = c("Continuous", "missing"),
  function(x, y, ...) {
    # replace current options with new settings passed in
    mod <- as.list(match.call())[-(1:2)]
    sl <- slots.to.list(x)
    sl[names(mod)] <- mod
    do.call(callGeneric, sl)
  })

setMethod("Bin", signature = "data.frame",
  function(x, y, min.iv=0.01, min.cnt=10, min.res=0, max.bin=10, mono=0,
           exceptions=numeric(0), name = "NONE", ...) {
    cols <- colnames(x)
    classing <- list()
    for (i in seq_along(cols)) {
      vname <- cols[i]
      progress <- paste(rep("=", (10*i/ncol(x))), collapse="")
      cat(sprintf("\rClassing   : %-10s|", progress))
      classing[[vname]] <- callGeneric(
        x[,vname], y=y, min.iv=min.iv, min.cnt=min.cnt, min.res=min.res,
        max.bin=max.bin, mono=mono, exceptions=exceptions, name=vname)
    }
    cat("", sep="\n")
    new("Classing", classing=classing, y=y)
  })


