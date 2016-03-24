#' @export
setMethod("summary", "Bin",
  function(object, ...) {
    df <- as.data.frame(object)

    Nex <- if (is(object, "continuous"))
      sum(object@x %in% object@exceptions) else 0
    Nna <- sum(is.na(object@x))

    ## return a data.frame of summary info
    data.frame(
      "Type"    = class(object),
      "Dropped" = if (object@drop) "yes" else "no",
      "IV"      = sprintf("%1.3f", df["Total","IV"]),
      "# Bins"  = nrow(df) - 2, # subtract missing & total rows
      "# Uniq"  = length(unique(object@x)),
      "Tot N"   = df["Total", "N"],
      "# Valid" = df["Total", "N"] - Nex - Nna,
      "# Missing" = Nna,
      "# Exceptions" = Nex,
      check.names=F, stringsAsFactors=F)
  })

#' @export
setMethod("summary", "Classing",
  function(object, ...) {
    out <- list()
    for (i in seq_along(object)) {
      out[[object[[i]]@name]] <- summary(object[[i]])
      .progress(i, length(object), text = "Generating Summary")
    }
    cat("\n")
    s <- do.call(rbind, out)
    format(s, digits=9, big.mark=",", zero.print=".", scientific=FALSE)
  })


