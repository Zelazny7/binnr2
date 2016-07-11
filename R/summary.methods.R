get.bin.summary <- function(df, object) {
  Nex <- if (is(object, "continuous"))
    sum(object@x %in% object@exceptions) else 0
  Nna <- sum(is.na(object@x))

  ## summarize the meta data
  meta <- names(getSlots("Meta"))
  meta <- meta[!meta %in% c("history", "summary")]
  meta <- sapply(meta, function(x) c("N" , "Y")[slot(object, x) + 1],
    simplify = F)

  ## return a data.frame of summary info
  data.frame(
    "Type"    = class(object),
    "IV"      = sprintf("%1.4f", df["Total","IV"]),
    "# Bins"  = nrow(df) - 2, # subtract missing & total rows
    "# Uniq"  = length(unique(object@x)),
    "Tot N"   = df["Total", "N"],
    "# Valid" = df["Total", "N"] - Nex - Nna,
    "# Missing" = Nna,
    "# Exceptions" = Nex,
    meta,
    check.names=F, stringsAsFactors=F)
}

#' @export
setMethod("summary", "Bin",
  function(object, ...) {
    as.data.frame(object@summary, check.names=FALSE, stringsAsFactors=FALSE)
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

#' @export
setMethod("summary", "Scorecard",
  function(object, ...) {
    classing <- classing(object)
    out <- list()
    for (i in seq_along(classing)) {
      out[[classing[[i]]@name]] <- summary(classing[[i]])
      .progress(i, length(classing), text = "Generating Summary")
    }
    cat("\n")
    s <- do.call(rbind, out)

    ## merge in other stats for the scorecard
    s$Contribution <- rep(0, nrow(s))
    s[names(object@contribution),"Contribution"] <- object@contribution

    s$Coefficient <- rep(0, nrow(s))
    s[names(object@coef)[-1],"Coefficient"] <- object@coef[-1]

    format(s, digits=9, big.mark=",", zero.print=".", scientific=FALSE)
  })


#' @export
setMethod("summary", "Segmented-Classing",
  function(object, ...) {
    lapply(object@classings, summary)
  })

#' @export
setMethod("summary", "Segmented-Scorecard",
  function(object, ...) {
    lapply(object@scorecards, summary)
  })

