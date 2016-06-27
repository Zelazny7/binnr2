#' @export
setGeneric("compare", function(object, ...) standardGeneric("compare"))

setMethod("compare", signature=c(object="Scorecard"),
  function(object, ...) {
    others <- list(...)
    stopifnot(all(sapply(others, inherits, "Scorecard")))

    ## create data.frame of scorecard summary
    dfs <- lapply(c(object, others), as.data.frame)

    ## merge them all
    contribution <- lapply(dfs, `[`, 'Contribution')
    coefficients <- lapply(dfs, `[`, 'Coefficients')

    # merge helper for use with Reduce
    .merge <- function(a, b) {
      tmp <- merge(a,b, by=0, all=T)
      row.names(tmp) <- tmp$Row.names
      subset(tmp, select = -Row.names)
    }

    out <- merge(
      Reduce(.merge, contribution),
      Reduce(.merge, coefficients),
      by=0, all=T)

    cols <- c("Contribution", "Coefficients")
    colnames(out) <- c("Variable", paste(rep(cols, each=length(dfs)),
                                         seq_along(dfs)))

    out
  })


setMethod("compare", signature=c(object="Segmented-Scorecard"),
  function(object, ...) {
    others <- list(...)

    stopifnot(all(sapply(others, inherits, "Segmented-Scorecard")))

    ## make sure all models have the same segments
    levels <- lapply(c(object, others), function(x) {
      levels(object@segmentor)
    })

    if (!all(duplicated(levels)[-1L])) {
      stop("Compared scorecards do not all have the same segment variables")
    }



  })


