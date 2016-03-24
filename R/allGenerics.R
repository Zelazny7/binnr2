#' @include bin.class.R segmented.class.R

#' @export
setGeneric("drop<-", function(x, value) standardGeneric("drop<-"))

#' @export
setGeneric("collapse", function(object, x, ...) standardGeneric("collapse"))

#' @export
setGeneric("Update", function(object, keep = TRUE, ...) standardGeneric("Update"))

#' @export
setGeneric("Bin", valueClass = c("Bin", "Classing", "Segmented", "NULL"),
  function(x, y, w, seg, name="NONE", ...) {
    ## Checks for the size
    stopifnot(NROW(x) > 0)
    if (!missing(y)) stopifnot(NROW(x) == NROW(y))
    standardGeneric("Bin")
  })

#' @export
setGeneric("fit", function(object, x, y, w, seg, ...) standardGeneric("fit"))

#' @export
setGeneric("classing", function(object) standardGeneric("classing"))

#' @export
setGeneric("adjust", function(x) standardGeneric("adjust"))

#' @export
setGeneric("set.meta.attr", function(x, value, .slot) standardGeneric("set.meta.attr"))

#' @export
setGeneric("get.meta.attr", function(x, .slot) standardGeneric("get.meta.attr"))

setGeneric(".predict", function(object, x, type="woe", seg, ...) standardGeneric(".predict"))

#' @export
setGeneric("plot")

#' @export
setGeneric("as.data.frame")

#' @export
setGeneric("c")

#' @export
setGeneric("summary")

