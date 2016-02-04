#' @include Bin.class.R segmented.class.R

#' @export
setGeneric("collapse", function(object, x, ...) standardGeneric("collapse"))

#' @export
setGeneric("Update", function(object, keep = TRUE, ...) standardGeneric("Update"))

#' @export
setGeneric("Bin", valueClass = c("Bin", "Classing", "Segmented", "NULL"),
  function(x, y, seg, name="NONE", ...) {
    ## Checks for the size
    stopifnot(NROW(x) > 0)
    stopifnot(NROW(x) == NROW(y))
    standardGeneric("Bin")
  })

#' @export
setGeneric("fit", function(object, x, y, seg, ...) standardGeneric("fit"))

#' @export
setGeneric("classing", function(object) standardGeneric("classing"))

#' @export
setGeneric("adjust", function(x) standardGeneric("adjust"))

#' @export
setGeneric("drop<-", function(x, value) standardGeneric("drop<-"))

setGeneric(".predict", function(object, x, type="woe", seg, ...) standardGeneric(".predict"))

#' @export
setGeneric("plot")

#' @export
setGeneric("as.data.frame")

#' @export
setGeneric("c")

#' @export
setGeneric("summary")

