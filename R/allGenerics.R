#' @include Bin.class.R

#' @export
setGeneric("collapse",
  function(object, x, ...) {
    if (missing(x)) x <- object@x
    callGeneric(object, x=x, ...)
  })

#' @export
setGeneric("Update", function(object, keep = TRUE, ...) standardGeneric("Update"))

#' @export
setGeneric("Bin", valueClass = c("Bin", "Classing"),
  function(x, y, min.iv=0.01, min.cnt=10, min.res=0, max.bin=10, mono=0,
           exceptions=numeric(0), name = "NONE", ...) standardGeneric("Bin"))

#' @export
setGeneric("fit",
  function(object, data, y, ...) standardGeneric("fit"))

#' @export
setGeneric("classing", function(object) standardGeneric("classing"))

#' @export
setGeneric("predict")

#' @export
setGeneric("plot")

#' @export
setGeneric("as.data.frame")

#' @export
setGeneric("c")
