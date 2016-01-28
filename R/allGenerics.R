#' @include Bin.class.R

#' @export
setGeneric("collapse", function(object, x, ...) standardGeneric("collapse"))

#' @export
setGeneric("Update", function(object, keep = TRUE, ...) standardGeneric("Update"))

#' @export
setGeneric("Bin", valueClass = c("Bin", "Classing", "NULL"),
  function(x, y, name="NONE", ...) standardGeneric("Bin"))

#' @export
setGeneric("fit", function(object, data, y, ...) standardGeneric("fit"))

#' @export
setGeneric("classing", function(object) standardGeneric("classing"))

#' @export
setGeneric("adjust", function(x) standardGeneric("adjust"))

#' @export
setGeneric("predict")

#' @export
setGeneric("plot")

#' @export
setGeneric("as.data.frame")

#' @export
setGeneric("c")
