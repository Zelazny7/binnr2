#' @include bin.class.R segmented.class.R

#' @export
setGeneric("collapse", function(object, x, ...) standardGeneric("collapse"))

#' @export
setGeneric("sas", function(object, pfx="", coef=1, method="min", ...) standardGeneric("sas"))

#' @export
setGeneric("Update", function(object, ...) standardGeneric("Update"))

#' Bin numeric & factor vectors and data.frames
#'
#' @name Bin
#' @description Bin is the workhorse function of the Binnr2 package. At the
#' heart of the function is a fast discretization algorithm written in C that
#' optimizes information value given a set of constraints.
#' @param x Object to be binned. See details.
#' @param y Dependent variable used for discretization. Only binary outcomes are
#' currently supported. The discretization algorithm maximizes information value
#' through binary partitions.
#' @param w An optional numeric weight variable.
#' @param seg An optional factor to be used for creating a segmented classing
#' object. See details.
#' @param A character vector of length 1 used to name the Bin. This name is used
#' to match column names when predicting on a \code{data.frame}.
#' @param ... Optional arguments passed on to Bin.
#' @return A single Bin objecte or a \link{\code{Classing}} object depending on
#' the value of \code{x} that was passed in.
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
setGeneric("adjust", function(x, header=NULL) standardGeneric("adjust"))

#' @export
setGeneric("set.meta.attr", function(x, value, .slot) standardGeneric("set.meta.attr"))

#' @export
setGeneric("get.meta.attr", function(x, .slot) standardGeneric("get.meta.attr"))

setGeneric(".predict", function(object, x, type="woe", seg, coef, drop=FALSE, method="min", ...) standardGeneric(".predict"))

#' @export
setGeneric("plot")

#' @export
setGeneric("as.data.frame")

#' @export
setGeneric("summary")

