#' @include Bin.class.R

#' @export
setAs("Classing", "list", def = function(from) from@classing)

#' @export
`drop<-` <- function(x, value) set.meta.attr(x, value, "drop")

#' @export
dropped <- function(x) get.meta.attr(x, "drop")

#' @export
`inmodel<-` <- function(x, value) set.meta.attr(x, value, "inmodel")

#' @export
inmodel <- function(x) get.meta.attr(x, "inmodel")

#' @export
`new<-` <- function(x, value) set.meta.attr(x, value, "new")

#' @export
.new <- function(x) get.meta.attr(x, "new")

setMethod("set.meta.attr", "Bin",
  function(x, value, .slot) {
    slot(x, .slot) <- value
    initialize(x)
})

setMethod("set.meta.attr", "Classing",
  function(x, value, .slot) {
    for (i in seq_along(x)) x[[i]] <- callGeneric(x[[i]], value, .slot)
    initialize(x)
  })

setMethod("set.meta.attr", "Scorecard",
  function(x, value, .slot) {
    initialize(x, classing=callGeneric(x@classing, value=value, .slot=.slot))
  })

setMethod("get.meta.attr", "Bin", function(x, .slot) slot(x, .slot))

setMethod("get.meta.attr", "Classing", function(x, .slot) {
  sapply(as(x, "list"), slot, .slot)
})

setMethod("get.meta.attr", "Scorecard", function(x, .slot) {
  sapply(as(x@classing, "list"), slot, .slot)
})
