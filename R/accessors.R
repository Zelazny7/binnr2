#' @include bin.class.R

#' @export
setAs("Classing", "list", def = function(from) from@classing)

#' @export
`dropped<-` <- function(x, value) {
  set.meta.attr(x, value, "drop")
}

#' @export
dropped <- function(x) get.meta.attr(x, "drop")

#' @export
`inmodel<-` <- function(x, value) set.meta.attr(x, value, "inmodel")

#' @export
inmodel <- function(x) get.meta.attr(x, "inmodel")

#' @export
`new.vars<-` <- function(x, value) set.meta.attr(x, value, "new")

#' @export
new.vars <- function(x) get.meta.attr(x, "new")

#' @export
`steptwo<-` <- function(x, value) set.meta.attr(x, value, "steptwo")

#' @export
steptwo <- function(x) get.meta.attr(x, "steptwo")

#' @export
`approved<-` <- function(x, value) set.meta.attr(x, value, "approved")

#' @export
approved <- function(x) get.meta.attr(x, "approved")


#' @export
setMethod("names", "Classing", function(x) {
  sapply(as(x, "list"), slot, "name")
})

setMethod("set.meta.attr", "Bin",
  function(x, value, .slot) {
    slot(x, .slot) <- value
    initialize(x)
})

setMethod("set.meta.attr", "Classing",
  function(x, value, .slot) {
    for (i in seq_along(x)) x[[i]] <- callGeneric(x[[i]], value[i], .slot)
    initialize(x)
  })

setMethod("set.meta.attr", "Scorecard",
  function(x, value, .slot) {
    initialize(x, classing=callGeneric(x@classing, value=value, .slot=.slot))
  })

setMethod("set.meta.attr", "ANY",
function(x, value, .slot) {
  cat(sprintf("Method not implemented for class: %s", class(x)))
})

## TODO: Implement some day...

## segmented methods
# setMethod("set.meta.attr", "Segmented-Classing",
#  function(x, value, .slot) {
#    browser()
#    classings <- lapply(x@classings, set.meta.attr, value, .slot)
#    initialize(x, classings=classings)
#  })

# setMethod("set.meta.attr", "Segmented-Scorecard",
#   function(x, value, .slot) {
#     scorecards <- lapply(x@scorecards, set.meta.attr, value, .slot)
#     initialize(x, scorecards=scorecards)
#   })

setMethod("get.meta.attr", "Bin", function(x, .slot) slot(x, .slot))

setMethod("get.meta.attr", "Classing", function(x, .slot) {
  n <- names(x)
  out <- sapply(as(x, "list"), slot, .slot, USE.NAMES = FALSE)
  names(out) <- n
  out
})

setMethod("get.meta.attr", "Scorecard", function(x, .slot) {
  callGeneric(x@classing, .slot=.slot)
})

setMethod("get.meta.attr", "Segmented-Classing", function(x, .slot) {
  lapply(x@classings, get.meta.attr, .slot)
})

setMethod("get.meta.attr", "Segmented-Scorecard", function(x, .slot) {
  lapply(x@scorecards, get.meta.attr, .slot)
})
