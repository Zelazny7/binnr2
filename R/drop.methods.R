#' @export
setMethod("drop<-", "Bin",
  function(x, value) {
    initialize(x, drop=value)
})

#' @export
setMethod("drop<-", "Classing",
  function(x, value) {
    for (i in seq_along(x)) drop(x[[i]]) <- value
    x
  })

#' @export
setMethod("drop<-", "Scorecard",
  function(x, value) {
    browser()
    initialize(x, classing=callGeneric(x, value=value))
  })

#'
#' #' @export
#' setMethod("drop", "Scorecard",
#'   function(object, i, undrop=FALSE, ...) {
#'     out <- callGeneric(object@classing, i=i, undrop=undrop)
#'     initialize(object, classing=out)
#'   })
#'
#' #' @export
#' setMethod("drop", "Scorecard",
#'   function(object, i, undrop=FALSE, ...) {
#'     out <- callGeneric(object@classing, i=i, undrop=undrop)
#'     initialize(object, classing=out)
#'   })
