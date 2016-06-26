meta <- names(formals(make.meta))
get.meta.attr <- function(b, z=meta) {match.arg(z); UseMethod("get.meta.attr")}
set.meta.attr <- function(b, z=meta, v) {match.arg(z); UseMethod("set.meta.attr")}

get.meta.attr.bin <- function(b, z) {
  if (is.null(b[["meta"]])) stop("bin object does not have meta component")
  b[["meta"]][[z]]
}

set.meta.attr.bin <- function(b, z, v) {
  if (is.null(b[["meta"]])) stop("object does not have meta component")
  if (class(b[["meta"]][[z]]) != class(v)) {
    stop(sprintf("Passed value does not match type for meta attribute: %s", z))
  }
  b[["meta"]][[z]] <- v
  b
}

get.meta.attr.classing <- function(b, z) sapply(b, get.meta.attr, z)

set.meta.attr.classing <- function(b, z, v) {
  browser()
  for (i in seq_along(b)) {b[[i]] <- set.meta.attr(b[[i]], z, v)}
  b
}


dropped <- function(x) get.meta.attr(x, "drop")
`dropped<-` <- function(x, value) set.meta.attr(x, "drop", value)

dropped(b)[1:2] <- TRUE

