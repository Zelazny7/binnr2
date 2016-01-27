## standard generics
setGeneric("predict", function(object, ...) standardGeneric("predict"))

setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

setGeneric("as.data.frame",
           function(x, row.names = NULL, optional = FALSE, ...)
             standardGeneric("as.data.frame"))

