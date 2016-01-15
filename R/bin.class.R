setClass(Class = "Bin",
  slots = list(
    Name  = "character",
    Data  = "list",
    Opts  = "Opts",
    Meta  = "Meta",
    Notes = "character"),

  prototype = prototype(Data=list(x=numeric(0),y=numeric(0))),

  validity = function(object) {
    errs <- c()
    if (length(object@Data$x) != length(object@Data$y)) {
      errs <- c(errs, "x and y lengths do not match")
    }

    if (any(is.na(object@Data$y))){
      errs <- c(errs, "y cannot have NA values")
    }

    if (any(is.na(factor(object@Data$y, levels=c(0,1))))) {
      errs <- c(errs, "y must only have 0 and 1 values")
    }

    if (var(object@Data$y) == 0) {
      errs <- c(errs, "y only contains one value")
    }

    if (is.null(errs)) TRUE else errs
  }
)

setClass("Continuous Bin",
  slots = list(
    Levels = "Continuous",
    Counts = "Continuous",
    Values = "Continuous"),
  contains = "Bin")

setClass("Discrete Bin",
  slots = list(
   Levels = "Discrete",
   Counts = "Discrete",
   Values = "Discrete"),
  contains = "Bin")

setClass(Class = "Opts", slots = list(
  Min.iv  = "numeric",
  Min.cnt = "numeric",
  Min.res = "numeric",
  Max.bin = "numeric",
  Mono    = "numeric",
  Exceptions = "numeric"))

setClassUnion("Levels", c("numeric", "factor"))

setClass(Class = "Continuous", slots=list(
  Var = "numeric",
  Exc = "numeric",
  Nas = "numeric"
))

setClass(Class = "Discrete", slots=list(
  Var = "factor",
  Exc = "factor",
  Nas = "factor"
))

setClass(Class = "Meta", slots = list(
  Dropped  = "logical",
  Type     = "character",
  Inmodel  = "logical",
  New      = "logical",
  Modified = "character"))



