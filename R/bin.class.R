setClassUnion("ValidBinType", c("numeric", "factor"))
setClassUnion("cantBin", c("character","logical"))

setClass("Meta", slots = list(
  drop = "logical",
  new  = "logical",
  history = "list"),
  prototype = prototype(drop=FALSE, new=TRUE),
  contains = "VIRTUAL")

## virtual class contained by all bins
setClass("Bin", slots = list(
  name = "character",
  x    = "ValidBinType",
  y    = "numeric",
  woe  = "numeric",
  rcs  = "character",
  pred = "numeric"),
  contains = c("Meta", "VIRTUAL"))

# bin options class used by Continuous bins
setClass("Bin.opts", slots = list(
  min.iv  = "numeric",
  min.cnt = "numeric",
  min.res = "numeric",
  max.bin = "numeric",
  mono    = "numeric",
  exceptions = "numeric"
), contains  = "VIRTUAL")

# Continuous bins use cutpoints and have options controlling the discretization
setClass("Continuous",
  slots= list(
    cuts   = "numeric"),
  contains = c("Bin", "Bin.opts"))

# discrete bins map collapsed levels to raw levls
setClass("Discrete",
  slots=list(
    map    = "list"),
  contains = "Bin")

setClass("Classing",
  slots = list(
    classing = "list",
    y        = "numeric"),
  validity = function(object) {
    if (all(sapply(object@classing, is, "Bin"))) TRUE
    else "All members of a Classing object must be Bin objects"
  })
