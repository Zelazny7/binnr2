setClassUnion("ValidBinType", c("numeric", "factor"))
setClassUnion("cantBin", c("character","logical"))
setClassUnion("NullOrDF", c("NULL","data.frame"))

setClass("Meta", slots = list(
  drop    = "logical",
  inmodel = "logical",
  new     = "logical",
  steptwo = "logical",
  approved= "logical",
  penalty = "numeric",
  cache   = "NullOrDF",
  history = "list",
  summary = "list"),
  prototype = prototype(drop=FALSE, inmodel=FALSE, new=FALSE, steptwo=FALSE,
                        approved=FALSE, penalty=1, cache=NULL),
  contains = "VIRTUAL")

## virtual class contained by all bins
setClass("Bin", slots = list(
  name = "character",
  x    = "ValidBinType",
  y    = "numeric",
  w    = "numeric",
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
setClass("continuous",
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
    y        = "numeric",
    w        = "numeric",
    bookmark = "character"),
  validity = function(object) {
    if (all(sapply(object@classing, is, "Bin"))) TRUE
    else "All members of a Classing object must be Bin objects"
  })
