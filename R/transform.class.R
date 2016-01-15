## transform class has data, and exceptions
# setClassUnion("transformData", c("factor", "numeric"))

setClass("Transform" ,
  slots = list(
    filter = "logical",
    binned = "factor"),
  contains = "VIRTUAL")

setClass("Continuous",
  slots= list(
    x    = "numeric",
    cuts = "numeric",
    exceptions = "numeric"),

  contains="Transform")

setClass("Discrete",
  slots=list(
    x    = "factor",
    map  = "list"),
  contains="Transform")


setMethod("show", signature = "Continuous", definition = function(object) {
  print(table(object@binned))
})

setMethod("show", signature = "Discrete", definition = function(object) {
  print(table(object@binned))
})

setMethod("show", signature = "Bin", definition = function(object) {
  print(cbind(tapply(object@y, object@transform@binned, mean)))
})

setGeneric("collapse")

setMethod("collapse", signature = "Transform", function(object) {
  print("what's up!?")
})

setMethod("collapse", signature = "Continuous", function(object) {
  f <- object@filter
  bins <- cut(object@x[f], object@cuts, include.lowest = T, dig.lab=3)
  x <- factor(object@x, exclude=NULL, levels=c(levels(bins), object@exceptions, NA))
  x[f] <- bins
  object@binned <- x
  object
})

setMethod("collapse", signature = "Discrete", function(object) {
  x <- object@x
  levels(x) <- c(levels(x), NA)
  object@binned <- x
  object
})

Continuous <- function(x, cuts, exceptions) {
  f <- !is.na(x) & !(x %in% exceptions)
  obj <- new("Continuous", x=x, cuts=cuts, exceptions=exceptions, filter=f)
  obj <- collapse(obj)
  obj
}

Discrete <- function(x) {
  f <- !is.na(x)
  obj <- new("Discrete", x=x, filter=f)
  obj <- collapse(obj)
  obj
}

setClass("Bin", slots = list(y="numeric", transform="Transform"))

res <- rbinom(1000, 1, .25)

Bin <- function(x=x, y=res) {
  if(is.null(cuts)) cuts <- cuts

  tf <- Continuous(x, cuts, exceptions)

  new("Bin", y=y, transform=tf)

}





x <- runif(1000)
x[sample(1:1000, 50)] <- NA
x[1:100] <- -1

exceptions <- -1
f <- !is.na(x) & !(x %in% exceptions)

cuts <- c(-Inf, quantile(x[f], seq(0,1,0.05), na.rm = T)[-c(1,21)], Inf)

y <- factor(sample(letters, 1000, replace=T))

test <- Continuous(x=x, cuts=cuts, exceptions = exceptions)
test2 <- Discrete(x=y)










