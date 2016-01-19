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
  out <- tapply(object@y, object@transform@binned, function(y) {
      c(N=length(y), Sum=sum(y), Mean=mean(y))
    }, simplify=T)

  out <- do.call(rbind, out)

  print(out)
})

collapse <- function(object) {}

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

Discrete <- function(x, map) {
  f <- !is.na(x)
  obj <- new("Discrete", x=x, filter=f, map=map)
  obj <- collapse(obj)
  obj
}

setClass("Bin", slots = list(y="numeric", transform="Transform"))

Bin <- function(x, y, min.iv=0.01, min.cnt=10, min.res=0, max.bin=10, mono=0, exceptions=numeric(0), ...) {
  print("In here!?")
}

setGeneric("Bin", valueClass = "Bin")

setMethod("Bin", signature = "numeric", definition = function(x, y, min.iv=0.01, min.cnt=10, min.res=0, max.bin=10, mono=0, exceptions=numeric(0), ...) {
  print("Butt!")
  f <- !is.na(x)
  cuts <- .Call('bin', as.double(x[f]), as.double(y[f]),
                as.double(min.iv), as.integer(min.cnt), as.integer(min.res),
                as.integer(max.bin), as.integer(mono), as.double(exceptions))

  tf <- Continuous(x, cuts, exceptions)
  new("Bin", y=y, transform=tf)
})

setMethod("Bin", signature = "factor", definition = function(x, y, ...) {
  print("Factor face!")

  map <- as.list(levels(x))
  names(map) <- levels(x)

  tf <- Discrete(x, map)
  new("Bin", y=y, transform=tf)
})

setClassUnion("cantBin", c("character","logical"))

setMethod("Bin", signature = "cantBin", definition = function(x, y, ...) {
  print("What you doing fool?")
  NULL
})


setMethod("-", signature = "Bin", definition = function(e1, e2) {
  stopifnot(all(diff(e2)==1))
  if (length(e2) <= 1) return(e1)
  out <- callGeneric(e1@transform, e2)
  e1@transform <- out
  e1
})


setMethod("-", signature = "Continuous", definition = function(e1, e2) {
  print("In here")
  e2 <- unique(pmax(pmin(tail(e2, -1), length(e1@cuts) - 1), 2))
  new.cuts <- e1@cuts[-(e2)]
  Continuous(e1@x, cuts=new.cuts, exceptions = e1@exceptions)
})


#Bin(x=titanic$SibSp, y=titanic$Survived)
#Bin(x=titanic$Embarked, y=titanic$Survived)
