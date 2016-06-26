predict.bin <- function(object, ..., data, type="woe", coef=1, method="min") {
  if (missing(data)) data <- object$x
  binned <- as.character(collapse(object, data))
  switch(
    type,
     "bins"  = binned,
     "woe"   = object$pred[binned],
     "rcs"   = object$rcs[binned],
     "dist"  = distance(object$pred[binned], coef, method)
    )
}

predict.classing <- function(object, ..., data, type="woe", coef=1, method="min") {
  if (missing(data)) data <- as.data.frame(object)
  data.frame(
    sapply(object, predict, type=type, coef=coef, method=method, USE.NAMES = T,
           simplify = F)
    )
}
