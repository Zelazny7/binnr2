setOldClass("cv.glmnet")

#' @include bin.class.R
#' @export
setClass("Scorecard",
  slots=list(
   y        = "numeric",
   fit      = "cv.glmnet",
   coef     = "numeric",
   classing = "Classing",
   contribution = "numeric",
   performance  = "numeric"))
