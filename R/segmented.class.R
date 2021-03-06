#' @include bin.class.R

setClass("Segmented", slots=list(
  segmentor  = "factor"), contains = "VIRTUAL")

setClass("Segmented-Classing", slots=list(
  classings="list"),
  contains = "Segmented")

setClass("Segmented-Scorecard", slots=list(
  scorecards="list",
  performance="numeric"),
  contains = c("Segmented"))
