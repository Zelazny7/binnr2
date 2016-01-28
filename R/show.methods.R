#' @include Scorecard.class.R Bin.class.R

setMethod("show", signature = "Bin",
  function(object) {
    df <- as.data.frame(object)
    iv <- df['Total', "IV"]
    cat(sprintf("Variable Report: %s IV: %0.3f ", object@name, iv), sep = '\n')
    print(df, digits=5)
  })

setMethod("show", signature = "Classing",
  function(object) {
    lvls <- c("Discrete", "Continuous")
    cnts <- table(factor(sapply(object@classing, class), levels=lvls))
    cat("Classing object\n")
    cat(sprintf("  |-- %3d Bins Total\n", sum(cnts)))
    cat(sprintf("  |-- %3d Discrete\n"  , cnts[1]))
    cat(sprintf("  |-- %3d Continuous\n", cnts[2]))
  })

setMethod("show", signature = "Scorecard",
  function(object) {
    cat(sprintf("Scorecard model with %d predictors\n", sum(object@coef != 0) - 1))
    show(object@classing)
  })
