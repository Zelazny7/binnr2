#' @include Scorecard.class.R Bin.class.R

setMethod("show", signature = "Bin",
  function(object) {
    df <- as.data.frame(object)
    # iv <- df['Total', "IV"]
    cat(sprintf("\n> %-20s | drop: %5s", object@name, as.character(object@drop)),
        sep = '\n')
    print(df, digits=5)
  })

setMethod("show", signature = "Classing",
  function(object) {
    lvls <- c("Discrete", "Continuous")
    cnts <- table(factor(sapply(object@classing, class), levels=lvls))
    ndrop <- sum(sapply(object@classing, slot, "drop"))
    cat("Classing object\n")
    cat(sprintf("  |-- %3d Bins Total\n", sum(cnts)))
    cat(sprintf("  |-- %3d Discrete\n"  , cnts[1]))
    cat(sprintf("  |-- %3d Continuous\n", cnts[2]))
    cat(sprintf("  |-- %3d Dropped\n"   , ndrop))
  })

setMethod("show", signature = "Scorecard",
  function(object) {
    cat("Scorecard object\n")
    cat(sprintf("  |-- %5d predictors\n", sum(object@coef != 0) - 1))
    cat(sprintf("  |-- %2.2f development ks\n", object@performance * 100))

    ## print a little var importance table
    cat("\nVariable Contribution:\n")
    out <- merge(object@coef[-1], object@contribution, by=0, all=T)
    rownames(out) <- out$Row.names
    out$Row.names <- NULL
    out <- head(with(out, out[order(-out[,2]),]), 5)

    cnt <- out[,2]/max(out[,2]) * 10
    out[,3] <- sapply(cnt, function(i) paste(rep("*", i), collapse=""))
    out[,3] <- format(out[,3], justify = "left")
    colnames(out) <- c("Coefficient", "Contribution", "Importance")
    cat("\n")
    print(out)
    cat("...\n")
  })

setMethod("show", signature = "Segmented-Scorecard",
  function(object) {
    cat("Segmented-Scorecard object\n")
    cat(sprintf("  |-- %5d segments\n", length(levels(object@segmentor))))
    cat(sprintf("  |-- %2.2f development ks\n", object@performance * 100))

    ## loop over segments and show
    for (seg in levels(object@segmentor)) {
      border <- paste(c("+", rep("-", nchar(seg) + 11), "+"), collapse="")
      cat(sprintf("\n%2$s\n| Segment: %s |\n%2$s\n", seg, border))
      show(object@scorecards[[seg]])
    }
  })
