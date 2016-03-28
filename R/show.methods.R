#' @include Scorecard.class.R bin.class.R

setMethod("show", signature = "Bin",
  function(object) {
    df <- as.data.frame(object)

    # iv <- df['Total', "IV"]
    cat(sprintf("\n%-32s\nDropped [%1s] | In Model [%1s] | New [%1s]\n",
                object@name,
                ifelse(object@drop, "y", "n"),
                ifelse(object@inmodel, "y", "n"),
                ifelse(object@new, "y", "n")), sep = '\n')

    # make the bin look pretty
    df <- cbind(
      lapply(df[,1:3], prettyNum, big.mark = ',', scientific=FALSE),
      format(df[,4:7] , digits=2, scientific=FALSE),
      format(df[,8:10], digits=5, scientific=FALSE))
    print(df)
  })

setMethod("show", signature = "Classing",
  function(object) {
    lvls <- c("Discrete", "continuous")
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
    out <- merge(object@contribution, object@coef[-1], by=0, all=T)
    rownames(out) <- out$Row.names
    out$Row.names <- NULL


    cnt <- out[,1]/max(out[,1]) * 10
    out[,3] <- sapply(cnt, function(i) paste(rep("*", i), collapse=""))
    out[,3] <- format(out[,3], justify = "left")

    out <- cbind(New=ifelse(.new(object@classing[names(object@contribution)]),
                        "N", ""), out)

    out <- out[order(-out[,3]),]

    colnames(out) <- c("New", "Coefficient", "Contribution", "Importance")
    cat("\n")
    print(out)
  })

setMethod("show", signature = "Segmented-Classing",
  function(object) {
    cat("Segmented-Classing object\n")
    cat(sprintf("  |-- %5d segments\n", length(levels(object@segmentor))))

    ## loop over segments and show
    for (seg in levels(object@segmentor)) {
      border <- paste(c("+", rep("-", nchar(seg) + 11), "+"), collapse="")
      cat(sprintf("\n%2$s\n| Segment: %s |\n%2$s\n", seg, border))
      show(object@classings[[seg]])
    }
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
