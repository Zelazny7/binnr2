slots.to.list <- function(s4) {
  slotnames <- slotNames(s4)
  slotlist <- vector("list", length(slotnames))
  names(slotlist) <- slotnames
  for(i in slotnames) slotlist[[i]] <- slot(s4, i)
  slotlist
}

# weighted version of .bv
.bv <- function(y, w, Y1, Y0, W, f) {
  N1  <- sum((y == 1) * w)
  N0  <- sum((y == 0) * w)
  # P1  <- N1/sum((Y == 1) * W)
  # P0  <- N0/sum((Y == 0) * W)
  P1  <- N1/Y1
  P0  <- N0/Y0
  WoE <- log(P1 / P0)
  IV  <- (P1 - P0) * WoE
  #c("N", "#1", "#0", "%N","%1","%0","P(1)","WoE","IV", "Pred")
  c(`N`=sum(w), `#1`=N1, `#0`=N0, `%N`=sum(w)/W, `%1`=N1/Y1, `%0`=N0/Y0,
    `P(1)`=N1/sum(w), WoE=WoE, IV=IV, Pred=WoE)
}

#' @export
mono <- function(Object, val) {
  if (!is(Object, "continuous")) {
    l <- readline("Cannot set monotonicity for discrete bins\nPress [enter] to continue")
    return(Object)
  } else if (!is.numeric(val)) {
    l <- readline("Monotonicity values must be numeric\nPress [enter] to continue")
    return(Object)
  } else {
    val <- if(val %in% c(-1,0,1,2)) val else 0

    ## get the new cuts
    b <- Bin(x=Object, mono=val)

    Object@history[[1]] <- Object
    Object@cuts <- b@cuts
    Update(Object)
  }
}

#' @export
reset <- function(Object) {
  Bin(Object)
}

#' @export
exception <- function(Object, val) {
  if (!is(Object, "continuous")) {
    l <- readline("Cannot set exception values for discrete bins\nPress [enter] to continue")
    return(Object)
  } else if (!is.numeric(val)) {
    l <- readline("Exception values must be numeric\nPress [enter] to continue")
    return(Object)
  } else {
    Object@exceptions <- val
    Update(initialize(Object))
  }
}


## calcualte LR2 for logistic regression
.lr2 <- function(f, y, w) {
  f <- plogis(f)
  sum((y == 1)*w*log(f) + (y == 0)*w*log(1 - f))
}

## calculate score contributions for Scorecard
.contributions <- function(data, coefs, y, w) {
  base <- .lr2(0, y, w)
  lr2 <- sapply(1:length(coefs), function(i) {
    if (i > 1) {
      coefs[i] <- 0
    }
    1 - (.lr2(data %*% coefs[-1] + coefs[1], y, w)/base)
  })
  names(lr2) <- c("Base", names(coefs)[-1])
  lr2[1] - lr2[-1]
}

.ks <- function(score, y, w) {
  cml.bds <- cumsum(((y == 1)*w)[order(-score)]) / sum((y == 1) * w)
  cml.gds <- cumsum(((y == 0)*w)[order(-score)]) / sum((y == 0) * w)
  max(abs(cml.bds - cml.gds))
}

## print progress update bar to console
.progress <- function(i, max, text = "Progress", extra="") {
  progress <- paste(rep("=", (10*i/max)), collapse="")
  cat(sprintf("\r%s : %-10s| %-50s", text, progress, extra))
}

