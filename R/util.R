slots.to.list <- function(s4) {
  slotnames <- slotNames(s4)
  slotlist <- vector("list", length(slotnames))
  names(slotlist) <- slotnames
  for(i in slotnames) slotlist[[i]] <- slot(s4, i)
  slotlist
}

# weighted version of .bv
.bv <- function(y, w, Y, W, f) {
  N1  <- sum((y == 1) * w)
  N0  <- sum((y == 0) * w)
  P1  <- N1/sum((Y == 1) * W)
  P0  <- N0/sum((Y == 0) * W)
  WoE <- log(P1 / P0)
  IV  <- (P1 - P0) * WoE
  c(N=sum(w), N1, N0, PN=sum(w)/sum(W), P1, P0, N1/sum(w), WoE, IV)
}

#' @export
mono <- function(Object, val) {
  val <- if(val %in% c(-1,0,1,2)) val else 0
  b <- Bin(x=Object, mono=val)
  b@history[[1]] <- Object
  b
}

#' @export
reset <- function(Object) {
  Bin(Object)
}

#' @export
exception <- function(Object, val) {
  stopifnot(is(Object, "continuous"))
  stopifnot(is.numeric(val))
  Bin(x=Object, exceptions=val)
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
  cat(sprintf("\r%s : %-10s| %s", text, progress, extra))
}



