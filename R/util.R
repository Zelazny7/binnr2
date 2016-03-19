slots.to.list <- function(s4) {
  slotnames <- slotNames(s4)
  slotlist <- vector("list", length(slotnames))
  names(slotlist) <- slotnames
  for(i in slotnames) slotlist[[i]] <- slot(s4, i)
  slotlist
}

.bv <- function(y, Y, f) {
  N1  <- sum(y == 1)
  N0  <- sum(y == 0)
  P1  <- N1/sum(Y[f] == 1)
  P0  <- N0/sum(Y[f] == 0)
  WoE <- log(P1 / P0)
  IV  <- (P1 - P0) * WoE
  c(N=length(y), N1, N0, PN=length(y)/length(Y), P1, P0, N1/length(y), WoE, IV)
}

# weighted version of .bv
.bv2 <- function(y, w, Y, W, f) {
  N1  <- sum((y == 1) * w)
  N0  <- sum((y == 0) * w)
  P1  <- N1/sum((Y[f] == 1) * W[f])
  P0  <- N0/sum((Y[f] == 0) * W[f])
  WoE <- log(P1 / P0)
  IV  <- (P1 - P0) * WoE
  c(N=sum(w), N1, N0, PN=sum(w)/sum(W), P1, P0, N1/sum(w), WoE, IV)
}

#' @export
mono <- function(Object, val) {
  val <- if(val %in% c(-1,0,1,2)) val else 0
  Bin(x=Object, mono=val)
}

#' @export
reset <- function(Object) {
  Bin(Object)
}

#' @export
exception <- function(Object, val) {
  stopifnot(is(Object, "Continuous"))
  stopifnot(is.numeric(val))
  Bin(x=Object, exceptions=val)
}


## calcualte LR2 for logistic regression
.lr2 <- function(f, y) {
  f <- plogis(f)
  sum((y == 1)*log(f) + (y == 0)*log(1 - f))
}

## calculate score contributions for Scorecard
.contributions <- function(data, coefs, y) {
  base <- .lr2(0, y)
  lr2 <- sapply(1:length(coefs), function(i) {
    if (i > 1) {
      coefs[i] <- 0
    }
    1 - (.lr2(data %*% coefs[-1] + coefs[1], y)/base)
  })
  names(lr2) <- c("Base", names(coefs)[-1])
  lr2[1] - lr2[-1]
}

.ks <- function(score, y) {
  cml.bds <- cumsum(y[order(-score)] == 1) / sum(y == 1)
  cml.gds <- cumsum(y[order(-score)] == 0) / sum(y == 0)
  max(abs(cml.bds - cml.gds))
}

## print progress update bar to console
.progress <- function(i, max, text = "Progress") {
  progress <- paste(rep("=", (10*i/max)), collapse="")
  cat(sprintf("\r%s : %-10s|", text, progress))
}
