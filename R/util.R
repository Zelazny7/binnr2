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

#' @export
mono <- function(Object, val) {
  val <- if(val %in% c(-1,0,1,2)) val else 0
  Bin(x=Object, mono=val)
}

#' @export
reset <- function(Object) {
  Bin(x=Object@x, y=Object@y)
}

#' @export
exception <- function(Object, val) {
  stopifnot(is(Object, "Continuous"))
  stopifnot(is.numeric(val))
  Bin(x=Object, exceptions=val)
}
