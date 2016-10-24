setMethod("Ops", c("Bin", "numeric"),
  function(e1, e2) {
    e1@history[[length(e1@history)]] <- e1
    callGeneric(e1=e1, e2=e2)
  })

setMethod("!=", signature = c("Bin", "numeric"),
  function(e1, e2) {
    e1@history[[1]] <- e1
    e2 <- pmax(pmin(e2, length(e1@pred)), 1)
    e1@pred[e2] <- 0
    e1
  })

setMethod("-", signature = c("continuous", "numeric"),
  function(e1, e2) {
    if (length(e2) == 1) return(e1)

    ## fill in gaps if first and last are selected, for example
    e2 <- seq(min(e2), max(e2))

    e1@history[[1]] <- e1
    ## make sure the requested collapse levels are within the acceptable range
    d <- unique(pmax(pmin(tail(e2, -1), length(e1@cuts) - 1), 2))
    e1@cuts <- e1@cuts[-(d)]

    e1@cache <- collapse_cache_numeric(e1@cuts, e1@cache, e2)
    e1@pred <- head(e1@cache$Pred, -1)
    e1

  })

setMethod("+", signature = c("continuous", "numeric"),
  function(e1, e2) {
    f <- ! (is.na(e1@x) | e1@x %in% e1@exceptions)

    a <- min(max(1, e2), length(e1@cuts))  # can't be smaller than 1
    z <- max(min(e2 + 1, length(e1@cuts)), a) # or larger than max els
    if (a == z) return(e1)
    e1@history[[1]] <- e1

    vals <- e1@x[e1@x > e1@cuts[a] & e1@x <= e1@cuts[z] & f]
    q <- unique(quantile(vals, seq(0, 1, 0.2))) # quintiles

    e1@cuts <- sort(c(e1@cuts[-z], q))
    Update(e1)
  })

setMethod("+", signature = c("Discrete", "numeric"),
  function(e1, e2) {
    e1@history[[1]] <- e1
    f <- e1@map %in% unique(e1@map)[e2]
    e1@map[f] <- levels(e1@x)[f]
    Update(e1)
  })

setMethod("-", signature = c("Discrete", "numeric"),
  function(e1, e2) {

    e1@history[[1]] <- e1

    f <- which(e1@map %in% unique(e1@map)[e2]) ## which values were selected for collapse?

    e1@map[f] <- paste(names(e1@map)[f], collapse=',') # collapse them with commas
    e1@cache <- collapse_cache_discrete(e1@map, e1@cache, f)
    e1@pred <- head(e1@cache$Pred, -1)
    e1
  })


aggregate_collapse <- function(d, i) {
  # browser()
  agg = apply(d[i,], 2, sum)
  agg["P(1)"] = agg["#1"] / agg["N"]
  agg["WoE"]  = log(agg["%1"] / agg["%0"])
  agg["IV"]   = agg["WoE"] * (agg["%1"] - agg["%0"])

  out = d[-i[-1],] ## drop all but the first one
  out[i[1],] <- agg ## replace with aggregated version
  out["Pred"] <- out["WoE"]
  out
}


## function that collapses a cached data.frame
collapse_cache_numeric <- function(cuts, d, i) {
  out = aggregate_collapse(d, i)
  rn <- c(fmt_numeric_cuts(cuts), "Missing")
  row.names(out) <- c(add_guides_to_rownames(rn), "Total")
  out["Total","IV"] <- sum(head(out[,"IV"], -1))
  out
}


collapse_cache_discrete <- function(map, d, i) {
  out = aggregate_collapse(d, i)
  rn <- c(unique(map), "Missing")
  row.names(out) <- c(add_guides_to_rownames(rn), "Total")
  out["Total","IV"] <- sum(head(out[,"IV"], -1))
  out
}


## set pred value equal to another
#' @export
set.equal <- function(b, v1, v2) {
  b@history[[1]] <- b
  b@pred[v1] <- b@pred[v2]
  b@cache$Pred[1:length(b@pred)] <- b@pred
  b
}

## manually set cut points for bin
#' @export
set.cutpoints <- function(b, cps) {
  b@history[[1]] <- b
  cps <- sort(unique(c(-Inf, cps, Inf)))
  b@cuts <- cps
  Update(b)
}


