setMethod("Ops", c("Bin", "numeric"),
  function(e1, e2) {
    print("In here!")
    e1@history[[length(e1@history)]] <- e1
    callGeneric(e1=e1, e2=e2)
  })

setMethod("-", signature = c("Continuous", "numeric"),
  function(e1, e2) {
    if (!all(diff(e2)==1)) return(e1)
    e1@history[[1]] <- e1
    ## make sure the requested collapse levels are within the acceptable range
    e2 <- unique(pmax(pmin(tail(e2, -1), length(e1@cuts) - 1), 2))
    e1@cuts <- e1@cuts[-(e2)]
    Update(e1)
  })

setMethod("+", signature = c("Continuous", "numeric"),
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
    f <- e1@map %in% e1@map[e2]
    e1@map[f] <- levels(e1@x)[f]
    Update(e1)
  })

setMethod("-", signature = c("Discrete", "numeric"),
  function(e1, e2) {
    e1@history[[1]] <- e1

        ## which values were selected for collapse?
    f <- which(e1@map %in% unique(e1@map)[e2])
    #f <- e1@map %in% names(e1@map)[e2]

    # collapse them with commas
    e1@map[f] <- paste(names(e1@map)[f], collapse=',')
    Update(e1)
  })


## set pred value equal to another
#' @export
set.equal <- function(b, v1, v2) {
  b@pred[v1] <- b@pred[v2]
  b
}





