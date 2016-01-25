setMethod("-", signature = c("Bin", "numeric"),
  function(e1, e2) {
    if (length(e2) <= 1) return(e1)
    e1@transform <- callGeneric(e1@transform, e2)
    e1
  }, valueClass = "Bin")

setMethod("-", signature = c("Continuous", "numeric"),
  function(e1, e2) {
    if (!all(diff(e2)==1)) return(e1)
    ## make sure the requested collapse levels are within the acceptable range
    e2 <- unique(pmax(pmin(tail(e2, -1), length(e1@cuts) - 1), 2))
    e1@cuts <- e1@cuts[-(e2)]
    e1
  })

setMethod("+", signature = c("Continuous", "numeric"),
  function(e1, e2) {
    f <- ! (is.na(e1@x) | e1@x %in% e1@exceptions)

    a <- min(max(1, e2), length(e1@cuts))  # can't be smaller than 1
    z <- max(min(e2 + 1, length(e1@cuts)), a) # or larger than max els
    if (a == z) return(e1)

    vals <- e1@x[e1@x > e1@cuts[a] & e1@x <= e1@cuts[z] & f]
    q <- unique(quantile(vals, seq(0, 1, 0.2))) # quintiles

    e1@cuts <- sort(c(e1@cuts[-z], q))
    e1
  })

setMethod("+", signature = c("Discrete", "numeric"),
  function(e1, e2) {
    f <- e1@map %in% e1@map[e2]
    e1@map[f] <- levels(e1@x)[f]
    e1
  })

setMethod("-", signature = c("Discrete", "numeric"),
  function(e1, e2) {
    ## create a local copy of the map
    ## which values were selected for collapse?
    f <- e1@map %in% names(e1@map)[e2]

    # collapse them with commas
    e1@map[f] <- paste(names(e1@map)[f], collapse=',')
    e1
  })
