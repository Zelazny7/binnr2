setMethod("sas", signature = "continuous",
  function(object, coef=1, method, pfx='') {

    values <- tail(head(object@cuts, -1), -1)

    v <- object@name
    p <- object@pred * coef
    nms <- names(p)

    ## prediction indices
    idx <- list(
      M=which(nms == "Missing"),
      E=which(nms %in% as.character(object@exceptions)),
      O=which(!nms %in% c("Missing", as.character(object@exceptions))))

    ## WoE Substitution
    c(sprintf("if missing(%1$s)\n  then %2$s_%1$s_w = %3$s;", v, pfx, p[idx$M]),

      sprintf("else if %1$s = %2$s\n  then %3$2_%1$s_w = %4$s;" , v,
              object@exceptions, pfx, p[idx$E]),

      sprintf("else if %1$s <= %2$s\n  then %3$s_%1$s_w = %4$s;" , v, values,
              pfx, head(p[idx$O], -1)),

      sprintf("else %s_%s_w = %s;" , pfx, v, tail(p[idx$O], 1)))

    ## Distance Calculations

    ## Adverse Action Codes
})

setMethod("sas", signature = "Discrete",
  function(object, coef=1, method, pfx) {

    values <- lapply(split(names(object@map), unlist(object@map)),
                     paste, collapse="','")

    v <- object@name
    p <- object@pred * coef
    nms <- names(p)

    idx <- list(M=which(nms == "Missing"), O=which(!nms =="Missing"))

    ## WoE Substitution
    c(sprintf("if missing(%1$s)\n  then %2$s_%1$s_w = %3$s;", v, pfx, p[idx$M]),
      sprintf("else if %1$s in ('%2$s')\n  then %3$s_%1$s_w = %4$s;" , v, values, pfx, p[idx$O]),
      sprintf("else %s_%s_w = 0;", pfx, v))
    ## exceptions

    ## normal values
  })
