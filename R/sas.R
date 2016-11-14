setMethod("sas", signature = "continuous",
  function(object, pfx='', coef=1, method, i) {

    val <- tail(head(object@cuts, -1), -1)

    v <- object@name
    p <- object@pred * coef

    ref <- switch(method,"min"=min(p),"max"=max(p),"neutral"=0)

    E <- object@exceptions
    m <- which(names(p) == "Missing")
    e <- which(names(p) %in% as.character(E))
    o <- seq_along(p)[-c(m, e)]

    ## WoE Substitution
    c(sprintf("\n/*** %s ***/", v),
      sprintf("if missing(%s)\n  then %s_V%02d_w = %s;", v, pfx, i, p[m]),
      sprintf("else if %s = %s\n  then %s_V%02d_w = %s;", v, E, pfx, i, p[e]),
      sprintf("else if %s <= %s\n  then %s_V%02d_w = %s;", v, val, pfx, i,
              head(p[o], -1)),
      sprintf("else %s_V%02d_w = %s;" , pfx, i, tail(p[o], 1)),

      ## Reason Codes
      sprintf("\nif missing(%s)\n  then %s_AA_code_%02d = \"&%s_AA_%02d\";", v, pfx, i, pfx, i),
      sprintf("else if %s = %s\n  then %s_AA_code_%02d = \"&%s_AA_%02d\";", v, E, pfx, i, pfx, i),
      sprintf("else if %s <= %s\n  then %s_AA_code_%02d = \"&%s_AA_%02d\";", v, val, pfx, i, pfx, i),
      sprintf("else %s_AA_code_%02d = \"&%s_AA_%02d\";" , pfx, i, pfx, i),

      ## Distance Calculations
      sprintf("\n%s_AA_dist_%02d = %s - %s_V%02d_w;", pfx, i, ref, pfx, i))
})


setMethod("sas", signature = "Discrete",
  function(object, pfx='', coef=1, method, i) {

    val <- lapply(split(names(object@map), unlist(object@map)),
                     paste, collapse="','")

    v <- object@name
    p <- object@pred[c("Missing", names(val))] * coef

    ref <- switch(method,"min"=min(p),"max"=max(p),"neutral"=0)

    o <- seq_along(p)[-1]

    ## WoE Substitution
    c(sprintf("\n/*** %s ***/", v),
      sprintf("if missing(%s)\n  then %s_V%02d_w = %s;", v, pfx, i, p[1]),
      sprintf("else if %s in ('%s')\n  then %s_V%02d_w = %s;", v, val, pfx, i, p[o]),
      sprintf("else %s_V%02d_w = 0;" , pfx, i),

      ## AA Code
      sprintf("\nif missing(%s)\n  then %s_AA_code_%02d = \"&%s_AA_%02d\";", v, pfx, i, pfx, i),
      sprintf("else if %s in ('%s')\n  then %s_AA_code_%02d = \"&%s_AA_%02d\";", v, val, pfx, i, pfx, i),
      sprintf("else %s_AA_code_%02d = \"&%s_AA_%02d\";" , pfx, i, pfx, i),

      ## AA Dist
      sprintf("\n%s_AA_dist_%02d = %s - %s_V%02d_w;", pfx, i, ref, pfx, i))
  })


setMethod("sas", signature = "Scorecard",
  function(object, pfx='', method="min") {

    v <- which(inmodel(object))
    coefs <- object@coef[-1]

    ## Print the reason code mappings
    out <- "/** Adverse Action Code Mappings **/"
    out <- c(out, lapply(seq_along(v), function(i) {
      sprintf("%%let %s_AA_%02d = \"\"; /** %s **/", pfx, i, names(object@classing)[v[i]])
    }))

    ### Print the variables
    out <- c(out, lapply(seq_along(v), function(i) {
      sas(object@classing[[v[i]]], pfx=pfx, coef=coefs[i], method=method, i=i)
    }))

    out <- c(out,
      sprintf("\n/*** Final Score Calculation ***/"),
      sprintf("%s_lgt = %s", pfx, object@coef[1]),
      sprintf("  + %s_V%02d_w", pfx, seq_along(v)),
      ";")

    unlist(out)
  })

setMethod("sas", signature = "Segmented-Scorecard",
  function(object, pfx='', method) {

    lvls <- paste0("s", gsub("\\W+", "_", levels(object@segmentor)))
    code <- mapply(sas, object@scorecards, lvls, MoreArgs=list(method=method))

    out <- lapply(names(code), function(seg) {
      header <- sprintf("*** Segment: %s ***;", seg)

      c(paste0("\n", paste(rep("*", nchar(header) - 1), collapse=""), ";"),
        header,
        paste0(paste(rep("*", nchar(header) - 1), collapse=""), ";\n"),
        code[[seg]])
    })

    out <- c(out,
      sprintf("\n*** Final Score Assignment ***;"),
      sprintf("if SEGVAR = '%s'\n   then final_score = %s_lgt;",
              levels(object@segmentor), lvls))
    unlist(out)
  })
