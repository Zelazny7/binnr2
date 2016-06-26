fit <- function(object, x, y, w, fixed=FALSE, nfolds=3, lower.limits=0, upper.limits=3,
                family="binomial", alpha=1, keep=TRUE, ...) {
  UseMethod("fit")
}

fit.classing <- function(obj, data=NULL, y=NULL, w=NULL, fixed=FALSE, nfolds=3,
                         lower.limits=0, upper.limits=3, family="binomial", alpha=1,
                         keep=TRUE, ...) {


  #browser()
  if (is.null(data)) data <- as.data.frame(obj)
  if (is.null(y)) y <- attr(obj, "y")
  if (is.null(w)) w <- attr(obj, "w")

  ## stopifnots
  if (nrow(data) != length(y)) stop("data and y must be the same length")
  if (length(y) != length(w)) stop("y and w must be the same length")

  k <- !get.meta.attr(obj, "drop")
  woe <- data.matrix(predict(obj[k], data=x, type="woe"))

  ## set the penalty factor for fixed vars
  im <- get.meta.attr(obj, "inmodel")
  pf <- rep(1, length(obj))
  if (fixed) pf[im] <- 0

  fit <- glmnet::cv.glmnet(
    x=woe, y=y, weights=w, nfolds=nfolds, lower.limits=lower.limits,
    upper.limits=upper.limits, family=family, alpha=alpha, keep=keep,
    penalty.factor=pf[k], ...)

  coefs <- coef(fit, s="lambda.min")[,1]
  nz <- which(coefs[-1] != 0)
  coefs <- coefs[coefs != 0]

  #contributions <- .contributions(woe[,names(coefs)[-1]], coefs, y, w)

  ## flag vars as in the model
  obj <- set.meta.attr(obj, "inmodel", FALSE)
  obj[nz[-1]] <- set.meta.attr(obj[nz[-1]], "inmodel", TRUE)

  ## flag the new vars
  obj <- set.meta.attr(obj, "nu", FALSE)
  nu <- get.meta.attr(obj, "nu") &
  obj[nz[-1]] <- set.meta.attr(obj[nz[-1]], "inmodel", TRUE)

  #new.vars(object)[which(!im & inmodel(object))] <- TRUE

  obj

}
#
# function( {
#
#   # browser()
#   stopifnot(NROW(x) == NROW(y))
#
#   if (missing(w)) w <- rep(1, NROW(x))
#
#   ## get vector of keep vars
#   k <- !dropped(object)
#   woe <- data.matrix(.predict(object[k], x=x, type="woe"))
#
#   ## set the penalty factor for fixed vars
#   im <- inmodel(object)
#   pf <- rep(1, length(object))
#   if (fixed) pf[im] <- 0
#
#   fit <- glmnet::cv.glmnet(woe, y, weights=w, nfolds=nfolds,
#                            lower.limits=lower.limits,
#                            upper.limits=upper.limits, family=family,
#                            alpha=alpha, keep = keep, penalty.factor = pf[k], ...)
#
#   coefs <- coef(fit, s="lambda.min")[,1]
#   coefs <- coefs[coefs != 0]
#   contributions <- .contributions(woe[,names(coefs)[-1]], coefs, y, w)
#
#   ## flag vars as in the model
#   inmodel(object)[] <- FALSE
#   inmodel(object)[names(coefs)[-1]] <- TRUE
#
#   ## flag the new vars
#   new.vars(object)[] <- FALSE
#   new.vars(object)[which(!im & inmodel(object))] <- TRUE
#
#   ## step two predictors
#   ## Find at least the next 10 variables that would have come in
#   steptwo(object)[] <- FALSE
#   d <- fit$glmnet.fit$df
#   idx <- which(d[which.min(fit$cvm)] + 10 < d)[1]
#   if (is.na(idx)) idx <- length(d)
#   lambda <- fit$lambda[min(idx, length(fit$lambda))]
#   step2 <- coef(fit, s=lambda)[,1]
#   step2 <- names(step2[step2 != 0])[-1]
#   steptwo(object)[setdiff(step2, names(coefs)[-1])] <- TRUE
#
#   ## Reorder the bins ##
#   vbest  <- names(sort(contributions, decreasing = T))
#   vstep2 <- setdiff(step2, names(coefs)[-1])
#   vrest  <- setdiff(names(object), c(vbest, vstep2))
#
#   ## drop non step 1 and step 2
#   dropped(object)[vrest] <- TRUE
#
#   ord <- c(vbest, vstep2, vrest)
#
#   ## calculate performance metrics
#   if (keep) {
#     ks <- .ks(fit$fit.preval[,which.min(fit$cvm)], y, w) # kfold
#   } else {
#     ks <- .ks(woe[,names(coefs)[-1]] %*% coefs[-1] + coefs[1], y, w) # dev
#   }
#
#   new("Scorecard", fit=fit, classing=object[ord], y=y, coef=coefs,
#       contribution=contributions, performance=ks)
# })
