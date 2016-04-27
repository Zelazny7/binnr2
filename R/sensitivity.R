# ## From Xeno output
# # Odds Ratio moving from Level i (row i) to Level j (column j)
# # Odds Ratio in cell_i,j = exp ( ln(odds) weight for column j - ln(odds) weight for row i )
#
# ## measure the estimated change in odds from one bin to another
# sensitivity.analysis <- function(sc) {
#
#   ## limit to vars in model
#   v <- which(inmodel(sc))
#
#   coef <- sc@coef[-1]
#
#   ## get the WoE
#   woes <- mapply(function(b, coef) {
#     tmp <- head(as.data.frame(b)["WoE"], -1)
#     out <- tmp[,1]
#     names(out) <- row.names(tmp)
#     out
#   }, as(mod@classing, "list")[v], coef)
#
#
#   ## calculate the change in odds
#   s <- lapply(woes, function(x) {
#     tmp <- exp(outer(x, x, "-"))
#     -40*(1-tmp)/log(2) ## scale to points
#   })
#
#   ## add the marignal weights
#   out <- mapply(function(s, w){
#     tmp <- cbind(c(0, w), rbind(w, s))
#     row.names(tmp)[1] <- "Marginal Weight"
#     colnames(tmp)[1] <- "Marginal Weight"
#     tmp
#   }, s, woes)
# }
#
# ## for binnr 1 objects
# sensitivity.analysis2 <- function(sc) {
#
#   ## limit to vars in model
#   v <- names(which(sapply(sc$bins, function(x) x$meta$inmodel)))
#
#   coef <- sc$coef[-1]
#
#   ## get the WoE
#   woes <- mapply(function(b, coef) {
#     tmp <- head(binnr:::as.data.frame.bin(b)["WoE"], -1) * coef
#     out <- tmp[,1]
#     names(out) <- row.names(tmp)
#     out
#   }, sc$bins[v], coef[v])
#
#
#   ## calculate the change in odds
#   s <- lapply(woes, function(x) {
#     tmp <- exp(outer(x, x, "-"))
#     -40*log(tmp)/log(2) ## scale to points
#   })
#
#   ## add the marignal weights
#   out <- mapply(function(s, w){
#     tmp <- cbind(c(0, w), rbind(w, s))
#     row.names(tmp)[1] <- "Marginal Weight"
#     colnames(tmp)[1] <- "Marginal Weight"
#     tmp
#   }, s, woes)
#   out
# }
#
#
# ### blended model
#
# sensitiviy.blend <- lapply(blend.mods.final.v5, sensitivity.analysis2)
# sensitiviy.bus   <- lapply(bus.mods.final.v7, sensitivity.analysis2)
#
# ### get the max movement
# get.max <- function(s) {
#   nms <- row.names(s)
#   idx <- which.max(s)
#   val <- s[idx]
#   r <- row(s)[idx]; c <- col(s)[idx]
#   data.frame(max_shift = val, from = nms[c], to = nms[r], dist = r - c)
# }
#
# summary.blend <- lapply(sensitiviy.blend, function(x) do.call(rbind, lapply(x, get.max)))
# summary.bus <- lapply(sensitiviy.bus, function(x) do.call(rbind, lapply(x, get.max)))
#
# ## output to excel
# mk.sensitiviy.tab <- function(wb, name, ss, su) {
#   addWorksheet(wb, name)
#
#   ## write the summary
#   writeData(wb, name, su, startRow = 1, rowNames=TRUE)
#
#   ### loop over sensitivity and write to excel
#   n <- 3 + nrow(su)
#   for (i in seq_along(ss)) {
#     s <- ss[[i]]
#     writeData(wb, name, names(ss)[i], startRow = n)
#     writeData(wb, name, s, startRow = n + 1, rowNames = TRUE)
#     n <- n + nrow(s) + 3
#   }
#
# }
#
# library(openxlsx)
# wb <- createWorkbook()
# mk.sensitiviy.tab(wb, "Bus - LN & SBFE", sensitiviy.bus$`LN & SBFE`, summary.bus$`LN & SBFE`)
# mk.sensitiviy.tab(wb, "Bus - LN Only"  , sensitiviy.bus$`LN ONLY`, summary.bus$`LN ONLY`)
# mk.sensitiviy.tab(wb, "Blend - LN & SBFE"  , sensitiviy.blend$`LN & SBFE`, summary.blend$`LN & SBFE`)
# mk.sensitiviy.tab(wb, "Blend - LN Only"    , sensitiviy.blend$`LN ONLY`, summary.blend$`LN ONLY`)
# mk.sensitiviy.tab(wb, "Blend - Unscorable" , sensitiviy.blend$UNSCORABLE, summary.blend$UNSCORABLE)
#
#
# saveWorkbook(wb, file="F:/SBFE1508_5710/docs/variable_sensitivity_v2.xlsx", overwrite = T)
#
#
# #
# #
# # library(binnr)
# #
# #
# # bus.mods.final.v7$`LN & SBFE`
