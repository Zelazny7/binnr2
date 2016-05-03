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
#     round(-40*(1-tmp)/log(2)) ## scale to points
#   })
#
#   s
#
#   ## add the marignal weights
#   # out <- mapply(function(s, w){
#   #   tmp <- cbind(c(0, w), rbind(w, s))
#   #   row.names(tmp)[1] <- "Marginal Weight"
#   #   colnames(tmp)[1] <- "Marginal Weight"
#   #   tmp
#   # }, s, woes)
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
#     out <- round(-40*log(tmp)/log(2)) ## scale to points
#
#     colnames(out) <- paste("Bin", 1:NCOL(out))
#     rownames(out) <- paste("Bin", 1:NROW(out))
#     out
#   })
#
#   ## add the marignal weights
#   # out <- mapply(function(s, w){
#   #   tmp <- cbind(c(0, w), rbind(w, s))
#   #   row.names(tmp)[1] <- "Marginal Weight"
#   #   colnames(tmp)[1] <- "Marginal Weight"
#   #   tmp
#   # }, s, woes)
#   s
# }
#
# mod.gov.names <- list()
# mk.names <- function(l, mod.name) {
#   tmp <- readClipboard()
#   tmp <- strsplit(tmp, "\\t")
#   l[[mod.name]] <- lapply(tmp, '[[', 2)
#   names(l[[mod.name]]) <- sapply(tmp, '[[', 1)
#   l
# }
#
# # mod.gov.names <- mk.names(mod.gov.names, "SBOM - LN & SBFE")
# # mod.gov.names <- mk.names(mod.gov.names, "SBOM - LN ONLY")
# # mod.gov.names <- mk.names(mod.gov.names, "SBBM - LN & SBFE")
# # mod.gov.names <- mk.names(mod.gov.names, "SBBM - LN ONLY")
# # mod.gov.names <- mk.names(mod.gov.names, "SBBM - UNSCORABLE")
#
# # save(mod.gov.names, file="Z:/User Folders/Eric G/SBFE Customer Data/Final Models/121815/mod.gov.names.rda")
#
# load("Z:/User Folders/Eric G/SBFE Customer Data/Final Models/121815/final_models_wRCs.rData")
# load(file="Z:/User Folders/Eric G/SBFE Customer Data/Final Models/121815/mod.gov.names.rda")
#
# ### blended model
# sensitiviy.blend <- lapply(blend.mods.final.v5, sensitivity.analysis2)
# names(sensitiviy.blend$`LN & SBFE`) <- mod.gov.names$`SBBM - LN & SBFE`[names(sensitiviy.blend$`LN & SBFE`)]
# names(sensitiviy.blend$`LN ONLY`) <- mod.gov.names$`SBBM - LN ONLY`[names(sensitiviy.blend$`LN ONLY`)]
# names(sensitiviy.blend$UNSCORABLE) <- mod.gov.names$`SBBM - UNSCORABLE`[names(sensitiviy.blend$UNSCORABLE)]
#
# sensitiviy.bus   <- lapply(bus.mods.final.v7, sensitivity.analysis2)
# names(sensitiviy.bus$`LN & SBFE`) <- mod.gov.names$`SBOM - LN & SBFE`[names(sensitiviy.bus$`LN & SBFE`)]
# names(sensitiviy.bus$`LN ONLY`) <- mod.gov.names$`SBOM - LN ONLY`[names(sensitiviy.bus$`LN ONLY`)]
#
#
# ## change the names of the lists
#
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
#
# library(openxlsx)
# grayStyle <- createStyle(fgFill = "#D3D3D3", border = "TopBottomLeftRight")
#
# ## output to excel
# mk.sensitiviy.tab <- function(wb, name, ss) {
#   addWorksheet(wb, name)
#
#
#   ## write the summary
#   # writeData(wb, name, su, startRow = 1, rowNames=TRUE)
#
#   ### loop over sensitivity and write to excel
#   #n <- 3 + nrow(su)
#   n <- 1
#   for (i in seq_along(ss)) {
#     s <- ss[[i]]
#
#     header <- as.data.frame(t(c(names(ss)[i], rep("", NCOL(s)))))
#
#     writeData(wb, name, header, startRow = n, borders="surrounding",
#               colNames = FALSE)
#
#     writeData(wb, name, s, startRow = n + 1, rowNames = TRUE, borders = "all")
#
#     ## gray out the header and diagonal
#     addStyle(wb, name, style=grayStyle, rows = n + 1, cols = 1:ncol(s) + 1)
#     addStyle(wb, name, style=grayStyle, rows = (n + 1):(n + 1 + nrow(s)), cols = 1)
#
#     for (i in 1:ncol(s) + 1) {
#       addStyle(wb, name, style=grayStyle, rows = n + i, cols = i)
#     }
#
#     n <- n + nrow(s) + 3
#   }
#
# }
#
# # library(openxlsx)
# wb <- createWorkbook()
# mk.sensitiviy.tab(wb, "Bus - LN & SBFE", sensitiviy.bus$`LN & SBFE`)
# mk.sensitiviy.tab(wb, "Bus - LN Only"  , sensitiviy.bus$`LN ONLY`)
# mk.sensitiviy.tab(wb, "Blend - LN & SBFE"  , sensitiviy.blend$`LN & SBFE`)
# mk.sensitiviy.tab(wb, "Blend - LN Only"    , sensitiviy.blend$`LN ONLY`)
# mk.sensitiviy.tab(wb, "Blend - Unscorable" , sensitiviy.blend$UNSCORABLE)
#
#
# saveWorkbook(wb, file="F:/SBFE1508_5710/docs/variable_sensitivity_v3.xlsx", overwrite = T)
#
#
# #
# #
# # library(binnr)
# #
# #
# # bus.mods.final.v7$`LN & SBFE`
