## test various ways of predicting
data(titanic)
tests <- list()

### bin level tests
## Dummy data
y <- sample(c(0,1), 1000, replace = T)
w <- rep(1, 1000)
v1 <- factor(sample(letters[1:10], 1000, replace = T))
df <- data.frame(v1, y, w)
## End dummy data

b1 <- Bin(df$v1, df$y, max.bin=100, min.iv=0)
b2 <- by(df, v1, function(x) {
  binnr2:::.bv(x$y, x$w, df$y, df$w)
})

b2 <- do.call(rbind, b2)
comp1 <- sort(b2[,8], decreasing = T)
names(comp1) <- NULL
comp2 <- head(as.data.frame(b1)$WoE, -2)
tests[["Binned data is equal to manually calculated version"]] <- all.equal(comp1, comp2)

## check that WoE-subbed vars are the same
p1 <- predict(b1)
p2 <- lapply(split(df, df$v1), function(x) {
  tmp <- binnr2:::.bv(x$y, x$w, df$y, df$w)
  rep(tmp[8], nrow(x))
})
p2 <- unsplit(p2, df$v1)
names(p2) <- df$v1
tests[["WoE substitutions are the same for Binnr and manual versions"]] <- all.equal(p1, p2)

## check that WoE-subbed vars are the same with new data
y2 <- sample(c(0,1), 500, replace = T)
v2 <- factor(sample(letters[1:10], 500, replace = T))

pred <- c(b2[,8], "Missing"=0)

p1 <- predict(b1, v2)
idx <- as.character(v2)
idx[is.na(idx)] <- "Missing"
p2 <- pred[idx]
tests[["WoE substitutions are the same for Binnr and manual versions w/new data"]] <-
  all.equal(p1, p2)

## now do it with missing values
y2 <- sample(c(0,1), 500, replace = T)
v2 <- factor(sample(letters[1:10], 500, replace = T))
v2[50:100] <- NA

p1 <- predict(b1, v2)
idx <- as.character(v2)
idx[is.na(idx)] <- "Missing"
p2 <- pred[idx]
tests[["WoE substitutions are the same for Binnr and manual versions w/missing data"]] <-
  all.equal(p1, p2)

## add some ""
v3 <- factor(sample(letters[1:10], 500, replace = T))
levels(v3) <- c(levels(v3), " ")
v3[50:100] <- " "

p1 <- predict(b1, v2)
idx <- as.character(v2)
idx[is.na(idx)] <- "Missing"
p2 <- pred[idx]
tests[["WoE substitutions are the same for Binnr and manual versions w/blank data"]] <-
  all.equal(p1, p2)


### BINNING TESTS ###


b1 <- Bin(titanic[,2:5], titanic$Survived)
b2 <- Bin(titanic[,6:8], titanic$Survived)

## combine them
b.all1 <- Bin(titanic[,-1], titanic$Survived)
b.all2 <- c(b1, b2) ## add check that when combining bins -- passes

tests[["concatenated bins are equal to full bins"]] = all.equal(b.all1, b.all2)

## predicting WoE
woe1 <- predict(b.all1)
woe2 <- predict(b.all1, x=titanic)

## predicting bins
bins1 <- predict(b.all1, type="bins")
bins2 <- predict(b.all1, x=titanic, type="bins")

tests[["WoE predictions are same with and without passed data"]] <-
  all.equal(woe1, woe2)
tests[["bin predictions are same with and without passed data"]] <-
  all.equal(bins1, bins2)


## fitting models
set.seed(123)
mod1 <- fit(b.all1)
set.seed(123)
mod2 <- fit(b.all1, x=titanic, y=titanic$Survived)
tests[["Fitted models are the same with and without passed data"]]=
       all.equal(mod1, mod2)

## predicting models
p1 <- predict(mod1, type="score")
p2 <- predict(mod1, x=titanic, type="score")
tests[["Model predictions are the same with and without passed data"]]=
       all.equal(p1, p2)

## segmented bins
seg <- Bin(titanic[,-1], titanic$Survived, seg=titanic$Pclass)

set.seed(123)
mod1 <- fit(seg)
set.seed(123)
mod2 <- fit(seg, x=titanic, y=titanic$Survived, seg=titanic$Pclass) ## fix this test

tests[["Segmented models are the same with and without datasets"]]=
  all.equal(mod1, mod2)

## make sure predictions are the same
p1 <- predict(mod1, type="score")
p2 <- predict(mod2, x=titanic, seg=titanic$Pclass, type="score")
tests[["Segmented model predicted scores are the same with and without datasets"]]=
  all.equal(p1, p2)

woe1 <- predict(mod1, type="woe")
woe2 <- predict(mod2, x=titanic, seg=titanic$Pclass, type="woe")
tests[["Segmented model predicted WoE are the same with and without datasets"]]=
  all.equal(woe1, woe2)

bins1 <- predict(mod1, type="bins")
bins2 <- predict(mod2, x=titanic, seg=titanic$Pclass, type="bins")
tests[["Segmented model predicted bins are the same with and without datasets"]]=
  all.equal(bins1,bins2)

print("Passed Tests")
print(names(tests)[unlist(tests)])

print("Failed Tests")
print(names(tests)[!unlist(tests)])


