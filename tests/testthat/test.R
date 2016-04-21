## test various ways of predicting

data(titanic)

b1 <- Bin(titanic[,2:5], titanic$Survived)
b2 <- Bin(titanic[,6:8], titanic$Survived)

## combine them
b.all1 <- Bin(titanic[,-1], titanic$Survived)
b.all2 <- c(b1, b2) ## add check that when combining bins -- passes

print("Test 1")
test_that("concatenated bins are equal to full bins", {
  expect_equal(b.all1, b.all2)
})

## predicting WoE
woe1 <- predict(b.all1)
woe2 <- predict(b.all1, x=titanic)

## predicting bins
bins1 <- predict(b.all1, type="bins")
bins2 <- predict(b.all1, x=titanic, type="bins")

print("Test 2")
test_that("predictions are same with and without passed data", {
  expect_equal(woe1, woe2)
  expect_equal(bins1, bins2)
})


## fitting models
print("Test 3 - A")
set.seed(123)
mod1 <- fit(b.all1)
print("Test 3 - B")
set.seed(123)
mod2 <- fit(b.all1, x=titanic, y=titanic$Survived)
test_that("Fitted models are the same with and without passed data", {
  expect_equal(mod1, mod2)
})

## predicting models
p1 <- predict(mod1, type="score")
p2 <- predict(mod1, x=titanic, type="score")
test_that("Model predictions are the same with and without passed data", {
  expect_equal(p1, p2)
})

## segmented bins
seg <- Bin(titanic[,-1], titanic$Survived, seg=titanic$Pclass)

set.seed(123)
mod1 <- fit(seg)
set.seed(123)
mod2 <- fit(seg, x=titanic, y=titanic$Survived, seg=titanic$Pclass) ## fix this test

## make sure predictions are the same
p1 <- predict(mod1, type="score")
p2 <- predict(mod2, x=titanic, seg=titanic$Pclass, type="score")
all.equal(p1, p2)

woe1 <- predict(mod1, type="woe")
woe2 <- predict(mod2, x=titanic, seg=titanic$Pclass, type="woe")
all.equal(woe1, woe2)

bins1 <- predict(mod1, type="bins")
bins2 <- predict(mod2, x=titanic, seg=titanic$Pclass, type="bins")
all.equal(bins1, bins2)



