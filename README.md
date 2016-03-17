# binnr2
Refactoring binnr to S4. S4 provides multiple dispatch and a more rigorous class definition system.
This allows different functions to be called based on multiple input arguments and not just the first
one.

Otherwise, the main functionality is the same. The underlying "engine" is very different however. Binnr2
uses the same storage mechanism for both discrete and continuous variables which makes the rest of the
code easier to create, update, and maintain. It is also a lot easier to reason about.

## How to use

```
library(binnr2)

data(titanic)

# bin the data (notice the capital 'B')
# monotonicity and exceptions apply globally now
bins <- Bin(titanic[,-1] titanic[,1], mono=2, exceptions=-1)

# adjust the bins interactively
# press 'h' for list of commands -- not all of them are re-implemented
bins <- adjust(bins) 

# get a summary
# this takes longer because of the new underlying engine
su <- summary(bins

# fit the model
# uses the input dataset to fit, can pass new data
mod <- fit(bins)

# predict the score
scr.dev <- predict(mod)

# ... and on newdata
scr.val <- predict(mod, newdata = head(titanic))

# predict other things, too
woe <- predict(bins, type="woe")

predict(bins, type="bins")
```

## Advanced usage

Binnr2 also support segmented bins and models. These can be created, adjust, and fit in the same way.

```

# created segmented bins by Sex
bins <- Bin(titanic[,-1], titanic[,1], seg=titanic$Sex)

# this will prompt a choice of which segment
bins <- adjust(bins)

# fit all segments using LASSO
mods <- fit(bins)

# predict using dev data 
scr <- predict(mods)

# or on new data -- requires segment to be passed
scr.new <- predict(mods, head(titanic), seg=head(titanic$Sex))

```




