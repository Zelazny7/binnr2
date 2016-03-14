#' @include bin.class.R

#' @export
setMethod("adjust", "Segmented-Scorecard",
  function(x) {
    choices <- c(names(x@scorecards))
    i <- menu(choices, graphics = TRUE, title = "Select Segment")
    while(i) {
      x@scorecards[[i]] <- adjust(x@scorecards[[i]])
      i <- menu(choices, graphics = TRUE, title = "Select Segment")
    }
    x
  })

#' @export
setMethod("adjust", "Segmented-Classing",
  function(x) {
    choices <- c(names(x@classings))
    i <- menu(choices, graphics = TRUE, title = "Select Segment")
    while(i) {
      x@classings[[i]] <- adjust(x@classings[[i]])
      i <- menu(choices, graphics = TRUE, title = "Select Segment")
    }
    x
  })

#' @export
setMethod("adjust", "Scorecard",
  function(x) {
    initialize(x, classing=adjust(x@classing))
})

#' @export
setMethod("adjust", "Classing",
  function(x) {
    i <- 1
    while(i <= length(x)) {
      cat("\014") # clear the console
      print(x[[i]])
      plot(x[[i]])

      cat ("\nEnter command (Q to quit):")
      command <- readLines(n = 1)
      if (command == "Q") {
        break
      }  else if (command %in% c("h", "help")) {
        cat(
          "binnr interactive commands:
          (Q)uit
          (n)ext, (N)ext new var
          (p)revious, (P)revious new var
          (g)oto
          (m)ono
          (e)xceptions
          (s)et equal
          (u)ndo
          (r)eset
          (d)rop
          (a)ssign reason code
          binnr bin operations
          != <#> : Neutralize level
          +  <#> : Expand level
          -  <#> : Collapse level(s)
          <= <#> : Cap at # and rebin\n")
        cat("Press any key to continue")
        readLines(n=1)
        invisible()
      } else if (command == "a") {
        # cat("Enter position(optional) and reason code")
        # inp <- readLines(n=1)
        # invisible()
        # type1 <- grep("\\d+\\s+\\S+", inp) # check for matching input
        # type2 <- grep("^\\s*\\S+\\s*$", inp) # check for matching input
        # if (length(type1) > 0) {
        #   inp <- strsplit(inp, "\\s+")
        #   pos <- as.integer(inp[[1]][1])
        #   aac <- (inp[[1]][2])
        #   rcs(out[[i]])[pos] <- aac
        # } else if (length(type2) > 0) {
        #   aac <- gsub("\\s", "", inp)
        #   rcs(out[[i]]) <- aac
        # }
      } else if (command == "g") {
        cat("Select a variable:")
        i <- menu(names(x@classing), graphics=TRUE, title = "Select Variable")
      } else if (command == "d") {
        drop(x[[i]]) <- !slot(x[[i]], "drop")
      } else if (command == "m") {
        cat("Enter Monotonicity:")
        v <- as.numeric(readLines(n = 1))
        x[[i]] <- mono(x[[i]], v)
      } else if (command == "e") {
        cat("Enter Exceptions:")
        v <- readLines(n = 1)
        e <- eval(parse(text=v))
        if (is.numeric(e) | is.null(e)) x[[i]] <- Bin(x[[i]], exceptions=e)
      } else if (command == "s") {
        cat("Enter Level(s) to Change:")
        v1 <- as.integer(readLines(n = 1))
        cat("Change WoE to which level?:")
        v2 <- as.integer(readLines(n = 1))
        x[[i]] <- set.equal(x[[i]], v1, v2)
      } else if (command == "n") {
        i <- i + 1
      } else if (command == "N") {
        nv <- .new(x)
        nvi <- which(nv)
        if (any(nv) & any(nvi > i)) i <- nvi[nvi > i][1]
      } else if (command == "p") {
        if (i > 1) {
          i <- i - 1
        } else {
          cat("\nAt beginning of list")
        }
      } else if (command == "P") {
        nv <- .new(x)
        nvi <- rev(which(nv)) # index of the last in model
        if (any(nv) & any(nvi < i)) i <- nvi[nvi < i][1]
      } else if (command == "u") {
        if (length(x[[i]]@history) > 0) x[[i]] <- x[[i]]@history[[1]]
      } else if (command == "r") {
        x[[i]] <- Bin(x[[i]])
      } else {
        tryCatch({
          x[[i]] <- eval(parse(text=paste("x[[i]]", command)))
        }, error = function(err) {
          cat("\nInvalid command entered")
        })
      }
    }
    return(x)
  })
