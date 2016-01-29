#' @include Bin.class.R

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
          (n)ext, (N)ext in model
          (p)revious, (P)revious in model
          (g)oto
          (m)ono
          (e)xceptions
          (s)et equal
          (u)ndo
          (r)eset
          (d)rop
          (a)ssign reason code
          (c)omment
          binnr bin operations
          != <#> : Neutralize level
          +  <#> : Expand level
          -  <#> : Collapse level(s)
          <= <#> : Cap at # and rebin\n")
        cat("Press any key to continue")
        readLines(n=1)
        invisible()
      } else if (command == "c") {
        cat("Enter a comment")
        inp <- readLines(n=1)
        invisible()
        if (inp == "DELETE") {
          # x[[i]]$notes <- NULL
        } else if (length(grep("\\S+", inp)) > 0) {
          # out[[i]]$notes <- paste(out[[i]]$notes, inp, sep="\n -")
        }
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
        cat("Goto variable:")
        v <- readLines(n = 1)
        # find the position of the variable
        while (!(v %in% c("","Q"))) {
          pos <- which(names(x@classing) == v)[1]
          if (is.na(pos)) {
            # find similar matches
            sim <- agrep(v, names(x@classing), ignore.case = T, max.distance = 0.1)
            if (length(sim) > 0){
              cat(sprintf("%s not found, similar matches:", v))
              cat(sprintf("\n %2d: %s", seq_along(sim), names(x)[sim]))
              cat("\nGoto variable:")
              inp <- readLines(n = 1)
              n <- suppressWarnings(as.integer(inp))
              if (!is.na(n) & n <= length(sim)) { # check if number entered
                v <- names(x@classing)[sim[n]]
              } else {
                v <- inp
              }
            } else {
              cat("No similar variables found")
              cat("\nHit [Enter] to continue")
              readLines(n=1)
              invisible()
              break
            }
          } else { # found exact match
            i <- pos
            break
          }
        }
      } else if (command == "d") {
        slot(x[[i]], "drop") <- !slot(x[[i]], "drop")
      } else if (command == "m") {
        cat("Enter Monotonicity:")
        v <- as.numeric(readLines(n = 1))
        x[[i]] <- mono(x[[i]], v)
      } else if (command == "e") {
        # cat("Enter Exceptions:")
        # v <- readLines(n = 1)
        # e <- eval(parse(text=v))
        # if (is.numeric(e) | is.null(e)) out[[i]] <- exception(out[[i]], e)
      } else if (command == "s") {
        cat("Enter Level(s) to Change:")
        v1 <- as.integer(readLines(n = 1))
        cat("Change WoE to which level?:")
        v2 <- as.integer(readLines(n = 1))
        x[[i]] <- set.equal(x[[i]], v1, v2)
      } else if (command == "n") {
        i <- i + 1
      } else if (command == "N") {
        # nv <- get.meta.attr(out, "new")
        # nvi <- which(nv) # index of the in-model
        # if (any(nv) & any(nvi > i)) i <- nvi[nvi > i][1]
      } else if (command == "p") {
        if (i > 1) {
          i <- i - 1
        } else {
          cat("\nAt beginning of list")
        }
      } else if (command == "P") {
        # nv <- get.meta.attr(out, "new")
        # nvi <- rev(which(nv)) # index of the last in model
        # if (any(nv) & any(nvi < i)) i <- nvi[nvi < i][1]
      } else if (command == "u") {
        #out[[i]] <- undo(out[[i]])
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
