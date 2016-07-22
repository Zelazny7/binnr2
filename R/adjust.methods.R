#' @include bin.class.R

#' @export
setMethod("adjust", "Segmented-Scorecard",
  function(x) {
    choices <- c(names(x@scorecards))
    i <- menu(choices, graphics = FALSE, title = "Select Segment")
    while(i) {
      x@scorecards[[i]] <- adjust(x@scorecards[[i]], header=names(x@scorecards[i]))
      i <- menu(choices, graphics = FALSE, title = "Select Segment")
    }
    x
  })

#' @export
setMethod("adjust", "Segmented-Classing",
  function(x) {
    choices <- c(names(x@classings))
    i <- menu(choices, graphics = FALSE, title = "Select Segment")
    while(i) {
      x@classings[[i]] <- adjust(x@classings[[i]], header=names(x@classings[i]))
      i <- menu(choices, graphics = FALSE, title = "Select Segment")
    }
    x
  })

#' @export
setMethod("adjust", "Scorecard",
  function(x, header=NULL) {
    initialize(x, classing=adjust(x@classing, header))
})

## function that calculates position in classing
.update.counters <- function(x, i) {
  ntot <- length(x)
  .new <- which(new.vars(x))
  .im  <- which(inmodel(x))
  .s2  <- which(steptwo(x))

  .imx <- match(i, .im,  nomatch=0)
  .inx <- match(i, .new, nomatch=0)
  .s2x <- match(i, .s2,  nomatch=0)

  txt <- "\nTotal [%d/%d] In Model [%d/%d] New [%d/%d] Step 2 [%d/%d]"
  sprintf(txt, i, ntot, .imx, length(.im), .inx, length(.new), .s2x, length(.s2))
}

#' @export
setMethod("adjust", "Classing",
  function(x, header=NULL) {

    ## check if first time through loop
    i <- 1
    bm <- slot(x, "bookmark")
    if (length(bm) > 0) {
      l <- readline(
        sprintf("Bookmark found. Resume at: %s? (y/n): ", bm))
      if (l == "y") {
        i <- if (l == "y") which(names(x@classing) == bm) else i
        if (length(i) == 0) i <- 1
      }
    }

    while(i <= length(x)) {

      ## Print everything ##
      cat("\014") # clear the console
      if (!is.null(header)) cat(sprintf("Segment: %s", header))
      print(x[[i]])
      plot(x[[i]])
      cat(.update.counters(x, i))
      cat ("\nEnter command (Q to quit):")

      command <- readLines(n = 1)
      if (command == "Q") {
        break
      }  else if (command %in% c("h", "help")) {
        cat(
          "binnr interactive commands:
          (Q)uit
          (n)ext (nn) new var (ns) step two
          (p)rev (pp) new var (ps) step two
          (g)oto
          (m)ono
          (e)xceptions
          (s)et equal
          penalty (f)actor
          (b)ookmark
          (u)ndo
          (r)eset
          (d)rop
          (a)pprove
          (c)ut points
          binnr bin operations
          != <#> : Neutralize level
          +  <#> : Expand level
          -  <#> : Collapse level(s)
          <= <#> : Cap at # and rebin\n")
        cat("Press any key to continue")
        readLines(n=1)
        invisible()
      } else if (command == "a") {
        approved(x)[i] <- !approved(x)[i]
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
      } else if (command == "b") {
        bm <- slot(x, "bookmark")
        if (length(bm) == 0) {
          slot(x, "bookmark") <- names(x@classing)[i]
        } else {
          l <- readline(
            sprintf("Bookmark already exists at: %s. Replace? (y/n): ", bm))
          if (l == "y") {
            slot(x, "bookmark") <- names(x@classing)[i]
          }
        }
      } else if (command == "c") {
        if (!is(x[[i]], "continuous")) {
          cat("Can only enter cut-points for continuous variables.")
        } else {
          cat("Enter cut-points separated by spaces:")
          cps <- as.numeric(strsplit(readline(), "\\s+")[[1]])
          x[[i]] <- set.cutpoints(x[[i]], cps)
        }
      } else if (command == "g") {
          cat("Goto variable:")
          v <- readLines(n = 1)
          n <- suppressWarnings(as.numeric(v))

          if (v == "b") {
            i <- which(names(x) == slot(x, "bookmark"))
          } else if (!is.na(n)) {
            i <- max(1, min(n, length(x)))
          } else {

            while (!(v %in% c("","Q"))) {
              pos <- which(names(x@classing) == v)[1]
              if (is.na(pos)) {
                # find similar matches
                sim <- agrep(v, names(x@classing), ignore.case = T, max.distance = 0.1)
                if (length(sim) > 0){
                  cat(sprintf("%s not found, similar matches:", v))
                  cat(sprintf("\n %2d: %s", seq_along(sim), names(x@classing)[sim]))
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
          }
      } else if (command == "d") {
        dropped(x[[i]]) <- !slot(x[[i]], "drop")
      } else if (command == "m") {
        cat("Enter Monotonicity:")
        v <- as.numeric(readLines(n = 1))
        x[[i]] <- mono(x[[i]], v)
      } else if (command == "e") {
        cat("Enter Exceptions:")
        v <- readLines(n = 1)
        x[[i]] <- exception(x[[i]], val = eval(parse(text=v)))
        # v <- readLines(n = 1)
        # e <- eval(parse(text=v))
        # if (is.numeric(e) | is.null(e)) {
        #
        # }
      } else if (command == "s") {
        cat("Enter Level(s) to Change:")
        v1 <- as.integer(readLines(n = 1))
        cat("Change WoE to which level?:")
        v2 <- as.integer(readLines(n = 1))
        x[[i]] <- set.equal(x[[i]], v1, v2)
      } else if (command == "n") {
        i <- i + 1
      } else if (command == "nn") {
        nv <- new.vars(x)
        nvi <- which(nv)
        if (any(nv) & any(nvi > i)) i <- nvi[nvi > i][1]
      } else if (command == "p") {
        if (i > 1) {
          i <- i - 1
        } else {
          cat("\nAt beginning of list")
        }
      } else if (command == "pp") {
        nv <- new.vars(x)
        nvi <- rev(which(nv)) # index of the last in model
        if (any(nv) & any(nvi < i)) i <- nvi[nvi < i][1]
      } else if (command == "ns") {
        ns <- steptwo(x)
        nsi <- which(ns)
        if (any(ns) & any(nsi > i)) i <- nsi[nsi > i][1]
      } else if (command == "ps") {
        ns <- steptwo(x)
        nsi <- rev(which(ns)) # index of the last in model
        if (any(ns) & any(nsi < i)) i <- nsi[nsi < i][1]
      } else if (command == "u") {
        if (length(x[[i]]@history) > 0) x[[i]] <- x[[i]]@history[[1]]
      } else if (command == "r") {
        x[[i]] <- Bin(x[[i]])
      } else if (command == "f") {
        cat("Enter Penalty Factor:")
        v <- as.integer(readLines(n = 1))
        penalty(x[[i]]) <- if (!is.na(v)) v else 1
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
