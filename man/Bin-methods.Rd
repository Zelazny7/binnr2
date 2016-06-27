\name{bin-methods}
\alias{bin-methods}
\alias{Bin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bin methods for discretizing variables
}
\description{
Bin is a generic function that works differently depending on the inputs.
}
\usage{
Bin(x, y, w, seg, name, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    A numeric or factor variable or a \code{data.frame}.
    }
  \item{y}{
    A binary, numeric variable with values of 0 and 1 only.
  }
  \item{w}{
    An optional numeric vector of positive weights.
  }
  \item{seg}{
    An optional segment \code{factor}
  }
  \item{name}{
    Name of the bin. Automatically provided if binning a \code{data.frame}
  }
  \item{...}{
    Additional arguments passed on to the Bin method that control discretization. See
    details for available options
  }
}
\details{

Additional options can be passed to \code{Bin} to control the discretization process. These
arguments are as follows:

\itemize{
  \item{min.iv}{ The gain in information value for a split to occur. Defaults to 0.001.}
  \item{min.cnt}{ The minimum number of observations after a split.}
  \item{min.res}{ The minimum number of response after a split.}
  \item{mono}{ The direction of the monotonic relationship between the independent variable
  and the response. Splits that would violate this relationship are ignored. Valid values
  are:
  \itemize{
    \item{0}{ No monotonocity}
    \item{1}{ Increasing monotonicity}
    \item{-1}{ Decreasing monotonicity}
    \item{2}{ Either increasing or decreasing monotonicity. monotonicity is enforced by using
    the direction of the first, unconstrained split for all subsequent splits.}
    }
  }
  \item{exceptions}{ A numeric vector of special values, or exceptions, that are excluded
  from the discretization process. Exception values will always be held out as their own bins.}
  }
}

\value{
Bin will return various binned objects depending on the arguments provided:
\itemize{
  \item{\code{x = numeric or factor}}{ A single bin object of class \code{\link{continuous}} or
  \code{\link{Discrete}}.}
  \item{\code{x = data.frame}}{ A \code{\link{Classing}} object.}
  \item{\code{x = data.frame & seg = factor}}{ A \code{\link{Segmented-Classing}} object is returned.}
  }
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(mtcars)
y <- as.integer(mtcars$mpg > 20)
seg <- factor(mtcars$cyl)
Bin(mtcars$disp, y)
Bin(mtcars, y)
Bin(mtcars, y, seg=seg)
}
\keyword{ ~Bin }% use one of  RShowDoc("KEYWORDS")
