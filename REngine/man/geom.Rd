\name{geom}
\alias{geom}
%%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create a geom object}
%%  ~~function to do ... ~~

\description{
Create a geom object (not a class). This is used in the SlopeStability function.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
geom(...)
}
%%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{...}{The different geometries of the different layers. For now,
    only one geometry may be used (will be adding more at a future
    date). Must be of the form 'tag = list(x = c(), y = c()) with x and
    y being the same length and specifying points.}
%%     ~~Describe \code{x} here~~
}

\details{
None
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Reutns a geom object. In this version, this is a data frame with x, y,
and the name of the geometry piece.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Ben Aldridge \email{bgaldridge@ucdavis.edu}
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{link{material}}
  \code{\link{SlopeStability}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
geo = geom(S1 = list(x = c(0,150,150,110,50,0), y = c(0,0,50,50,20,20)))
}
%% Add one or more standard keywords, see file 'KEYWORDS' in the
%% R documentation directory.
\keyword{geom}
