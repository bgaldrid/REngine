\name{material}
\alias{material}
%%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create a material object}
%%  ~~function to do ... ~~

\description{
Create a material object (not a class). This is used in the SlopeStability function.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
material(...)
}
%%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{...}{The different material properties the different materials. For now,
    only one material may be used in SlopeStability (will be adding more at a future
    date). Must be of the form 'propertyname = c()' with c() holding all
    sample values for the property. For SlopeStability, the properties
    "cohesion_eff" (in pascals), "FrictionAngle_eff" (in degrees), and
    "UnitWeight" (in N/m^3) are
    needed. All units should be standard metric.}
%%     ~~Describe \code{x} here~~
}

\details{
None
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Reutns a materials object. In this version, this is a data frame with
the mean, sd, skewness, and kurtosis of the data property.

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
%%  \code{link{geom}}
%%  \code{\link{SlopeStability}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
materials = material(cohesion_eff = c(10,11,9,10,10,11,12,7,10,9)*1000,
FrictionAngle_eff = c(20,19,22,17,20,21,20,21,19), UnitWeight =
c(20,20.5,19,19.5,18.5,21,21,20,20.5)*1000)

}
%% Add one or more standard keywords, see file 'KEYWORDS' in the
%% R documentation directory.
\keyword{material}
