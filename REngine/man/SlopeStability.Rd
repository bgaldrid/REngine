\name{SlopeStability}
\alias{SlopeStability}
%%- Also NEED an '\alias' for EACH other topic documented here.
\title{Run a probabilistic slope stability analysis}
%%  ~~function to do ... ~~

\description{
Runs a probabilistic slope stability analysis according to
method.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SlopeStability(geom, materials, xrange, yrange, Rrange, n = 10, N = 10,
method = "oms", startX = mean(xrange), startY = mean(yrange), startR = mean(Rrange))
}
%%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{geom}{A geometry object (not a class yet, but hope to make it
    one soon!) created from geom().}
  \item{materials}{A materials object (not a class yet, but hope to make
  it one soon!) created from material(). As of this iteration of code,
  only one material may be used.}
  \item{xrange}{X range for optim to search for the lowest factor of
    safety. See details.}
  \item{yrange}{Y range for optim to search for the lowest factor of
    safety. See details.}
  \item{Rrange}{Radisu range for optim to search for the lowest factor of
    safety. See details.}
  \item{n}{The number of slices to seperate each failure plane
    into. Hope to automate this in the future (or at least give the
    option of automation).}
  \item{N}{The number of sets of each material property to iterate over
    to produce multiple factor of safeties.}
  \item{method}{Method to use to calculate the factor of safety. Right
    now, only one is available (oms). Will be adding Bishops and
    Spencers at a future date.}
  \item{startX}{The position to start optim at in the X direction.}
  \item{startY}{The position to start optim at in the Y direction.}
  \item{startR}{The position to start optim for the radius, R.}
  %%     ~~Describe \code{x} here~~
}

\details{
xrange and yrange specify a square area in which optim is allowed to
search. Rrange specifies the lengths of the radius allowed. As of this
iteration of the code, the user must be careful to limit the choices so
that they do not results in a boundry conflict (i.e. circle bigger then
geometry, the circular failure plane not entering the geometry, ect..).
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a single value which is the probability of failure.
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
%%  \code{\link{geom}}
%%  \code{\link{material}}
%%  \code{\link{optim}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#Creat the geometry
geo = geom(S1 = list(x = c(0,150,150,110,50,0), y = c(0,0,50,50,20,20)))

#Create the materials
materials = material(cohesion_eff = c(10,11,9,10,10,11,12,7,10,9)*1000,
FrictionAngle_eff = c(20,19,22,17,20,21,20,21,19), UnitWeight =
c(20,20.5,19,19.5,18.5,21,21,20,20.5)*1000)

#Run the simulation with 10*10*10 (=1000) iterations
temp = SlopeStability(geom = geo, materials = materials, xrange =
c(69,71), yrange = c(69,71), Rrange = c(59,61), N=10)

temp

}
%% Add one or more standard keywords, see file 'KEYWORDS' in the
%% R documentation directory.
\keyword{SlopeStability}
