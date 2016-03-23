\name{calculateIAE}
\alias{calculateIAE}
\title{
Calculate the Index of Achievable Efficacy
}
\description{
Calculates the index of achievable efficacy, or IAE, (a measure of a drug combination's efficacy
relative to toxicological and pharmacological constraints) based on a response surface
representation of a combination in the form of a full BRAID surface parameter vector.
}
\usage{
calculateIAE(parv, lev, macs, same = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{parv}{ a full 10-element BRAID surface parameter vector }
  \item{lev}{ a single value or vector representing the effect level or levels at which the IAE
should be calculated }
  \item{macs}{ the maximum achievable concentrations of the two drugs whose effects are represented
by the response surface. A single value may be given, which will be used for both drugs }
  \item{same}{ a boolean variable representing whether the two drugs in the combination are
believed to be the same drug (their dose-response parameters need not be identical).  If
\code{TRUE}, the values in \code{macs} must be identical }
}
\details{
The IAE is a measure of the aggregate ratio of achievable dose-pairs for a given combination and
the minimum dose pairs required to achieve a given effect.  Formally, it is defined as
\deqn{IAE=\left(\frac{\int_{AC}{dD_A dD_B}}{\int_{AC}{\left(1-H\left(\frac{E_{AB}(D_A,D_B)-E}{E_f-E}\right)\right)dD_A dD_B}}\right)^{1/2}}{%
IAE = (Int(AC)[dDA dDB]/Int(AC)[(1-H[(EAB(DA,DB)-E)/(Ef-E)])dDA dDB])^(1/2)}
where \eqn{AC} is the space of all achievable dose pairs as determined by pharmacological or
toxicological constraints.  In this function, this space is represented either as a rectangular
region in dose-pair-space bounded by the two values in the parameter \code{macs} (when \code{same}
is \code{FALSE}) or the lower triangular region of dose-pair-space in which the sum of the two
doses is less than the single value in \code{macs} (when \code{same} is \code{TRUE}).
}
\value{
A single value or vector of values containing the estimated IAE for the response surface at the
effect level or levels specified in \code{lev}.
}
\references{
Twarog, N.R., Stewart, E., Vowell Hamill, C., and Shelat, A.  BRAID: A Unifying Paradigm for the 
Analysis of Combined Drug Action. \emph{Scientific Reports} \strong{In Press} (2016).
}
\author{
Nathaniel R. Twarog
}

\seealso{
\code{\link{makeBRAIDreport}}
}
\examples{
data(es8analysis)
bfit <- es8analysis$braidFit
# Modelled effect is base-10 logarithm of cell survival relative to negative controls,
# so a level of -2 reflects 99% cell killing
calculateIAE(bfit$fullpar,c(-1,-2),c(1.015*10^-6,5*10^-5))
}

\keyword{ misc }
