\name{braidReports-package}
\alias{braidReports-package}
\alias{braidReports}
\docType{package}
\title{
Visualize Combined Action Response Surfaces and Report BRAID Analyses
}
\description{
Building on the \code{ggplot2} plotting package, this package provides functions to generate,
format, and style surface plots for visualizing combined action data without resorting to
three-dimensional surface plots, which can be difficult to interpret.  The package also provides
functions for reporting on a BRAID analysis, including plotting curve-shifts, calculating IAE
values, and producing full BRAID analysis reports.
}
\details{
\tabular{ll}{
Package: \tab braidReports\cr
Type: \tab Package\cr
Version: \tab 0.5.3\cr
Date: \tab 2016-04-05\cr
License: \tab GPL (>=3)\cr
}
The heart of the package is the function \code{\link{responseMap}}, which produces a \code{ggplot2}
tile or surface plot from combined action data, consolidating many of the minutiae of plotting such
surfaces into a single, easily used function.  The functions \code{\link{formatResponseMap}} and
\code{\link{formatDifferenceMap}} provide pre-built formatting and themes for quickly generating
publishable or shareable activity plots and error plots.  In addition, several functions provide
additional analytical and reporting options for BRAID analyses (from the package \code{braidrm}).
The function \code{\link{calculateIAE}} estimates the combined therapeutic window for a pair of
active compounds; the function \code{\link{potentiationPlot}} produces fast and easy curve-shift
plots to visualize the potentiation of one compound by the presence of another; and
\code{\link{makeBRAIDreport}} serves as a (nearly) fully-functional apparatus for translating
a BRAID analysis into a one-page comprehensive report.
}
\author{
Nathaniel R. Twarog <nathaniel.twarog@stjude.org>
}
\references{
Twarog, N.R., Stewart, E., Vowell Hamill, C., and Shelat, A.  BRAID: A Unifying Paradigm for the 
Analysis of Combined Drug Action. \emph{Scientific Reports} \strong{In Press} (2016).
}
\keyword{ package }
\seealso{
\code{\link{braidrm-package}}
}
