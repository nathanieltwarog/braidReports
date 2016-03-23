\name{responseMap}
\alias{responseMap}
\alias{responseMap.default}
\alias{responseMap.formula}
\title{
Visualizing Combination Response Maps
}
\description{
Produces a \code{ggplot2} plot depicting some response variable as a function of
two input variables; generally this will be some biological effect as a function
of two compound concentrations.
}
\usage{
\method{responseMap}{default}(model,data,irreg=FALSE,interpolate=TRUE,margins=TRUE,
			logscale=TRUE,zlims=NULL,raster=TRUE)
\method{responseMap}{formula}(model,data,...)
}
\arguments{
  \item{model}{ a two-column array containing the values of input 1 and input 2 in each data point, or a symbolic formula
(e.g. \code{act ~ conc1+conc2}) specifying which variables are to be visualized }
  \item{data}{ if \code{model} is an array, a vector of values in response to inputs 1 and 2; if \code{model} is a formula,
a data frame containing the columns specified in \code{formula} }
  \item{irreg}{ reflects whether or not data points lie in a regularly-spaced grid in the space of data points to be plotted }
  \item{interpolate}{ determines whether the plot will produce a smoothed response surface using Gaussian interpolation, or
if the space will be visualized as a set of discrete measurements using nearest neighbor segmentation }
  \item{margins}{ determines whether input pairs in which one of the two inputs is zero will be plotted; applies only
to logarithmically scaled plots }
  \item{logscale}{ determines whether the input variables will be plotted on a logarithmic scale, or a standard linear scale.
Because this function is intended primarily for visualizing combined action dose-response, a logarithmic dose-pair space is
the default. }
  \item{zlims}{ an optional parameter specifying lower and upper bounds on the response variable.  If present, all effect
values outside the given range will be clipped to the minimum and maximum values }
  \item{raster}{ by default, interpolated response surfaces will be rendered using the \code{ggplot2} function
\code{geom_raster}, a faster and optimized raster image version of \code{geom_tile}.  In certain settings, however,
\code{geom_raster} can produce unusual appearance.  Setting this parameter to FALSE will force the plot to be rendered
using \code{geom_tile} }
  \item{\dots}{ not used }
}
\details{
This function produces and returns a \code{ggplot2} plot, but does not draw it to a graphics device.  As the strength of
\code{ggplot2} is in the ability to annotate, build on, and style plots using additional layers and themes, this function
adds no theme information, and very little scale information.  To produce a more aesthetic plot, it is recommended that
plot be given a custom color scale, axis labels, and theme by using the appropriate \code{ggplot2} functions.  For convenience,
the functions \code{\link{formatResponseMap}} and \code{\link{formatDifferenceMap}} format and add labels to plots produced
by \code{responseMap}, but any \code{ggplot2} changes that are desired can be made. If you wish to add additional layers 
(such as contour curves), the data variables available for mapping aesthetics are \code{x}, \code{y}, and \code{z}.
}
\value{ A \code{ggplot2} plot object, depicting the response variable (represented by the plot\'s \'fill\' variable) as a function of
two inputs (the x- and y-dimensions). }
\references{
Twarog, N.R., Stewart, E., Vowell Hamill, C., and Shelat, A.  BRAID: A Unifying Paradigm for the 
Analysis of Combined Drug Action. \emph{Scientific Reports} \strong{In Press} (2016).
}
\author{
Nathaniel R. Twarog
}
\seealso{
\code{\link{formatResponseMap}}, \code{\link{formatDifferenceMap}}, \code{\link{voronoi}}
}
\examples{
data(es8olatmz,package="braidrm")
rmap <- responseMap(act~conc1+conc2,es8olatmz)
formatResponseMap(rmap,xl="Olaparib",yl="TMZ",zl="Survival")
rmap <- responseMap(act~conc1+conc2,es8olatmz,interpolate=FALSE,margins=FALSE)
formatDifferenceMap(rmap,zcenter=-1,xl="Olaparib",yl="TMZ",zl="Survival")
}
\keyword{ hplot }
