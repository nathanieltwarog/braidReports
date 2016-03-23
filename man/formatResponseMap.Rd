\name{formatResponseMap}
\alias{formatResponseMap}
\alias{formatDifferenceMap}
\title{
Format Response and Error Maps
}
\description{
Format response maps as activity maps using a multicolor gradient or error maps using a bidirectional gradient.
}
\usage{
formatResponseMap(rmap, palette = NULL, cscale = NULL, xl = expression(Conc[A]),
					yl = expression(Conc[B]), zl = "Activity")
formatDifferenceMap(rmap, zcenter = NULL, xl = expression(Conc[A]),
					yl = expression(Conc[B]), zl = "Diff")
}
\arguments{
  \item{rmap}{ a \code{ggplot2} plot object depicting a response map, produced by the function
\code{responseMap} }
  \item{palette}{ a character vector or two or more colors defining the colorscale to be applied to the fill
variable in the plot. }
  \item{cscale}{ a value around which the colorscale applied to the fill variable will be centered. If
\code{NULL} (the default), the colorscale will range from the minimum fill variable to the maximum.  }
  \item{xl}{ The character string or expression used to label the x-axis}
  \item{yl}{ The character string or expression used to label the y-axis }
  \item{zl}{ The character string or expression used to label the colorscale representing the response variable }
  \item{zcenter}{ the value that forms the centerpoint (white) in the bidirectional color scale.  If \code{NULL}
(the default), the default setting for \code{scale_fill_gradient2} is used.}
}
\details{
These functions are largely convenience functions, taking a response map produced by \code{\link{responseMap}}
and formatting it with colorscales and axis labels.  \code{formatResponseMap} with a multicolor gradient; by
default it uses a modified 'jet' colormap, ranging from dark red to dark green, rather than the traditional
dark blue.  We find this function useful for visualizing response surfaces representing variables such as
cytotoxicity, cell viability, enzyme activity, etc.  The other formats the response map with a bidirectional
color gradient (using the \code{ggplot2} function \code{scale_fill_gradient2}; we find this format ideal for
visualizing error maps.
}
\value{ A \code{ggplot2} plot object, with the appropriate scales and labels added. }
\author{
Nathaniel R. Twarog
}
\references{
Twarog, N.R., Stewart, E., Vowell Hamill, C., and Shelat, A.  BRAID: A Unifying Paradigm for the 
Analysis of Combined Drug Action. \emph{Scientific Reports} \strong{In Press} (2016).
}
\seealso{
\code{\link{responseMap}}
}
\examples{
data(es8olatmz,package="braidrm")
rmap <- responseMap(act~conc1+conc2,es8olatmz)
formatResponseMap(rmap,xl="Olaparib",yl="TMZ",zl="Survival")
formatDifferenceMap(rmap,zcenter=-1)
}

\keyword{ aplot }
