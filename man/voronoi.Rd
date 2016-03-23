\name{voronoi}
\alias{voronoi}
\title{
Voronoi segmentation
}
\description{
Calculates the Voronoi (nearest-neighbor) segmentation of a two-dimensional rectangular region
based on a set of points.
}
\usage{
voronoi(pts, bbox = NULL)
}
\arguments{
  \item{pts}{ a two column array of points around which to calculate Voronoi cells }
  \item{bbox}{ the rectangular bounding box the Voronoi cells must fill, represented by a length
4 vector of the form \code{c(xmin,xmax,ymin,ymax)}.  If \code{NULL}, a  bounding box will be
constructed containing all points with padding on each side }
}
\details{
Though this function is not strictly necessary to the use of other functions in this package, we
found the construction of a Voronoi segmentation to be a rather tedious task, and felt that making
the function available to those who want a simple segmentation function was a reasonable choice.
}
\value{
A data frame intended for use in the \code{ggplot2} function \code{\link{geom_polygon}}.  The data
frame describes one polygon for each given input point, with the 'x' and 'y' columns specifying the
x- and y-coordinates of the vertices of the polygon, and the column 'poly' specifying the index of
the point (in the input array \code{pts}) around which the polygon sits.
}
\author{
Nathaniel R. Twarog
}
\seealso{
\code{\link{responseMap}}
}
\keyword{ dplot }