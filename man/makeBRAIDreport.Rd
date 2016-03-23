\name{makeBRAIDreport}
\alias{makeBRAIDreport}
\title{
Make a BRAID Report Page
}
\description{
Produces a one page report depicting the results of a full BRAID analysis for a single
combination.
}
\usage{
makeBRAIDreport(brdAnalysis, compounds, iaelevs, macs, control = list())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{brdAnalysis}{ a list representing the results of a full BRAID analysis, produced by the
\code{braidrm} function \code{\link{runBRAIDanalysis}} }
  \item{compounds}{ A two element character vector containing the names of the two compounds
tested. Should be the same string repeated if combinaion is self-vs.-self }
  \item{iaelevs}{ one or two effect levels at which the IAE and potentiation will be estimated }
  \item{macs}{ the maximum achievable concentrations for the two compounds tested for the
calculation of the IAE.  If one concentration is provided, it will be used for both compounds.
If combination is self-vs.-self the two concentrations must be equal. }
  \item{control}{ A list of optional control parameters adjusting the appearance of the report
see Details for a full description }
}
\details{
This is, by design, a rather massive function that attempts, however foolhardily, to consolidate
all the pieces necessary to produce a comprehensive visualization and report on the results of a
BRAID analysis.  By default, it produces a one page report containing four rows of three panels.
The first row contains a plot of the measured data passed to the BRAID fit, plotted without
interpolation; a table of the best fit BRAID parameters, with confidence intervals; and tables of
one or more IAE values (with confidence intervals), as determined by the given maximum achievable
concentrations and either a purely additive surface or the best fit BRAID surface.  The second row
contains the measured data plotted with interpolation to convey the shape of the measured response
surface; a plot of the predicted response surface assuming the measured dose-response behavior and
BRAID additivity; and a plot of the best fit BRAID response surface.  The third row contains a
uninterpolated plot of the error resulting from simple smoothing (to provide a reasonable upper
bound on quality of fit); a plot of the error resulting from a predicted additive surface (with
R-squared value); and a plot of the error resulting from the best fit BRAID surface (with R-squared
value).  The fourth and final row shows potentiation in the best fit surface, containing tables of
EC values for a specified threshold for each drug in the presence of various levels of the other;
a potentiation plot of the first drug potentiated by the second; and the corresponding plot with
the two drugs reversed.  The theme settings and sizes are intended for use on a portrait-oriented
8.5 by 11 inch page.

Many details of the plots and report are adjusted by the \code{control} parameter, which may
contain any or all of the following elements:
\describe{
  \item{plot}{ a boolean determining whether the function arranges the constructed plots and tables
and prints them to the existing graphics device.  Doing so requires the package \code{gridExtra}
(version 2.0.0 or greater); if this package is not included, a warning will be printed and this
parameter will be reset to \code{FALSE}.  Default value is \code{TRUE} }
  \item{return}{ a boolean determining whether the pieces generated for the report are returned as
a result of the function. This option can be used if the user wishes to arrange the plot him or
herself, rather than using the default layout approach.  If \code{TRUE}, the function returns a
list of fourteen objects; the nine \code{ggplot} plots described above, followed by the five tables
assembled for the report.  Note that the tables are returned as standard R arrays, not grid or
graphics objects. Using this option with \code{plot} set to \code{FALSE} does not require the
presence of \code{gridExtra}. Default value is \code{FALSE} }
  \item{actlabel}{ a string or R expression to be used as the label of the modeled effect in all
plots. Default value is \code{"Effect"} }
  \item{abbs}{ in axis labels and tables, the two drugs being combined are represented by
abbreviations.  By default these abbreviations consist of the first three characters of each
compound name, with non alphanumeric characters removed.  However, the user may wish to use custom
abbreviations (e.g. "TMZ" for "temozolomide"); they may do so by specifying two abbreviations as
two strings in this optional parameter. }
  \item{levtext}{ an optional character vector the same length as \code{iaelevs}, determining how
the corresponding levels will be displayed in tables.  For example, if the modelled effect were
proportional cell survival, the user might want to specify a label of "90" for a modelled effect
of 0.1. }
  \item{irreg}{ a boolean variable, passed to \code{\link{responseMap}}, reflecting whether the
dose pairs being plotted lie on a regularly spaced grid.  Default value is \code{FALSE} }
  \item{clog}{ a boolean variable, reflecting whether the concentration variables are plotted on a
logarithmic scale.  This affects both the x- and y-axes in the seven surface plots, and the x-axis
in the TWO potentiation plots. Default value is \code{TRUE} }
  \item{zlog}{ a boolean variable, reflecting whether the modeled effect is plotted on a
logarithmic scale.  This affects only the y-axis in the two potentiaton plots.  Default value is
\code{FALSE} }
  \item{xtrans}{ an optional function mapping the concentration of the first drug to the
transformed value that the user wishes to plot.  A simple example would be one in which the fit was
performed on concentrations expressed in molar, but the user wishes to plot the concentrations in
micromolar, in which case the value of 'xtrans' might be \code{function(x) x*10^6}. }
  \item{ytrans}{ an optional function mapping the concentration of the second drug to the
transformed value that the user wishes to plot }
  \item{xunit}{ a string or R expression (e.g. \code{expression(mu*M)}) specifying how the units of
the first drug should be displayed in axis labels and tables.  If not specified, units will be
omitted. }
  \item{yunit}{ a string or R expression (e.g. \code{expression(mu*M)}) specifying how the units of
the second drug should be displayed in axis labels and tables.  If not specified, units will be
omitted. }
  \item{ztrans}{ an optional function mapping the modelled effect to the transformed value that the
user wishes to plot.  An example might be a case in which the modelled effect variable is a
logarithmic transform of a real-world measurement, and the user wishes to plot the measured value,
in which case the value of 'ztrans' might be \code{function(z) 10^z}. }
  \item{palette}{ a vector of two or more strings representing R colors, which will be passed to the
\code{ggplot2} function \code{\link{scale_fill_gradientn}} to specify the colormap for the plotted
response surfaces.  If left unspecified, a modified version of the classic "jet" colorscale (with
dark green substituted for dark blue) will be used.  Alternatively, if the user sets this parameter
to the string \code{"gjet"} or \code{"revgjet"}, the plots will use the modified "jet" colormap or
its reverse. }
  \item{ptsize}{ an optional size parameter passed to \code{\link{geom_point}} to control the size
of plotted points }
  \item{lnsize}{ an optional size parameter passed to \code{\link{geom_line}} to control the size
of plotted lines. }
}
}
\value{
If the \code{control} parameter \code{return} is set \code{TRUE}, a list of \code{ggplot2} plots
and tables as described in the Description section above.  If \code{return} is set to \code{FALSE}
or omitted, the function returns \code{NULL}.
}
\references{
Twarog, N.R., Stewart, E., Vowell Hamill, C., and Shelat, A.  BRAID: A Unifying Paradigm for the 
Analysis of Combined Drug Action. \emph{Scientific Reports} \strong{In Press} (2016).
}
\author{
Nathaniel R. Twarog
}
\seealso{
\code{\link{runBRAIDanalysis}}, \code{\link{responseMap}}, \code{\link{potentiationPlot}}
}
\examples{
data(es8analysis)
pcont<-list(xtrans=function(x) x*10^6,ytrans=function(y) y*10^6,ztrans=function(z) 10^z,
			xunit=expression(mu*M),yunit=expression(mu*M),zlog=TRUE,abbs=c("Ola","TMZ"),
			levtext=c("90","99"),palette="revgjet",plot=FALSE,return=TRUE)
brep <- makeBRAIDreport(es8analysis,c("Olaparib","Temozolomide"),
			c(-1,-2),c(10^-6,5*10^-5),pcont)
brep[[1]]
brep[[10]]
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
