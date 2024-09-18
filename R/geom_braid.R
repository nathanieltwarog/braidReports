
#' BRAID Heatmaps
#'
#' Summarize and plot measurements of two inputs as a discrete raster or
#' "stained-glass" plot
#'
#' @inheritParams ggplot2::layer
#' @param space Parameter specifying the separation between marginal tiles and
#' the main grid.  Describes the distance from the center of the marginal tile
#' to the center of the nearest main grid tile, divided by the width or height
#' of the tile.  If a single value is provided, it is used for both left-right
#' and top-bottom margin tiles.  If two values are provided, the first is used
#' for left-right margin tiles and the second is used for top-bottom margin
#' tiles.
#' @param ... Additional parameters to be passed to [ggplot2::geom_tile()]
#' @param trim Should values that are finite in one dimension be dropped if
#' their finite coordinates lie outside the bounds of the main grid?
#' @param shared Should marginal offsets and trimming be calculated separately
#' for each facet if plots are faceted.  If `FALSE`, the default, each facet
#' will have its own bounds and marginal offsets; if `TRUE`, offsets will be
#' calculated for the full data and shared across all facets.
#' @param na.rm If `FALSE`, the default, missing values are removed with a
#' warning. If `TRUE`, missing values are silently removed.
#'
#' @details
#' While the existing [ggplot2] package includes several functions that are
#' extremely effective and versatile for visualizing two-dimensional responses,
#' including [ggplot2::geom_raster()], [ggplot2::geom_tile()], and
#' [ggplot2::geom_contour()], a number of considerations particular to combination
#' data make these functions, as is, somewhat difficult to use.  First, these
#' functions are not designed for data in which pairs of x- and y-coordinates
#' are duplicated; yet this is very common in experimental data.  While such
#' duplications can be handled prior to calling a visualization function,
#' handling them automatically reduces the barrier to plotting.
#'
#' A second, and much more challenging consideration, is that for many drug
#' combination studies, drug concentrations are measured as a series of equal
#' ratio dilutions; visualizing such doses is most intuitive on a logarithmic
#' scale.  But when inputs are scaled logarithmically, zeros become infinite
#' and are automatically removed by nearly all [ggplot2] functions.  This makes
#' it very difficult to plot measurements of drugs in isolation and in
#' combination in the same plot.  `geom_braid` addresses this by automatically
#' offsetting any measurements whose transformed coordinates are infinite to
#' margins within the plotted space, so that all values can be plotted together.
#'
#' While `geom_braid` is suitable for most response surfaces, some surfaces
#' feature measurements that are not arranged in a evenly spaced checkerboard.
#' For such surfaces, `geom_braid_glass` produces a set of Voronoi polygons
#' centered on the available transformed coordinates, creating what we call a
#' "stained glass" plot.  Marginal points are still represented by rectangles,
#' but with width and height such that boundaries are equidistant between
#' adjacent points.
#'
#' `stat_braid` and `stat_braid_glass` are simply the corresponding `stat_`
#' functions for these two functions.
#'
#' @export
#' @examples
#' concentrations <- c(0,2^(-3:3))
#' surface <- data.frame(
#' 	  concA = rep(rep(concentrations,each=length(concentrations)),each=3),
#'	  concB = rep(rep(concentrations,times=length(concentrations)),each=3),
#'	  replicate = rep(c(1,2,3),times=(length(concentrations)^2))
#' )
#' surface$actual <- evalBraidModel(
#'   surface$concA,
#'	 surface$concB,
#'	 c(1, 1, 3, 3, 2, 0, 100, 100, 100)
#' )
#' surface$measure <- surface$actual + rnorm(nrow(surface),sd=7)
#'
#' ggplot(surface,aes(x=concA,y=concB))+
#'     geom_braid(aes(fill=measure))+
#'     scale_x_log10()+
#'     scale_y_log10()+
#'     scale_fill_distiller(palette="RdYlBu")+
#'     coord_equal()+
#'     labs(x="Drug A",y="Drug B",fill="Effect")
#'
#' glassSurface <- surface
#' glassSurface$concA[glassSurface$replicate==2] <-
#'     glassSurface$concA[glassSurface$replicate==2]*1.25
#' glassSurface$concB[glassSurface$replicate==3] <-
#'     glassSurface$concB[glassSurface$replicate==3]*1.25
#' glassSurface$actual <- evalBraidModel(
#'     glassSurface$concA,
#'     glassSurface$concB,
#'     c(1, 1, 3, 3, -0.5, 0, 60, 100, 100)
#' )
#' glassSurface$measure <- glassSurface$actual+rnorm(nrow(glassSurface),sd=7)
#'
#' ggplot(glassSurface,aes(x=concA,y=concB))+
#'     geom_braid_glass(aes(fill=measure))+
#'     scale_x_log10("Drug A")+
#'     scale_y_log10("Drug B")+
#'     scale_fill_distiller("Effect",palette="RdYlBu")+
#'     coord_equal()
#'
#' glassSurface$tilewidth <- log10(2)*0.9
#' glassSurface$tilewidth[glassSurface$concA==0] <- log10(2)/2
#'
#' glassSurface$tileheight <- log10(2)*0.9
#' glassSurface$tileheight[glassSurface$concB==0] <- log10(2)/2
#'
#' ggplot(glassSurface,aes(x=concA,y=concB))+
#'     geom_braid_glass(aes(fill=measure,width=tilewidth,height=tileheight),space=2)+
#'     scale_x_log10("Drug A")+
#'     scale_y_log10("Drug B")+
#'     scale_fill_distiller("Effect",palette="RdYlBu")+
#'     coord_equal()
geom_braid <- function(mapping = NULL,
					   data = NULL,
					   stat = "braid",
					   position = "identity",
					   ...,
					   space = 1.5,
					   trim = TRUE,
					   shared = FALSE,
					   na.rm = FALSE,
					   show.legend = NA,
					   inherit.aes = TRUE) {
	layer(
		geom = "tile",
		data = data,
		mapping = mapping,
		stat = stat,
		position = position,
		show.legend = show.legend,
		inherit.aes = inherit.aes,
		params = list(na.rm = na.rm,
					  space = space,
					  trim = trim,
					  shared = shared,
					  ...)
	)
}

#' @rdname geom_braid
#' @export
stat_braid <- function(mapping = NULL,
					   data = NULL,
					   geom = "tile",
					   position = "identity",
					   ...,
					   space = 1.5,
					   trim = TRUE,
					   shared = FALSE,
					   na.rm = FALSE,
					   show.legend = NA,
					   inherit.aes = TRUE) {
	layer(
		stat = StatBraid,
		data = data,
		mapping = mapping,
		geom = geom,
		position = position,
		show.legend = show.legend,
		inherit.aes = inherit.aes,
		params = list(na.rm = na.rm,
					  space = space,
					  trim = trim,
					  shared = shared,
					  ...)
	)
}

#' @rdname geom_braid
#' @export
geom_braid_glass <- function(mapping = NULL,
							 data = NULL,
							 stat = "braid_glass",
							 position = "identity",
							 ...,
							 space = 1.5,
							 trim = TRUE,
							 shared = FALSE,
							 na.rm = FALSE,
							 show.legend = NA,
							 inherit.aes = TRUE) {
	layer(
		geom = "polygon",
		data = data,
		mapping = mapping,
		stat = stat,
		position = position,
		show.legend = show.legend,
		inherit.aes = inherit.aes,
		params = list(na.rm = na.rm,
					  space = space,
					  trim = trim,
					  shared = shared,
					  ...)
	)
}

#' @inheritParams ggplot2::layer
#' @inheritParams geom_braid
#' @export
#' @rdname geom_braid
stat_braid_glass <- function(mapping = NULL,
							 data = NULL,
							 geom = "polygon",
							 position = "identity",
							 ...,
							 space = 1.5,
							 trim = TRUE,
							 shared = FALSE,
							 na.rm = FALSE,
							 show.legend = NA,
							 inherit.aes = TRUE) {
	layer(
		stat = StatBraidGlass,
		data = data,
		mapping = mapping,
		geom = geom,
		position = position,
		show.legend = show.legend,
		inherit.aes = inherit.aes,
		params = list(na.rm = na.rm,
					  space = space,
					  trim = trim,
					  shared = shared,
					  ...)
	)
}

#' @export
#' @rdname braidReports-ggproto
StatBraid <- ggproto(
	"StatBraid",
	Stat,
	required_aes = c("x","y","fill"),
	optional_aes = c("width","height"),
	setup_data = function(data, params) {

		summary <- mean
		row.names(data) <- seq_len(nrow(data))
		newdata <- unique(data[,c("x","y","PANEL")])
		newdata <- data[row.names(newdata),,drop=FALSE]

		newdata$group <- NULL
		newdata$fill <- NA
		for (i in seq_len(nrow(newdata))) {
			rows <- which(data$x == newdata$x[[i]] &
						  	data$y == newdata$y[[i]] &
						  	data$PANEL == newdata$PANEL[[i]])
			if (length(rows)==0) { next }
			newdata$fill[[i]] <- summary(data$fill[rows])
		}
		newdata$group <- seq_len(nrow(newdata))


		if (params$shared) {
			newdata <- redo_margins(newdata,NULL,NULL,params$space,params$trim)
		}
		newdata
	},
	compute_layer = function(self, data, params, layout) {
		# Make sure required_aes consists of the used set of aesthetics in case of
		# "|" notation in self$required_aes
		required_aes <- intersect(
			names(data),
			unlist(strsplit(self$required_aes, "|", fixed = TRUE))
		)

		data <- remove_missing(data, params$na.rm,
							   c(required_aes, self$non_missing_aes),
							   "stat_braid",
							   finite = FALSE
		)
		# Trim off extra parameters
		params <- params[intersect(names(params), self$parameters())]

		panels <- lapply(split(data,as.factor(data$PANEL)),function(d) {
			scales <- layout$get_scales(data$PANEL[1])
			do.call(self$compute_panel,c(list(data=d,scales=scales),params))
		})

		newdata <- do.call(rbind,panels)
		newdata
	},
	compute_panel = function(self, data, scales, space=1.5, trim=FALSE, shared=FALSE) {
		if (!shared) { data <- redo_margins(data,NULL,NULL,space,trim) }
		data[,c("xsign","ysign")] <- NULL
		data
	}
)

#' @export
#' @rdname braidReports-ggproto
StatBraidGlass <- ggproto(
	"StatBraidGlass",
	StatBraid,
	dropped_aes = union(ggproto_parent(StatBraid,NULL)$dropped_aes,c("width","height")),
	compute_panel = function(self, data, scales, space=1.5, trim=TRUE, shared=FALSE) {
		if (!shared) { data <- redo_margins(data,NULL,NULL,space,trim) }
		# print(data)
		if (any((data$xsign==0) & (data$ysign==0))) {
			data_rel <- data[(data$xsign==0) & (data$ysign==0),]
			xmin <- min(data_rel$x)-0.5*data_rel$width[which.min(data_rel$x)]
			xmax <- max(data_rel$x)+0.5*data_rel$width[which.max(data_rel$x)]
			ymin <- min(data_rel$y)-0.5*data_rel$height[which.min(data_rel$y)]
			ymax <- max(data_rel$y)+0.5*data_rel$height[which.max(data_rel$y)]
			bbox <- c(xmin,xmax,ymin,ymax)
			polys_xy <- voronoi(cbind(data_rel$x,data_rel$y),bbox)
			polys_xy$group <- data_rel$group[polys_xy$group]
			data_rel$x <- NULL
			data_rel$y <- NULL
			data_rel <- merge(polys_xy,data_rel)
			data_poly <- data_rel
		} else {
			data_poly <- data.frame()
		}

		for (si in c(-1,1)) {
			if (any(data$xsign==0 & data$ysign==si)) {
				data_rel <- data[data$xsign==0 & data$ysign==si,]
				data_rel <- data_rel[order(data_rel$x),]
				xmin <- min(data_rel$x)-0.5*data_rel$width[which.min(data_rel$x)]
				xmax <- max(data_rel$x)+0.5*data_rel$width[which.max(data_rel$x)]
				betweens <- (data_rel$x[-nrow(data_rel)]+data_rel$x[-1])/2
				data_rel$xmin <- c(xmin,betweens)
				data_rel$xmax <- c(betweens,xmax)
				data_rel$ymin <- data_rel$y-data_rel$height/2
				data_rel$ymax <- data_rel$y+data_rel$height/2
				data_poly <- rbind(data_poly, rectToPoly(data_rel))
			}
		}

		for (si in c(-1,1)) {
			if (any(data$xsign==si & data$ysign==0)) {
				data_rel <- data[data$xsign==si & data$ysign==0,]
				data_rel <- data_rel[order(data_rel$y),]
				ymin <- min(data_rel$y)-0.5*data_rel$height[which.min(data_rel$y)]
				ymax <- max(data_rel$y)+0.5*data_rel$height[which.max(data_rel$y)]
				betweens <- (data_rel$y[-nrow(data_rel)]+data_rel$y[-1])/2
				data_rel$xmin <- data_rel$x-data_rel$width/2
				data_rel$xmax <- data_rel$x+data_rel$width/2
				data_rel$ymin <- c(ymin,betweens)
				data_rel$ymax <- c(betweens,ymax)
				data_poly <- rbind(data_poly, rectToPoly(data_rel))
			}
		}

		for (si in c(-1,1)) {
			for (sj in c(-1,1)) {
				if (any(data$xsign==si & data$ysign==sj)) {
					data_rel <- data[data$xsign==si & data$ysign==sj,]
					data_rel$xmin <- data_rel$x-data_rel$width/2
					data_rel$xmax <- data_rel$x+data_rel$width/2
					data_rel$ymin <- data_rel$y-data_rel$height/2
					data_rel$ymax <- data_rel$y+data_rel$height/2
					data_poly <- rbind(data_poly, rectToPoly(data_rel))
				}
			}
		}
		data_poly
	}
)

#' Smoothed BRAID Surfaces
#'
#' Summarize and plot measurements of two inputs as a smoothed response surface
#'
#' @inheritParams ggplot2::layer
#' @inheritParams geom_braid
#' @param ... Additional parameters to be passed to [ggplot2::geom_tile()]
#' @param npoints The number of interpolated values in the x- and y- directions
#' that are used to generate the smoothed raster function
#' @param tight If true, the generated raster will span the precise range of
#' transformed and plotted data; this will produce a range of tiles that are
#' strictly smaller than those produced by [geom_braid] (as those tiles extend
#' above and below the plotted tile centers).  If `FALSE` (the default), the
#' interpolated values will be selected to span the same (slightly larger) range
#' of values that would be covered by running [geom_braid] with the same
#' parameters.
#' @param na.rm If `FALSE`, the default, missing values are removed with a
#' warning. If `TRUE`, missing values are silently removed.
#'
#' @details
#' Like [geom_braid], this function involves several pre-processing steps to
#' allow quick visualization of drug combination data. These steps include
#' summarizing duplicated measurements and offsetting non-finite plotted
#' coordinates. In addition to these steps, `geom_braid_smooth` generates a
#' regular, densely sampled grid of coordinates and smoothly interpolates the
#' given data to produces a smoothed raster heatmap.  Smoothing in the x- and
#' y- directions is governed by the width and height aesthetic respectively; if
#' these aesthetics are not included, they are estimated from the minimum
#' spacing of the data.
#'
#' @export
#' @examples
#' surface <- synergisticExample
#'
#' ggplot(surface,aes(x=concA,y=concB))+
#'	   geom_braid_smooth(aes(fill=measure))+
#'     scale_x_log10("Drug A")+
#'     scale_y_log10("Drug B")+
#'     scale_fill_distiller("Effect",palette="RdYlBu")+
#'     coord_equal()
geom_braid_smooth <- function(mapping = NULL,
							  data = NULL,
							  stat = "braid_smooth",
							  position = "identity",
							  ...,
							  space = 1.5,
							  trim = TRUE,
							  shared = FALSE,
							  npoints = 50,
							  tight = FALSE,
							  na.rm = FALSE,
							  show.legend = NA,
							  inherit.aes = TRUE) {
	layer(
		geom = "tile",
		data = data,
		mapping = mapping,
		stat = stat,
		position = position,
		show.legend = show.legend,
		inherit.aes = inherit.aes,
		params = list(na.rm = na.rm,
					  space = space,
					  trim = trim,
					  npoints = npoints,
					  tight = tight,
					  shared = shared,
					  ...)
	)
}

#' @inheritParams ggplot2::layer
#' @inheritParams geom_braid
#' @inheritParams geom_braid_smooth
#' @export
#' @rdname geom_braid_smooth
stat_braid_smooth <- function(mapping = NULL,
							  data = NULL,
							  geom = "tile",
							  position = "identity",
							  ...,
							  space = 1.5,
							  trim = TRUE,
							  shared = FALSE,
							  npoints = 50,
							  tight = FALSE,
							  na.rm = FALSE,
							  show.legend = NA,
							  inherit.aes = TRUE) {
	layer(
		stat = StatBraidSmooth,
		data = data,
		mapping = mapping,
		geom = geom,
		position = position,
		show.legend = show.legend,
		inherit.aes = inherit.aes,
		params = list(na.rm = na.rm,
					  space = space,
					  trim = trim,
					  npoints = npoints,
					  tight = tight,
					  shared = shared,
					  ...)
	)
}

#' @export
#' @rdname braidReports-ggproto
StatBraidSmooth <- ggproto(
	"StatBraidSmooth",
	StatBraid,
	compute_panel = function(self, data, scales, space=1.5, trim=TRUE, shared=FALSE, npoints=50, tight=FALSE) {
		if (!shared) { data <- redo_margins(data,NULL,NULL,space,trim) }
		if (any((data$xsign==0) & (data$ysign==0))) {
			data_rel <- data[(data$xsign==0) & (data$ysign==0),]
			if (tight) {
				xrng <- range(data_rel$x)
				yrng <- range(data_rel$y)
			} else {
				xrng <- c(
					min(data_rel$x)-0.5*data_rel$width[which.min(data_rel$x)],
					max(data_rel$x)+0.5*data_rel$width[which.max(data_rel$x)]
				)
				yrng <- c(
					min(data_rel$y)-0.5*data_rel$height[which.min(data_rel$y)],
					max(data_rel$y)+0.5*data_rel$height[which.max(data_rel$y)]
				)
			}
			data_smooth <- smooth2ddata(data_rel,xrng,yrng,npoints)
		} else {
			data_smooth <- data.frame()
		}

		for (si in c(-1,1)) {
			if (any(data$xsign==0 & data$ysign==si)) {
				data_rel <- data[data$xsign==0 & data$ysign==si,]
				data_rel <- data_rel[order(data_rel$x),]
				if (tight) {
					xrng <- range(data_rel$x)
				} else {
					xrng <- c(
						min(data_rel$x)-0.5*data_rel$width[which.min(data_rel$x)],
						max(data_rel$x)+0.5*data_rel$width[which.max(data_rel$x)]
					)
				}
				data_smooth <- rbind(data_smooth,smooth1ddata(data_rel,"x",xrng,npoints))
			}
		}

		for (si in c(-1,1)) {
			if (any(data$xsign==si & data$ysign==0)) {
				data_rel <- data[data$xsign==si & data$ysign==0,]
				data_rel <- data_rel[order(data_rel$y),]
				if (tight) {
					yrng <- range(data_rel$y)
				} else {
					yrng <- c(
						min(data_rel$y)-0.5*data_rel$height[which.min(data_rel$y)],
						max(data_rel$y)+0.5*data_rel$height[which.max(data_rel$y)]
					)
				}
				data_smooth <- rbind(data_smooth,smooth1ddata(data_rel,"y",yrng,npoints))
			}
		}

		for (si in c(-1,1)) {
			for (sj in c(-1,1)) {
				if (any(data$xsign==si & data$ysign==sj)) {
					data_rel <- data[data$xsign==si & data$ysign==sj,]
					data_smooth <- rbind(data_smooth, data_rel[,c("x","y","fill","width","height")])
				}
			}
		}
		data_smooth$PANEL <- data$PANEL[[1]]
		data_smooth$group <- seq_len(nrow(data_smooth))
		data_smooth
	}
)


#' Smoothed BRAID Surface Contours
#'
#' Generate contours of a smoothed two-dimensional response surface
#'
#' @inheritParams ggplot2::geom_contour
#' @inheritParams geom_braid
#' @inheritParams geom_braid_smooth
#'
#' @details
#' When evaluating a plotted response surface it is often more effective to
#' plot the precise contours at which a set of levels is reached by the
#' combination.  Because [ggplot2::stat_contour] requires that data lie in an
#' evenly space raster grid (and does not support duplicated values), it is
#' difficult to apply to more discrete or irregularly sampled data.  This
#' function uses the same smoothing and interpolation utilities as
#' [geom_braid_smooth] to preprocess and smooth data, which is then passed to
#' the contour estimation code of [ggplot2::stat_contour], producing contours
#' which are smoothed and sufficiently regularly spaced.
#'
#' @export
#' @examples
#' surface <- antagonisticExample
#'
#' ggplot(surface,aes(x=concA,y=concB))+
#'	   geom_braid_smooth(aes(fill=measure))+
#'	   geom_braid_contour(aes(z=measure),colour="black",breaks=((1:9)/10))+
#'     scale_x_log10("Drug A")+
#'     scale_y_log10("Drug B")+
#'     scale_fill_distiller("Effect",palette="RdYlBu")+
#'     coord_equal()
geom_braid_contour <- function(mapping = NULL,
							   data = NULL,
							   stat = "braid_contour",
							   position = "identity",
							   ...,
							   bins = NULL,
							   binwidth = NULL,
							   breaks = NULL,
							   npoints = 50,
							   tight = FALSE,
							   trim = TRUE,
							   shared = FALSE,
							   na.rm = FALSE,
							   show.legend = NA,
							   inherit.aes = TRUE) {
	layer(
		geom = "contour",
		data = data,
		mapping = mapping,
		stat = stat,
		position = position,
		show.legend = show.legend,
		inherit.aes = inherit.aes,
		params = list(na.rm = na.rm,
					  bins = bins,
					  binwidth = binwidth,
					  breaks = breaks,
					  npoints = npoints,
					  tight = tight,
					  trim = trim,
					  shared = shared,
					  ...)
	)
}

#' @inheritParams ggplot2::stat_contour
#' @inheritParams geom_braid
#' @inheritParams geom_braid_smooth
#' @export
#' @rdname geom_braid_contour
stat_braid_contour <- function(mapping = NULL,
							   data = NULL,
							   geom = "contour",
							   position = "identity",
							   ...,
							   bins = NULL,
							   binwidth = NULL,
							   breaks = NULL,
							   npoints = 50,
							   tight = FALSE,
							   trim = TRUE,
							   shared = FALSE,
							   na.rm = FALSE,
							   show.legend = NA,
							   inherit.aes = TRUE) {
	layer(
		stat = StatBraidContour,
		data = data,
		mapping = mapping,
		geom = geom,
		position = position,
		show.legend = show.legend,
		inherit.aes = inherit.aes,
		params = list(na.rm = na.rm,
					  bins = bins,
					  binwidth = binwidth,
					  breaks = breaks,
					  npoints = npoints,
					  tight = tight,
					  trim = trim,
					  shared = shared,
					  ...)
	)
}

#' @export
#' @rdname braidReports-ggproto
StatBraidContour <- ggproto(
	"StatBraidContour",
	StatContour,
	optional_aes = union(ggproto_parent(StatContour,NULL)$optional_aes,c("width","height")),
	dropped_aes = union(ggproto_parent(StatContour,NULL)$dropped_aes,c("width","height")),
	setup_data = function(data, params) {
		summary <- mean
		row.names(data) <- seq_len(nrow(data))
		newdata <- unique(data[,c("x","y","PANEL","group")])
		newdata <- data[row.names(newdata),,drop=FALSE]

		newdata$z <- NA
		for (i in seq_len(nrow(newdata))) {
			rows <- which(data$x == newdata$x[[i]] &
						  	data$y == newdata$y[[i]] &
						  	data$PANEL == newdata$PANEL[[i]] &
						  	data$group == newdata$group[[i]])
			if (length(rows)==0) { next }
			newdata$z[[i]] <- summary(data$z[rows])
		}
		if (params$shared) {
			newdata <- redo_margins(newdata,NULL,NULL,1.5,params$trim)
		}
		newdata
	},
	compute_group = function(self, data, scales, z.range, trim=TRUE, shared=FALSE, npoints=50, tight=FALSE,
							 bins = NULL, binwidth = NULL, breaks = NULL) {
		if (!shared) { data <- redo_margins(data,NULL,NULL,1.5,trim) }
		if (any((data$xsign==0) & (data$ysign==0))) {
			data_rel <- data[(data$xsign==0) & (data$ysign==0),]
			if (tight) {
				xrng <- range(data_rel$x)
				yrng <- range(data_rel$y)
			} else {
				xrng <- c(
					min(data_rel$x)-0.5*data_rel$width[which.min(data_rel$x)],
					max(data_rel$x)+0.5*data_rel$width[which.max(data_rel$x)]
				)
				yrng <- c(
					min(data_rel$y)-0.5*data_rel$height[which.min(data_rel$y)],
					max(data_rel$y)+0.5*data_rel$height[which.max(data_rel$y)]
				)
			}
			data_smooth <- smooth2dcontours(data_rel,xrng,yrng,npoints)
		} else {
			data_smooth <- data.frame()
		}

		data_smooth$PANEL <- data$PANEL[[1]]
		data_smooth$group <- data$group[[1]]
		ggproto_parent(StatContour,self)$compute_group(
			data_smooth, scales, z.range = z.range,
			bins = bins, binwidth = binwidth, breaks = breaks
		)
	}
)

redo_margins <- function(data,width=NULL,height=NULL,space=1.5,trim=FALSE) {
	full_finite <- is.finite(data$x) & is.finite(data$y)
	if (trim && any(full_finite)) {
		xrange <- range(data$x[full_finite])
		yrange <- range(data$y[full_finite])
		data <- data[(!is.finite(data$x) | (data$x>=xrange[[1]] & data$x<=xrange[[2]])) &
					 	(!is.finite(data$y) | (data$y>=yrange[[1]] & data$y<=yrange[[2]])),]
	}
	if (any(is.finite(data$x))) {
		width <- ifelse(is.null(width),resolution(data$x[is.finite(data$x)],FALSE),width)
		xrange <- range(data$x[is.finite(data$x)])
	} else {
		width <- ifelse(is.null(width),1,width)
		xrange <- c(0,0)
	}
	if (any(is.finite(data$y))) {
		height <- ifelse(is.null(height),resolution(data$y[is.finite(data$y)],FALSE),height)
		yrange <- range(data$y[is.finite(data$y)])
	} else {
		height <- ifelse(is.null(height),1,height)
		yrange <- c(0,0)
	}

	if (is.null(data$width)) {
		data$width <- width
	} else {
		data$width <- ifelse(is.finite(data$width),data$width,width)
	}
	if (is.null(data$height)) {
		data$height <- height
	} else {
		data$height <- ifelse(is.finite(data$height),data$height,height)
	}

	data$xsign <- 0
	data$xsign[-Inf==data$x] <- -1
	data$xsign[Inf==data$x] <- 1
	data$ysign <- 0
	data$ysign[-Inf==data$y] <- -1
	data$ysign[Inf==data$y] <- 1

	if (length(space)==1) { space <- c(space,space) }
	if (any(data$xsign < 0)) {
		rel <- data$xsign < 0
		width_1 <- max(data$width[rel])
		data$x[rel] <- xrange[[1]]-space[[1]]*width_1
	}
	if (any(data$xsign > 0)) {
		rel <- data$xsign > 0
		width_2 <- max(data$width[rel])
		data$x[rel] <- xrange[[2]]+space[[1]]*width_2
	}
	if (any(data$ysign < 0)) {
		rel <- data$ysign < 0
		height_1 <- max(data$height[rel])
		data$y[rel] <- yrange[[1]]-space[[2]]*height_1
	}
	if (any(data$ysign > 0)) {
		rel <- data$ysign > 0
		height_2 <- max(data$height[rel])
		data$y[rel] <- yrange[[2]]+space[[2]]*height_2
	}

	data
}

smooth2dcontours <- function(data,xrng,yrng,npoints=50) {
	xstep <- diff(xrng)/npoints
	xvec <- xrng[[1]]+diff(xrng)*(seq_len(npoints+1)-1)/npoints
	ystep <- diff(yrng)/npoints
	yvec <- yrng[[1]]+diff(yrng)*(seq_len(npoints+1)-1)/npoints

	xv <- rep(xvec,times=length(yvec))
	yv <- rep(yvec,each=length(xvec))
	swidth <- stats::median(data$width)
	sheight <- stats::median(data$height)
	zv <- gaussInterp2d(data$x,data$y,data$z,xv,yv,swidth/2,sheight/2)

	data.frame(x=xv, y=yv, z=zv, width=xstep, height=ystep)
}
smooth2ddata <- function(data,xrng,yrng,npoints=50) {
	xstep <- diff(xrng)/npoints
	xvec <- xrng[[1]]+diff(xrng)*(seq_len(npoints)-0.5)/npoints
	ystep <- diff(yrng)/npoints
	yvec <- yrng[[1]]+diff(yrng)*(seq_len(npoints)-0.5)/npoints

	xv <- rep(xvec,times=length(yvec))
	yv <- rep(yvec,each=length(xvec))
	swidth <- stats::median(data$width)
	sheight <- stats::median(data$height)
	fv <- gaussInterp2d(data$x,data$y,data$fill,xv,yv,swidth/2,sheight/2)

	data.frame(x=xv, y=yv, fill=fv, width=xstep, height=ystep)
}
smooth1ddata <- function(data,dir="x",rng,npoints=50) {
	if (dir=="x") {
		xstep <- diff(rng)/npoints
		xv <- rng[[1]]+diff(rng)*(seq_len(npoints)-0.5)/npoints
		swidth <- stats::median(data$width)
		fv <- gaussInterp1d(data$x,data$fill,xv,swidth/2)

		return(
			data.frame(x=xv, y=stats::median(data$y), fill=fv, width=xstep, height=stats::median(data$height))
		)
	} else {
		ystep <- diff(rng)/npoints
		yv <- rng[[1]]+diff(rng)*(seq_len(npoints)-0.5)/npoints
		sheight <- stats::median(data$height)
		fv <- gaussInterp1d(data$y,data$fill,yv,sheight/2)

		return(
			data.frame(x=stats::median(data$x), y=yv, fill=fv, width=stats::median(data$width), height=ystep)
		)
	}
}
gaussInterp2d <- function(x,y,z,nx,ny,sigx,sigy) {
	if (length(x)!=length(y)) { stop("Vectors of x- and y-coordinates muat have the same length.") }
	nz <- rep(0,length(nx))
	for (i in 1:length(nz)) {
		gv <- exp(-((nx[i]-x)^2/(2*sigx^2)+(ny[i]-y)^2/(2*sigy^2)))
		nz[i] <- sum(gv*z)/sum(gv)
	}
	return(nz)
}
gaussInterp1d <- function(x,y,nx,sig) {
	ny <- rep(0,length(nx))
	for (i in 1:length(ny)) {
		gv <- exp(-(nx[i]-x)^2/(2*sig^2))
		ny[i] <- sum(gv*y)/sum(gv)
	}
	return(ny)
}

rectToPoly <- function(data) {
	polyxy <- data.frame(
		group = rep(data$group,times=4),
		pvertex = rep(1:4,each=nrow(data)),
		x = c(data$xmax,data$xmax,data$xmin,data$xmin),
		y = c(data$ymin,data$ymax,data$ymax,data$ymin)
	)
	data$x <- NULL
	data$y <- NULL
	data$xmin <- NULL
	data$xmax <- NULL
	data$ymin <- NULL
	data$ymax <- NULL
	data <- merge(data,polyxy,sort=FALSE)
	data <- data[order(data$group,data$pvertex),]
	data$pvertex <- NULL
	data
}

# ggproto -----------------------------------------------------------------

#' Base ggproto classes for braidReports
#'
#' @seealso [`ggproto()`][ggplot2::ggproto]
#' @keywords internal
#' @name braidReports-ggproto
NULL
