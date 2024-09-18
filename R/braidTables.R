# braidTables.R

parameterTable <- function(parameters,title=NULL,widths=NULL,parse=TRUE,base_size=12) {
	if (is.null(names(parameters))) {
		pnames <- as.character(seq_along(parameters))
	} else {
		pnames <- names(parameters)
	}
	if (!is.character(parameters)) {
		parameters <- as.character(parameters)
		names(parameters) <- pnames
	}
	pvalues <- unname(parameters)

	ngrobs <- lapply(pnames,function(label) {
		if (parse) {
			label <- tryCatch(parse(text=label),
							  error = function(e) label)
		}
		grid::textGrob(label = label, x = grid::unit(base_size/2, "points"),just = "left",
				 gp = grid::gpar(col = "black", fontsize = base_size, lineheight = 1.2))
	})
	vgrobs <- lapply(pvalues,function(label) {
		grid::textGrob(label = label, x = grid::unit(1,"npc")-grid::unit(base_size/2, "points"), just = "right",
				 gp = grid::gpar(col = "black", fontsize = base_size, lineheight = 1.2))
	})

	if (is.null(widths)) {
		nwidth <- max(do.call(grid::unit.c,lapply(ngrobs, grid::grobWidth))) + grid::unit(base_size*1.5, "points")
		vwidth <- max(do.call(grid::unit.c,lapply(vgrobs, grid::grobWidth))) + grid::unit(base_size*1.5, "points")
		cwidths <- grid::unit.c(nwidth,vwidth)
	} else {
		if (is.numeric(widths)) { cwidths <- grid::unit(widths,"mm") }
		else { cwidths <- widths }
	}
	rheights <- grid::unit(rep(base_size,length(parameters)),"points")
	for (ri in seq_along(rheights)) {
		rheights[[ri]] <- rheights[[ri]] + max(grid::grobHeight(ngrobs[[ri]]),grid::grobHeight(vgrobs[[ri]]))
	}

	bkgds <- lapply(seq_along(parameters),function(i) {
		grid::rectGrob(gp = grid::gpar(col = NA,
				 		   fill = ifelse((i%%2)==0,"#EEEEF8","#DDDDEE"),
						   lwd = 0))
	})

	g <- gtable::gtable_matrix("table-fg",grobs=cbind(ngrobs,vgrobs),widths=grid::unit.c(nwidth,vwidth),heights=rheights)
	g <- gtable::gtable_add_grob(g,bkgds,t=seq_along(parameters),l=1,r=2,z=0,name="table-bg")

	if (is.null(title)) { return(g) }
	tgrob <- grid::textGrob(label = title,
					  gp = grid::gpar(col = "black", fontsize = base_size*1.2, lineheight = 1.2))
	fgrobs <- list(tgrob,g)

	fwidth <- max(do.call(grid::unit.c,lapply(fgrobs,grid::grobWidth)))
	fheights <- do.call(grid::unit.c,lapply(fgrobs,grid::grobHeight))
	fheights[[1]] <- fheights[[1]]+grid::unit(base_size,"points")
	fheights[[2]] <- sum(rheights)
	newg <- gtable::gtable_matrix("table-title",grobs=cbind(fgrobs),widths=fwidth,heights=fheights)
}

dataFrameTable <- function(df,title=NULL,names=NULL,widths=NULL,parse="none",base_size=12) {
	if (is.null(names)) {
		namevec <- names(df)
	} else {
		namevec <- names
	}

	ngrobs <- lapply(namevec,function(label) {
		if (parse %in% c("header","both")) {
			label <- tryCatch(parse(text=label),
							  error = function(e) label)
		}
		grid::textGrob(label = label,
				 gp = grid::gpar(col = "black", fontsize = base_size, lineheight = 1.2))
	})
	vgrobs <- do.call(c,
		lapply(df,function(v) {
			labels <- as.character(v)
			lapply(labels,function(label) {
				grid::textGrob(label = label,
						 gp = grid::gpar(col = "black", fontsize = base_size, lineheight = 1.2))
			})

		})
	)
	vgrobs <- matrix(vgrobs,nrow(df),ncol(df))
	allgrobs <- rbind(ngrobs,vgrobs)


	if (is.null(widths)) {
		cwidths <- do.call(grid::unit.c,apply(
			allgrobs, 2,
			function(c) max(do.call(grid::unit.c,lapply(c,grid::grobWidth))) + grid::unit(2*base_size,"points")
		))
	} else {
		if (is.numeric(widths)) { cwidths <- grid::unit(widths,"mm") }
		else { cwidths <- widths }
	}
	rheights <- do.call(grid::unit.c,apply(
		allgrobs, 1,
		function(c) max(do.call(grid::unit.c,lapply(c,grid::grobHeight))) + grid::unit(base_size,"points")
	))

	bkgds <- lapply(seq_len(nrow(df)+1)-1,function(i) {
		grid::rectGrob(gp = grid::gpar(col = NA,
				 		  fill = ifelse(i==0,"#BBBBDD",ifelse((i%%2)==0,"#EEEEF8","#DDDDEE")),
				 		  lwd = 0))
	})

	g <- gtable::gtable_matrix("table-fg",grobs=allgrobs,widths=cwidths,heights=rheights)
	g <- gtable::gtable_add_grob(g,bkgds,t=seq_len(nrow(df)+1),l=1,r=ncol(df),z=0,name="table-bg")

	if (is.null(title)) { return(g) }
	if (parse %in% c("title","both")) {
		title <- tryCatch(parse(text=title),error=function(e) title)
	}
	tgrob <- grid::textGrob(label = title,
					  gp = grid::gpar(col = "black", fontsize = base_size*1.2, lineheight = 1.2))
	fgrobs <- list(tgrob,g)

	fwidth <- max(grid::unit.c(grid::grobWidth(tgrob),sum(cwidths)))
	fheights <- grid::unit.c(grid::grobHeight(tgrob)+grid::unit(base_size*1.5,"points"),sum(rheights))
	newg <- gtable::gtable_matrix("table-title",grobs=cbind(fgrobs),widths=fwidth,heights=fheights)
}
