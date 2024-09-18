
kappa_breaks <- function(n=5,...) {
	n_default <- n
	function(x, n=n_default) {
		x <- x[is.finite(x)]
		if (length(x) == 0) {
			return(numeric())
		}
		rng <- range(x)
		kappa_sets <- list(
			c(-1.995,-1.96,-1.75,0,12,100,800),
			c(-1.99,-1.96,-1.86,-1.5,0,5,30,100,400),
			c(-1.995,-1.985,-1.96,-1.9,-1.75,-1.25,0,3,12,40,100,300,800),
			c(-1.99,-1.98,-1.96,-1.93,-1.86,-1.75,-1.5,-1,0,2,5,12,30,55,100,200,400),
			c(-1.995,-1.99,-1.985,-1.975,-1.96,-1.94,-1.9,-1.85,-1.75,-1.55,
			  -1.25,-0.75,0,1,3,7,12,20,40,65,100,175,300,500,800),
			c(-1.99,-1.985,-1.98,-1.97,-1.96,-1.95,-1.93,-1.9,-1.86,-1.8,-1.75,-1.6,-1.5,
			  -1.25,-1,-0.5,0,1,2,3,5,8,12,20,30,40,55,75,100,150,200,300,400),
			c(-1.975,-1.97,-1.96,-1.95,-1.94,-1.92,-1.9,-1.87,-1.84,-1.8,-1.75,
			  -1.65,-1.55,-1.4,-1.25,-1,-0.75,-0.5,0,0.5,1,2,3.5,5,7,9,12,16,
			  20,30,40,50,65,80,100,140,175)
		)
		set_range <- rep(0,length(kappa_sets))
		for (i in seq_along(kappa_sets)) {
			set_range[[i]] <- length(which(kappa_sets[[i]]>=rng[[1]] & kappa_sets[[i]]<=rng[[2]]))
		}
		if (all(set_range==0)) {
			return(exp(stats::median(log((rng+2)/2)))*2 - 2)
		}
		bestset <- which.min(abs(log(set_range)-log(n)))
		kappa_set <- kappa_sets[[bestset]]
		kappa_set <- kappa_set[kappa_set>=rng[[1]] & kappa_set<=rng[[2]]]
		kappa_set
	}
}


#' BRAID kappa Transforms
#'
#' @description
#' Functions to linearize the BRAID interaction parameter kappa, which ordinarily
#' ranges from -2 to infinity. `kappa_trans` produces a `scales` transform
#' object which can be used in `ggplot2` continuous scale object.
#' `scale_x_kappa` and `scale_y_kappa` are wrappers for `scale_x_continuous`
#' and `scale_y_continuous` which set the `trans` or `transform` parameter to
#' `kappa_trans()`.
#'
#' @param ... Additional parameter to be passed to `scale_*_continuous`
#'
#' @return For `kappa_trans` a `scales` transform object.  For `scale_*_kappa`,
#' a continuous position scale layer for a ggplot object.
#' @export
#' @examples
#' transform <- kappa_trans()
#'
#' transform$transform(c(-1.96, 100))
#' transform$inverse(c(-1, 1))
#'
#' data <- merckValues_stable
#' ggplot(data,aes(x=kappa))+
#'     geom_density()+
#'     scale_x_kappa("BRAID kappa")
kappa_trans <- function() {
	scales::new_transform(
		"kappa",
		transform=function(k) log((k+2)/2),
		inverse=function(l) (2*exp(l)-2),
		d_transform=function(k) 1/(k+2),
		d_inverse=function(l) 2*exp(l),
		breaks=kappa_breaks(),
		domain=c(-2,Inf)
	)
}

#' @rdname kappa_trans
#' @export
scale_x_kappa <- function(...) {
	if (utils::packageVersion("ggplot2")>="3.5.0") {
		scale_x_continuous(...,transform=kappa_trans())
	} else {
		scale_x_continuous(...,trans=kappa_trans())
	}
}

#' @rdname kappa_trans
#' @export
scale_y_kappa <- function(...) {
	if (utils::packageVersion("ggplot2")>="3.5.0") {
		scale_y_continuous(...,transform=kappa_trans())
	} else {
		scale_y_continuous(...,trans=kappa_trans())
	}
}
