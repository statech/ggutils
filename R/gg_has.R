#' @title Whether Has Facetting Ribbon
#'
#' @description Check if a ggplot2 object has facetting ribbon structure, i.e.
#'  if \code{\link[ggplot2]{facet_wrap}} is used in building the ggplot2 object
#' @param ggplot_obj a ggplot2 object
#' @return \code{TRUE} if \code{\link[ggplot2]{facet_wrap}} is used;
#'  \code{FALSE} otherwise
#' @examples
#' p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
#' print(gg_has_facet_wrap(p))
#' p <- p + facet_wrap(~ cyl + drv)
#' print(gg_has_facet_wrap(p))
#' @export
#' @author Feiyang Niu (Feiyang.Niu@gilead.com)
gg_has_facet_wrap <- function(ggplot_obj) {
    if(!is.ggplot(ggplot_obj)) stop('ggplot_obj must be a ggplot2 object')
    build <- ggplot2::ggplot_build(ggplot_obj)
    return('facets' %in% names(build$plot$facet$params))
}


#' @title Whether Has Facetting Grid
#'
#' @description Check if a ggplot2 object has facetting grid structure, i.e.
#'  if \code{\link[ggplot2]{facet_grid}} is used in building the ggplot2 object
#' @param ggplot_obj a ggplot2 object
#' @return \code{TRUE} if \code{\link[ggplot2]{facet_grid}} is used;
#'  \code{FALSE} otherwise
#' @examples
#' p <- ggplot(mpg, aes(displ, cty)) + geom_point()
#' print(gg_has_facet_grid(p))
#' p <- p + facet_grid(. ~ cyl)
#' print(gg_has_facet_grid(p))
#' @export
#' @author Feiyang Niu (Feiyang.Niu@gilead.com)
gg_has_facet_grid <- function(ggplot_obj) {
    if(!is.ggplot(ggplot_obj)) stop('ggplot_obj must be a ggplot2 object')
    build <- ggplot2::ggplot_build(ggplot_obj)
    return(all(c('rows', 'cols') %in% names(build$plot$facet$params)))
}


#' @title Whether Has Facetting Panels
#'
#' @description Check if a ggplot2 object has facetting panel structure, i.e.
#'  if either \code{\link[ggplot2]{facet_grid}} or
#'  \code{\link[ggplot2]{facet_grid}} is used in building the ggplot2 object
#' @param ggplot_obj a ggplot2 object
#' @return \code{TRUE} if either \code{\link[ggplot2]{facet_grid}} or
#'  \code{\link[ggplot2]{facet_grid}}is used; \code{FALSE} otherwise
#' @examples
#' p <- ggplot(mpg, aes(displ, cty)) + geom_point()
#' print(gg_has_facet_grid(p))
#' print(gg_has_facet_layout(p + facet_grid(. ~ cyl)))
#' print(gg_has_facet_layout(p + facet_wrap(~ cyl)))
#' @export
#' @author Feiyang Niu (Feiyang.Niu@gilead.com)
gg_has_facet_layout <- function(ggplot_obj) {
    return(gg_has_facet_wrap(ggplot_obj) || gg_has_facet_grid(ggplot_obj))
}
