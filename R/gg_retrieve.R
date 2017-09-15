#' @title Retrieve X-axis Variable
#'
#' @description Get the name of the column that is mapped to the x-axis
#' @param ggplot_obj a ggplot2 object
#' @return A column name
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point(aes(colour = factor(cyl)))
#' x <- gg_retrieve_x(p)
#' print(x)
#' @export
#' @author Feiyang Niu (Feiyang.Niu@gilead.com)
gg_retrieve_x <- function(ggplot_obj) {
    if(!is.ggplot(ggplot_obj)) stop('ggplot_obj must be a ggplot2 object')
    build <- ggplot2::ggplot_build(ggplot_obj)
    return(build$plot$mapping$x)
}


#' @title Retrieve Y-axis Variable
#'
#' @description Get the name of the column that is mapped to the y-axis
#' @param ggplot_obj a ggplot2 object
#' @return A column name
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point(aes(colour = factor(cyl)))
#' y <- gg_retrieve_y(p)
#' print(y)
#' @export
#' @author Feiyang Niu (Feiyang.Niu@gilead.com)
gg_retrieve_y <- function(ggplot_obj) {
    if(!is.ggplot(ggplot_obj)) stop('ggplot_obj must be a ggplot2 object')
    build <- ggplot2::ggplot_build(ggplot_obj)
    return(build$plot$mapping$y)
}


#' @title Retrieve X-axis Label
#'
#' @description Get x-axis label of a ggplot2 object
#' @param ggplot_obj a ggplot2 object
#' @return X-axis label
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point(aes(colour = factor(cyl)))
#' x_label <- gg_retrieve_x_lab(p)
#' print(x_label)
#' @export
#' @author Feiyang Niu (Feiyang.Niu@gilead.com)
gg_retrieve_x_lab <- function(ggplot_obj) {
    if(!is.ggplot(ggplot_obj)) stop('ggplot_obj must be a ggplot2 object')
    build <- ggplot2::ggplot_build(ggplot_obj)
    return(build$plot$labels$x)
}


#' @title Retrieve Y-axis Label
#'
#' @description Get y-axis label of a ggplot2 object
#' @param ggplot_obj a ggplot2 object
#' @return Y-axis label
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point(aes(colour = factor(cyl)))
#' y_label <- gg_retrieve_y_lab(p)
#' print(y_label)
#' @export
#' @author Feiyang Niu (Feiyang.Niu@gilead.com)
gg_retrieve_y_lab <- function(ggplot_obj) {
    if(!is.ggplot(ggplot_obj)) stop('ggplot_obj must be a ggplot2 object')
    build <- ggplot2::ggplot_build(ggplot_obj)
    return(build$plot$labels$y)
}


#' @title Retrieve Plot Title
#'
#' @description Get plot title of a ggplot2 object
#' @param ggplot_obj a ggplot2 object
#' @return Plot title
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point(aes(colour = factor(cyl)))
#' plot_title <- gg_retrieve_title(p)
#' print(plot_title)
#' @export
#' @author Feiyang Niu (Feiyang.Niu@gilead.com)
gg_retrieve_title <- function(ggplot_obj) {
    if(!is.ggplot(ggplot_obj)) stop('ggplot_obj must be a ggplot2 object')
    build <- ggplot2::ggplot_build(ggplot_obj)
    return(build$plot$labels$title)
}


#' @title Retrieve Row Facetting Variable
#'
#' @description Get the name of the column that is mapped to row facetting
#'  variable of a ggplot2 object through \code{\link[ggplot2]{facet_grid}}
#' @param ggplot_obj a ggplot2 object
#' @return Row facetting variable. `NULL` is returned if
#'  \code{\link[ggplot2]{facet_grid}} is not used in building the ggplot2
#'  object
#' @examples
#' p <- ggplot(mpg, aes(displ, cty)) + geom_point() + facet_grid(. ~ cyl)
#' facet_row <- gg_retrieve_facet_row(p)
#' print(facet_row)
#' @export
#' @author Feiyang Niu (Feiyang.Niu@gilead.com)
gg_retrieve_facet_row <- function(ggplot_obj) {
    if(!is.ggplot(ggplot_obj)) stop('ggplot_obj must be a ggplot2 object')
    build <- ggplot2::ggplot_build(ggplot_obj)
    return(names(build$plot$facet$params$rows))
}


#' @title Retrieve Column Facetting Variable
#'
#' @description Get the name of the column that is mapped to column facetting
#'  variable of a ggplot2 object through \code{\link[ggplot2]{facet_grid}}.
#'  Refer to \code{\link[ggplot2]{facet_grid}} for the details
#' @param ggplot_obj a ggplot2 object
#' @return Column facetting variable. `NULL` is returned if
#'  \code{\link[ggplot2]{facet_grid}} is not used in building the ggplot2
#'  object
#' @examples
#' p <- ggplot(mpg, aes(displ, cty)) + geom_point() + facet_grid(. ~ cyl)
#' facet_col <- gg_retrieve_facet_col(p)
#' print(facet_col)
#' @export
#' @author Feiyang Niu (Feiyang.Niu@gilead.com)
gg_retrieve_facet_col <- function(ggplot_obj) {
    if(!is.ggplot(ggplot_obj)) stop('ggplot_obj must be a ggplot2 object')
    build <- ggplot2::ggplot_build(ggplot_obj)
    return(names(build$plot$facet$params$cols))
}


#' @title Retrieve Wrap Panel Variable(s)
#'
#' @description Get the name(s) of the variable(s) passed to the `facets`
#'  argument of \code{\link[ggplot2]{facet_wrap}} that wraps a 1d sequence
#'  of panels into 2d. Refer to \code{\link[ggplot2]{facet_wrap}} for the
#'  details
#' @param ggplot_obj a ggplot2 object
#' @return Wrap panel variable(s). `NULL` is returned if
#'  \code{\link[ggplot2]{facet_wrap}} is not used in building the ggplot2
#'  object
#' @examples
#' p <- ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_wrap(~ cyl + drv)
#' facet_wrap <- gg_retrieve_facet_wrap(p)
#' print(facet_wrap)
#' @export
#' @author Feiyang Niu (Feiyang.Niu@gilead.com)
gg_retrieve_facet_wrap <- function(ggplot_obj) {
    if(!is.ggplot(ggplot_obj)) stop('ggplot_obj must be a ggplot2 object')
    build <- ggplot2::ggplot_build(ggplot_obj)
    return(names(build$plot$facet$params$facets))
}


#' @title Retrieve Facetting Layout
#' @author Feiyang Niu (Feiyang.Niu@gilead.com)
gg_retrieve_facet_layout <- function(ggplot_obj, facet_var_only = TRUE) {
    if(!is.ggplot(ggplot_obj)) stop('ggplot_obj must be a ggplot2 object')
    has_facet_wrap <- gg_has_facet_wrap(ggplot_obj)
    has_facet_grid <- gg_has_facet_grid(ggplot_obj)
    if(!(has_facet_wrap || has_facet_grid)) return(NULL)
    build <- ggplot2::ggplot_build(ggplot_obj)
    if(!facet_var_only) return(build$layout$panel_layout)
    else {
        if(has_facet_wrap) facet_var <- gg_retrieve_facet_wrap(ggplot_obj)
        else facet_var <- c(gg_retrieve_facet_row(ggplot_obj),
                            gg_retrieve_facet_col(ggplot_obj))
        return(build$layout$panel_layout[facet_var])
    }
}


#' @title Retrieve Axis Ranges from A ggplot2 Object
#'
#' @description Get the actual x- and y-axis plotting ranges from a ggplot2
#'  object
#' @param ggplot_obj a ggplot2 object
#' @return A dataframe, which always contains the following columns
#'  * `x_min`: lower limit of x-axis
#'  * `x_max`: upper limit of x-axis
#'  * `y_min`: lower limit of y-axis
#'  * `y_max`: upper limit of y-axis
#'  In the case of facetting panels, i.e. with the use of either
#'  \code{\link[ggplot2]{facet_grid}} or \code{\link[ggplot2]{facet_wrap}},
#'  besides the above four, additional columns of facetting variable(s) are
#'  included as well.
#' @examples
#' # axis range of a scatter plot
#' scatter_plot <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' print(gg_retrieve_range(scatter_plot))
#'
#' # axis range of an errorbar plot
#' df <- data.frame(
#'     trt = factor(c(1, 1, 2, 2)),
#'     resp = c(1, 5, 3, 4),
#'     group = factor(c(1, 2, 1, 2)),
#'     upper = c(1.1, 5.3, 3.3, 4.2),
#'     lower = c(0.8, 4.6, 2.4, 3.6)
#' )
#' errorbar_plot <- ggplot(df, aes(trt, resp, colour = group)) +
#'     geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2)
#' print(gg_retrieve_range(errorbar_plot))
#'
#' # axis range of a bar chart
#' bar_chart <- ggplot(mpg, aes(class)) + geom_bar()
#' print(gg_retrieve_range(bar_chart))
#'
#' # axis range of panels in a grid
#' facet_col_plot <- ggplot(mpg, aes(displ, cty)) + geom_point() +
#'     facet_grid(. ~ cyl)
#' print(gg_retrieve_range(facet_col_plot))
#'
#' # axis range of panels in a 2d grid
#' facet_2d_plot <- ggplot(mpg, aes(displ, cty)) + geom_point() +
#'     facet_grid(drv ~ cyl)
#' print(gg_retrieve_range(facet_2d_plot))
#' @export
#' @author Feiyang Niu (Feiyang.Niu@gilead.com)
gg_retrieve_range <- function(ggplot_obj) {
    if(!is.ggplot(ggplot_obj)) stop('ggplot_obj must be a ggplot2 object')
    range_list2df <- function(range_list) {
        res <- setNames(c(range_list$x.range, range_list$y.range),
                        c('x_min', 'x_max', 'y_min', 'y_max'))
        res <- data.frame(t(res))
        return(res)
    }
    build <- ggplot2::ggplot_build(ggplot_obj)
    range_list <- build$layout$panel_ranges
    range_df <- do.call(rbind, lapply(range_list, range_list2df))
    has_facet_wrap <- gg_has_facet_wrap(ggplot_obj)
    has_facet_grid <- gg_has_facet_grid(ggplot_obj)
    if(!has_facet_wrap && !has_facet_grid) {
        return(range_df)
    } else if(has_facet_wrap) {
        wrap_var <- gg_retrieve_facet_wrap(ggplot_obj)
        wrap_df <- build$layout$panel_layout[wrap_var]
        return(cbind(wrap_df, range_df))
    } else if(has_facet_grid) {
        grid_var <- c(gg_retrieve_facet_row(ggplot_obj),
                      gg_retrieve_facet_col(ggplot_obj))
        grid_df <- build$layout$panel_layout[grid_var]
        return(cbind(grid_df, range_df))
    }
}
