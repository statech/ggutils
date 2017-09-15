#' Remove Plot Elements
#'
#' Remove one or more elements from a \href{http://ggplot2.org/}{ggplot2} Object
#'
#' @param ggplot_obj a ggplot2 object
#' @param elements_to_remove String vector: a character vector dictating which
#'  elements(s) to be removed from \code{ggplot_obj} with elements from:
#'  'xlab', 'ylab', 'title', 'subtitle', 'legend_title', 'xaxis', 'yaxis',
#'  'legend', 'grid', 'grid_x', 'grid_y', 'xticks', 'yticks'. By default,
#'  nothing gets removed, i.e. same \code{ggplot_obj} gets returned.
#' @return A ggplot2 object
#' @examples
#' p <- ggplot(mtcars, aes(mpg, wt, colour = cyl)) + geom_point() +
#'     labs(colour = 'Cylinders',
#'          x = 'Miles/(US) gallon',
#'          y = 'Weight (1000 lbs)',
#'          title = 'New plot title',
#'          subtitle = 'A subtitle',
#'          caption = '(based on data from ...)')
#' new_p <- gg_remove(p, c('title', 'caption'))
#' p_no_xlab <- gg_remove_xlab(p)
#' p_no_legend_title <- gg_remove_legend_title(p)
#' @rdname gg_remove
#' @export
#' @author Feiyang Niu (Feiyang.Niu@gilead.com)
gg_remove <- function(ggplot_obj, elements_to_remove = NULL) {
    if(!is.ggplot(ggplot_obj)) stop('ggplot_obj must be a ggplot2 object')
    if(length(elements_to_remove) == 0) return(ggplot_obj)
    all_elements_to_remove <- c(
        'xlab', 'ylab', 'title', 'subtitle', 'caption', 'legend_title', 'xaxis',
        'yaxis', 'legend', 'grid', 'grid_x', 'grid_y', 'xticks', 'yticks'
    )
    if(!all(elements_to_remove %in% all_elements_to_remove)) {
        stop(paste0(
            '`elements_to_remove` must be a vector with elements from:',
            '\n', paste(paste('*', all_elements_to_remove), collapse = '\n')
        ))
    }
    if('xlab' %in% elements_to_remove) {
        ggplot_obj <- ggplot_obj + theme(axis.title.x = element_blank())
    }
    if('ylab' %in% elements_to_remove) {
        ggplot_obj <- ggplot_obj + theme(axis.title.y = element_blank())
    }
    if('title' %in% elements_to_remove) {
        ggplot_obj <- ggplot_obj + theme(plot.title = element_blank())
    }
    if('subtitle' %in% elements_to_remove) {
        ggplot_obj <- ggplot_obj + theme(plot.subtitle = element_blank())
    }
    if('caption' %in% elements_to_remove) {
        ggplot_obj <- ggplot_obj + theme(plot.caption = element_blank())
    }
    if('legend_title' %in% elements_to_remove) {
        ggplot_obj <- ggplot_obj + theme(legend.title = element_blank())
    }
    if('xaxis' %in% elements_to_remove) {
        ggplot_obj <- ggplot_obj +
            theme(axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank())
    }
    if('yaxis' %in% elements_to_remove) {
        ggplot_obj <- ggplot_obj +
            theme(axis.title.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank())
    }
    if('legend' %in% elements_to_remove) {
        ggplot_obj <- ggplot_obj + theme(legend.position = 'none')
    }
    if('grid' %in% elements_to_remove) {
        ggplot_obj <- ggplot_obj +
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor = element_blank())
    }
    if('grid_x' %in% elements_to_remove) {
        ggplot_obj <- ggplot_obj +
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.minor = element_blank())
    }
    if('grid_y' %in% elements_to_remove) {
        ggplot_obj <- ggplot_obj +
            theme(panel.grid.major.y = element_blank(),
                  panel.grid.minor = element_blank())
    }
    if('xticks' %in% elements_to_remove) {
        ggplot_obj <- ggplot_obj + theme(axis.ticks.x = element_blank())
    }
    if('yticks' %in% elements_to_remove) {
        ggplot_obj <- ggplot_obj + theme(axis.ticks.y = element_blank())
    }
    return(ggplot_obj)
}


#' Remove x-axis label
#' @rdname gg_remove_xlab
gg_remove_xlab <- function(ggplot_obj) {
    if(!is.ggplot(ggplot_obj)) stop('ggplot_obj must be a ggplot2 object')
    ggplot_obj <- ggplot_obj + theme(axis.title.x = element_blank())
    return(ggplot_obj)
}


#' Remove y-axis label
#' @rdname gg_remove_ylab
gg_remove_ylab <- function(ggplot_obj) {
    if(!is.ggplot(ggplot_obj)) stop('ggplot_obj must be a ggplot2 object')
    ggplot_obj <- ggplot_obj + theme(axis.title.y = element_blank())
    return(ggplot_obj)
}


#' Remove plot title
#' @rdname gg_remove_title
gg_remove_title <- function(ggplot_obj) {
    if(!is.ggplot(ggplot_obj)) stop('ggplot_obj must be a ggplot2 object')
    ggplot_obj <- ggplot_obj + theme(plot.title = element_blank())
    return(ggplot_obj)
}


#' Remove plot subtitle
#' @rdname gg_remove_subtitle
gg_remove_subtitle <- function(ggplot_obj) {
    if(!is.ggplot(ggplot_obj)) stop('ggplot_obj must be a ggplot2 object')
    ggplot_obj <- ggplot_obj + theme(plot.subtitle = element_blank())
    return(ggplot_obj)
}


#' Remove plot caption
#' @rdname gg_remove_caption
gg_remove_caption <- function(ggplot_obj) {
    if(!is.ggplot(ggplot_obj)) stop('ggplot_obj must be a ggplot2 object')
    ggplot_obj <- ggplot_obj + theme(plot.caption = element_blank())
    return(ggplot_obj)
}


#' Remove legend title
#' @rdname gg_remove_legend_title
gg_remove_legend_title <- function(ggplot_obj) {
    if(!is.ggplot(ggplot_obj)) stop('ggplot_obj must be a ggplot2 object')
    ggplot_obj <- ggplot_obj + theme(legend.title = element_blank())
    return(ggplot_obj)
}


#' Remove x-axis
#' @rdname gg_remove_xaxis
gg_remove_xaxis <- function(ggplot_obj) {
    if(!is.ggplot(ggplot_obj)) stop('ggplot_obj must be a ggplot2 object')
    ggplot_obj <- ggplot_obj +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank())
    return(ggplot_obj)
}


#' Remove y-axis
#' @rdname gg_remove_yaxis
gg_remove_yaxis <- function(ggplot_obj) {
    if(!is.ggplot(ggplot_obj)) stop('ggplot_obj must be a ggplot2 object')
    ggplot_obj <- ggplot_obj +
        theme(axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank())
    return(ggplot_obj)
}


#' Remove plot legend
#' @rdname gg_remove_legend
gg_remove_legend <- function(ggplot_obj) {
    if(!is.ggplot(ggplot_obj)) stop('ggplot_obj must be a ggplot2 object')
    ggplot_obj <- ggplot_obj + theme(legend.position = 'none')
    return(ggplot_obj)
}


#' Remove both x- and y-axis grids
#' @rdname gg_remove_grid
gg_remove_grid <- function(ggplot_obj) {
    if(!is.ggplot(ggplot_obj)) stop('ggplot_obj must be a ggplot2 object')
    ggplot_obj <- ggplot_obj +
        theme(panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor = element_blank())
    return(ggplot_obj)
}


#' Remove x-axis grids
#' @rdname gg_remove_grid_x
gg_remove_grid_x <- function(ggplot_obj) {
    if(!is.ggplot(ggplot_obj)) stop('ggplot_obj must be a ggplot2 object')
    ggplot_obj <- ggplot_obj +
        theme(panel.grid.major.x = element_blank(),
              panel.grid.minor = element_blank())
    return(ggplot_obj)
}


#' Remove y-axis grids
#' @rdname gg_remove_grid_y
gg_remove_grid_y <- function(ggplot_obj) {
    if(!is.ggplot(ggplot_obj)) stop('ggplot_obj must be a ggplot2 object')
    ggplot_obj <- ggplot_obj +
        theme(panel.grid.major.y = element_blank(),
              panel.grid.minor = element_blank())
    return(ggplot_obj)
}


#' Remove x-axis tick marks
#' @rdname gg_remove_xticks
gg_remove_xticks <- function(ggplot_obj) {
    if(!is.ggplot(ggplot_obj)) stop('ggplot_obj must be a ggplot2 object')
    ggplot_obj <- ggplot_obj + theme(axis.ticks.x = element_blank())
    return(ggplot_obj)
}


#' Remove y-axis tick marks
#' @rdname gg_remove_yticks
gg_remove_yticks <- function(ggplot_obj) {
    if(!is.ggplot(ggplot_obj)) stop('ggplot_obj must be a ggplot2 object')
    ggplot_obj <- ggplot_obj + theme(axis.ticks.y = element_blank())
    return(ggplot_obj)
}


