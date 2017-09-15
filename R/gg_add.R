#' Add Text to a Plot
#'
#' Add text to a \href{http://ggplot2.org/}{ggplot2} object in a similar way to
#'  \code{\link[graphics]{legend}}
#'
#' @param ggplot_obj a ggplot2 object
#' @param x,y the x and y co-ordinates to be used to position the legend. They
#'  can be specified by keyword or in any way which is accepted by
#'  \code{\link[grDevices]{xy.coords}}. See the details of
#'  \code{\link[graphics]{legend}}
#' @param text a character vector or \code{\link[base]{expression}} specifying
#'  the text to be written.
#' @param on_border when the location is specified by setting \code{x} to a
#'  single keyword from the list "bottomright", "bottom", "bottomleft", "left",
#'  "topleft", "top", "topright", "right" and "center", \code{text} can be
#'  placed right adjacent to the border (`on_border = TRUE`) or placed with
#'  proper space away from the border (`on_border = FALSE`)
#' @param panel_recycling When \code{text} is added to a plot with panel
#'  structure, i.e. either \code{\link[ggplot2]{facet_grid}} or
#'  \code{\link[ggplot2]{facet_grid}} is used in building the ggplot2 object,
#'  and lengths of \code{x}, \code{y}, and \code{text} are smaller than the
#'  number of panels, \code{text} and its corresponding location will be
#'  recycled until the last panel if `panel_recycling = TRUE`
#' @param ... more arguments passed to \code{\link[ggplot2]{geom_text}}
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p_with_text <- gg_add_text(
#'     p, 'topright',
#'     text = paste('Correlation:', round(cor(mtcars$wt, mtcars$mpg), 3))
#' )
#'
#' library(dplyr)
#' q <- ggplot(data = diamonds, aes(depth, price)) + geom_point() +
#'     facet_grid(~ cut)
#' corr_df <- diamonds %>%
#'     group_by(cut) %>%
#'     summarise(corr = paste('Correlation:', round(cor(price, depth), 3)))
#' q_with_text <- gg_add_text(
#'     q, 'top', text = corr_df$corr, on_border = TRUE
#' )
#' @export
#' @author Feiyang Niu (Feiyang.Niu@gilead.com)
gg_add_text <- function(ggplot_obj, x, y = NULL, text, on_border = FALSE,
                        panel_recycling = FALSE, ...) {
    if(!is.ggplot(ggplot_obj)) stop('ggplot_obj must be a ggplot2 object')
    if(is.character(x)) {
        choices_char_x <- c('bottomright', 'bottom', 'bottomleft',  'left',
                            'topleft', 'top', 'topright', 'right', 'center')
        arg_in_choices(x, choices_char_x)
    }
    if (missing(text) && !missing(y) && (is.character(y) || is.expression(y))) {
        text <- y
        y <- NULL
    }
    more_args_list <- list(...)
    has_hjust <- 'hjust' %in% names(more_args_list)
    has_vjust <- 'vjust' %in% names(more_args_list)
    hjust <- if(has_hjust) more_args_list[['hjust']] else NA
    vjust <- if(has_vjust) more_args_list[['vjust']] else NA
    has_facet_layout <- gg_has_facet_layout(ggplot_obj)
    range_df <- gg_retrieve_range(ggplot_obj)
    n_rows <- nrow(range_df)
    if(has_facet_layout && panel_recycling) {
        n_coord <- max(length(x), length(text))
        if(!missing(y) && !is.null(y)) {
            n_coord <- max(n_coord, length(y))
            y <- rep(y, length.out = n_coord)
            length(y) <- n_rows
        }
        x <- rep(x, length.out = n_coord)
        text <- rep(text, length.out = n_coord)
        hjust <- rep(hjust, length.out = n_coord)
        vjust <- rep(vjust, length.out = n_coord)
        length(x) <- n_rows
        length(text) <- n_rows
        length(hjust) <- n_rows
        length(vjust) <- n_rows
    } else {
        n_rows <- max(length(x), length(text), n_rows)
        if(!missing(y) && !is.null(y)) {
            n_rows <- max(n_rows, length(y))
            y <- rep(y, length.out = n_rows)
        }
        x <- rep(x, length.out = n_rows)
        text <- rep(text, length.out = n_rows)
        hjust <- rep(hjust, length.out = n_rows)
        vjust <- rep(vjust, length.out = n_rows)
        range_df <- data.frame(lapply(range_df, rep, length.out = n_rows))
    }
    if(is.character(x)) {
        if(on_border) {
            text_min_x <- rep(-Inf, n_rows)
            text_max_x <- rep(Inf, n_rows)
            text_min_y <- rep(-Inf, n_rows)
            text_max_y <- rep(Inf, n_rows)
        } else {
            corner_space <- 0.04
            text_min_x <- with(range_df, x_min + (x_max - x_min) * corner_space)
            text_max_x <- with(range_df, x_max - (x_max - x_min) * corner_space)
            text_min_y <- with(range_df, y_min + (y_max - y_min) * corner_space)
            text_max_y <- with(range_df, y_max - (y_max - y_min) * corner_space)
        }
        text_mid_x <- with(range_df, (x_min + x_max) / 2)
        text_mid_y <- with(range_df, (y_min + y_max) / 2)
        range_df$x <- rep(NA, n_rows)
        range_df$y <- rep(NA, n_rows)
        for(i in seq_len(n_rows)) {
            if(is.na(x[i])) {
                range_df$x[i] <- NA; range_df$y[i] <- NA
                hjust[i] <- NA; vjust[i] <- NA
            } else if(x[i] == 'bottomleft') {
                range_df$x[i] <- text_min_x[i]; range_df$y[i] <- text_min_y[i]
                if(is.na(hjust[i])) hjust[i] <- 0
                if(is.na(vjust[i])) vjust[i] <- 0
            } else if(x[i] == 'left') {
                range_df$x[i] <- text_min_x[i]; range_df$y[i] <- text_mid_y[i]
                if(is.na(hjust[i])) hjust[i] <- 0
                if(is.na(vjust[i])) vjust[i] <- 0.5
            } else if(x[i] == 'topleft') {
                range_df$x[i] <- text_min_x[i]; range_df$y[i] <- text_max_y[i]
                if(is.na(hjust[i])) hjust[i] <- 0
                if(is.na(vjust[i])) vjust[i] <- 1
            } else if(x[i] == 'top') {
                range_df$x[i] <- text_mid_x[i]; range_df$y[i] <- text_max_y[i]
                if(is.na(hjust[i])) hjust[i] <- 0.5
                if(is.na(vjust[i])) vjust[i] <- 1
            } else if(x[i] == 'topright') {
                range_df$x[i] <- text_max_x[i]; range_df$y[i] <- text_max_y[i]
                if(is.na(hjust[i])) hjust[i] <- 1
                if(is.na(vjust[i])) vjust[i] <- 1
            } else if(x[i] == 'right') {
                range_df$x[i] <- text_max_x[i]; range_df$y[i] <- text_mid_y[i]
                if(is.na(hjust[i])) hjust[i] <- 1
                if(is.na(vjust[i])) vjust[i] <- 0.5
            } else if(x[i] == 'bottomright') {
                range_df$x[i] <- text_max_x[i]; range_df$y[i] <- text_min_y[i]
                if(is.na(hjust[i])) hjust[i] <- 1
                if(is.na(vjust[i])) vjust[i] <- 0
            } else if (x[i] == 'bottom') {
                range_df$x[i] <- text_mid_x[i]; range_df$y[i] <- text_min_y[i]
                if(is.na(hjust[i])) hjust[i] <- 0.5
                if(is.na(vjust[i])) vjust[i] <- 0
            } else {
                range_df$x[i] <- text_mid_x[i]; range_df$y[i] <- text_mid_y[i]
                if(is.na(hjust[i])) hjust[i] <- 0.5
                if(is.na(vjust[i])) vjust[i] <- 0.5
            }
        }
    } else {
        range_df$x <- x
        range_df$y <- y
    }
    range_df$label <- text
    range_df$hjust <- hjust
    range_df$vjust <- vjust
    cols_to_remove <- c(c('x_min', 'x_max', 'y_min', 'y_max'))
    range_df <-range_df[setdiff(names(range_df), cols_to_remove)]

    more_args_list$hjust <- NULL
    more_args_list$vjust <- NULL
    more_args_list$mapping <- aes(x = x, y =y, label = label,
                                  hjust = hjust, vjust = vjust)
    more_args_list$data <- range_df
    more_args_list$inherit.aes <- FALSE
    ggplot_obj <- ggplot_obj + do.call(geom_text, more_args_list)
    return(ggplot_obj)
}
