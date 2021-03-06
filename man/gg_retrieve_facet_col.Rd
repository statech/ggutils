% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gg_retrieve.R
\name{gg_retrieve_facet_col}
\alias{gg_retrieve_facet_col}
\title{Retrieve Column Facetting Variable}
\usage{
gg_retrieve_facet_col(ggplot_obj)
}
\arguments{
\item{ggplot_obj}{a ggplot2 object}
}
\value{
Column facetting variable. `NULL` is returned if
 \code{\link[ggplot2]{facet_grid}} is not used in building the ggplot2
 object
}
\description{
Get the name of the column that is mapped to column facetting
 variable of a ggplot2 object through \code{\link[ggplot2]{facet_grid}}.
 Refer to \code{\link[ggplot2]{facet_grid}} for the details
}
\examples{
p <- ggplot(mpg, aes(displ, cty)) + geom_point() + facet_grid(. ~ cyl)
facet_col <- gg_retrieve_facet_col(p)
print(facet_col)
}
\author{
Feiyang Niu (Feiyang.Niu@gilead.com)
}
