#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
scatClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "scatClass",
        inherit = scatBase,
        private = list(
            .init = function() {
                image <- self$results$plot
            },
            .run = function() {
                if (is.null(self$options$x) || is.null(self$options$y)) {
                    return()
                }

                private$.preparePlotData()
            },
            #### Plot functions ----
            .preparePlotData = function() {
                image <- self$results$plot

                group <- self$options$group

                if (is.null(group)) {
                    df <- self$data |>
                        dplyr::select(x = !!sym(self$options$x), y = !!sym(self$options$y)) |>
                        dplyr::mutate(x = jmvcore::toNumeric(x), y = jmvcore::toNumeric(y))
                } else {
                    df <- self$data |>
                        dplyr::select(
                            x = !!sym(self$options$x),
                            y = !!sym(self$options$y),
                            group = !!sym(group)
                        ) |>
                        dplyr::mutate(
                            x = jmvcore::toNumeric(x),
                            y = jmvcore::toNumeric(y),
                            group = factor(group)
                        )
                }

                image$setState(df)
            },
            .scatterPlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                group <- self$options$group
                line <- self$options$regLine

                if (is.null(group)) {
                    p <- ggplot(image$state, aes(x = x, y = y)) +
                        ggplot2::geom_point(
                            size = self$options$pointSize,
                            color = theme$color[1],
                            fill = theme$fill[2]
                        ) +
                        ggtheme

                    if (line) {
                        p <- p +
                            ggplot2::geom_smooth(
                                method = self$options$lineMethod,
                                se = self$options$lineSE,
                                formula = y ~ x,
                                color = theme$color[1],
                                fill = theme$fill[2]
                            )
                    }
                } else {
                    p <- ggplot(image$state, aes(x = x, y = y, color = group, fill = group)) +
                        ggplot2::geom_point(
                            size = self$options$pointSize,
                        ) +
                        ggtheme +
                        formatLegend(self$options)

                    if (line) {
                        p <- p +
                            ggplot2::geom_smooth(
                                method = self$options$lineMethod,
                                se = self$options$lineSE,
                                formula = y ~ x,
                            )
                    }
                }

                ylims <- NULL
                if (self$options$yAxisRangeType == "manual") {
                    ylims <- c(self$options$yAxisRangeMin, self$options$yAxisRangeMax)
                }

                xlims <- NULL
                if (self$options$xAxisRangeType == "manual") {
                    xlims <- c(self$options$xAxisRangeMin, self$options$xAxisRangeMax)
                }

                if (self$options$flipAxes) {
                    p <- p + ggplot2::coord_flip(ylim = ylims, xlim = xlims)
                } else {
                    p <- p + ggplot2::coord_cartesian(ylim = ylims, xlim = xlims)
                }

                labelDefaults <- list(
                    xLabel = self$options$x,
                    yLabel = self$options$y,
                    groupLabel = self$options$group
                )
                p <- p +
                    setLabels(options = self$options, defaults = labelDefaults) +
                    formatLabels(options = self$options, flipAxes = self$options$flipAxes)

                return(p)
            }
        ),
        public = list(
            asSource = function() {
                return(.("Syntax mode for plots is not yet available."))
            }
        )
    )
}
