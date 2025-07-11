#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
scatClass <- if (requireNamespace('jmvcore', quietly = TRUE)) {
    R6::R6Class(
        "scatClass",
        inherit = scatBase,
        private = list(
            .run = function() {
                if (is.null(self$options$x) || is.null(self$options$y)) {
                    return()
                }

                private$.preparePlotData()
            },
            #### Plot functions ----
            .preparePlotData = function() {
                image <- self$results$plot
                image$setSize(self$options$width, self$options$height)

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

                # In previous version of scat, the `line` option was a string value ("none",
                # "linear", or "smooth"). Because in the current version, the `line` option is
                # a boolean we add a check and convert the string value to a boolean to remain
                # backwards compatible.
                line <- self$options$line
                if (!is.logical(line)) {
                    line <- line != "none"
                }

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

                if (self$options$flipAxes) {
                    p <- p + ggplot2::coord_flip()
                }

                if (self$options$yAxisRangeType == "manual") {
                    p <- p + ggplot2::ylim(self$options$yAxisRangeMin, self$options$yAxisRangeMax)
                }

                if (self$options$xAxisRangeType == "manual") {
                    p <- p + ggplot2::xlim(self$options$xAxisRangeMin, self$options$xAxisRangeMax)
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
