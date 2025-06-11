#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
histClass <- if (requireNamespace('jmvcore', quietly = TRUE)) {
    R6::R6Class(
        "histClass",
        inherit = histBase,
        private = list(
            .run = function() {
                if (is.null(self$options$var)) {
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
                        dplyr::select(y = !!sym(self$options$var)) |>
                        dplyr::mutate(y = jmvcore::toNumeric(y))
                } else {
                    df <- self$data |>
                        dplyr::select(y = !!sym(self$options$var), group = !!sym(group)) |>
                        dplyr::mutate(y = jmvcore::toNumeric(y), group = factor(group))
                }

                image$setState(df)
            },
            .histPlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                group <- self$options$group

                if (is.null(group)) {
                    p <- ggplot(image$state, aes(x = y))

                    if (self$options$bins) {
                        p <- p +
                            ggplot2::geom_histogram(
                                binwidth = self$options$binWidth,
                                color = theme$color[1],
                                fill = theme$fill[2],
                                alpha = self$options$binOpacity
                            )
                    }

                    if (self$options$line) {
                        p <- p +
                            ggplot2::geom_freqpoly(
                                binwidth = self$options$binWidth,
                                color = theme$color[1],
                                fill = theme$fill[2],
                                size = self$options$lineSize
                            )
                    }

                    if (self$options$density) {
                        p <- p +
                            ggplot2::geom_density(
                                aes(y = ggplot2::after_stat(count) * self$options$binWidth),
                                color = theme$color[1],
                                fill = theme$fill[2],
                                alpha = self$options$densityOpacity,
                                size = self$options$densityLineSize
                            )
                    }

                    p <- p + ggtheme
                } else {
                    p <- ggplot(
                        image$state,
                        aes(x = y, fill = group, color = group)
                    )

                    if (self$options$bins) {
                        p <- p +
                            ggplot2::geom_histogram(
                                position = "identity",
                                binwidth = self$options$binWidth,
                                color = theme$color[1],
                                alpha = self$options$binOpacity
                            )
                    }

                    if (self$options$line) {
                        p <- p +
                            ggplot2::geom_freqpoly(
                                position = "identity",
                                binwidth = self$options$binWidth,
                                size = self$options$lineSize
                            )
                    }

                    if (self$options$density) {
                        p <- p +
                            ggplot2::geom_density(
                                aes(y = ggplot2::after_stat(count) * self$options$binWidth),
                                position = "identity",
                                alpha = self$options$densityOpacity,
                                size = self$options$densityLineSize
                            )
                    }

                    p <- p + ggtheme + formatLegend(self$options)
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
                    xLabel = self$options$var,
                    yLabel = .("Frequency (N)"),
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
