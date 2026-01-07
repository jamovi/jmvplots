#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
jmvhistClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "jmvhistClass",
        inherit = jmvhistBase,
        private = list(
            .init = function() {
                image <- self$results$plot
                image$setSize(self$options$width, self$options$height)
            },
            .run = function() {
                if (is.null(self$options$var)) {
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
                if (self$options$binWidthType == "manual") {
                    binWidth <- self$options$binWidth
                } else {
                    binWidth <- private$.calculateBinWidth(image$state$y)
                }

                if (is.null(group)) {
                    p <- ggplot(image$state, aes(x = y))

                    if (self$options$bins) {
                        p <- p +
                            ggplot2::geom_histogram(
                                binwidth = binWidth,
                                color = theme$color[1],
                                fill = theme$fill[2],
                                alpha = self$options$binOpacity
                            )
                    }

                    if (self$options$line) {
                        p <- p +
                            ggplot2::geom_freqpoly(
                                binwidth = binWidth,
                                color = theme$color[1],
                                fill = theme$fill[2],
                                size = self$options$lineSize
                            )
                    }

                    if (self$options$density) {
                        p <- p +
                            ggplot2::geom_density(
                                aes(y = ggplot2::after_stat(count) * binWidth),
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
                                binwidth = binWidth,
                                color = theme$color[1],
                                alpha = self$options$binOpacity
                            )
                    }

                    if (self$options$line) {
                        p <- p +
                            ggplot2::geom_freqpoly(
                                position = "identity",
                                binwidth = binWidth,
                                size = self$options$lineSize
                            )
                    }

                    if (self$options$density) {
                        p <- p +
                            ggplot2::geom_density(
                                aes(y = ggplot2::after_stat(count) * binWidth),
                                position = "identity",
                                alpha = self$options$densityOpacity,
                                size = self$options$densityLineSize
                            )
                    }

                    p <- p + ggtheme + formatLegend(self$options)
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
                    xLabel = self$options$var,
                    yLabel = .("Frequency (N)"),
                    groupLabel = self$options$group
                )
                p <- p +
                    setLabels(options = self$options, defaults = labelDefaults) +
                    formatLabels(options = self$options, flipAxes = self$options$flipAxes)

                return(p)
            },
            # Calculates an optimal histogram bin width using the Freedman-Diaconis rule.
            # Provides a robust fallback to Scott's rule if the Interquartile Range (IQR) is zero.
            #
            # @param x A numeric vector of data.
            # @return A numeric value for the calculated bin width.
            #
            .calculateBinWidth = function(x) {
                x <- x[!is.na(x)]

                # Return a default binwidth if there is insufficient data for calculation
                if (length(x) < 2) {
                    return(1)
                }

                n <- length(x)
                iqr_val <- IQR(x, type = 7)

                # Use the Freedman-Diaconis rule if IQR is non-zero
                if (iqr_val > 0) {
                    return(2 * iqr_val / (n^(1 / 3)))
                }

                # Fallback for zero IQR (often occurs with discrete or constant data)
                sd_val <- sd(x)

                # If data is constant (sd is 0), return a default binwidth of 1
                if (sd_val == 0) {
                    return(1)
                }

                # As a final fallback, use Scott's rule, which is based on standard deviation
                return(3.5 * sd_val / (n^(1 / 3)))
            }
        ),
        public = list(
            asSource = function() {
                return(.("Syntax mode for plots is not yet available."))
            }
        )
    )
}
