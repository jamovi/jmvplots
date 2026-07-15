#' @importFrom ggplot2 ggplot aes after_stat
#' @importFrom rlang sym
#' @importFrom jmvcore .
jmvhistClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "jmvhistClass",
        inherit = jmvhistBase,
        private = list(
            .init = function() {
                image <- self$results$plot
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

                if (self$options$binWidthType == "manual") {
                    binWidth <- self$options$binWidth
                } else {
                    binWidth <- private$.calculateBinWidth(image$state$y)
                }

                plot_call_list <- private$.getPlotCallList(image$state, theme, binWidth)

                theme_call_list_args <- list()
                if (!is.null(self$options$group)) {
                    theme_call_list_args <- utils::modifyList(
                        theme_call_list_args,
                        getLegendThemeCallArgs(self$options)
                    )
                }
                theme_call_list_args <- utils::modifyList(
                    theme_call_list_args,
                    getLabelsThemeCallArgs(self$options, self$options$flipAxes)
                )

                p <- createPlotFromCallStack(plot_call_list) +
                    ggtheme +
                    do.call(ggplot2::theme, theme_call_list_args)

                p <- autoscalePlotBreaks(p, image$width, image$height)
                return(p)
            },
            .getPlotCallList = function(data, theme, binWidth) {
                group <- self$options$group
                if (is.null(group)) {
                    mapping <- aes(x = y)
                } else {
                    mapping <- aes(x = y, fill = group, color = group)
                }

                plot_call_list <- list(
                    "ggplot" = list(
                        fun = ggplot2::ggplot,
                        args = list(data = data, mapping = mapping)
                    )
                )

                if (is.null(group)) {
                    if (self$options$bins) {
                        plot_call_list$geom_histogram <- list(
                            fun = ggplot2::geom_histogram,
                            args = list(
                                binwidth = binWidth,
                                color = theme$color[1],
                                fill = theme$fill[2],
                                alpha = self$options$binOpacity
                            )
                        )
                    }

                    if (self$options$line) {
                        plot_call_list$geom_freqpoly <- list(
                            fun = ggplot2::geom_freqpoly,
                            args = list(
                                binwidth = binWidth,
                                color = theme$color[1],
                                fill = theme$fill[2],
                                linewidth = self$options$lineSize
                            )
                        )
                    }

                    if (self$options$density) {
                        plot_call_list$geom_density <- list(
                            fun = ggplot2::geom_density,
                            args = list(
                                mapping = eval(bquote(aes(y = after_stat(count) * .(binWidth)))),
                                color = theme$color[1],
                                fill = theme$fill[2],
                                alpha = self$options$densityOpacity,
                                linewidth = self$options$densityLineSize
                            )
                        )
                    }
                } else {
                    if (self$options$bins) {
                        plot_call_list$geom_histogram <- list(
                            fun = ggplot2::geom_histogram,
                            args = list(
                                position = "identity",
                                binwidth = binWidth,
                                color = theme$color[1],
                                alpha = self$options$binOpacity
                            )
                        )
                    }

                    if (self$options$line) {
                        plot_call_list$geom_freqpoly <- list(
                            fun = ggplot2::geom_freqpoly,
                            args = list(
                                position = "identity",
                                binwidth = binWidth,
                                linewidth = self$options$lineSize
                            )
                        )
                    }

                    if (self$options$density) {
                        plot_call_list$geom_density <- list(
                            fun = ggplot2::geom_density,
                            args = list(
                                mapping = eval(bquote(aes(y = after_stat(count) * .(binWidth)))),
                                position = "identity",
                                alpha = self$options$densityOpacity,
                                linewidth = self$options$densityLineSize
                            )
                        )
                    }
                }

                labelDefaults <- list(
                    xLabel = self$options$var,
                    yLabel = .("Frequency (N)"),
                    groupLabel = self$options$group
                )
                plot_call_list$labs <- getLabsCallList(self$options, labelDefaults)

                ylims <- NULL
                if (self$options$yAxisRangeType == "manual") {
                    ylims <- c(self$options$yAxisRangeMin, self$options$yAxisRangeMax)
                }

                xlims <- NULL
                if (self$options$xAxisRangeType == "manual") {
                    xlims <- c(self$options$xAxisRangeMin, self$options$xAxisRangeMax)
                }

                if (self$options$flipAxes) {
                    plot_call_list$coord_flip <- list(
                        fun = ggplot2::coord_flip,
                        args = list(ylim = ylims, xlim = xlims)
                    )
                } else {
                    plot_call_list$coord_cartesian <- list(
                        fun = ggplot2::coord_cartesian,
                        args = list(ylim = ylims, xlim = xlims)
                    )
                }

                return(plot_call_list)
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
                if (is.null(self$options$var)) {
                    return("")
                }

                data_prep_code <- generateDataPrepCode(self$options, "hist")

                var <- self$options$var
                y_vals <- jmvcore::toNumeric(self$data[[var]])
                if (self$options$binWidthType == "manual") {
                    binWidth <- self$options$binWidth
                } else {
                    binWidth <- private$.calculateBinWidth(y_vals)
                }

                call_list <- private$.getPlotCallList(
                    data = self$data,
                    theme = getSyntaxThemeColors(self$options$theme, self$options$palette),
                    binWidth = binWidth
                )
                return(finalizePlotSyntax(
                    self$options,
                    call_list,
                    data_prep_code,
                    hasLegend = !is.null(self$options$group),
                    flipAxes = self$options$flipAxes,
                    continuousX = TRUE
                ))
            }
        )
    )
}
