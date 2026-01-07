#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
jmvlineClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "jmvlineClass",
        inherit = jmvlineBase,
        private = list(
            .init = function() {
                image <- self$results$plot
                image$setSize(self$options$width, self$options$height)
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

                mode <- self$options$mode
                if (mode == "individual") {
                    df <- private$.prepareIndividualPlotData()
                } else if (mode == "aggregate") {
                    df <- private$.prepareAggregatedPlotData()
                }

                image$setState(df)
            },
            .prepareIndividualPlotData = function() {
                group <- self$options$group
                if (is.null(group)) {
                    df <- self$data |>
                        dplyr::select(
                            x = !!sym(self$options$x),
                            y = !!sym(self$options$y),
                        ) |>
                        dplyr::mutate(y = jmvcore::toNumeric(y))
                } else {
                    df <- self$data |>
                        dplyr::select(
                            x = !!sym(self$options$x),
                            y = !!sym(self$options$y),
                            group = !!sym(group)
                        ) |>
                        dplyr::mutate(group = factor(group)) |>
                        dplyr::group_by(group) |>
                        dplyr::mutate(y = jmvcore::toNumeric(y))
                }

                return(df)
            },
            .prepareAggregatedPlotData = function() {
                group <- self$options$group
                if (is.null(group)) {
                    df <- self$data |>
                        dplyr::select(
                            x = !!sym(self$options$x),
                            y = !!sym(self$options$y),
                        ) |>
                        dplyr::mutate(yOld = jmvcore::toNumeric(y)) |>
                        dplyr::group_by(x) |>
                        dplyr::summarize(
                            y = ifelse(
                                self$options$aggregateType == "median",
                                median(yOld, na.rm = TRUE),
                                mean(yOld, na.rm = TRUE)
                            ),
                            n = dplyr::n(),
                            sd = sd(yOld, na.rm = TRUE),
                        ) |>
                        dplyr::mutate(se = sd / sqrt(n)) |>
                        dplyr::mutate(ci = se * qt((self$options$ciWidth / 100) / 2 + .5, n - 1)) |>
                        dplyr::ungroup()
                } else {
                    df <- self$data |>
                        dplyr::select(
                            x = !!sym(self$options$x),
                            y = !!sym(self$options$y),
                            group = !!sym(group)
                        ) |>
                        dplyr::mutate(
                            yOld = jmvcore::toNumeric(y),
                            group = factor(group)
                        ) |>
                        dplyr::group_by(x, group) |>
                        dplyr::summarize(
                            y = ifelse(
                                self$options$aggregateType == "median",
                                median(yOld, na.rm = TRUE),
                                mean(yOld, na.rm = TRUE)
                            ),
                            n = dplyr::n(),
                            sd = sd(yOld, na.rm = TRUE),
                        ) |>
                        dplyr::mutate(se = sd / sqrt(n)) |>
                        dplyr::mutate(ci = se * qt((self$options$ciWidth / 100) / 2 + .5, n - 1)) |>
                        dplyr::ungroup() |>
                        dplyr::select(group, x, y, se, sd, ci)
                }
                return(df)
            },
            .linePlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                if (is.null(self$options$group)) {
                    p <- ggplot(image$state, aes(x = x, y = y, group = 1))

                    if (self$options$line) {
                        p <- p +
                            ggplot2::geom_line(size = self$options$lineSize, color = theme$color[1])
                    }

                    if (self$options$point) {
                        p <- p +
                            ggplot2::geom_point(
                                size = self$options$pointSize,
                                color = theme$color[1]
                            )
                    }

                    p <- p + ggtheme
                } else {
                    p <- ggplot(image$state, aes(x = x, y = y, group = group))

                    if (self$options$groupColor) {
                        p <- p + aes(color = group)
                    }

                    if (self$options$groupPointType) {
                        p <- p + aes(shape = group)
                    }

                    if (self$options$line) {
                        p <- p +
                            ggplot2::geom_line(
                                if (self$options$groupLineType) aes(linetype = group),
                                position = ggplot2::position_dodge(
                                    width = self$options$groupPositionDodge
                                ),
                                size = self$options$lineSize
                            )
                    }

                    if (self$options$point) {
                        p <- p +
                            ggplot2::geom_point(
                                position = ggplot2::position_dodge(
                                    width = self$options$groupPositionDodge
                                ),
                                size = self$options$pointSize
                            )
                    }

                    p <- p + ggtheme + formatLegend(self$options)
                }

                if (self$options$mode == "aggregate") {
                    errorBars <- self$options$errorBars
                    if (errorBars != "none") {
                        p <- p +
                            ggplot2::geom_errorbar(
                                aes(ymin = y - !!sym(errorBars), ymax = y + !!sym(errorBars)),
                                size = self$options$errorBarSize,
                                position = ggplot2::position_dodge(
                                    width = self$options$groupPositionDodge
                                ),
                                width = self$options$errorBarWidth,
                                show.legend = FALSE
                            )
                    }
                }

                if (self$options$flipAxes) {
                    p <- p + ggplot2::coord_flip()
                }

                if (self$options$yAxisRangeType == "manual") {
                    p <- p + ggplot2::ylim(self$options$yAxisRangeMin, self$options$yAxisRangeMax)
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
