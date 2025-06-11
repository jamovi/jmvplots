#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
boxClass <- if (requireNamespace('jmvcore', quietly = TRUE)) {
    R6::R6Class(
        "boxClass",
        inherit = boxBase,
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

                var <- self$options$var
                group1 <- self$options$group1
                group2 <- self$options$group2

                if (is.null(group1) && is.null(group2)) {
                    df <- self$data |>
                        dplyr::select(y = !!sym(var)) |>
                        dplyr::mutate(x = "1")
                } else if (is.null(group1) || is.null(group2)) {
                    group <- ifelse(is.null(group1), group2, group1)

                    df <- self$data |>
                        dplyr::select(x = !!sym(group), y = !!sym(var))
                } else {
                    df <- self$data |>
                        dplyr::select(x = !!sym(group1), y = !!sym(var), group = !!sym(group2))
                }

                df <- df |>
                    dplyr::mutate(y = jmvcore::toNumeric(y))

                image$setState(df)
            },
            .boxPlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                if (is.null(self$options$group1) || is.null(self$options$group2)) {
                    p <- ggplot(image$state, aes(x = x, y = y)) +
                        ggplot2::geom_boxplot(
                            notch = self$options$notch,
                            width = self$options$boxWidth,
                            outliers = self$options$outliers,
                            color = theme$color[1],
                            fill = theme$fill[2]
                        ) +
                        ggtheme
                } else {
                    p <- ggplot(image$state, aes(x = x, y = y, fill = group)) +
                        ggplot2::geom_boxplot(
                            notch = self$options$notch,
                            width = self$options$boxWidth,
                            outliers = self$options$outliers,
                            color = theme$color[1],
                            position = ggplot2::position_dodge2(
                                width = self$options$boxWidth,
                                preserve = "single",
                                padding = 0.3
                            )
                        ) +
                        ggtheme +
                        formatLegend(self$options)
                }

                if (self$options$yAxisRangeType == "manual") {
                    p <- p + ggplot2::ylim(self$options$yAxisRangeMin, self$options$yAxisRangeMax)
                }

                if (self$options$xAxisLabelFontSizeRevLabels) {
                    p <- p + ggplot2::scale_x_discrete(limits = rev)
                }

                labelDefaults <- private$.getDefaultLabels()
                p <- p +
                    setLabels(options = self$options, defaults = labelDefaults) +
                    formatLabels(options = self$options, flipAxes = self$options$flipAxes)

                if (self$options$flipAxes) {
                    p <- p + ggplot2::coord_flip()
                }

                return(p)
            },
            #### Helper functions ----
            .getDefaultLabels = function() {
                if (is.null(self$options$group1) && !is.null(self$options$group2)) {
                    xLabel <- self$options$group2
                    groupLabel <- NULL
                } else if (!is.null(self$options$group1) && is.null(self$options$group2)) {
                    xLabel <- self$options$group1
                    groupLabel <- NULL
                } else {
                    xLabel <- self$options$group1
                    groupLabel <- self$options$group2
                }

                return(list(
                    xLabel = xLabel,
                    yLabel = self$options$var,
                    groupLabel = groupLabel
                ))
            }
        ),
        public = list(
            asSource = function() {
                return(.("Syntax mode for plots is not yet available."))
            }
        )
    )
}
