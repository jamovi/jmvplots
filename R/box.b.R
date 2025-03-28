#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
boxClass <- if (requireNamespace('jmvcore', quietly = TRUE))
    R6::R6Class(
        "boxClass",
        inherit = boxBase,
        private = list(
            .run = function() {
                if (is.null(self$options$var)) return()

                private$.preparePlotData()
            },
            #### Plot functions ----
            .preparePlotData = function() {
                image <- self$results$plot
                image$setSize(self$options$width, self$options$height)

                group <- self$options$group
                var <- self$options$var

                if (is.null(group)) {
                    df <- self$data |>
                        dplyr::select(y = !!sym(var)) |>
                        dplyr::mutate(x = "1")
                } else {
                    df <- self$data |>
                        dplyr::select(x = !!sym(group), y = !!sym(var))
                }

                image$setState(df)
            },
            .boxPlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) return(FALSE)

                p <- ggplot(image$state, aes(x = x, y = y)) +
                    ggplot2::geom_boxplot(
                        notch = self$options$notch,
                        color = theme$color[1],
                        fill = theme$fill[2]
                    ) +
                    ggtheme

                if (self$options$flipAxes) p <- p + ggplot2::coord_flip()

                if (self$options$yAxisRangeType == "manual")
                    p <- p + ggplot2::ylim(self$options$yAxisRangeMin, self$options$yAxisRangeMax)

                if (self$options$xAxisLabelFontSizeRevLabels)
                    p <- p + ggplot2::scale_x_discrete(limits = rev)

                labelDefaults <- list(xLabel = self$options$group, yLabel = self$options$var)
                p <- p +
                    setLabels(options = self$options, defaults = labelDefaults) +
                    formatLabels(options = self$options, flipAxes = self$options$flipAxes)

                return(p)
            }
        )
    )
