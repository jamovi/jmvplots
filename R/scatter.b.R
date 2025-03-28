#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
scatterClass <- if (requireNamespace('jmvcore', quietly = TRUE))
    R6::R6Class(
        "scatterClass",
        inherit = scatterBase,
        private = list(
            .run = function() {
                if (is.null(self$options$x) || is.null(self$options$y)) return()

                private$.preparePlotData()
            },
            #### Plot functions ----
            .preparePlotData = function() {
                image <- self$results$plot
                image$setSize(self$options$width, self$options$height)

                df <- self$data |>
                    dplyr::select(x = !!sym(self$options$x), y = !!sym(self$options$y)) |>
                    dplyr::mutate(x = jmvcore::toNumeric(x), y = jmvcore::toNumeric(y))

                image$setState(df)
            },
            .scatterPlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) return(FALSE)

                p <- ggplot(image$state, aes(x = x, y = y)) +
                    ggplot2::geom_point(
                        size = self$options$pointSize,
                        color = theme$color[1],
                        fill = theme$fill[2]
                    ) +
                    ggtheme

                if (self$options$flipAxes) p <- p + ggplot2::coord_flip()

                if (self$options$yAxisRangeType == "manual")
                    p <- p + ggplot2::ylim(self$options$yAxisRangeMin, self$options$yAxisRangeMax)

                if (self$options$xAxisRangeType == "manual")
                    p <- p + ggplot2::xlim(self$options$xAxisRangeMin, self$options$xAxisRangeMax)

                labelDefaults <- list(xLabel = self$options$x, yLabel = self$options$y)
                p <- p +
                    setLabels(options = self$options, defaults = labelDefaults) +
                    formatLabels(options = self$options, flipAxes = self$options$flipAxes)

                return(p)
            }
        )
    )
