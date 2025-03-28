#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
histClass <- if (requireNamespace('jmvcore', quietly = TRUE))
    R6::R6Class(
        "histClass",
        inherit = histBase,
        private = list(
            .run = function() {
                if (is.null(self$options$var)) return()

                private$.preparePlotData()
            },
            #### Plot functions ----
            .preparePlotData = function() {
                image <- self$results$plot
                image$setSize(self$options$width, self$options$height)

                df <- data.frame(
                    y = self$data[[self$options$var]]
                )

                image$setState(df)
            },
            .histPlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) return(FALSE)

                p <- ggplot(image$state, aes(y)) +
                    ggplot2::geom_histogram(
                        bins = self$options$nBins,
                        color = theme$color[1],
                        fill = theme$fill[2]
                    ) +
                    ggtheme

                if (self$options$flipAxes) p <- p + ggplot2::coord_flip()

                if (self$options$yAxisRangeType == "manual")
                    p <- p + ggplot2::ylim(self$options$yAxisRangeMin, self$options$yAxisRangeMax)

                if (self$options$xAxisRangeType == "manual")
                    p <- p + ggplot2::xlim(self$options$xAxisRangeMin, self$options$xAxisRangeMax)

                labelDefaults <- list(xLabel = self$options$var, yLabel = "Count")
                p <- p +
                    setLabels(options = self$options, defaults = labelDefaults) +
                    formatLabels(options = self$options, flipAxes = self$options$flipAxes)

                return(p)
            }
        )
    )
