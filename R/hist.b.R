#' @importFrom ggplot2 ggplot aes geom_histogram labs theme element_text xlim ylim
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
                    geom_histogram(
                        bins = self$options$nBins,
                        color = theme$color[1],
                        fill = theme$fill[2]
                    ) +
                    ggtheme

                if (self$options$flipAxes) p <- p + coord_flip()

                if (self$options$yAxisRangeType == "manual")
                    p <- p + ylim(self$options$yAxisRangeMin, self$options$yAxisRangeMax)

                if (self$options$xAxisRangeType == "manual")
                    p <- p + xlim(self$options$xAxisRangeMin, self$options$xAxisRangeMax)

                title <- self$options$title
                subtitle <- self$options$subtitle
                caption <- self$options$caption
                xLabel <- self$options$xLabel
                yLabel <- self$options$yLabel

                if (title == "") title <- NULL
                if (subtitle == "") subtitle <- NULL
                if (caption == "") caption <- NULL
                if (xLabel == "") xLabel <- self$options$var
                if (yLabel == "") yLabel <- "Count"

                p <- p +
                    labs(
                        title = title,
                        subtitle = subtitle,
                        caption = caption,
                        x = xLabel,
                        y = yLabel
                    )

                if (self$options$flipAxes) {
                    xLabelFontSize <- self$options$yLabelFontSize
                    xLabelAlign <- self$options$yLabelAlign
                    yLabelFontSize <- self$options$xLabelFontSize
                    yLabelAlign <- self$options$xLabelAlign
                    xAxisLabelFontSize <- self$options$yAxisLabelFontSize
                    yAxisLabelFontSize <- self$options$xAxisLabelFontSize
                } else {
                    xLabelFontSize <- self$options$xLabelFontSize
                    xLabelAlign <- self$options$xLabelAlign
                    yLabelFontSize <- self$options$yLabelFontSize
                    yLabelAlign <- self$options$yLabelAlign
                    xAxisLabelFontSize <- self$options$xAxisLabelFontSize
                    yAxisLabelFontSize <- self$options$yAxisLabelFontSize
                }

                p <- p +
                    theme(
                        plot.title = element_text(
                            size = self$options$titleFontSize,
                            hjust = alignText2Number(self$options$titleAlign)
                        ),
                        plot.subtitle = element_text(
                            size = self$options$subtitleFontSize,
                            hjust = alignText2Number(self$options$subtitleAlign)
                        ),
                        plot.caption = element_text(
                            size = self$options$captionFontSize,
                            hjust = alignText2Number(self$options$captionAlign)
                        ),
                        axis.title.x = element_text(
                            size = xLabelFontSize,
                            hjust = alignText2Number(xLabelAlign)
                        ),
                        axis.title.y = element_text(
                            size = yLabelFontSize,
                            hjust = alignText2Number(yLabelAlign)
                        ),
                        axis.text.x = element_text(size = xAxisLabelFontSize),
                        axis.text.y = element_text(size = yAxisLabelFontSize)
                    )

                return(p)
            }
        )
    )
