#' @importFrom ggplot2 ggplot aes geom_point labs theme element_text coord_flip xlim ylim
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
                    geom_point(
                        size = self$options$pointSize,
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
                if (xLabel == "") xLabel <- self$options$x
                if (yLabel == "") yLabel <- self$options$y

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
                        plot.title = ggtext::element_markdown(
                            size = self$options$titleFontSize,
                            hjust = alignText2Number(self$options$titleAlign)
                        ),
                        plot.subtitle = ggtext::element_markdown(
                            size = self$options$subtitleFontSize,
                            hjust = alignText2Number(self$options$subtitleAlign)
                        ),
                        plot.caption = ggtext::element_markdown(
                            size = self$options$captionFontSize,
                            hjust = alignText2Number(self$options$captionAlign)
                        ),
                        axis.title.x = ggtext::element_markdown(
                            size = xLabelFontSize,
                            hjust = alignText2Number(xLabelAlign)
                        ),
                        axis.title.y = ggtext::element_markdown(
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
