#' @importFrom ggplot2 ggplot aes geom_boxplot labs theme element_text
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
                    geom_boxplot(
                        notch = self$options$notch,
                        color = theme$color[1],
                        fill = theme$fill[2]
                    ) +
                    ggtheme

                if (self$options$flipAxes) p <- p + coord_flip()

                if (self$options$yAxisRangeType == "manual")
                    p <- p + ylim(self$options$yAxisRangeMin, self$options$yAxisRangeMax)

                if (self$options$xAxisLabelFontSizeRevLabels)
                    p <- p + scale_x_discrete(limits = rev)

                title <- self$options$title
                subtitle <- self$options$subtitle
                caption <- self$options$caption
                xLabel <- self$options$xLabel
                yLabel <- self$options$yLabel

                if (title == "") title <- NULL
                if (subtitle == "") subtitle <- NULL
                if (caption == "") caption <- NULL
                if (xLabel == "") xLabel <- self$options$group
                if (yLabel == "") yLabel <- self$options$var

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
