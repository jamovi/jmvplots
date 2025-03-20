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

                df <- data.frame(
                    x = self$data[[self$options$group]],
                    y = self$data[[self$options$var]]
                )

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

                p <- p +
                    theme(
                        plot.title = element_text(
                            hjust = private$.alignText2Number(self$options$titleAlign)
                        ),
                        plot.subtitle = element_text(
                            hjust = private$.alignText2Number(self$options$subtitleAlign)
                        ),
                        plot.caption = element_text(
                            hjust = private$.alignText2Number(self$options$captionAlign)
                        ),
                        axis.title.x = element_text(
                            hjust = private$.alignText2Number(self$options$xLabelAlign)
                        ),
                        axis.title.y = element_text(
                            hjust = private$.alignText2Number(self$options$yLabelAlign)
                        )
                    )

                return(p)
            },
            .alignText2Number = function(text) {
                if (text == "left") return(0)
                if (text == "center") return(0.5)
                if (text == "right") return(1)
            }
        )
    )
