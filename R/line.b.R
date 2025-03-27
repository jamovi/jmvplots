#' @importFrom ggplot2 ggplot aes geom_point geom_line labs theme element_text coord_flip xlim ylim
lineClass <- if (requireNamespace('jmvcore', quietly = TRUE))
    R6::R6Class(
        "lineClass",
        inherit = lineBase,
        private = list(
            .run = function() {
                if (is.null(self$options$x) || is.null(self$options$y)) return()

                private$.preparePlotData()
            },
            #### Plot functions ----
            .preparePlotData = function() {
                image <- self$results$plot
                image$setSize(self$options$width, self$options$height)

                mode <- self$options$mode
                if (mode == "individual") {
                    df <- private$.prepareIndividualPlotData()
                } else if (mode == "aggregate") {
                    df <- private$.prepareAggregatedPlotData()
                }

                image$setState(df)
            },
            .prepareIndividualPlotData = function() {
                df <- self$data |>
                    dplyr::select(x = !!sym(self$options$x), y = !!sym(self$options$y)) |>
                    dplyr::mutate(y = jmvcore::toNumeric(y))

                return(df)
            },
            .prepareAggregatedPlotData = function() {
                df <- self$data |>
                    dplyr::group_by(!!sym(self$options$x)) |>
                    dplyr::summarize(
                        y = ifelse(
                            self$options$aggregateType == "median",
                            median(!!sym(self$options$y), na.rm = TRUE),
                            mean(!!sym(self$options$y), na.rm = TRUE)
                        ),
                        n = dplyr::n(),
                        sd = sd(!!sym(self$options$y), na.rm = TRUE),
                    ) |>
                    dplyr::mutate(se = sd / sqrt(n)) |>
                    dplyr::mutate(ci = se * qt((self$options$ciWidth) / 2 + .5, n - 1)) |>
                    dplyr::ungroup() |>
                    dplyr::rename(x = !!sym(self$options$x))

                return(df)
            },
            .linePlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) return(FALSE)

                p <- ggplot(image$state, aes(x = x, y = y, group = 1)) +
                    geom_line(size = self$options$lineSize, color = theme$color[1]) +
                    geom_point(size = self$options$pointSize, color = theme$color[1]) +
                    ggtheme

                if (self$options$mode == "aggregate") {
                    errorBars <- self$options$errorBars
                    if (errorBars == "se") {
                        p <- p +
                            geom_errorbar(aes(ymin = y - se, ymax = y + se), width = 0.1)
                    } else if (errorBars == "sd") {
                        p <- p +
                            geom_errorbar(aes(ymin = y - sd, ymax = y + sd), width = 0.1)
                    } else if (errorBars == "ci") {
                        p <- p + geom_errorbar(aes(ymin = y - ci, ymax = y + ci), width = 0.1)
                    }
                }

                if (self$options$flipAxes) p <- p + coord_flip()

                if (self$options$yAxisRangeType == "manual")
                    p <- p + ylim(self$options$yAxisRangeMin, self$options$yAxisRangeMax)

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
