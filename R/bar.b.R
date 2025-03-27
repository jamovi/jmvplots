#' @importFrom ggplot2 ggplot aes geom_bar labs coord_flip geom_errorbar theme element_blank
#' element_text ylim scale_x_discrete
#' @importFrom rlang sym
barClass <- if (requireNamespace('jmvcore', quietly = TRUE))
    R6::R6Class(
        "barClass",
        inherit = barBase,
        private = list(
            .run = function() {
                mode <- self$options$mode
                if (
                    (mode == "categorical" && is.null(self$options$catvar)) ||
                        (mode == "continuous" && is.null(self$options$convar)) ||
                        (mode == "counts" && is.null(self$options$counts))
                )
                    return()

                private$.preparePlotData()
            },
            #### Plot functions ----
            .preparePlotData = function() {
                image <- self$results$plot
                image$setSize(self$options$width, self$options$height)

                mode <- self$options$mode
                if (mode == "continuous") {
                    df <- private$.prepareContinuousPlotData()
                } else if (mode == "categorical") {
                    df <- private$.prepareCategoricalPlotData()
                } else if (mode == "counts") {
                    df <- private$.prepareCountsPlotData()
                }

                image$setState(df)
            },
            .prepareContinuousPlotData = function() {
                var = self$options$convar
                group = self$options$congroup

                if (is.null(group)) {
                    df <- self$data |>
                        dplyr::summarize(
                            y = mean(!!sym(var), na.rm = TRUE),
                            n = dplyr::n(),
                            sd = sd(!!sym(var), na.rm = TRUE),
                        ) |>
                        dplyr::mutate(se = sd / sqrt(n)) |>
                        dplyr::mutate(ci = se * qt((self$options$ciWidth) / 2 + .5, n - 1)) |>
                        dplyr::mutate(x = "1")
                } else {
                    df <- self$data |>
                        dplyr::group_by(!!sym(group)) |>
                        dplyr::summarize(
                            y = mean(!!sym(var), na.rm = TRUE),
                            n = dplyr::n(),
                            sd = sd(!!sym(var), na.rm = TRUE),
                        ) |>
                        dplyr::mutate(se = sd / sqrt(n)) |>
                        dplyr::mutate(ci = se * qt((self$options$ciWidth) / 2 + .5, n - 1)) |>
                        dplyr::ungroup() |>
                        dplyr::rename(x = !!sym(group))
                }

                return(df)
            },
            .prepareCategoricalPlotData = function() {
                var = self$options$catvar

                df <- self$data |>
                    dplyr::count(!!sym(var)) |>
                    dplyr::rename(y = n) |>
                    dplyr::rename(x = !!sym(var)) |>
                    dplyr::filter(!is.na(x))

                return(df)
            },
            .prepareCountsPlotData = function() {
                var = self$options$counts
                labels = self$options$countsLabels

                if (is.null(labels)) {
                    df <- self$data |>
                        dplyr::select(!!sym(var)) |>
                        dplyr::rename(y = !!sym(var)) |>
                        dplyr::filter(!is.na(y)) |>
                        dplyr::mutate(y = jmvcore::toNumeric(y)) |>
                        dplyr::mutate(x = factor(1:dplyr::n()))
                } else {
                    df <- self$data |>
                        dplyr::select(!!sym(var), !!sym(labels)) |>
                        dplyr::rename(y = !!sym(var), x = !!sym(labels)) |>
                        dplyr::filter(!is.na(y)) |>
                        dplyr::mutate(y = jmvcore::toNumeric(as.numeric(y)))
                }

                return(df)
            },
            .barPlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) return(FALSE)

                mode <- self$options$mode

                p <- ggplot(image$state, aes(x = x, y = y)) +
                    geom_bar(
                        stat = "identity",
                        width = self$options$barWidth,
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

                if (mode == "categorical") {
                    if (xLabel == "") xLabel <- self$options$catvar
                    if (yLabel == "") yLabel <- "Count"
                } else if (mode == "continuous") {
                    if (xLabel == "") xLabel <- self$options$congroup
                    if (yLabel == "") yLabel <- self$options$convar

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

                    if (is.null(self$options$congroup)) {
                        p <- p +
                            theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
                    }
                } else if (mode == "counts") {
                    if (xLabel == "") xLabel <- self$options$countsLabels
                    if (yLabel == "") yLabel <- self$options$counts

                    if (is.null(self$options$countsLabels)) {
                        p <- p +
                            theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
                    }
                }

                if (self$options$yAxisRangeType == "manual")
                    p <- p + ylim(self$options$yAxisRangeMin, self$options$yAxisRangeMax)

                if (self$options$xAxisLabelFontSizeRevLabels)
                    p <- p + scale_x_discrete(limits = rev)

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
