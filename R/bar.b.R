#' @importFrom ggplot2 ggplot aes
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
                    ggplot2::geom_bar(
                        stat = "identity",
                        width = self$options$barWidth,
                        color = theme$color[1],
                        fill = theme$fill[2]
                    ) +
                    ggtheme

                if (self$options$flipAxes) p <- p + ggplot2::coord_flip()

                if (mode == "categorical") {
                    xLabel <- self$options$catvar
                    yLabel <- "Count"
                } else if (mode == "continuous") {
                    xLabel <- self$options$congroup
                    yLabel <- self$options$convar

                    errorBars <- self$options$errorBars
                    if (errorBars == "se") {
                        p <- p +
                            ggplot2::geom_errorbar(aes(ymin = y - se, ymax = y + se), width = 0.1)
                    } else if (errorBars == "sd") {
                        p <- p +
                            ggplot2::geom_errorbar(aes(ymin = y - sd, ymax = y + sd), width = 0.1)
                    } else if (errorBars == "ci") {
                        p <- p +
                            ggplot2::geom_errorbar(aes(ymin = y - ci, ymax = y + ci), width = 0.1)
                    }

                    if (is.null(self$options$congroup)) {
                        p <- p +
                            ggplot2::theme(
                                axis.text.x = ggplot2::element_blank(),
                                axis.ticks.x = ggplot2::element_blank()
                            )
                    }
                } else if (mode == "counts") {
                    xLabel <- self$options$countsLabels
                    yLabel <- self$options$counts

                    if (is.null(self$options$countsLabels)) {
                        p <- p +
                            ggplot2::theme(
                                axis.text.x = ggplot2::element_blank(),
                                axis.ticks.x = ggplot2::element_blank()
                            )
                    }
                }

                if (self$options$yAxisRangeType == "manual")
                    p <- p + ggplot2::ylim(self$options$yAxisRangeMin, self$options$yAxisRangeMax)

                if (self$options$xAxisLabelFontSizeRevLabels)
                    p <- p + ggplot2::scale_x_discrete(limits = rev)

                labelDefaults <- list(xLabel = xLabel, yLabel = yLabel)
                p <- p +
                    setLabels(options = self$options, defaults = labelDefaults) +
                    formatLabels(options = self$options, flipAxes = self$options$flipAxes)

                return(p)
            }
        )
    )
