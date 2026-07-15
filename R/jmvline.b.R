#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
jmvlineClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "jmvlineClass",
        inherit = jmvlineBase,
        private = list(
            .init = function() {
                image <- self$results$plot
            },
            .run = function() {
                if (is.null(self$options$x) || is.null(self$options$y)) {
                    return()
                }

                private$.preparePlotData()
            },
            #### Plot functions ----
            .preparePlotData = function() {
                image <- self$results$plot

                mode <- self$options$mode
                if (mode == "individual") {
                    df <- private$.prepareIndividualPlotData()
                } else if (mode == "aggregate") {
                    df <- private$.prepareAggregatedPlotData()
                }

                image$setState(df)
            },
            .prepareIndividualPlotData = function() {
                group <- self$options$group
                if (is.null(group)) {
                    df <- self$data |>
                        dplyr::select(
                            x = !!sym(self$options$x),
                            y = !!sym(self$options$y),
                        ) |>
                        dplyr::mutate(y = jmvcore::toNumeric(y))
                } else {
                    df <- self$data |>
                        dplyr::select(
                            x = !!sym(self$options$x),
                            y = !!sym(self$options$y),
                            group = !!sym(group)
                        ) |>
                        dplyr::mutate(
                            group = factor(group),
                            y = jmvcore::toNumeric(y)
                        ) |>
                        dplyr::group_by(group)
                }

                return(df)
            },
            .prepareAggregatedPlotData = function() {
                group <- self$options$group
                if (is.null(group)) {
                    df <- self$data |>
                        dplyr::select(
                            x = !!sym(self$options$x),
                            y = !!sym(self$options$y),
                        ) |>
                        dplyr::mutate(yOld = jmvcore::toNumeric(y)) |>
                        dplyr::group_by(x) |>
                        dplyr::summarize(
                            y = ifelse(
                                self$options$aggregateType == "median",
                                median(yOld, na.rm = TRUE),
                                mean(yOld, na.rm = TRUE)
                            ),
                            n = sum(!is.na(yOld)),
                            sd = sd(yOld, na.rm = TRUE),
                        ) |>
                        dplyr::mutate(se = sd / sqrt(n)) |>
                        dplyr::mutate(ci = se * qt((self$options$ciWidth / 100) / 2 + .5, n - 1)) |>
                        dplyr::ungroup()
                } else {
                    df <- self$data |>
                        dplyr::select(
                            x = !!sym(self$options$x),
                            y = !!sym(self$options$y),
                            group = !!sym(group)
                        ) |>
                        dplyr::mutate(
                            yOld = jmvcore::toNumeric(y),
                            group = factor(group)
                        ) |>
                        dplyr::group_by(x, group) |>
                        dplyr::summarize(
                            y = ifelse(
                                self$options$aggregateType == "median",
                                median(yOld, na.rm = TRUE),
                                mean(yOld, na.rm = TRUE)
                            ),
                            n = sum(!is.na(yOld)),
                            sd = sd(yOld, na.rm = TRUE),
                        ) |>
                        dplyr::mutate(se = sd / sqrt(n)) |>
                        dplyr::mutate(ci = se * qt((self$options$ciWidth / 100) / 2 + .5, n - 1)) |>
                        dplyr::ungroup() |>
                        dplyr::select(group, x, y, se, sd, ci)
                }
                return(df)
            },
            .linePlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                data <- image$state
                if (self$options$naOmit) {
                    data <- data |>
                        dplyr::filter(!is.na(x) & !is.na(y))

                    if ("group" %in% colnames(data)) {
                        data <- data |> dplyr::filter(!is.na(group))
                    }
                }

                plot_call_list <- private$.getPlotCallList(data, theme)

                theme_call_list_args <- list()
                if (!is.null(self$options$group)) {
                    theme_call_list_args <- utils::modifyList(
                        theme_call_list_args,
                        getLegendThemeCallArgs(self$options)
                    )
                }
                theme_call_list_args <- utils::modifyList(
                    theme_call_list_args,
                    getLabelsThemeCallArgs(self$options, self$options$flipAxes)
                )

                p <- createPlotFromCallStack(plot_call_list) +
                    ggtheme +
                    do.call(ggplot2::theme, theme_call_list_args)

                p <- autoscalePlotBreaks(p, image$width, image$height)
                return(p)
            },
            .getPlotCallList = function(data, theme) {
                group <- self$options$group
                mapping <- aes(x = x, y = y)
                if (is.null(group)) {
                    mapping$group <- quote(1)
                } else {
                    mapping$group <- quote(group)
                    if (self$options$groupColor) {
                        mapping$colour <- quote(group)
                    }
                    if (self$options$groupPointType) {
                        mapping$shape <- quote(group)
                    }
                }

                plot_call_list <- list(
                    "ggplot" = list(
                        fun = ggplot2::ggplot,
                        args = list(data = data, mapping = mapping)
                    )
                )

                if (self$options$line) {
                    line_args <- list(linewidth = self$options$lineSize)
                    if (is.null(group)) {
                        line_args$color <- theme$color[1]
                    } else {
                        if (self$options$groupLineType) {
                            line_args$mapping <- aes(linetype = group)
                        }
                        line_args$position <- ggplot2::position_dodge(
                            width = self$options$groupPositionDodge
                        )
                    }
                    plot_call_list$geom_line <- list(
                        fun = ggplot2::geom_line,
                        args = line_args
                    )
                }

                if (self$options$point) {
                    point_args <- list(size = self$options$pointSize)
                    if (is.null(group)) {
                        point_args$color <- theme$color[1]
                    } else {
                        point_args$position <- ggplot2::position_dodge(
                            width = self$options$groupPositionDodge
                        )
                    }
                    plot_call_list$geom_point <- list(
                        fun = ggplot2::geom_point,
                        args = point_args
                    )
                }

                if (self$options$mode == "aggregate") {
                    errorBars <- self$options$errorBars
                    if (errorBars != "none") {
                        errorbar_args <- list(
                            mapping = aes(ymin = y - !!sym(errorBars), ymax = y + !!sym(errorBars)),
                            linewidth = self$options$errorBarSize,
                            position = ggplot2::position_dodge(
                                width = self$options$groupPositionDodge
                            ),
                            width = self$options$errorBarWidth,
                            show.legend = FALSE
                        )
                        plot_call_list$geom_errorbar <- list(
                            fun = ggplot2::geom_errorbar,
                            args = errorbar_args
                        )
                    }
                }

                labelDefaults <- list(
                    xLabel = self$options$x,
                    yLabel = self$options$y,
                    groupLabel = self$options$group
                )
                plot_call_list$labs <- getLabsCallList(self$options, labelDefaults)

                ylims <- NULL
                if (self$options$yAxisRangeType == "manual") {
                    ylims <- c(self$options$yAxisRangeMin, self$options$yAxisRangeMax)
                }

                if (self$options$flipAxes) {
                    plot_call_list$coord_flip <- list(
                        fun = ggplot2::coord_flip,
                        args = list(ylim = ylims)
                    )
                } else {
                    plot_call_list$coord_cartesian <- list(
                        fun = ggplot2::coord_cartesian,
                        args = list(ylim = ylims)
                    )
                }

                return(plot_call_list)
            }
        ),
        public = list(
            asSource = function() {
                if (is.null(self$options$x) || is.null(self$options$y)) {
                    return("")
                }

                data_prep_code <- generateDataPrepCode(self$options, "line")
                call_list <- private$.getPlotCallList(
                    data = self$data,
                    theme = getSyntaxThemeColors(self$options$theme, self$options$palette)
                )
                # jamovi leaves the line x axis continuous when the variable is
                # numeric, and discrete when it is a factor.
                continuousX <- is.numeric(self$data[[self$options$x]])
                return(finalizePlotSyntax(
                    self$options,
                    call_list,
                    data_prep_code,
                    hasLegend = !is.null(self$options$group),
                    flipAxes = self$options$flipAxes,
                    continuousX = continuousX
                ))
            }
        )
    )
}
