#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
barClass <- if (requireNamespace('jmvcore', quietly = TRUE))
    R6::R6Class(
        "barClass",
        inherit = barBase,
        #### Active bindings ----
        active = list(
            group = function() {
                mode <- self$options$mode
                if (mode == "categorical") {
                    return(self$options$catgroup)
                } else if (mode == "continuous" && !is.null(self$options$congroup1)) {
                    return(self$options$congroup2)
                } else if (mode == "counts") {
                    return(self$options$countsgroup)
                }

                return(NULL)
            },
            grouped = function() {
                if (is.null(private$.grouped)) {
                    if (is.null(self$group)) {
                        private$.grouped <- FALSE
                    } else {
                        private$.grouped <- TRUE
                    }
                }

                return(private$.grouped)
            }
        ),
        private = list(
            #### Member variables ----
            .grouped = NULL,
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
            .prepareCategoricalPlotData = function() {
                var = self$options$catvar
                group = self$options$catgroup

                if (is.null(group)) {
                    df <- self$data |>
                        dplyr::count(!!sym(var), .drop = FALSE) |>
                        dplyr::rename(y = n, x = !!sym(var)) |>
                        dplyr::filter(!is.na(x))
                } else {
                    df <- self$data |>
                        dplyr::group_by(!!sym(group)) |>
                        dplyr::count(!!sym(var), .drop = FALSE) |>
                        dplyr::rename(y = n, x = !!sym(var), group = !!sym(group)) |>
                        dplyr::filter(!is.na(x))
                }
                return(df)
            },
            .prepareContinuousPlotData = function() {
                var = self$options$convar

                if (is.null(self$options$congroup1)) {
                    group1 = self$options$congroup2
                    group2 = NULL
                } else {
                    group1 = self$options$congroup1
                    group2 = self$options$congroup2
                }

                if (is.null(group1)) {
                    df <- self$data |>
                        dplyr::mutate("{var}" := jmvcore::toNumeric(!!sym(var))) |>
                        dplyr::summarize(
                            y = mean(!!sym(var), na.rm = TRUE),
                            n = dplyr::n(),
                            sd = sd(!!sym(var), na.rm = TRUE),
                        ) |>
                        dplyr::mutate(se = sd / sqrt(n)) |>
                        dplyr::mutate(ci = se * qt((self$options$ciWidth / 100) / 2 + .5, n - 1)) |>
                        dplyr::mutate(x = "1")
                } else if (is.null(group2)) {
                    df <- self$data |>
                        dplyr::mutate("{var}" := jmvcore::toNumeric(!!sym(var))) |>
                        dplyr::group_by(!!sym(group1)) |>
                        dplyr::summarize(
                            y = mean(!!sym(var), na.rm = TRUE),
                            n = dplyr::n(),
                            sd = sd(!!sym(var), na.rm = TRUE),
                        ) |>
                        dplyr::mutate(se = sd / sqrt(n)) |>
                        dplyr::mutate(ci = se * qt((self$options$ciWidth / 100) / 2 + .5, n - 1)) |>
                        dplyr::ungroup() |>
                        dplyr::rename(x = !!sym(group1))
                } else {
                    df <- self$data |>
                        dplyr::mutate("{var}" := jmvcore::toNumeric(!!sym(var))) |>
                        dplyr::group_by(!!sym(group1), !!sym(group2)) |>
                        dplyr::summarize(
                            y = mean(!!sym(var), na.rm = TRUE),
                            n = dplyr::n(),
                            sd = sd(!!sym(var), na.rm = TRUE),
                        ) |>
                        dplyr::mutate(se = sd / sqrt(n)) |>
                        dplyr::mutate(ci = se * qt((self$options$ciWidth / 100) / 2 + .5, n - 1)) |>
                        dplyr::ungroup() |>
                        dplyr::rename(x = !!sym(group1), group = !!sym(group2))
                }

                return(df)
            },
            .prepareCountsPlotData = function() {
                var = self$options$counts
                labels = self$options$countsLabels
                group = self$options$countsgroup

                if (is.null(group)) {
                    if (is.null(labels)) {
                        df <- self$data |>
                            dplyr::mutate(.row_id = dplyr::row_number()) |>
                            dplyr::select(.row_id, !!sym(var)) |>
                            dplyr::rename(y = !!sym(var), x = .row_id) |>
                            dplyr::filter(!is.na(y)) |>
                            dplyr::mutate(y = jmvcore::toNumeric(y), x = factor(x))
                    } else {
                        df <- self$data |>
                            dplyr::mutate(.row_id = dplyr::row_number()) |>
                            dplyr::select(.row_id, !!sym(var), !!sym(labels)) |>
                            dplyr::rename(
                                y = !!sym(var),
                                label_text = !!sym(labels),
                                x = .row_id
                            ) |>
                            dplyr::filter(!is.na(y)) |>
                            dplyr::mutate(y = jmvcore::toNumeric(y), x = factor(x))
                    }
                } else {
                    if (is.null(labels)) {
                        num_groups <- length(levels(self$data[[group]]))

                        df <- self$data |>
                            dplyr::mutate(.row_id = 1:dplyr::n()) |>
                            dplyr::mutate(x_category_num = ceiling(.row_id / num_groups)) |>
                            dplyr::select(
                                x_category_num,
                                group_category = !!sym(group),
                                y_value = !!sym(var)
                            ) |>
                            dplyr::mutate(
                                x = factor(x_category_num),
                                group = factor(group_category),
                                y = jmvcore::toNumeric(y_value)
                            ) |>
                            dplyr::filter(!is.na(y) & !is.na(x) & !is.na(group)) |>
                            dplyr::select(x, y, group) |>
                            dplyr::ungroup()

                        # df <- self$data |>
                        #     dplyr::mutate(.row_id = dplyr::row_number()) |>
                        #     dplyr::select(.row_id, !!sym(var), !!sym(group)) |>
                        #     dplyr::group_by(!!sym(group)) |>
                        #     dplyr::rename(y = !!sym(var), x = .row_id, group = !!sym(group)) |>
                        #     dplyr::filter(!is.na(y)) |>
                        #     dplyr::mutate(
                        #         y = jmvcore::toNumeric(y),
                        #         x = factor(x),
                        #         group = factor(group)
                        #     )
                    } else {
                        df <- self$data |>
                            dplyr::select(
                                x_category = !!sym(labels),
                                group_category = !!sym(group),
                                y_value = !!sym(var)
                            ) |>
                            dplyr::mutate(
                                x = factor(x_category),
                                group = factor(group_category),
                                y = jmvcore::toNumeric(y_value)
                            ) |>
                            dplyr::filter(!is.na(y) & !is.na(x) & !is.na(group)) |>
                            dplyr::select(x, y, group) |>
                            dplyr::ungroup()
                        # df <- self$data |>
                        #     dplyr::mutate(.row_id = dplyr::row_number()) |>
                        #     dplyr::select(.row_id, !!sym(var), !!sym(labels), !!sym(group)) |>
                        #     dplyr::group_by(!!sym(group)) |>
                        #     dplyr::rename(
                        #         y = !!sym(var),
                        #         label_text = !!sym(labels),
                        #         x = .row_id,
                        #         group = !!sym(group)
                        #     ) |>
                        #     dplyr::filter(!is.na(y)) |>
                        #     dplyr::mutate(
                        #         y = jmvcore::toNumeric(y),
                        #         x = factor(x),
                        #         ,
                        #         group = factor(group)
                        #     )
                    }
                }

                return(df)
            },
            .barPlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) return(FALSE)

                if (!self$grouped) {
                    p <- ggplot(image$state, aes(x = x, y = y)) +
                        ggplot2::geom_bar(
                            stat = "identity",
                            width = self$options$barWidth,
                            color = theme$color[1],
                            fill = theme$fill[2]
                        ) +
                        ggtheme
                } else {
                    p <- ggplot(image$state, aes(x = x, y = y, group = group, fill = group))

                    if (self$options$groupBarType == "grouped") {
                        position = ggplot2::position_dodge(
                            width = self$options$barWidth,
                            preserve = "single"
                        )

                        p <- p +
                            ggplot2::geom_bar(
                                color = theme$color[1],
                                stat = "identity",
                                width = self$options$barWidth * 0.9,
                                position = position
                            )
                    } else {
                        p <- p +
                            ggplot2::geom_bar(
                                color = theme$color[1],
                                stat = "identity",
                                width = self$options$barWidth
                            )
                    }

                    p <- p + ggtheme + formatLegend(self$options)
                }

                if (private$.hasErrorBars()) p <- p + private$.getErrorBars()

                if (self$options$yAxisRangeType == "manual")
                    p <- p + ggplot2::ylim(self$options$yAxisRangeMin, self$options$yAxisRangeMax)

                if (
                    !self$grouped &&
                        self$options$mode == "counts" &&
                        !is.null(self$options$countsLabels)
                )
                    p <- p + ggplot2::scale_x_discrete(labels = image$state$label_text)

                if (self$options$xAxisLabelFontSizeRevLabels)
                    p <- p + ggplot2::scale_x_discrete(limits = rev)

                labelDefaults <- private$.getDefaultLabels()
                p <- p +
                    setLabels(options = self$options, defaults = labelDefaults) +
                    formatLabels(options = self$options, flipAxes = self$options$flipAxes)

                if (self$options$flipAxes) p <- p + ggplot2::coord_flip()

                return(p)
            },
            #### Helper functions ----
            .getDefaultLabels = function() {
                mode <- self$options$mode
                if (mode == "categorical") {
                    xLabel <- self$options$catvar
                    yLabel <- "Count"
                } else if (mode == "continuous") {
                    xLabel <- self$options$congroup1

                    errorBars <- self$options$errorBars
                    if (private$.hasErrorBars()) {
                        error <- paste(
                            " +/-",
                            ifelse(
                                errorBars == "ci",
                                paste0(round(self$options$ciWidth, 1), "%"),
                                ""
                            ),
                            toupper(errorBars)
                        )
                    } else {
                        error <- ""
                    }

                    yLabel <- jmvcore::format(
                        "{var} (mean{error})",
                        var = self$options$convar,
                        error = error
                    )
                } else if (mode == "counts") {
                    xLabel <- self$options$countsLabels
                    yLabel <- self$options$counts
                }

                return(list(xLabel = xLabel, yLabel = yLabel, groupLabel = self$group))
            },
            .hasErrorBars = function() {
                mode <- self$options$mode
                if (mode == "continuous") {
                    errorBars <- self$options$errorBars
                    hasErrorBars <- errorBars != "none" &&
                        (!self$grouped ||
                            (self$grouped && self$options$groupBarType == "grouped"))
                    return(hasErrorBars)
                }

                return(FALSE)
            },
            .getErrorBars = function() {
                errorBars <- self$options$errorBars

                if (self$grouped && self$options$groupBarType == "grouped") {
                    geom <- ggplot2::geom_errorbar(
                        aes(
                            ymin = y - !!sym(errorBars),
                            ymax = y + !!sym(errorBars),
                            group = group
                        ),
                        size = self$options$errorBarSize,
                        width = self$options$errorBarWidth,
                        position = ggplot2::position_dodge(
                            width = self$options$barWidth,
                            preserve = "single"
                        ),
                        show.legend = FALSE
                    )
                } else if (!self$grouped) {
                    geom <- ggplot2::geom_errorbar(
                        aes(
                            ymin = y - !!sym(errorBars),
                            ymax = y + !!sym(errorBars)
                        ),
                        size = self$options$errorBarSize,
                        width = self$options$errorBarWidth,
                        show.legend = FALSE
                    )
                } else {
                    geom <- NULL
                }

                return(geom)
            }
        )
    )
