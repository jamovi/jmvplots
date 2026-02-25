#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
jmvbarClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "jmvbarClass",
        inherit = jmvbarBase,
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
            .init = function() {
                image <- self$results$plot
            },
            .run = function() {
                mode <- self$options$mode
                if (
                    (mode == "categorical" && is.null(self$options$catvar)) ||
                        (mode == "continuous" && is.null(self$options$convar)) ||
                        (mode == "counts" && is.null(self$options$counts))
                ) {
                    return()
                }

                private$.preparePlotData()
            },
            #### Plot functions ----
            .preparePlotData = function() {
                image <- self$results$plot

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
                var <- self$options$catvar
                group <- self$options$catgroup

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
                var <- self$options$convar

                if (is.null(self$options$congroup1)) {
                    group1 <- self$options$congroup2
                    group2 <- NULL
                } else {
                    group1 <- self$options$congroup1
                    group2 <- self$options$congroup2
                }

                if (is.null(group1)) {
                    df <- self$data |>
                        dplyr::mutate("{var}" := jmvcore::toNumeric(!!sym(var))) |>
                        dplyr::rename(y_full = !!sym(var)) |>
                        dplyr::summarize(
                            y = mean(y_full, na.rm = TRUE),
                            n = dplyr::n(),
                            sd = sd(y_full, na.rm = TRUE),
                        ) |>
                        dplyr::mutate(se = sd / sqrt(n)) |>
                        dplyr::mutate(ci = se * qt((self$options$ciWidth / 100) / 2 + .5, n - 1)) |>
                        dplyr::mutate(x = "1")
                } else if (is.null(group2)) {
                    df <- self$data |>
                        dplyr::mutate("{var}" := jmvcore::toNumeric(!!sym(var))) |>
                        dplyr::group_by(!!sym(group1)) |>
                        dplyr::rename(y_full = !!sym(var), x = !!sym(group1)) |>
                        dplyr::summarize(
                            y = mean(y_full, na.rm = TRUE),
                            n = dplyr::n(),
                            sd = sd(y_full, na.rm = TRUE),
                        ) |>
                        dplyr::mutate(se = sd / sqrt(n)) |>
                        dplyr::mutate(ci = se * qt((self$options$ciWidth / 100) / 2 + .5, n - 1)) |>
                        dplyr::ungroup()
                } else {
                    df <- self$data |>
                        dplyr::mutate("{var}" := jmvcore::toNumeric(!!sym(var))) |>
                        dplyr::group_by(!!sym(group1), !!sym(group2)) |>
                        dplyr::rename(
                            y_full = !!sym(var),
                            x = !!sym(group1),
                            group = !!sym(group2)
                        ) |>
                        dplyr::summarize(
                            y = mean(y_full, na.rm = TRUE),
                            n = dplyr::n(),
                            sd = sd(y_full, na.rm = TRUE),
                        ) |>
                        dplyr::mutate(se = sd / sqrt(n)) |>
                        dplyr::mutate(ci = se * qt((self$options$ciWidth / 100) / 2 + .5, n - 1)) |>
                        dplyr::ungroup()
                }

                return(df)
            },
            .prepareCountsPlotData = function() {
                var <- self$options$counts
                labels <- self$options$countsLabels
                group <- self$options$countsgroup

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
                    }
                }

                return(df)
            },
            .barPlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                plot_call_list <- list(
                    "ggplot" = private$.getInitPlotCallList(image$state),
                    "geom_bar" = private$.getGeomBarCallList(theme)
                )

                if (private$.hasErrorBars()) {
                    plot_call_list$geom_errorbar <- private$.getGeomErrorBarCallList()
                }

                if (self$options$valueLabels) {
                    plot_call_list$geom_text <- private$.getGeomTextCallList()
                }

                if (
                    !self$grouped &&
                        self$options$mode == "counts" &&
                        !is.null(self$options$countsLabels)
                ) {
                    plot_call_list$scale_x_discrete <- list(
                        ggplot2::scale_x_discrete,
                        list(labels = image$state$label_text)
                    )
                } else if (self$options$xAxisLabelFontSizeRevLabels) {
                    plot_call_list$scale_x_discrete <- list(
                        ggplot2::scale_x_discrete,
                        list(labels = rev)
                    )
                }

                labelDefaults <- private$.getDefaultLabels()
                plot_call_list$labs <- getLabsCallList(self$options, labelDefaults)

                ylims <- NULL
                if (self$options$yAxisRangeType == "manual") {
                    ylims <- c(self$options$yAxisRangeMin, self$options$yAxisRangeMax)
                }

                if (self$options$flipAxes) {
                    plot_call_list$coord_flip <- list(ggplot2::coord_flip, list(ylim = ylims))
                } else {
                    plot_call_list$coord_cartesian <- list(
                        ggplot2::coord_cartesian,
                        list(ylim = ylims)
                    )
                }

                theme_call_list_args <- list()
                if (self$grouped) {
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
            #### Helper functions ----
            .getDefaultLabels = function() {
                mode <- self$options$mode
                if (mode == "categorical") {
                    xLabel <- self$options$catvar
                    yLabel <- .("Frequency (N)")
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
                        .("{var} (mean{error})"),
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
            #' Get the function and arguments to initialize the plot
            #'
            #' @param data The data to be used in the plot
            #' @return A list containing the function and arguments to initialize the plot
            .getInitPlotCallList = function(data) {
                args <- list(data = data)

                if (!self$grouped) {
                    args$mapping <- aes(x = x, y = y)
                } else {
                    args$mapping <- aes(x = x, y = y, group = group, fill = group)
                }

                return(list(fun = ggplot2::ggplot, args = args))
            },
            #' Get the function and arguments to create the bar plot
            #'
            #' @param theme The theme to be used in the plot
            #' @return A list containing the function and arguments to create the bar plot
            .getGeomBarCallList = function(theme) {
                args <- list(stat = "identity", color = theme$color[1])

                if (!self$grouped) {
                    args$width <- self$options$barWidth
                    args$fill <- theme$fill[2]
                } else {
                    if (self$options$groupBarType == "grouped") {
                        position <- ggplot2::position_dodge(
                            width = self$options$barWidth
                        )

                        args$width <- self$options$barWidth * 0.9
                        args$position <- position
                    } else {
                        args$width <- self$options$barWidth
                    }
                }

                return(list(fun = ggplot2::geom_bar, args = args))
            },
            #' Get the function and arguments to create the error bars
            #'
            #' @return A list containing the function and arguments to create the error bars
            .getGeomErrorBarCallList = function() {
                errorBars <- self$options$errorBars

                args <- list(
                    size = self$options$errorBarSize,
                    width = self$options$errorBarWidth,
                    show.legend = FALSE
                )

                if (self$grouped) {
                    args$mapping <- aes(
                        ymin = y - !!sym(errorBars),
                        ymax = y + !!sym(errorBars),
                        group = group
                    )
                    args$position <- ggplot2::position_dodge(
                        width = self$options$barWidth,
                        preserve = "single"
                    )
                } else {
                    args$mapping <- aes(
                        ymin = y - !!sym(errorBars),
                        ymax = y + !!sym(errorBars)
                    )
                }

                return(list(fun = ggplot2::geom_errorbar, args = args))
            },
            #' Get the function and arguments to create the text labels
            #'
            #' @return A list containing the function and arguments to create the text labels
            .getGeomTextCallList = function() {
                args <- list(mapping = aes(label = round(y, 2)), size = 5)

                if (self$grouped) {
                    if (self$options$groupBarType == "grouped") {
                        args$position <- ggplot2::position_dodge(
                            width = self$options$barWidth
                        )
                    } else {
                        args$position <- ggplot2::position_stack(vjust = 0.5)
                    }
                }

                if (!self$grouped || (self$grouped && self$options$groupBarType == "grouped")) {
                    if (self$options$flipAxes) {
                        args$hjust <- -0.5
                    } else {
                        args$vjust <- -0.5
                    }
                }

                return(list(fun = ggplot2::geom_text, args = args))
            }
        ),
        public = list(
            asSource = function() {
                return(.("Syntax mode for plots is not yet available."))
            }
        )
    )
}
