#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
scatClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "scatClass",
        inherit = scatBase,
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

                group <- self$options$group

                if (is.null(group)) {
                    df <- self$data |>
                        dplyr::select(x = !!sym(self$options$x), y = !!sym(self$options$y)) |>
                        dplyr::mutate(x = jmvcore::toNumeric(x), y = jmvcore::toNumeric(y))
                } else {
                    df <- self$data |>
                        dplyr::select(
                            x = !!sym(self$options$x),
                            y = !!sym(self$options$y),
                            group = !!sym(group)
                        ) |>
                        dplyr::mutate(
                            x = jmvcore::toNumeric(x),
                            y = jmvcore::toNumeric(y),
                            group = factor(group)
                        )
                }

                image$setState(df)
            },
            .scatterPlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                plot_call_list <- private$.getPlotCallList(image$state, theme)

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
                line <- self$options$regLine

                if (is.null(group)) {
                    mapping <- aes(x = x, y = y)
                } else {
                    mapping <- aes(x = x, y = y, colour = group, fill = group)
                }

                plot_call_list <- list(
                    "ggplot" = list(
                        fun = ggplot2::ggplot,
                        args = list(data = data, mapping = mapping)
                    )
                )

                geom_point_args <- list(size = self$options$pointSize)
                if (is.null(group)) {
                    geom_point_args$color <- theme$color[1]
                    geom_point_args$fill <- theme$fill[2]
                }

                plot_call_list$geom_point <- list(
                    fun = ggplot2::geom_point,
                    args = geom_point_args
                )

                if (line) {
                    geom_smooth_args <- list(
                        method = self$options$lineMethod,
                        se = self$options$lineSE,
                        formula = y ~ x
                    )
                    if (is.null(group)) {
                        geom_smooth_args$color <- theme$color[1]
                        geom_smooth_args$fill <- theme$fill[2]
                    }
                    plot_call_list$geom_smooth <- list(
                        fun = ggplot2::geom_smooth,
                        args = geom_smooth_args
                    )
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

                xlims <- NULL
                if (self$options$xAxisRangeType == "manual") {
                    xlims <- c(self$options$xAxisRangeMin, self$options$xAxisRangeMax)
                }

                if (self$options$flipAxes) {
                    plot_call_list$coord_flip <- list(
                        fun = ggplot2::coord_flip,
                        args = list(ylim = ylims, xlim = xlims)
                    )
                } else {
                    plot_call_list$coord_cartesian <- list(
                        fun = ggplot2::coord_cartesian,
                        args = list(ylim = ylims, xlim = xlims)
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

                data_prep_code <- generateDataPrepCode(self$options, "scat")
                call_list <- private$.getPlotCallList(
                    data = self$data,
                    theme = getSyntaxThemeColors(self$options$theme, self$options$palette)
                )
                return(finalizePlotSyntax(
                    self$options,
                    call_list,
                    data_prep_code,
                    hasLegend = !is.null(self$options$group),
                    flipAxes = self$options$flipAxes,
                    continuousX = TRUE
                ))
            }
        )
    )
}
