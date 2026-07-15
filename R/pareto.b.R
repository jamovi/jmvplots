#' @importFrom jmvcore .
paretoClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "paretoClass",
        inherit = paretoBase,
        private = list(
            .init = function() {
                image <- self$results$plot
            },
            .run = function() {
                if (is.null(self$options$x)) {
                    return()
                }

                private$.preparePlotData()
            },
            #### Plot functions ----
            .preparePlotData = function() {
                image <- self$results$plot

                x <- self$options$x
                counts <- self$options$counts
                data <- self$data
                valuesCol <- factor(data[[x]])

                if (!is.null(counts)) {
                    countsCol <- jmvcore::toNumeric(data[[counts]])
                    df <- data.frame(x = valuesCol, counts = countsCol)
                    df <- as.data.frame(xtabs(counts ~ x, data = df))

                    labels <- list(x = x, y = counts)
                } else {
                    df <- as.data.frame(table(valuesCol))
                    labels <- list(x = x, y = .("Frequency (N)"))
                }

                names(df) <- c("x", "counts")

                df <- df[order(df$counts, decreasing = TRUE), ]
                df$x <- factor(df$x, levels = df$x)
                df$cum <- cumsum(df$counts)

                image$setState(list(df = df, labels = labels))
            },
            .pareto = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                df <- image$state$df
                total <- sum(df$counts)

                plot_call_list <- private$.getPlotCallList(image$state, theme, total)

                # Evaluate nested sec.axis spec for rendering
                sec_axis_spec <- plot_call_list$scale_y_continuous$args$sec.axis
                if (is.list(sec_axis_spec) && !is.null(sec_axis_spec$fun)) {
                    plot_call_list$scale_y_continuous$args$sec.axis <- do.call(
                        sec_axis_spec$fun,
                        sec_axis_spec$args
                    )
                }

                theme_call_list_args <- list()
                theme_call_list_args <- utils::modifyList(
                    theme_call_list_args,
                    getLabelsThemeCallArgs(self$options, legend = FALSE)
                )

                p <- createPlotFromCallStack(plot_call_list) +
                    ggtheme +
                    do.call(ggplot2::theme, theme_call_list_args)

                p <- autoscalePlotBreaks(p, image$width, image$height)
                return(p)
            },
            .getPlotCallList = function(state, theme, total) {
                df <- state$df
                labels <- state$labels

                formula <- as.formula(paste0("~./", total, "*100"))

                plot_call_list <- list(
                    "ggplot" = list(
                        fun = ggplot2::ggplot,
                        args = list(
                            data = df,
                            mapping = aes(x = x)
                        )
                    ),
                    "geom_bar" = list(
                        fun = ggplot2::geom_bar,
                        args = list(
                            mapping = aes(y = counts),
                            width = self$options$barWidth,
                            color = theme$color[1],
                            fill = theme$fill[2],
                            stat = "identity"
                        )
                    ),
                    "geom_point" = list(
                        fun = ggplot2::geom_point,
                        args = list(
                            mapping = aes(y = cum),
                            size = 3,
                            color = theme$color[1]
                        )
                    ),
                    "geom_path" = list(
                        fun = ggplot2::geom_path,
                        args = list(
                            mapping = aes(y = cum, group = 1),
                            linewidth = 1.1,
                            lty = "dashed",
                            color = theme$color[1]
                        )
                    ),
                    "scale_y_continuous" = list(
                        fun = ggplot2::scale_y_continuous,
                        args = list(
                            sec.axis = list(
                                fun = ggplot2::sec_axis,
                                fun_name = "sec_axis",
                                args = list(
                                    formula,
                                    name = .("Cumulative Percentage")
                                )
                            )
                        )
                    )
                )

                labelDefaults <- list(xLabel = labels$x, yLabel = labels$y)
                plot_call_list$labs <- getLabsCallList(self$options, labelDefaults, legend = FALSE)

                return(plot_call_list)
            }
        ),
        public = list(
            asSource = function() {
                if (is.null(self$options$x)) {
                    return("")
                }

                data_prep_code <- generateDataPrepCode(self$options, "pareto")

                # Compute total sum for cumulative percentage formula
                x <- self$options$x
                counts <- self$options$counts
                if (!is.null(counts)) {
                    total <- sum(jmvcore::toNumeric(self$data[[counts]]), na.rm = TRUE)
                } else {
                    total <- sum(!is.na(self$data[[x]]))
                }

                mock_df <- data.frame(
                    x = factor(1:2),
                    counts = c(total, 0),
                    cum = c(total, total)
                )
                mock_labels <- list(
                    x = self$options$x,
                    y = if (!is.null(counts)) counts else .("Frequency (N)")
                )
                state <- list(df = mock_df, labels = mock_labels)

                call_list <- private$.getPlotCallList(
                    state = state,
                    theme = getSyntaxThemeColors(self$options$theme, self$options$palette),
                    total = total
                )
                return(finalizePlotSyntax(
                    self$options,
                    call_list,
                    data_prep_code,
                    hasLegend = FALSE,
                    legend = FALSE
                ))
            }
        )
    )
}
