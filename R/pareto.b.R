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
                labels <- image$state$labels

                formula <- paste0("~./", sum(df$counts), "*100")
                formula <- as.formula(formula)

                p <- ggplot2::ggplot(df, ggplot2::aes(x = x)) +
                    ggplot2::geom_bar(
                        ggplot2::aes(y = counts),
                        width = self$options$barWidth,
                        color = theme$color[1],
                        fill = theme$fill[2],
                        stat = "identity"
                    ) +
                    ggplot2::geom_point(
                        ggplot2::aes(y = cum),
                        size = 3,
                        color = theme$color[1]
                    ) +
                    ggplot2::geom_path(
                        ggplot2::aes(y = cum, group = 1),
                        size = 1.1,
                        lty = "dashed",
                        color = theme$color[1]
                    ) +
                    ggplot2::scale_y_continuous(
                        sec.axis = ggplot2::sec_axis(
                            formula,
                            name = .("Cumulative Percentage")
                        )
                    ) +
                    ggtheme

                labelDefaults <- list(xLabel = labels$x, yLabel = labels$y)
                p <- p +
                    setLabels(options = self$options, defaults = labelDefaults, legend = FALSE) +
                    formatLabels(options = self$options, legend = FALSE)

                p <- autoscalePlotBreaks(p, image$width, image$height)
                return(p)
            }
        ),
        public = list(
            asSource = function() {
                return(.("Syntax mode for plots is not yet available."))
            }
        )
    )
}
