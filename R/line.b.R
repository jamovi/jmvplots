#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
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
                    ggplot2::geom_line(size = self$options$lineSize, color = theme$color[1]) +
                    ggplot2::geom_point(size = self$options$pointSize, color = theme$color[1]) +
                    ggtheme

                if (self$options$mode == "aggregate") {
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
                }

                if (self$options$flipAxes) p <- p + ggplot2::coord_flip()

                if (self$options$yAxisRangeType == "manual")
                    p <- p + ggplot2::ylim(self$options$yAxisRangeMin, self$options$yAxisRangeMax)

                labelDefaults <- list(xLabel = self$options$x, yLabel = self$options$y)
                p <- p +
                    setLabels(options = self$options, defaults = labelDefaults) +
                    formatLabels(options = self$options, flipAxes = self$options$flipAxes)

                return(p)
            }
        )
    )
