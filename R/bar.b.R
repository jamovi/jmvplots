#' @importFrom ggplot2 ggplot aes geom_bar labs coord_flip geom_errorbar theme element_blank
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
                            se = sd(!!sym(var), na.rm = TRUE) / sqrt(dplyr::n())
                        ) |>
                        dplyr::mutate(x = "1")
                } else {
                    df <- self$data |>
                        dplyr::group_by(!!sym(group)) |>
                        dplyr::summarize(
                            y = mean(!!sym(var), na.rm = TRUE),
                            se = sd(!!sym(var), na.rm = TRUE) / sqrt(dplyr::n())
                        ) |>
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
                        dplyr::mutate(x = 1:dplyr::n())
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
                    geom_bar(stat = "identity", color = theme$color[1], fill = theme$fill[2]) +
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

                    if (self$options$se)
                        p <- p + geom_errorbar(aes(ymin = y - se, ymax = y + se), width = 0.1)

                    if (is.null(self$options$congroup)) {
                        p <- p +
                            theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
                    }
                } else if (mode == "counts") {
                    if (xLabel == "") xLabel <- self$options$counts
                    if (yLabel == "") yLabel <- "Count"

                    if (is.null(self$options$countsLabels)) {
                        p <- p +
                            theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
                    }
                }

                p <- p +
                    labs(
                        title = title,
                        subtitle = subtitle,
                        caption = caption,
                        x = xLabel,
                        y = yLabel
                    )

                return(p)
            }
        )
    )
