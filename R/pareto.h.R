
# This file is automatically generated, you probably don't want to edit this

paretoOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "paretoOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            x = NULL,
            counts = NULL,
            barWidth = 0.6,
            width = 550,
            height = 500,
            title = "",
            titleAlign = "center",
            titleFontSize = 16,
            subtitle = "",
            subtitleAlign = "left",
            subtitleFontSize = 16,
            caption = "",
            captionAlign = "right",
            captionFontSize = 12,
            xLabel = "",
            xLabelAlign = "center",
            xLabelFontSize = 16,
            yLabel = "",
            yLabelAlign = "center",
            yLabelFontSize = 16,
            titleType = "title",
            yAxisLabelFontSize = 12,
            yAxisLabelRotation = 0,
            xAxisLabelFontSize = 12,
            xAxisLabelRotation = 0, ...) {

            super$initialize(
                package="scatr",
                name="pareto",
                requiresData=TRUE,
                ...)

            private$..x <- jmvcore::OptionVariable$new(
                "x",
                x,
                suggested=list(
                    "nominal"),
                permitted=list(
                    "factor"))
            private$..counts <- jmvcore::OptionVariable$new(
                "counts",
                counts,
                default=NULL,
                suggested=list(
                    "continuous"),
                permitted=list(
                    "numeric"))
            private$..barWidth <- jmvcore::OptionNumber$new(
                "barWidth",
                barWidth,
                default=0.6,
                min=0)
            private$..width <- jmvcore::OptionNumber$new(
                "width",
                width,
                default=550)
            private$..height <- jmvcore::OptionNumber$new(
                "height",
                height,
                default=500)
            private$..title <- jmvcore::OptionString$new(
                "title",
                title,
                default="")
            private$..titleAlign <- jmvcore::OptionList$new(
                "titleAlign",
                titleAlign,
                options=list(
                    "left",
                    "center",
                    "right"),
                default="center")
            private$..titleFontSize <- jmvcore::OptionNumber$new(
                "titleFontSize",
                titleFontSize,
                default=16)
            private$..subtitle <- jmvcore::OptionString$new(
                "subtitle",
                subtitle,
                default="")
            private$..subtitleAlign <- jmvcore::OptionList$new(
                "subtitleAlign",
                subtitleAlign,
                options=list(
                    "left",
                    "center",
                    "right"),
                default="left")
            private$..subtitleFontSize <- jmvcore::OptionNumber$new(
                "subtitleFontSize",
                subtitleFontSize,
                default=16)
            private$..caption <- jmvcore::OptionString$new(
                "caption",
                caption,
                default="")
            private$..captionAlign <- jmvcore::OptionList$new(
                "captionAlign",
                captionAlign,
                options=list(
                    "left",
                    "center",
                    "right"),
                default="right")
            private$..captionFontSize <- jmvcore::OptionNumber$new(
                "captionFontSize",
                captionFontSize,
                default=12)
            private$..xLabel <- jmvcore::OptionString$new(
                "xLabel",
                xLabel,
                default="")
            private$..xLabelAlign <- jmvcore::OptionList$new(
                "xLabelAlign",
                xLabelAlign,
                options=list(
                    "left",
                    "center",
                    "right"),
                default="center")
            private$..xLabelFontSize <- jmvcore::OptionNumber$new(
                "xLabelFontSize",
                xLabelFontSize,
                default=16)
            private$..yLabel <- jmvcore::OptionString$new(
                "yLabel",
                yLabel,
                default="")
            private$..yLabelAlign <- jmvcore::OptionList$new(
                "yLabelAlign",
                yLabelAlign,
                options=list(
                    "left",
                    "center",
                    "right"),
                default="center")
            private$..yLabelFontSize <- jmvcore::OptionNumber$new(
                "yLabelFontSize",
                yLabelFontSize,
                default=16)
            private$..titleType <- jmvcore::OptionList$new(
                "titleType",
                titleType,
                options=list(
                    "title",
                    "subtitle",
                    "caption",
                    "xTitle",
                    "yTitle"),
                default="title")
            private$..yAxisLabelFontSize <- jmvcore::OptionNumber$new(
                "yAxisLabelFontSize",
                yAxisLabelFontSize,
                default=12)
            private$..yAxisLabelRotation <- jmvcore::OptionNumber$new(
                "yAxisLabelRotation",
                yAxisLabelRotation,
                default=0,
                min=0,
                max=360)
            private$..xAxisLabelFontSize <- jmvcore::OptionNumber$new(
                "xAxisLabelFontSize",
                xAxisLabelFontSize,
                default=12)
            private$..xAxisLabelRotation <- jmvcore::OptionNumber$new(
                "xAxisLabelRotation",
                xAxisLabelRotation,
                default=0,
                min=0,
                max=360)

            self$.addOption(private$..x)
            self$.addOption(private$..counts)
            self$.addOption(private$..barWidth)
            self$.addOption(private$..width)
            self$.addOption(private$..height)
            self$.addOption(private$..title)
            self$.addOption(private$..titleAlign)
            self$.addOption(private$..titleFontSize)
            self$.addOption(private$..subtitle)
            self$.addOption(private$..subtitleAlign)
            self$.addOption(private$..subtitleFontSize)
            self$.addOption(private$..caption)
            self$.addOption(private$..captionAlign)
            self$.addOption(private$..captionFontSize)
            self$.addOption(private$..xLabel)
            self$.addOption(private$..xLabelAlign)
            self$.addOption(private$..xLabelFontSize)
            self$.addOption(private$..yLabel)
            self$.addOption(private$..yLabelAlign)
            self$.addOption(private$..yLabelFontSize)
            self$.addOption(private$..titleType)
            self$.addOption(private$..yAxisLabelFontSize)
            self$.addOption(private$..yAxisLabelRotation)
            self$.addOption(private$..xAxisLabelFontSize)
            self$.addOption(private$..xAxisLabelRotation)
        }),
    active = list(
        x = function() private$..x$value,
        counts = function() private$..counts$value,
        barWidth = function() private$..barWidth$value,
        width = function() private$..width$value,
        height = function() private$..height$value,
        title = function() private$..title$value,
        titleAlign = function() private$..titleAlign$value,
        titleFontSize = function() private$..titleFontSize$value,
        subtitle = function() private$..subtitle$value,
        subtitleAlign = function() private$..subtitleAlign$value,
        subtitleFontSize = function() private$..subtitleFontSize$value,
        caption = function() private$..caption$value,
        captionAlign = function() private$..captionAlign$value,
        captionFontSize = function() private$..captionFontSize$value,
        xLabel = function() private$..xLabel$value,
        xLabelAlign = function() private$..xLabelAlign$value,
        xLabelFontSize = function() private$..xLabelFontSize$value,
        yLabel = function() private$..yLabel$value,
        yLabelAlign = function() private$..yLabelAlign$value,
        yLabelFontSize = function() private$..yLabelFontSize$value,
        titleType = function() private$..titleType$value,
        yAxisLabelFontSize = function() private$..yAxisLabelFontSize$value,
        yAxisLabelRotation = function() private$..yAxisLabelRotation$value,
        xAxisLabelFontSize = function() private$..xAxisLabelFontSize$value,
        xAxisLabelRotation = function() private$..xAxisLabelRotation$value),
    private = list(
        ..x = NA,
        ..counts = NA,
        ..barWidth = NA,
        ..width = NA,
        ..height = NA,
        ..title = NA,
        ..titleAlign = NA,
        ..titleFontSize = NA,
        ..subtitle = NA,
        ..subtitleAlign = NA,
        ..subtitleFontSize = NA,
        ..caption = NA,
        ..captionAlign = NA,
        ..captionFontSize = NA,
        ..xLabel = NA,
        ..xLabelAlign = NA,
        ..xLabelFontSize = NA,
        ..yLabel = NA,
        ..yLabelAlign = NA,
        ..yLabelFontSize = NA,
        ..titleType = NA,
        ..yAxisLabelFontSize = NA,
        ..yAxisLabelRotation = NA,
        ..xAxisLabelFontSize = NA,
        ..xAxisLabelRotation = NA)
)

paretoResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "paretoResults",
    inherit = jmvcore::Group,
    active = list(
        plot = function() private$.items[["plot"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Pareto Plot")
            self$add(jmvcore::Image$new(
                options=options,
                name="plot",
                title="",
                width=550,
                height=500,
                renderFun=".pareto"))}))

paretoBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "paretoBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "scatr",
                name = "pareto",
                version = c(1,0,0),
                options = options,
                results = paretoResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'auto')
        }))

#' Pareto Plot
#'
#' 
#' @param data the data as a data frame
#' @param x a string naming the variable from \code{data} that contains the
#'   values used for the chart
#' @param counts a string naming the variable from \code{data} that contains
#'   the counts for the values (optional)
#' @param barWidth .
#' @param width .
#' @param height .
#' @param title .
#' @param titleAlign .
#' @param titleFontSize .
#' @param subtitle .
#' @param subtitleAlign .
#' @param subtitleFontSize .
#' @param caption .
#' @param captionAlign .
#' @param captionFontSize .
#' @param xLabel .
#' @param xLabelAlign .
#' @param xLabelFontSize .
#' @param yLabel .
#' @param yLabelAlign .
#' @param yLabelFontSize .
#' @param titleType .
#' @param yAxisLabelFontSize .
#' @param yAxisLabelRotation .
#' @param xAxisLabelFontSize .
#' @param xAxisLabelRotation .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$plot} \tab \tab \tab \tab \tab a Pareto chart \cr
#' }
#'
#' @export
pareto <- function(
    data,
    x,
    counts = NULL,
    barWidth = 0.6,
    width = 550,
    height = 500,
    title = "",
    titleAlign = "center",
    titleFontSize = 16,
    subtitle = "",
    subtitleAlign = "left",
    subtitleFontSize = 16,
    caption = "",
    captionAlign = "right",
    captionFontSize = 12,
    xLabel = "",
    xLabelAlign = "center",
    xLabelFontSize = 16,
    yLabel = "",
    yLabelAlign = "center",
    yLabelFontSize = 16,
    titleType = "title",
    yAxisLabelFontSize = 12,
    yAxisLabelRotation = 0,
    xAxisLabelFontSize = 12,
    xAxisLabelRotation = 0) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("pareto requires jmvcore to be installed (restart may be required)")

    if ( ! missing(x)) x <- jmvcore::resolveQuo(jmvcore::enquo(x))
    if ( ! missing(counts)) counts <- jmvcore::resolveQuo(jmvcore::enquo(counts))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(x), x, NULL),
            `if`( ! missing(counts), counts, NULL))

    for (v in x) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])

    options <- paretoOptions$new(
        x = x,
        counts = counts,
        barWidth = barWidth,
        width = width,
        height = height,
        title = title,
        titleAlign = titleAlign,
        titleFontSize = titleFontSize,
        subtitle = subtitle,
        subtitleAlign = subtitleAlign,
        subtitleFontSize = subtitleFontSize,
        caption = caption,
        captionAlign = captionAlign,
        captionFontSize = captionFontSize,
        xLabel = xLabel,
        xLabelAlign = xLabelAlign,
        xLabelFontSize = xLabelFontSize,
        yLabel = yLabel,
        yLabelAlign = yLabelAlign,
        yLabelFontSize = yLabelFontSize,
        titleType = titleType,
        yAxisLabelFontSize = yAxisLabelFontSize,
        yAxisLabelRotation = yAxisLabelRotation,
        xAxisLabelFontSize = xAxisLabelFontSize,
        xAxisLabelRotation = xAxisLabelRotation)

    analysis <- paretoClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

