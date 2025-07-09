#' Convert alignment text to number
#'
#' @param text The alignment text, one of "left", "center", "right"
#' @return The alignment number
#' @keywords internal
alignText2Number = function(text) {
    if (text == "left") {
        return(0)
    }
    if (text == "center") {
        return(0.5)
    }
    if (text == "right") return(1)
}


#' Set plot labels based on options
#'
#' @param options The options list; needs to contain the appropriate fields
#' @param defaults The default values for the labels
#' @param legend Whether there is a legend or not
#' @return A ggplot2::labs object
#' @keywords internal
setLabels = function(options, defaults = list(), legend = TRUE) {
    title <- options$title
    subtitle <- options$subtitle
    caption <- options$caption
    xLabel <- options$xLabel
    yLabel <- options$yLabel

    if (legend) {
        groupLabel <- options$legendTitle
    } else {
        groupLabel <- ""
    }

    if (title == "") {
        title <- defaults$title
    }
    if (subtitle == "") {
        subtitle <- defaults$subtitle
    }
    if (caption == "") {
        caption <- defaults$caption
    }
    if (xLabel == "") {
        xLabel <- defaults$xLabel
    }
    if (yLabel == "") {
        yLabel <- defaults$yLabel
    }
    if (groupLabel == "") {
        groupLabel <- defaults$groupLabel
    }

    labels <- ggplot2::labs(
        title = title,
        subtitle = subtitle,
        caption = caption,
        x = xLabel,
        y = yLabel,
        group = groupLabel,
        color = groupLabel,
        fill = groupLabel,
        linetype = groupLabel,
        shape = groupLabel
    )

    return(labels)
}

#' Format plot labels based on options
#'
#' @param options The options list; needs to contain the appropriate fields
#' @param flipAxes Whether the axes are flipped or not
#' @param legend Whether there is a legend or not
#' @return A ggplot2::theme object
#' @keywords internal
formatLabels = function(options, flipAxes = FALSE, legend = TRUE) {
    if (flipAxes) {
        xLabelFontSize <- options$yLabelFontSize
        xLabelAlign <- options$yLabelAlign
        yLabelFontSize <- options$xLabelFontSize
        yLabelAlign <- options$xLabelAlign
        xAxisLabelFontSize <- options$yAxisLabelFontSize
        xAxisLabelRotation <- options$yAxisLabelRotation
        yAxisLabelFontSize <- options$xAxisLabelFontSize
        yAxisLabelRotation <- options$xAxisLabelRotation
    } else {
        xLabelFontSize <- options$xLabelFontSize
        xLabelAlign <- options$xLabelAlign
        yLabelFontSize <- options$yLabelFontSize
        yLabelAlign <- options$yLabelAlign
        xAxisLabelFontSize <- options$xAxisLabelFontSize
        xAxisLabelRotation <- options$xAxisLabelRotation
        yAxisLabelFontSize <- options$yAxisLabelFontSize
        yAxisLabelRotation <- options$yAxisLabelRotation
    }

    labels_theme <- ggplot2::theme(
        plot.title = ggplot2::element_text(
            size = options$titleFontSize,
            hjust = alignText2Number(options$titleAlign)
        ),
        plot.subtitle = ggplot2::element_text(
            size = options$subtitleFontSize,
            hjust = alignText2Number(options$subtitleAlign)
        ),
        plot.caption = ggplot2::element_text(
            size = options$captionFontSize,
            hjust = alignText2Number(options$captionAlign)
        ),
        axis.title.x = ggplot2::element_text(
            size = xLabelFontSize,
            hjust = alignText2Number(xLabelAlign)
        ),
        axis.title.y = ggplot2::element_text(
            size = yLabelFontSize,
            hjust = alignText2Number(yLabelAlign)
        ),
        axis.text.x = ggplot2::element_text(
            size = xAxisLabelFontSize,
            angle = xAxisLabelRotation
        ),
        axis.text.y = ggplot2::element_text(size = yAxisLabelFontSize, angle = yAxisLabelRotation)
    )

    if (legend) {
        labels_theme <- labels_theme +
            ggplot2::theme(
                legend.title = ggplot2::element_text(size = options$legendTitleFontSize),
                legend.text = ggplot2::element_text(size = options$legendLabelFontSize)
            )
    }

    return(labels_theme)
}

#' Format legend
#'
#' @param options The options list; needs to contain the appropriate fields
#' @return A ggplot2::theme object
#' @keywords internal
formatLegend = function(options) {
    if (options$legenPositionType == "hide") {
        legend_theme <- ggplot2::theme(
            legend.position = "none"
        )
    } else if (options$legenPositionType == "outside") {
        legend_theme <-
            ggplot2::theme(
                legend.position = options$legendPosition,
                legend.justification = options$legendJustification,
                legend.key.width = ggplot2::unit(options$legendKeyWidth, "cm"),
                legend.key.height = ggplot2::unit(options$legendKeyHeight, "cm")
            )
    } else if (options$legenPositionType == "inside") {
        legend_theme <-
            ggplot2::theme(
                legend.position = "inside",
                legend.position.inside = c(
                    options$legendPositionX,
                    options$legendPositionY
                ),
                legend.direction = options$legendDirection,
                legend.key.width = ggplot2::unit(options$legendKeyWidth, "cm"),
                legend.key.height = ggplot2::unit(options$legendKeyHeight, "cm")
            )
    }

    return(legend_theme)
}


#' Get the legend specific theming args
#'
#' @param options The options list; needs to contain the appropriate fields
#' @return A list of legend specific theme arguments
#' @keywords internal
getLegendThemeCallArgs = function(options) {
    args <- list()
    if (options$legenPositionType == "hide") {
        args$legend.position <- "none"
    } else if (options$legenPositionType == "outside") {
        args$legend.position <- options$legendPosition
        args$legend.justification <- options$legendJustification
        args$legend.key.width <- ggplot2::unit(options$legendKeyWidth, "cm")
        args$legend.key.height <- ggplot2::unit(options$legendKeyHeight, "cm")
    } else if (options$legenPositionType == "inside") {
        args$legend.position <- "inside"
        args$legend.position.inside <- c(
            options$legendPositionX,
            options$legendPositionY
        )
        args$legend.direction <- options$legendDirection
        args$legend.key.width <- ggplot2::unit(options$legendKeyWidth, "cm")
        args$legend.key.height <- ggplot2::unit(options$legendKeyHeight, "cm")
    }
    return(args)
}


#' Get the label specific theming args
#'
#' @param options The options list; needs to contain the appropriate fields
#' @param flipAxes Whether the axes are flipped or not
#' @return A list of legend specific theme arguments
#' @keywords internal
getLabelsThemeCallArgs = function(options, flipAxes = FALSE) {
    if (flipAxes) {
        xLabelFontSize <- options$yLabelFontSize
        xLabelAlign <- options$yLabelAlign
        yLabelFontSize <- options$xLabelFontSize
        yLabelAlign <- options$xLabelAlign
        xAxisLabelFontSize <- options$yAxisLabelFontSize
        xAxisLabelRotation <- options$yAxisLabelRotation
        yAxisLabelFontSize <- options$xAxisLabelFontSize
        yAxisLabelRotation <- options$xAxisLabelRotation
    } else {
        xLabelFontSize <- options$xLabelFontSize
        xLabelAlign <- options$xLabelAlign
        yLabelFontSize <- options$yLabelFontSize
        yLabelAlign <- options$yLabelAlign
        xAxisLabelFontSize <- options$xAxisLabelFontSize
        xAxisLabelRotation <- options$xAxisLabelRotation
        yAxisLabelFontSize <- options$yAxisLabelFontSize
        yAxisLabelRotation <- options$yAxisLabelRotation
    }
    args <- list()
    args$plot.title = ggplot2::element_text(
        size = options$titleFontSize,
        hjust = alignText2Number(options$titleAlign)
    )
    args$plot.subtitle = ggplot2::element_text(
        size = options$subtitleFontSize,
        hjust = alignText2Number(options$subtitleAlign)
    )
    args$plot.caption = ggplot2::element_text(
        size = options$captionFontSize,
        hjust = alignText2Number(options$captionAlign)
    )
    args$axis.title.x = ggplot2::element_text(
        size = xLabelFontSize,
        hjust = alignText2Number(xLabelAlign)
    )
    args$axis.title.y = ggplot2::element_text(
        size = yLabelFontSize,
        hjust = alignText2Number(yLabelAlign)
    )
    args$axis.text.x = ggplot2::element_text(
        size = xAxisLabelFontSize,
        angle = xAxisLabelRotation
    )
    args$axis.text.y = ggplot2::element_text(size = yAxisLabelFontSize, angle = yAxisLabelRotation)
    args$legend.title = ggplot2::element_text(size = options$legendTitleFontSize)
    args$legend.text = ggplot2::element_text(size = options$legendLabelFontSize)

    return(args)
}


#' Set plot labels based on options
#'
#' @param options The options list; needs to contain the appropriate fields
#' @param defaults The default values for the labels
#' @return A list containing the function and arguments to create the labels
#' @keywords internal
getLabsCallList = function(options, defaults = list()) {
    title <- options$title
    subtitle <- options$subtitle
    caption <- options$caption
    xLabel <- options$xLabel
    yLabel <- options$yLabel
    groupLabel <- options$legendTitle

    if (title == "") {
        title <- defaults$title
    }
    if (subtitle == "") {
        subtitle <- defaults$subtitle
    }
    if (caption == "") {
        caption <- defaults$caption
    }
    if (xLabel == "") {
        xLabel <- defaults$xLabel
    }
    if (yLabel == "") {
        yLabel <- defaults$yLabel
    }
    if (groupLabel == "") {
        groupLabel <- defaults$groupLabel
    }

    args <- list(
        title = title,
        subtitle = subtitle,
        caption = caption,
        x = xLabel,
        y = yLabel,
        group = groupLabel,
        color = groupLabel,
        fill = groupLabel,
        linetype = groupLabel,
        shape = groupLabel
    )

    return(list(ggplot2::labs, args))
}


#' Apply call stack for plot
#'
#' @param callStack The call stack to apply
#' @return A ggplot2::ggplot object
#' @keywords internal
createPlotFromCallStack = function(callStack) {
    init_call_spec <- callStack[[1]]
    p <- do.call(init_call_spec[[1]], init_call_spec[[2]])

    other_call_stack <- callStack[-1]
    for (call_spec in other_call_stack) {
        p <- p + do.call(call_spec[[1]], call_spec[[2]])
    }

    return(p)
}
