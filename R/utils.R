#' Convert alignment text to number
#'
#' @param text The alignment text, one of "left", "center", "right"
#' @return The alignment number
#' @keywords internal
alignText2Number = function(text) {
    if (text == "left") return(0)
    if (text == "center") return(0.5)
    if (text == "right") return(1)
}


#' Set plot labels based on options
#'
#' @param options The options list; needs to contain the appropriate fields
#' @param defaults The default values for the labels
#' @return A ggplot2::labs object
#' @keywords internal
setLabels = function(options, defaults = list()) {
    title <- options$title
    subtitle <- options$subtitle
    caption <- options$caption
    xLabel <- options$xLabel
    yLabel <- options$yLabel
    groupLabel <- options$legendTitle

    if (title == "") title <- defaults$title
    if (subtitle == "") subtitle <- defaults$subtitle
    if (caption == "") caption <- defaults$caption
    if (xLabel == "") xLabel <- defaults$xLabel
    if (yLabel == "") yLabel <- defaults$yLabel
    if (groupLabel == "") groupLabel <- defaults$groupLabel

    labels <- ggplot2::labs(
        title = title,
        subtitle = subtitle,
        caption = caption,
        x = xLabel,
        y = yLabel,
        group = groupLabel,
        color = groupLabel,
        linetype = groupLabel,
        shape = groupLabel
    )

    return(labels)
}

#' Format plot labels based on options
#'
#' @param options The options list; needs to contain the appropriate fields
#' @param flipAxes Whether the axes are flipped or not
#' @return A ggplot2::theme object
#' @keywords internal
formatLabels = function(options, flipAxes = FALSE) {
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
        axis.text.y = ggplot2::element_text(size = yAxisLabelFontSize, angle = yAxisLabelRotation),
        legend.title = ggplot2::element_text(size = options$legendTitleFontSize),
        legend.text = ggplot2::element_text(size = options$legendLabelFontSize)
    )

    return(labels_theme)
}
