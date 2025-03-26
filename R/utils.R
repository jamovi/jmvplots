#' Convert alignment text to number
#'
#' @param text The alignment text
#' @keywords internal
alignText2Number = function(text) {
    if (text == "left") return(0)
    if (text == "center") return(0.5)
    if (text == "right") return(1)
}
