#' Autoscaled breaks avoid scientific notation
testthat::test_that("autoscalePlotBreaks: avoids scientific notation for large round breaks", {
    # GIVEN a scatter plot spanning a range whose pretty breaks include round
    # powers of ten (e.g. 100000), which trips base R's format() into
    # switching the whole axis to scientific notation
    df <- data.frame(x = c(631, 113000), y = c(25.7, 78.1))
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) + ggplot2::geom_point()

    # WHEN axis breaks are autoscaled for the panel size
    p <- scatr:::autoscalePlotBreaks(p, width_px = 600, height_px = 400)

    # THEN none of the x-axis tick labels use scientific notation
    built <- ggplot2::ggplot_build(p)
    labels <- built$layout$panel_params[[1]]$x$get_labels()
    labels <- labels[!is.na(labels)]

    testthat::expect_true(length(labels) > 0)
    testthat::expect_false(any(grepl("e[+-]", labels, ignore.case = TRUE)))
})
