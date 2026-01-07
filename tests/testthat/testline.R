#' Line plot with ordinal grouping variable (individual points)
testthat::test_that("jmvline: ordinal grouping variable, individual points", {
    # GIVEN data with ordinal factors
    data <- data.frame(
        x = factor(c("A", "A", "B", "B", "C", "C")),
        y = c(1, 2, 3, 4, 5, 6),
        group = ordered(c(1, 2, 1, 2, 1, 2))
    )

    # WHEN the line plot is generated
    disp_line_jmvplot <- scatr::jmvline(data = data, x = "x", y = "y", group = "group")

    # THEN the plot should match the snapshot
    vdiffr::expect_doppelganger("jmvline-ordinal-group-individual", disp_line_jmvplot)
})

#' Line plot with ordinal grouping variable (aggregated data)
testthat::test_that("jmvline: ordinal grouping variable, aggregated data", {
    # GIVEN data suitable for aggregation
    data <- data.frame(
        x = factor(c("A", "A", "B", "B", "C", "C", "A", "A", "B", "B", "C", "C")),
        y = c(1, 2, 3, 4, 5, 6, 1.1, 2.1, 3.1, 4.1, 5.1, 6.1),
        group = ordered(c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2))
    )

    # WHEN the line plot is generated in aggregate mode
    disp_line_jmvplot <- scatr::jmvline(
        data = data,
        x = "x",
        y = "y",
        group = "group",
        mode = "aggregate"
    )

    # THEN the plot should show aggregated lines and match the snapshot
    vdiffr::expect_doppelganger("jmvline-ordinal-group-aggregated", disp_line_jmvplot)
})

#' Line plot with error bars (Confidence Interval)
testthat::test_that("jmvline: error bars CI", {
    # GIVEN ToothGrowth data for aggregation
    # x needs to be factor for jmvline
    df <- ToothGrowth
    df$dose <- factor(df$dose)

    # WHEN the line plot is generated with CI error bars
    plot_ci <- scatr::jmvline(
        data = df,
        x = "dose",
        y = "len",
        mode = "aggregate",
        errorBars = "ci"
    )

    # THEN the plot should display CI bars and match the snapshot
    vdiffr::expect_doppelganger("jmvline-ci", plot_ci)
})

#' Line plot with error bars (Standard Error)
testthat::test_that("jmvline: error bars SE", {
    # GIVEN ToothGrowth data for aggregation
    # x needs to be factor for jmvline
    df <- ToothGrowth
    df$dose <- factor(df$dose)

    # WHEN the line plot is generated with SE error bars
    plot_se <- scatr::jmvline(
        data = df,
        x = "dose",
        y = "len",
        mode = "aggregate",
        errorBars = "se"
    )

    # THEN the plot should display SE bars and match the snapshot
    vdiffr::expect_doppelganger("jmvline-se", plot_se)
})

#' Line plot with manual axis limits
testthat::test_that("jmvline: manual limits", {
    # GIVEN data suitable for a line plot
    data <- data.frame(
        x = factor(c("A", "B", "C")),
        y = c(1, 3, 5)
    )

    # WHEN the line plot is generated with manual Y-axis limits
    plot_limits <- scatr::jmvline(
        data = data,
        x = "x",
        y = "y",
        yAxisRangeType = "manual",
        yAxisRangeMin = 0,
        yAxisRangeMax = 10
    )

    # THEN the Y-axis should be limited and match the snapshot
    vdiffr::expect_doppelganger("jmvline-manual-limits", plot_limits)
})

#' Line plot with flipped axes
testthat::test_that("jmvline: flipped axes", {
    # GIVEN data suitable for a line plot
    data <- data.frame(
        x = factor(c("A", "B", "C")),
        y = c(1, 3, 5)
    )

    # WHEN the line plot is generated with flipped axes
    plot_flip <- scatr::jmvline(data = data, x = "x", y = "y", flipAxes = TRUE)

    # THEN the axes should be flipped and match the snapshot
    vdiffr::expect_doppelganger("jmvline-flipped", plot_flip)
})

#' Line plot with manual limits (zoom behavior)
testthat::test_that("jmvline: manual limits do not clip data (zoom)", {
    # GIVEN data with an outlier off-screen
    data <- data.frame(
        x = factor(c("A", "A", "B", "B")),
        y = c(10, 10, 10, 100),
        group = factor(c("G1", "G1", "G1", "G1"))
    )

    # WHEN line plot is generated with limits smaller than the outlier
    disp_line_zoom <- scatr::jmvline(
        data = data,
        x = "x",
        y = "y",
        group = "group",
        yAxisRangeType = "manual",
        yAxisRangeMin = 0,
        yAxisRangeMax = 20
    )

    # THEN the line should still be drawn towards the outlier (zoomed in)
    vdiffr::expect_doppelganger("jmvline-manual-limits-zoom", disp_line_zoom)
})
