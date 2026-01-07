#' Scatter plot basic usage
testthat::test_that("scat: basic usage", {
    # GIVEN a simple dataset with x and y variables
    df <- data.frame(
        x = c(1, 2, 3, 4, 5),
        y = c(2, 4, 6, 8, 10)
    )

    # WHEN the scatter plot is generated
    plot <- scatr::scat(data = df, x = "x", y = "y")

    # THEN the plot should match the snapshot
    vdiffr::expect_doppelganger("scat-simple", plot)
})

#' Scatter plot with grouping variable
testthat::test_that("scat: with grouping", {
    # GIVEN a dataset with x, y, and a grouping factor
    df <- data.frame(
        x = c(1, 2, 3, 4, 5, 6),
        y = c(2, 4, 6, 8, 10, 12),
        grp = factor(c("A", "A", "A", "B", "B", "B"))
    )

    # WHEN the scatter plot is generated with grouping
    plot <- scatr::scat(data = df, x = "x", y = "y", group = "grp")

    # THEN the plot should show grouped points and match the snapshot
    vdiffr::expect_doppelganger("scat-group", plot)
})

#' Scatter plot with regression line and standard error
testthat::test_that("scat: regression line with SE", {
    # GIVEN a dataset suitable for regression
    df <- data.frame(
        x = c(1, 2, 3, 4, 5, 2, 3, 4, 5, 6),
        y = c(1.1, 1.9, 3.2, 4.1, 5.0, 2.2, 3.1, 4.0, 5.1, 6.2)
    )

    # WHEN the scatter plot is generated with a regression line and standard error
    plot_se <- scatr::scat(data = df, x = "x", y = "y", regLine = TRUE, lineSE = TRUE)

    # THEN the plot should show the regression line with SE shading and match the snapshot
    vdiffr::expect_doppelganger("scat-regline-se", plot_se)
})

#' Scatter plot with regression line without standard error
testthat::test_that("scat: regression line without SE", {
    # GIVEN a dataset suitable for regression
    df <- data.frame(
        x = c(1, 2, 3, 4, 5, 2, 3, 4, 5, 6),
        y = c(1.1, 1.9, 3.2, 4.1, 5.0, 2.2, 3.1, 4.0, 5.1, 6.2)
    )

    # WHEN the scatter plot is generated with a regression line but NO standard error
    plot_no_se <- scatr::scat(data = df, x = "x", y = "y", regLine = TRUE, lineSE = FALSE)

    # THEN the plot should show the regression line without shading and match the snapshot
    vdiffr::expect_doppelganger("scat-regline-no-se", plot_no_se)
})

#' Scatter plot with manual axis limits
testthat::test_that("scat: manual limits", {
    # GIVEN a simple dataset
    df <- data.frame(
        x = c(1, 2, 3, 4, 5),
        y = c(2, 4, 6, 8, 10)
    )

    # WHEN the scatter plot is generated with manual X and Y axis limits
    plot_limits <- scatr::scat(
        data = df,
        x = "x",
        y = "y",
        xAxisRangeType = "manual",
        xAxisRangeMin = 0,
        xAxisRangeMax = 6,
        yAxisRangeType = "manual",
        yAxisRangeMin = 0,
        yAxisRangeMax = 12
    )

    # THEN the plot should respect the manual limits and match the snapshot
    vdiffr::expect_doppelganger("scat-manual-limits", plot_limits)
})

#' Scatter plot with flipped axes
testthat::test_that("scat: flipped axes", {
    # GIVEN a simple dataset
    df <- data.frame(
        x = c(1, 2, 3, 4, 5),
        y = c(2, 4, 6, 8, 10)
    )

    # WHEN the scatter plot is generated with flipped axes
    plot_flip <- scatr::scat(data = df, x = "x", y = "y", flipAxes = TRUE)

    # THEN the axes should be flipped and match the snapshot
    vdiffr::expect_doppelganger("scat-flipped", plot_flip)
})

#' Scatter plot with manual limits (zoom behavior)
testthat::test_that("scat: smooth line calculated on full data (zoom)", {
    # GIVEN data with an outlier affecting the trend
    data <- data.frame(
        x = c(1, 2, 3, 10),
        y = c(1, 2, 3, 100)
    )

    # WHEN scatter plot is zoomed to exclude the outlier
    # If filtered (ylim), regression on 1,2,3 is perfect line y=x.
    # If zoomed (coord_cartesian), regression includes (10,100), pulling the line up.
    disp_scat_zoom <- scatr::scat(
        data = data,
        x = "x",
        y = "y",
        regLine = TRUE,
        yAxisRangeType = "manual",
        yAxisRangeMin = 0,
        yAxisRangeMax = 5
    )

    # THEN the smooth line should reflect the influence of the outlier
    vdiffr::expect_doppelganger("scat-manual-limits-zoom", disp_scat_zoom)
})
