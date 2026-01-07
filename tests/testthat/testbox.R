#' Box plot with one variable
testthat::test_that("jmvbox: one variable", {
    # GIVEN data with a dependent variable
    df <- data.frame(
        dep = c(1, 2, 3, 4, 5, 2, 3, 4, 5, 6)
    )

    # WHEN the box plot is generated with one variable
    plot <- scatr::jmvbox(data = df, var = "dep")

    # THEN the plot should match the snapshot
    vdiffr::expect_doppelganger("jmvbox-simple", plot)
})

#' Box plot with one grouping variable
testthat::test_that("jmvbox: one variable, one group", {
    # GIVEN data with a dependent variable and one grouping factor
    df <- data.frame(
        dep = c(1, 2, 3, 4, 5, 2, 3, 4, 5, 6),
        grp = factor(c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B"))
    )

    # WHEN the box plot is generated with a grouping variable
    plot <- scatr::jmvbox(data = df, var = "dep", group1 = "grp")

    # THEN the plot should match the snapshot
    vdiffr::expect_doppelganger("jmvbox-group1", plot)
})

#' Box plot with two grouping variables
testthat::test_that("jmvbox: one variable, two groups", {
    # GIVEN data with a dependent variable and two grouping factors
    df <- data.frame(
        dep = c(1, 2, 3, 4, 5, 2, 3, 4, 5, 6),
        grp1 = factor(c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B")),
        grp2 = factor(c("X", "Y", "X", "Y", "X", "Y", "X", "Y", "X", "Y"))
    )

    # WHEN the box plot is generated with two grouping variables
    plot <- scatr::jmvbox(data = df, var = "dep", group1 = "grp1", group2 = "grp2")

    # THEN the plot should match the snapshot
    vdiffr::expect_doppelganger("jmvbox-group1-group2", plot)
})

#' Box plot with notch option enabled
testthat::test_that("jmvbox: option notch", {
    # GIVEN data suitable for a box plot
    df <- data.frame(
        dep = c(1, 2, 3, 4, 5, 2, 3, 4, 5, 6, 100),
        grp = factor(c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B", "B"))
    )

    # WHEN the box plot is generated with notch = TRUE
    plot_notch <- scatr::jmvbox(data = df, var = "dep", group1 = "grp", notch = TRUE)

    # THEN the plot should display notches and match the snapshot
    vdiffr::expect_doppelganger("jmvbox-notch", plot_notch)
})

#' Box plot with outliers hidden
testthat::test_that("jmvbox: option no outliers", {
    # GIVEN data containing outliers
    df <- data.frame(
        dep = c(1, 2, 3, 4, 5, 2, 3, 4, 5, 6, 100),
        grp = factor(c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B", "B"))
    )

    # WHEN the box plot is generated with outliers = FALSE
    plot_no_outliers <- scatr::jmvbox(data = df, var = "dep", group1 = "grp", outliers = FALSE)

    # THEN the outliers should be hidden and match the snapshot
    vdiffr::expect_doppelganger("jmvbox-no-outliers", plot_no_outliers)
})

#' Box plot with custom box width
testthat::test_that("jmvbox: option box width", {
    # GIVEN data suitable for a box plot
    df <- data.frame(
        dep = c(1, 2, 3, 4, 5, 2, 3, 4, 5, 6, 100),
        grp = factor(c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B", "B"))
    )

    # WHEN the box plot is generated with a custom box width of 0.2
    plot_width <- scatr::jmvbox(data = df, var = "dep", group1 = "grp", boxWidth = 0.2)

    # THEN the box width should be adjusted and match the snapshot
    vdiffr::expect_doppelganger("jmvbox-width-0.2", plot_width)
})

#' Box plot with manual Y-axis limits
testthat::test_that("jmvbox: manual limits", {
    # GIVEN data suitable for a box plot
    df <- data.frame(
        dep = c(1, 2, 3, 4, 5)
    )

    # WHEN the box plot is generated with manual Y-axis limits
    plot_limits <- scatr::jmvbox(
        data = df,
        var = "dep",
        yAxisRangeType = "manual",
        yAxisRangeMin = 0,
        yAxisRangeMax = 10
    )

    # THEN the Y-axis limits should be applied and match the snapshot
    vdiffr::expect_doppelganger("jmvbox-manual-limits", plot_limits)
})

#' Box plot with flipped axes
testthat::test_that("jmvbox: flipped axes", {
    # GIVEN data suitable for a box plot
    df <- data.frame(
        dep = c(1, 2, 3, 4, 5)
    )

    # WHEN the box plot is generated with axes flipped
    plot_flip <- scatr::jmvbox(data = df, var = "dep", flipAxes = TRUE)

    # THEN the axes should be flipped and match the snapshot
    vdiffr::expect_doppelganger("jmvbox-flipped", plot_flip)
})

#' Box plot with flipped axes and manual limits
testthat::test_that("jmvbox: flipped axes with manual limits", {
    # GIVEN data suitable for a box plot
    df <- data.frame(
        dep = c(1, 2, 3, 4, 5)
    )

    # WHEN the box plot is generated with flipped axes and manual Y-axis limits
    plot_flip_limits <- scatr::jmvbox(
        data = df,
        var = "dep",
        flipAxes = TRUE,
        yAxisRangeType = "manual",
        yAxisRangeMin = 0,
        yAxisRangeMax = 10
    )

    # THEN the plot should have flipped axes, correct limits, and match the snapshot
    vdiffr::expect_doppelganger("jmvbox-flipped-manual-limits", plot_flip_limits)
})

#' Box plot with manual limits (zoom behavior)
testthat::test_that("jmvbox: limits do not alter box statistics", {
    # GIVEN data with outliers that affect the median/quartiles
    # Median of 1,2,3,4,100 is 3.
    # If 100 is removed (ylim), median of 1,2,3,4 is 2.5.
    data <- data.frame(
        x = factor(rep("A", 5)),
        y = c(1, 2, 3, 4, 100)
    )

    # WHEN box plot is generated with limits excluding 100
    disp_box_zoom <- scatr::jmvbox(
        data = data,
        var = "y",
        group1 = "x",
        yAxisRangeType = "manual",
        yAxisRangeMin = 0,
        yAxisRangeMax = 10
    )

    # THEN the box plot stats should reflect the full data (median=3)
    vdiffr::expect_doppelganger("jmvbox-manual-limits-zoom", disp_box_zoom)
})
