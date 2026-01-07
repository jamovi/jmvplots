#' Bar plot in categorical mode with no grouping
testthat::test_that("jmvbar: mode categorical, no grouping variable", {
    # GIVEN categorical data
    # WHEN the bar plot is generated in categorical mode
    disp_bar_jmvplot <- scatr::jmvbar(data = ToothGrowth, mode = "categorical", catvar = dose)

    # THEN the plot should match the snapshot
    vdiffr::expect_doppelganger("jmvbar-categorical-no-group", disp_bar_jmvplot)
})

#' Bar plot in categorical mode with grouping
testthat::test_that("jmvbar: mode categorical, grouping variable", {
    # GIVEN categorical data with grouping
    # WHEN the bar plot is generated in categorical mode with grouping
    disp_bar_jmvplot <- scatr::jmvbar(
        data = ToothGrowth,
        mode = "categorical",
        catvar = dose,
        catgroup = supp
    )

    # THEN the plot should show grouped bars and match the snapshot
    vdiffr::expect_doppelganger("jmvbar-categorical-group", disp_bar_jmvplot)
})

#' Bar plot in continuous mode with no grouping
testthat::test_that("jmvbar: mode continuous, no grouping variable", {
    # GIVEN continuous data
    # WHEN the bar plot is generated in continuous mode
    disp_bar_jmvplot <- scatr::jmvbar(
        data = ToothGrowth,
        mode = "continuous",
        convar = len
    )

    # THEN the plot should match the snapshot
    vdiffr::expect_doppelganger("jmvbar-continuous-no-group", disp_bar_jmvplot)
})

#' Bar plot in continuous mode with 1 grouping variable
testthat::test_that("jmvbar: mode continuous, 1 grouping variable", {
    # GIVEN continuous data with one grouping variable
    # WHEN the bar plot is generated
    disp_bar_jmvplot <- scatr::jmvbar(
        data = ToothGrowth,
        mode = "continuous",
        convar = len,
        congroup1 = dose
    )

    # THEN the plot should match the snapshot
    vdiffr::expect_doppelganger("jmvbar-continuous-1-group", disp_bar_jmvplot)
})

#' Bar plot in continuous mode with 2 grouping variables
testthat::test_that("jmvbar: mode continuous, 2 grouping variable", {
    # GIVEN continuous data with two grouping variables
    # WHEN the bar plot is generated
    disp_bar_jmvplot <- scatr::jmvbar(
        data = ToothGrowth,
        mode = "continuous",
        convar = len,
        congroup1 = dose,
        congroup2 = supp
    )

    # THEN the plot should match the snapshot
    vdiffr::expect_doppelganger("jmvbar-continuous-2-group", disp_bar_jmvplot)
})

#' Bar plot in continuous mode with no grouping and error bars (CI)
testthat::test_that("jmvbar: mode continuous, no grouping variable, error bars", {
    # GIVEN continuous data
    # WHEN the bar plot is generated with CI error bars
    disp_bar_jmvplot <- scatr::jmvbar(
        data = ToothGrowth,
        mode = "continuous",
        convar = len,
        errorBars = "ci",
        ciWidth = 95,
    )

    # THEN the plot should display CI bars and match the snapshot
    vdiffr::expect_doppelganger("jmvbar-continuous-no-group-ci", disp_bar_jmvplot)
})

#' Bar plot in continuous mode with 1 grouping variable and error bars (CI)
testthat::test_that("jmvbar: mode continuous, 1 grouping variable, error bars", {
    # GIVEN continuous data with grouping
    # WHEN the bar plot is generated with CI error bars
    disp_bar_jmvplot <- scatr::jmvbar(
        data = ToothGrowth,
        mode = "continuous",
        convar = len,
        congroup1 = dose,
        errorBars = "ci",
        ciWidth = 95,
    )

    # THEN the plot should display CI bars on grouped data and match the snapshot
    vdiffr::expect_doppelganger("jmvbar-continuous-1-group-ci", disp_bar_jmvplot)
})

#' Bar plot in continuous mode with 2 grouping variables and error bars (CI)
testthat::test_that("jmvbar: mode continuous, 2 grouping variable, error bars", {
    # GIVEN continuous data with two grouping variables
    # WHEN the bar plot is generated with CI error bars
    disp_bar_jmvplot <- scatr::jmvbar(
        data = ToothGrowth,
        mode = "continuous",
        convar = len,
        congroup1 = dose,
        congroup2 = supp,
        errorBars = "ci",
        ciWidth = 95,
    )

    # THEN the plot should match the snapshot
    vdiffr::expect_doppelganger("jmvbar-continuous-2-group-ci", disp_bar_jmvplot)
})

#' Bar plot in counts mode with no labels
testthat::test_that("jmvbar: mode counts, no labels", {
    # GIVEN counts data
    # WHEN the bar plot is generated in counts mode
    df <- data.frame(
        counts = c(1, 2, 3)
    )

    disp_bar_jmvplot <- scatr::jmvbar(data = df, mode = "counts", counts = counts)

    # THEN the plot should match the snapshot
    vdiffr::expect_doppelganger("jmvbar-counts-no-labels", disp_bar_jmvplot)
})

#' Bar plot in counts mode with labels
testthat::test_that("jmvbar: mode counts, with labels", {
    # GIVEN counts data with labels
    # WHEN the bar plot is generated
    df <- data.frame(
        counts = c(1, 2, 3),
        labels = c("A", "B", "C")
    )

    disp_bar_jmvplot <- scatr::jmvbar(
        data = df,
        mode = "counts",
        counts = counts,
        countsLabels = labels
    )

    # THEN the plot should show labels and match the snapshot
    vdiffr::expect_doppelganger("jmvbar-counts-with-labels", disp_bar_jmvplot)
})

#' Bar plot in counts mode with no labels and grouping
testthat::test_that("jmvbar: mode counts, no labels, grouping variable", {
    # GIVEN counts data with grouping
    # WHEN the bar plot is generated
    df <- data.frame(
        counts = c(1, 2, 3),
        group = c("A", "B", "C")
    )

    # WHEN the bar plot is generated
    disp_bar_jmvplot <- scatr::jmvbar(
        data = df,
        mode = "counts",
        counts = counts,
        countsgroup = group
    )

    # THEN the plot should show grouped bars and match the snapshot
    vdiffr::expect_doppelganger("jmvbar-counts-no-labels-group", disp_bar_jmvplot)
})

#' Bar plot in counts mode with labels and grouping
testthat::test_that("jmvbar: mode counts, with labels, grouping variable", {
    # GIVEN counts data with labels and grouping
    df <- data.frame(
        counts = c(1, 2, 3, 4, 5, 6),
        labels = c("A", "A", "A", "B", "B", "B"),
        group = c("A", "B", "C", "A", "B", "C")
    )

    # WHEN the bar plot is generated
    disp_bar_jmvplot <- scatr::jmvbar(
        data = df,
        mode = "counts",
        counts = counts,
        countsLabels = labels,
        countsgroup = group
    )

    # THEN the plot should match the snapshot
    vdiffr::expect_doppelganger("jmvbar-counts-with-labels-group", disp_bar_jmvplot)
})

#' Bar plot with manual limits
testthat::test_that("jmvbar: manual limits", {
    # GIVEN data suitable for bar plot

    # WHEN the bar plot is generated with manual Y-axis limits
    disp_bar_limits <- scatr::jmvbar(
        data = ToothGrowth,
        mode = "continuous",
        convar = "len",
        yAxisRangeType = "manual",
        yAxisRangeMin = 0,
        yAxisRangeMax = 40
    )

    # THEN the Y-axis should be constrained and match the snapshot
    vdiffr::expect_doppelganger("jmvbar-manual-limits", disp_bar_limits)
})

#' Bar plot with flipped axes
testthat::test_that("jmvbar: flipped axes", {
    # GIVEN data suitable for bar plot

    # WHEN the bar plot is generated with flipped axes
    disp_bar_flip <- scatr::jmvbar(
        data = ToothGrowth,
        mode = "continuous",
        convar = "len",
        flipAxes = TRUE
    )

    # THEN the axes should be flipped and match the snapshot
    vdiffr::expect_doppelganger("jmvbar-flipped", disp_bar_flip)
})

#' Bar plot with manual limits (zoom behavior)
testthat::test_that("jmvbar: manual limits do not remove bars", {
    # GIVEN data with a high value (N=3 to avoid CI warnings)
    data <- data.frame(
        x = factor(rep(c("A", "B"), each = 3)),
        y = c(rep(10, 3), rep(100, 3))
    )

    # WHEN bar plot is generated with limits smaller than the high value
    disp_bar_zoom <- scatr::jmvbar(
        data = data,
        mode = "continuous",
        convar = "y",
        congroup1 = "x",
        yAxisRangeType = "manual",
        yAxisRangeMin = 0,
        yAxisRangeMax = 20
    )

    # THEN the bar should still be drawn (cropped)
    vdiffr::expect_doppelganger("jmvbar-manual-limits-zoom", disp_bar_zoom)
})
