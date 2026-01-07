#' Histogram with one variable, no grouping
testthat::test_that("jmvhist: one variable, no grouping variable", {
    # GIVEN no grouping setup
    # WHEN the histogram is generated for a single variable
    disp_hist_jmvplot <- scatr::jmvhist(data = ToothGrowth, var = "len")

    # THEN the plot should match the snapshot
    vdiffr::expect_doppelganger("jmvhist-no-group", disp_hist_jmvplot)
})

#' Histogram with one variable and a grouping variable
testthat::test_that("jmvhist: one variable, grouping variable", {
    # GIVEN grouping setup
    # WHEN the histogram is generated with a grouping variable
    disp_hist_jmvplot <- scatr::jmvhist(data = ToothGrowth, var = "len", group = "supp")

    # THEN the plot should show grouped data matching the snapshot
    vdiffr::expect_doppelganger("jmvhist-group", disp_hist_jmvplot)
})

#' Histogram with density and normal curve
testthat::test_that("jmvhist: density and normal curve", {
    # GIVEN standard dataset
    # WHEN the histogram is generated with density enabled
    disp_hist_density <- scatr::jmvhist(data = ToothGrowth, var = "len", density = TRUE)

    # THEN the plot should display density curves and match the snapshot
    vdiffr::expect_doppelganger("jmvhist-density", disp_hist_density)
})

#' Histogram with manual bin width
testthat::test_that("jmvhist: manual bins", {
    # GIVEN standard dataset
    # WHEN function is called with manual bin width
    disp_hist_bins <- scatr::jmvhist(data = ToothGrowth, var = "len", binWidthType = "manual", binWidth = 2)

    # THEN the bins should be sized accordingly in the snapshot
    vdiffr::expect_doppelganger("jmvhist-manual-bins", disp_hist_bins)
})

#' Histogram with manual axis limits
testthat::test_that("jmvhist: manual limits", {
    # GIVEN standard dataset
    # WHEN function is called with manual X and Y axis limits
    disp_hist_limits <- scatr::jmvhist(
        data = ToothGrowth,
        var = "len",
        yAxisRangeType = "manual",
        yAxisRangeMin = 0,
        yAxisRangeMax = 30,
        xAxisRangeType = "manual",
        xAxisRangeMin = 0,
        xAxisRangeMax = 40
    )

    # THEN the plot axes should be constrained to the limits in the snapshot
    vdiffr::expect_doppelganger("jmvhist-manual-limits", disp_hist_limits)
})

#' Histogram with flipped axes
testthat::test_that("jmvhist: flipped axes", {
    # GIVEN standard dataset
    # WHEN function is called with flipAxes = TRUE
    disp_hist_flip <- scatr::jmvhist(data = ToothGrowth, var = "len", flipAxes = TRUE)

    # THEN the axes should be flipped in the snapshot
    vdiffr::expect_doppelganger("jmvhist-flipped", disp_hist_flip)
})
