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
    disp_hist_bins <- scatr::jmvhist(
        data = ToothGrowth,
        var = "len",
        binWidthType = "manual",
        binWidth = 2
    )

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

#' Histogram with manual limits (zoom behavior)
testthat::test_that("jmvhist: bins are not removed by zoom", {
    # GIVEN data spanning 0-100 (N=10)
    data <- data.frame(val = c(rep(10, 5), rep(90, 5)))

    # WHEN histogram is zoomed into 0-50 range
    disp_hist_zoom <- scatr::jmvhist(
        data = data,
        var = "val",
        xAxisRangeType = "manual",
        xAxisRangeMin = 0,
        xAxisRangeMax = 50
    )

    # THEN the bin at 10 should be visible, unmodified by the missing 90s
    vdiffr::expect_doppelganger("jmvhist-manual-limits-zoom", disp_hist_zoom)
})

#' Histogram with custom font faces
testthat::test_that("jmvhist: custom font faces, sizes, and alignment", {
    # GIVEN standard dataset
    # WHEN the histogram is generated with custom font faces, sizes, and alignment
    disp_hist_jmvplot <- scatr::jmvhist(
        data = ToothGrowth,
        var = "len",
        title = "Bold Center 20",
        titleFontFace = "bold",
        titleFontSize = 20,
        titleAlign = "center",
        subtitle = "Italic Left 14",
        subtitleFontFace = "italic",
        subtitleFontSize = 14,
        subtitleAlign = "left",
        xLabel = "Bold Italic Right 18",
        xLabelFontFace = "bold-italic",
        xLabelFontSize = 18,
        xLabelAlign = "right",
        yLabel = "Plain Center 12",
        yLabelFontFace = "plain",
        yLabelFontSize = 12,
        yLabelAlign = "center",
        caption = "Bold Right 10",
        captionFontFace = "bold",
        captionFontSize = 10,
        captionAlign = "right"
    )

    # THEN the plot should match the snapshot
    vdiffr::expect_doppelganger("jmvhist-custom-styling", disp_hist_jmvplot)
})

#' Syntax mode verification tests
testthat::test_that("jmvhist syntax: simple, no grouping", {
    # GIVEN ToothGrowth data
    res <- scatr::jmvhist(data = ToothGrowth, var = "len", density = TRUE)

    # WHEN we request the R syntax
    syntax <- res$.__enclos_env__$private$.parent$asSource()

    # THEN it should generate a non-empty character string
    testthat::expect_type(syntax, "character")
    testthat::expect_true(nchar(syntax) > 0)
    testthat::expect_true(grepl("geom_density", syntax, fixed = TRUE))

    # AND when we evaluate the syntax in a clean environment
    test_env <- new.env()
    test_env$data <- ToothGrowth
    testthat::expect_no_error(eval(parse(text = syntax), envir = test_env))

    # THEN the plot and data objects should be generated correctly
    testthat::expect_true(exists("p", envir = test_env))
    testthat::expect_true(exists("plot_data", envir = test_env))
    testthat::expect_s3_class(test_env$p, "ggplot")
    
    # AND the prepared plot data should match the analysis plot's state
    testthat::expect_equal(test_env$plot_data$y, res$plot$state$y)
})

testthat::test_that("jmvhist syntax: grouping variable", {
    # GIVEN ToothGrowth data with group
    res <- scatr::jmvhist(data = ToothGrowth, var = "len", group = "supp")

    # WHEN we request the R syntax
    syntax <- res$.__enclos_env__$private$.parent$asSource()

    # THEN it should contain references to grouping
    testthat::expect_true(grepl("fill = group", syntax, fixed = TRUE))

    # AND when we evaluate the syntax in a clean environment
    test_env <- new.env()
    test_env$data <- ToothGrowth
    testthat::expect_no_error(eval(parse(text = syntax), envir = test_env))

    # THEN the plot and data objects should be generated correctly
    testthat::expect_true(exists("p", envir = test_env))
    testthat::expect_s3_class(test_env$p, "ggplot")

    # AND the prepared plot data should match the analysis plot's state
    testthat::expect_equal(test_env$plot_data$y, res$plot$state$y)
})
