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

#' Line plot with custom font faces
testthat::test_that("jmvline: custom font faces, sizes, and alignment", {
    # GIVEN data suitable for line plot
    data <- data.frame(
        x = factor(c("A", "B", "C")),
        y = c(1, 3, 5)
    )

    # WHEN the line plot is generated with custom font faces, sizes, and alignment
    plot <- scatr::jmvline(
        data = data,
        x = "x",
        y = "y",
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
    vdiffr::expect_doppelganger("jmvline-custom-styling", plot)
})

#' Line plot missing value exclusion
testthat::test_that("jmvline: naOmit = TRUE", {
    # GIVEN data with missing values
    data <- data.frame(
        x = factor(c("A", "A", "B", "B", NA, NA)),
        y = c(3, 3, 1, 2, NA, NA),
        group = factor(c("X", "Y", "X", "Y", NA, NA))
    )

    # WHEN the line plot is generated with naOmit = TRUE
    disp_line <- scatr::jmvline(
        data = data,
        x = "x",
        y = "y",
        group = "group",
        naOmit = TRUE,
        mode = "aggregate"
    )

    # THEN the plot should match the snapshot
    vdiffr::expect_doppelganger("jmvline-naOmit-true", disp_line)
})

#' Line plot missing value exclusion (naOmit=FALSE)
testthat::test_that("jmvline: naOmit = FALSE", {
    # GIVEN data with missing values
    data <- data.frame(
        x = factor(c("A", "A", "B", "B", NA, NA)),
        y = c(3, 3, 1, 2, NA, NA),
        group = factor(c("X", "Y", "X", "Y", NA, NA))
    )

    # WHEN the line plot is generated with naOmit = FALSE
    disp_line <- scatr::jmvline(
        data = data,
        x = "x",
        y = "y",
        group = "group",
        naOmit = FALSE,
        mode = "aggregate"
    )

    # THEN the plot should match the snapshot
    vdiffr::expect_doppelganger("jmvline-naOmit-false", disp_line)
})

#' Syntax mode verification tests
testthat::test_that("jmvline syntax: individual mode, grouped", {
    # GIVEN data with ordinal factor and group
    df <- data.frame(
        x = factor(c("A", "A", "B", "B", "C", "C")),
        y = c(1, 2, 3, 4, 5, 6),
        group = factor(c("X", "Y", "X", "Y", "X", "Y"))
    )
    res <- scatr::jmvline(data = df, x = "x", y = "y", group = "group", mode = "individual")

    # WHEN we request the R syntax
    syntax <- res$.__enclos_env__$private$.parent$asSource()

    # THEN it should generate a non-empty character string
    testthat::expect_type(syntax, "character")
    testthat::expect_true(nchar(syntax) > 0)
    testthat::expect_true(grepl("group = group", syntax, fixed = TRUE))

    # AND when we evaluate the syntax in a clean environment
    test_env <- new.env()
    test_env$data <- df
    testthat::expect_no_error(eval(parse(text = syntax), envir = test_env))

    # THEN the plot and data objects should be generated correctly
    testthat::expect_true(exists("p", envir = test_env))
    testthat::expect_true(exists("plot_data", envir = test_env))
    testthat::expect_s3_class(test_env$p, "ggplot")
    
    # AND the prepared plot data should match the analysis plot's state
    testthat::expect_equal(test_env$plot_data$y, res$plot$state$y)
})

testthat::test_that("jmvline syntax: aggregate mode, grouped with error bars", {
    # GIVEN ToothGrowth data
    df <- ToothGrowth
    df$dose <- factor(df$dose)
    res <- scatr::jmvline(
        data = df,
        x = "dose",
        y = "len",
        group = "supp",
        mode = "aggregate",
        errorBars = "ci",
        ciWidth = 95
    )

    # WHEN we request the R syntax
    syntax <- res$.__enclos_env__$private$.parent$asSource()

    # THEN it should contain references to grouping and CI calculation
    testthat::expect_true(grepl("group_by(x, group)", syntax, fixed = TRUE))
    testthat::expect_true(grepl("geom_errorbar", syntax, fixed = TRUE))

    # AND when we evaluate the syntax in a clean environment
    test_env <- new.env()
    test_env$data <- df
    testthat::expect_no_error(eval(parse(text = syntax), envir = test_env))

    # THEN the plot and data objects should be generated correctly
    testthat::expect_true(exists("p", envir = test_env))
    testthat::expect_s3_class(test_env$p, "ggplot")

    # AND the prepared plot data should match the analysis plot's state
    # Sort keys for alignment check if orders differ slightly
    testthat::expect_equal(
        test_env$plot_data[order(test_env$plot_data$x, test_env$plot_data$group), "y"],
        res$plot$state[order(res$plot$state$x, res$plot$state$group), "y"]
    )
    testthat::expect_equal(
        test_env$plot_data[order(test_env$plot_data$x, test_env$plot_data$group), "ci"],
        res$plot$state[order(res$plot$state$x, res$plot$state$group), "ci"]
    )
})
