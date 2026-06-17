#' Pareto plot with custom font faces
testthat::test_that("pareto: custom font faces, sizes, and alignment", {
    # GIVEN data suitable for pareto plot
    df <- data.frame(
        x = c("A", "B", "C"),
        counts = c(10, 5, 2)
    )

    # WHEN the pareto plot is generated with custom font faces, sizes, and alignment
    plot <- scatr::pareto(
        data = df,
        x = "x",
        counts = "counts",
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
    vdiffr::expect_doppelganger("pareto-custom-styling", plot)
})

#' Syntax mode verification tests
testthat::test_that("pareto syntax: with counts", {
    # GIVEN data with counts
    df <- data.frame(
        x = factor(c("A", "B", "C", "A", "B")),
        counts = c(10, 5, 2, 8, 3)
    )
    res <- scatr::pareto(data = df, x = "x", counts = "counts")

    # WHEN we request the R syntax
    syntax <- res$.__enclos_env__$private$.parent$asSource()

    # THEN it should generate a non-empty character string
    testthat::expect_type(syntax, "character")
    testthat::expect_true(nchar(syntax) > 0)
    testthat::expect_true(grepl("sec_axis", syntax, fixed = TRUE))

    # AND when we evaluate the syntax in a clean environment
    test_env <- new.env()
    test_env$data <- df
    testthat::expect_no_error(eval(parse(text = syntax), envir = test_env))

    # THEN the plot and data objects should be generated correctly
    testthat::expect_true(exists("p", envir = test_env))
    testthat::expect_true(exists("plot_data", envir = test_env))
    testthat::expect_s3_class(test_env$p, "ggplot")
    
    # AND the prepared plot data should match the analysis plot's state
    testthat::expect_equal(test_env$plot_data$counts, res$plot$state$df$counts)
    testthat::expect_equal(test_env$plot_data$cum, res$plot$state$df$cum)
})

testthat::test_that("pareto syntax: without counts", {
    # GIVEN data without counts
    df <- data.frame(
        x = factor(c("A", "B", "C", "A", "B", "A"))
    )
    res <- scatr::pareto(data = df, x = "x")

    # WHEN we request the R syntax
    syntax <- res$.__enclos_env__$private$.parent$asSource()

    # THEN it should generate a non-empty character string
    testthat::expect_type(syntax, "character")
    testthat::expect_true(nchar(syntax) > 0)

    # AND when we evaluate the syntax in a clean environment
    test_env <- new.env()
    test_env$data <- df
    testthat::expect_no_error(eval(parse(text = syntax), envir = test_env))

    # THEN the plot and data objects should be generated correctly
    testthat::expect_true(exists("p", envir = test_env))
    testthat::expect_s3_class(test_env$p, "ggplot")

    # AND the prepared plot data should match the analysis plot's state
    testthat::expect_equal(test_env$plot_data$counts, res$plot$state$df$counts)
})
