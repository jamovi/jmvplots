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
    expect_plot_snapshot("pareto-custom-styling", plot)
})

#' Syntax mode verification tests (see helper-syntax-equivalence.R)
testthat::test_that("pareto syntax: with counts", {
    df <- data.frame(
        x = factor(c("A", "B", "C", "A", "B")),
        counts = c(10, 5, 2, 8, 3)
    )
    res <- scatr::pareto(data = df, x = "x", counts = "counts")

    syntax <- res$.__enclos_env__$private$.parent$asSource()
    testthat::expect_type(syntax, "character")
    testthat::expect_true(grepl("sec_axis", syntax, fixed = TRUE))

    expect_plot_equivalent(res, df, ".pareto")
})

testthat::test_that("pareto syntax: without counts", {
    df <- data.frame(
        x = factor(c("A", "B", "C", "A", "B", "A"))
    )
    res <- scatr::pareto(data = df, x = "x")
    expect_plot_equivalent(res, df, ".pareto")
})

testthat::test_that("pareto syntax: no error when required variables are missing", {
    # GIVEN an analysis with no variables assigned (jamovi's pre-data state)
    analysis <- scatr:::paretoClass$new(
        options = scatr:::paretoOptions$new(),
        data = ToothGrowth
    )

    # WHEN the R syntax is requested
    # THEN it returns an empty string rather than erroring
    testthat::expect_no_error(syntax <- analysis$asSource())
    testthat::expect_identical(syntax, "")
})
