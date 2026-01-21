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
