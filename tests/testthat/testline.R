testthat::test_that("ordinal grouping variable, individual points", {
    data <- data.frame(
        x = factor(c("A", "A", "B", "B", "C", "C")),
        y = c(1, 2, 3, 4, 5, 6),
        group = ordered(c(1, 2, 1, 2, 1, 2))
    )

    disp_line_jmvplot <- scatr::jmvline(data = data, x = "x", y = "y", group = "group")

    vdiffr::expect_doppelganger("ordinal-group-individual", disp_line_jmvplot)
})

testthat::test_that("ordinal grouping variable, aggregated data", {
    data <- data.frame(
        x = factor(c("A", "A", "B", "B", "C", "C")),
        y = c(1, 2, 3, 4, 5, 6),
        group = ordered(c(1, 2, 1, 2, 1, 2))
    )

    disp_line_jmvplot <- scatr::jmvline(
        data = data,
        x = "x",
        y = "y",
        group = "group",
        mode = "aggregate"
    )

    vdiffr::expect_doppelganger("ordinal-group-aggregated", disp_line_jmvplot)
})
