testthat::test_that("mode categorical, no grouping variable", {
    disp_bar_jmvplot <- jmvplot::bar(data = ToothGrowth, mode = "categorical", catvar = dose)

    vdiffr::expect_doppelganger("cat-no-group", disp_bar_jmvplot)
})

testthat::test_that("mode categorical, grouping variable", {
    disp_bar_jmvplot <- jmvplot::bar(
        data = ToothGrowth,
        mode = "categorical",
        catvar = dose,
        catgroup = supp
    )

    vdiffr::expect_doppelganger("cat-group", disp_bar_jmvplot)
})

testthat::test_that("mode continuous, no grouping variable", {
    disp_bar_jmvplot <- jmvplot::bar(
        data = ToothGrowth,
        mode = "continuous",
        convar = len
    )

    vdiffr::expect_doppelganger("con-no-group", disp_bar_jmvplot)
})

testthat::test_that("mode continuous, 1 grouping variable", {
    disp_bar_jmvplot <- jmvplot::bar(
        data = ToothGrowth,
        mode = "continuous",
        convar = len,
        congroup1 = dose
    )

    vdiffr::expect_doppelganger("con-1-group", disp_bar_jmvplot)
})

testthat::test_that("mode continuous, 2 grouping variable", {
    disp_bar_jmvplot <- jmvplot::bar(
        data = ToothGrowth,
        mode = "continuous",
        convar = len,
        congroup1 = dose,
        congroup2 = supp
    )

    vdiffr::expect_doppelganger("con-2-group", disp_bar_jmvplot)
})

testthat::test_that("mode continuous, no grouping variable, error bars", {
    disp_bar_jmvplot <- jmvplot::bar(
        data = ToothGrowth,
        mode = "continuous",
        convar = len,
        errorBars = "ci",
        ciWidth = 95,
    )

    vdiffr::expect_doppelganger("con-no-group-ci", disp_bar_jmvplot)
})

testthat::test_that("mode continuous, 1 grouping variable, error bars", {
    disp_bar_jmvplot <- jmvplot::bar(
        data = ToothGrowth,
        mode = "continuous",
        convar = len,
        congroup1 = dose,
        errorBars = "ci",
        ciWidth = 95,
    )

    vdiffr::expect_doppelganger("con-1-group-ci", disp_bar_jmvplot)
})

testthat::test_that("mode continuous, 2 grouping variable, error bars", {
    disp_bar_jmvplot <- jmvplot::bar(
        data = ToothGrowth,
        mode = "continuous",
        convar = len,
        congroup1 = dose,
        congroup2 = supp,
        errorBars = "ci",
        ciWidth = 95,
    )

    vdiffr::expect_doppelganger("con-2-group-ci", disp_bar_jmvplot)
})

testthat::test_that("mode counts, no labels", {
    df <- data.frame(
        counts = c(1, 2, 3)
    )

    disp_bar_jmvplot <- jmvplot::bar(data = df, mode = "counts", counts = counts)

    vdiffr::expect_doppelganger("counts-no-labels", disp_bar_jmvplot)
})

testthat::test_that("mode counts, with labels", {
    df <- data.frame(
        counts = c(1, 2, 3),
        labels = c("A", "B", "C")
    )

    disp_bar_jmvplot <- jmvplot::bar(
        data = df,
        mode = "counts",
        counts = counts,
        countsLabels = labels
    )

    vdiffr::expect_doppelganger("counts-with-labels", disp_bar_jmvplot)
})

testthat::test_that("mode counts, no labels, grouping variable", {
    df <- data.frame(
        counts = c(1, 2, 3),
        group = c("A", "B", "C")
    )

    disp_bar_jmvplot <- jmvplot::bar(
        data = df,
        mode = "counts",
        counts = counts,
        countsgroup = group
    )

    vdiffr::expect_doppelganger("counts-no-labels-group", disp_bar_jmvplot)
})

testthat::test_that("mode counts, with labels, grouping variable", {
    df <- data.frame(
        counts = c(1, 2, 3, 4, 5, 6),
        labels = c("A", "A", "A", "B", "B", "B"),
        group = c("A", "B", "C", "A", "B", "C")
    )

    disp_bar_jmvplot <- jmvplot::bar(
        data = df,
        mode = "counts",
        counts = counts,
        countsLabels = labels,
        countsgroup = group
    )

    vdiffr::expect_doppelganger("counts-with-labels-group", disp_bar_jmvplot)
})
