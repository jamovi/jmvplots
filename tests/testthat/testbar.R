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
