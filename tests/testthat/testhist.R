testthat::test_that("one variable, no grouping variable", {
    disp_hist_jmvplot <- scatr::jmvhist(data = ToothGrowth, var = len)

    vdiffr::expect_doppelganger("hist-no-group", disp_hist_jmvplot)
})
