# Helpers for asserting that the ggplot2 syntax emitted by asSource() reproduces
# the plot jamovi actually renders.
#
# Rather than diffing rendered SVGs (font/device fragile, and only a boolean),
# we compare the two plots at the ggplot_build() level: the computed layer data
# (geoms, positions, stats, resolved fill/colour/shape) and the axis breaks.
# That is deterministic, platform-stable, and reports *what* differs.

# Build the ggplot jamovi actually draws, by driving the private render
# function with the same ggtheme/theme jmvcore injects at runtime.
jamoviGgplot <- function(res, renderFun) {
    analysis <- res$.__enclos_env__$private$.parent
    priv <- analysis$.__enclos_env__$private
    opts <- analysis$options
    gt <- utils::getFromNamespace("getGlobalTheme", "jmvcore")(opts$theme, opts$palette)
    priv[[renderFun]](res$plot, gt$ggtheme, gt$theme)
}

# Build the ggplot from the emitted syntax, evaluated in a clean environment
# holding only the raw `data` a user would have.
syntaxGgplot <- function(res, data) {
    analysis <- res$.__enclos_env__$private$.parent
    # Parent on globalenv so the `library()` calls the generated code runs are
    # visible to subsequent function lookups in the evaluated pipeline.
    env <- new.env(parent = globalenv())
    env$data <- data
    eval(parse(text = analysis$asSource()), envir = env)
    env$p
}

# Expectation: the emitted syntax reproduces the jamovi plot. Compares each
# layer's built data (common columns) and the x/y axis breaks.
expect_plot_equivalent <- function(res, data, renderFun) {
    jp <- jamoviGgplot(res, renderFun)
    sp <- syntaxGgplot(res, data)

    ba <- suppressWarnings(ggplot2::ggplot_build(jp))
    bb <- suppressWarnings(ggplot2::ggplot_build(sp))

    testthat::expect_equal(
        length(bb$data),
        length(ba$data),
        info = "layer count"
    )

    for (i in seq_along(ba$data)) {
        da <- ba$data[[i]]
        db <- bb$data[[i]]
        common <- intersect(names(da), names(db))
        testthat::expect_equal(
            db[common],
            da[common],
            tolerance = 1e-6,
            info = sprintf("layer %d built data", i)
        )
    }

    pa <- ba$layout$panel_params[[1]]
    pb <- bb$layout$panel_params[[1]]
    testthat::expect_equal(pb$x$breaks, pa$x$breaks, info = "x breaks")
    testthat::expect_equal(pb$y$breaks, pa$y$breaks, info = "y breaks")

    invisible(NULL)
}
