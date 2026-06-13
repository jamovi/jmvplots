# CLAUDE.md

Guidance for Claude Code working in this repository. This file is **specific to
jmvplot/scatr**; general jamovi conventions live in the skills below (and this
file wins where they disagree).

## Overview

**scatr** is a jamovi module and R package — a convenience wrapper around
`ggplot2` for creating plots inside jamovi. It is bundled with jamovi itself.
Analyses: `jmvbar`, `jmvbox`, `jmvhist`, `jmvline`, `pareto`, `scat` (Bar, Box,
Histogram, Line, Pareto, Scatter).

## Skills

This repo follows the jamovi team's opt-in skills — use them here:
`jamovi-module` (module mechanics), `jamovi-dev-standards` (commit + test
conventions), `jamovi-r-standards` (air, renv, testthat). If they aren't loaded,
install the `jamovi-module` and `jamovi-dev-standards` plugins first.

> **Runtime caveat:** scatr ships inside jamovi's own bundled R and ggplot2
> (currently ggplot2 3.5.2 — older than a typical dev `renv`). The real
> integration check is `jmvtools::install()` + running in jamovi; some
> version-specific failures will not reproduce in the dev environment.

## Commands

```r
renv::restore()                              # bootstrap the dev library
Rscript -e 'testthat::test_package("scatr")' # run all tests
testthat::test_file("tests/testthat/testbar.R")
vdiffr::manage_cases()                       # review/update visual snapshots
```

`air format .` formats R code (config in `air.toml`; `*.h.R` is excluded).

## Architecture

Each `R/<name>.b.R` implements `.preparePlotData()` (sets `image$state`), a
render function named in `<name>.r.yaml` (e.g. `.barPlot()`), and `asSource()`.

### `createPlotFromCallStack` (`R/utils.R`)

Render functions don't build ggplot objects directly. They build a **call
stack** — a list of `list(fun, args)` specs (often via a private
`.getPlotCallList()`) — and pass it to `createPlotFromCallStack()`. This keeps
plot construction composable and lets the render and syntax paths share it.

### Shared utilities (`R/utils.R`)

- `setLabels()` / `getLabsCallList()` — build `ggplot2::labs()` from options
- `formatLabels()` / `getLabelsThemeCallArgs()` — axis/title theme (handles `flipAxes`)
- `formatLegend()` / `getLegendThemeCallArgs()` — legend position (outside/inside/hide)
- `autoscalePlotBreaks()` — axis breaks from the plot's pixel dimensions
- `toGgplot()` — jamovi option values → ggplot2 values (e.g. `"bold-italic"` → `"bold.italic"`)

### Syntax mode (`asSource` + `R/syntax.R`)

jamovi's "Syntax mode" shows the standalone R a user could run to reproduce the
plot. Each analysis' `asSource()` builds it through `R/syntax.R`:

- `generateDataPrepCode()` — emits the dplyr pipeline that reshapes the raw data
  into the `plot_data` the plot expects (mirrors `.preparePlotData()`).
- the analysis' `.getPlotCallList()` — the **same** call stack the render path
  uses, so the geoms/scales stay in sync.
- `getSyntaxThemeColors()` — bakes in the real palette (mirrors
  `jmvcore::getTheme()`); `finalizePlotSyntax()` appends the theme and the axis
  break scaling that matches `autoscalePlotBreaks()` at jamovi's default size.
- `generatePlotCode()` / `formatValueForCode()` — render the call stack to a
  formatted code string (label strings are escaped via `quoteString()`).

`asSource()` returns `""` when the analysis' required variables aren't set
(mirroring the `.run()` guard) — jamovi calls it even for an unconfigured
analysis, so it must not fail.

The generated code must **reproduce the rendered plot**. This is verified by
`tests/testthat/helper-syntax-equivalence.R::expect_plot_equivalent()`, which
compares `ggplot2::ggplot_build()` of jamovi's render against the eval'd syntax
(layer data + axis breaks). Add an equivalence assertion for any new plot
behavior, not just a snapshot.

## Testing

`vdiffr` snapshot tests call the public function (e.g. `scatr::jmvbar(...)`) and
compare against SVGs in `tests/testthat/_snaps/`; run `vdiffr::manage_cases()`
for new/changed plots. Syntax-mode behavior is covered by the equivalence helper
above rather than snapshots.
