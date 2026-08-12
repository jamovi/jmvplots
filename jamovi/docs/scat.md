# Scatter Plot

## Overview

Draws one point per case, positioned by two continuous variables. It is the
standard way to look at the relationship between two quantities — its shape,
its strength, and anything that departs from it.

A scatter plot shows things no correlation coefficient can: curvature, distinct
clusters, and the single outlying point that is producing the correlation you
were about to report.

## When to use it

Use it when both variables are continuous and you want to see how they relate.

- If one variable is categorical, use **Box Plot** to compare distributions
  across its levels, or **Bar Plot** to compare summaries.
- If the x-axis is time or an ordered sequence, use **Line Plot**.
- For a single variable's distribution, use **Histogram**.

## Options

### Variables

**X-Axis** and **Y-Axis** take the two continuous variables. By convention the
x-axis carries the predictor or the variable you consider prior, and the y-axis
the outcome — the plot itself makes no such distinction.

**Grouping Variable** colours the points by a categorical variable, and fits a
separate line per group if a line is shown.

### General Options

**Point size** sets how large the markers are. Reduce it when points overlap
heavily; with a few hundred cases in a small plot area, the default can hide
the density of the middle.

**Show line** adds a fitted line, with **Method** choosing how it is fitted:

| Method | What it fits |
|---|---|
| **lm** | A straight line, by least squares |
| **loess** | A smooth curve, fitted locally |

Use **lm** when you intend to report a linear relationship, and **loess** to
check whether that assumption is reasonable. A loess curve that bends
substantially is telling you a straight line is the wrong summary — which is
the most useful thing this plot can tell you before you run a regression.

**Confidence interval** shades a band around the fitted line. It shows the
uncertainty in the *line*, not the spread of the points, so it stays narrow in
a large sample however scattered the data are.

These two methods are the ones offered here; for any other fit,
[the generated R code](topics/syntax-mode.md) is the way to it.

**Flip axes** swaps the two axes.

### Plot & Axis Titles, Axes, Legend

Shared across every plot in this module — see
[titles, axes and legends](topics/plot-appearance.md).

## Results

The plot.

Read it in this order: the overall shape first, then the strength, then the
exceptions. Curvature or distinct clusters matter more than the tightness of
the cloud, because both mean a single correlation would misdescribe the
relationship.

Points far from the rest deserve attention before anything is fitted. A single
extreme point can create or destroy an apparent relationship, and the plot is
where you find it — no summary statistic will show you.

Overlapping points are worth watching for. Where many cases share similar
values the markers coincide, and a dense region can look identical to a sparse
one. Reducing **Point size** is the quickest check.

## References

- Cleveland, W. S. (1979). Robust locally weighted regression and smoothing
  scatterplots. *Journal of the American Statistical Association, 74*(368),
  829–836.
