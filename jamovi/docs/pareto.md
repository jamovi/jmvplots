# Pareto Plot

## Overview

A bar plot with the categories sorted from most to least frequent, and a line
tracing the running total as a percentage. It answers one question: how few
categories account for most of the cases?

The sorting and the cumulative line are what distinguish it from an ordinary
bar plot. Together they turn a list of frequencies into a ranking with a
natural stopping point.

## When to use it

Use it when you have many categories and want to find the few that matter —
which faults account for most failures, which reasons for most complaints.

- If the categories have a meaningful order of their own, use **Bar Plot**.
  Sorting by frequency destroys that order.
- With only a few categories, a plain **Bar Plot** is clearer; the cumulative
  line earns its space only when there are enough categories for the ranking to
  be informative.
- To see the shape of a continuous variable, use **Histogram**.

## Options

### Variables

**X-Axis** takes the categorical variable, and its categories are counted and
sorted for you.

**Counts (optional)** takes a frequency variable, for data already summarised
as one row per category. Leave it empty and the cases are counted directly.

### General Options

**Width** sets bar thickness.

This plot has fewer options than the others here — there is no grouping
variable and so no legend, and the sort order is not adjustable, because
sorting by frequency is what makes it a Pareto plot. If you need something the
options do not offer, [the generated R code](topics/syntax-mode.md) will take
you further.

### Plot & Axis Titles, Axes

Shared across every plot in this module — see
[titles, axes and legends](topics/plot-appearance.md). The legend options
described there do not apply, since this plot has no groups.

## Results

The plot.

Bars are sorted from most to least frequent, and read against the left axis as
counts. The line reads against the right axis, **Cumulative Percentage**,
running from the height of the first bar up to 100% at the last.

Read the line rather than the bars. The point where it flattens is where
additional categories stop contributing much, and everything to the left of
that is the set worth acting on.

The "80/20" rule that gives the plot its name — that roughly 80% of effects
come from 20% of causes — is an observation about some data, not a property of
all of it. Read the percentage your own line reaches; do not assume it.

Two cautions. Categories with few cases pile up at the right and their labels
often overlap, which **Flip axes** or a label rotation will fix. And a category
that is rare but severe ranks low here, because this plot knows only how often
something occurs, not how much it matters.

## References

- Juran, J. M. (1951). *Quality Control Handbook*. McGraw-Hill.
