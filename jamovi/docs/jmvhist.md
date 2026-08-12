# Histogram

## Overview

Divides a continuous variable's range into intervals and draws a bar for how
many cases fall in each. It is the most direct picture of a distribution's
shape — where the mass sits, whether it is symmetric, whether it has one peak
or several.

## When to use it

Use it to see the shape of one continuous variable.

- To compare a variable across groups, use **Box Plot**. Several overlapping
  histograms are harder to read than several boxes.
- To relate two continuous variables, use **Scatter Plot**.
- For a categorical variable, use **Bar Plot**. The distinction matters: a
  histogram's bars touch because the axis is continuous, and its bins are a
  choice you make rather than categories the data comes with.

## Options

### Variables

**Variable** takes the continuous variable. **Grouping Variable** overlays one
histogram per group, which works for two groups and becomes unreadable beyond
about three — use **Box Plot** instead at that point.

### General Options

**Show bins** draws the bars themselves, with **Bin Width** either **auto** or
**manual** at a width you set, and **Bin opacity** controlling transparency.
Reduce opacity when groups overlap so the hidden one remains visible.

Bin width is **the most consequential option here**. Too wide and real
structure disappears into a few blocks; too narrow and random variation looks
like structure. The automatic width is a reasonable starting point, but it is
worth trying two or three widths before believing any feature you see — a
second peak that survives a change of bin width is worth taking seriously, and
one that does not is probably noise.

**Show line** traces the tops of the bins. **Show density** overlays a smooth
density curve, with **Density opacity** and **Line size** controlling its
appearance. The density curve makes shape easier to see and comparison across
groups easier still, at the cost of a smoothing choice you cannot see.

**Flip axes** turns the histogram on its side.

### Plot & Axis Titles, Axes, Legend

Shared across every plot in this module — see
[titles, axes and legends](topics/plot-appearance.md).

## Results

The plot.

Read it for four things: where the bulk of the data sits, whether the two sides
are symmetric, how many peaks there are, and whether anything sits away from
the rest.

A long tail on one side is skew, and it is the feature most likely to matter
for what you do next — most analyses assume symmetry of a kind, and a
noticeably skewed variable is worth knowing about before rather than after.

Two peaks usually mean two populations mixed together. If you can identify what
distinguishes them, a **Box Plot** split by that variable will show it more
clearly than the histogram did.

Gaps and isolated bars at the extremes are worth checking against the data.
They are as often a coding problem — an unrecoded missing value, a
misplaced decimal — as a genuine extreme case.
