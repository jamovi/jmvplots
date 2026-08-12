# Line Plot

## Overview

Connects points with lines to show how one variable changes across another.
The connection is the point: a line asserts that the values between the points
are meaningful, which makes it right for ordered data and wrong for unordered
categories.

It draws either every case individually or one summary per x-value, depending
on the mode you choose.

## When to use it

Use it when the x-axis has a natural order — time, dose, trial number, an
ordered scale — and you want to see how something changes across it.

- Where the categories have no order, use **Bar Plot**. Connecting unordered
  categories implies a progression that does not exist.
- To relate two continuous variables with no ordering to respect, use
  **Scatter Plot**.
- To compare distributions rather than trends, use **Box Plot**.

## Options

### Data display

The mode. **Individual** draws the data as it comes, one line per case or
group. **Aggregate** summarises the y-variable at each x-value first, drawing
one line through the summaries.

Aggregate is what you want for most grouped data: with many cases per x-value,
individual lines become an unreadable tangle, and the summary is the trend you
were looking for.

### Variables

**X-Axis** carries the ordered variable, **Y-Axis** the one being tracked, and
**Grouping Variable** draws a separate line per group.

### General Options

**Show lines** and **Show points** toggle the two elements independently, with
**Line size** and **Point size** setting their weight. Points mark where data
actually exists; without them a line implies measurements at places you never
measured. Keep them on unless the x-values are dense.

**Aggregate** chooses **mean** or **median** as the summary, in aggregate mode.
Median is the more robust choice where the distribution at each x-value is
skewed or has extreme cases.

**Error bars** add spread or uncertainty at each point — **SD**, **SE**, or
**CI** at the **Width** you set — with **Error bar width** and **Error bar line
size** controlling their appearance. As with the bar plot, SD describes the
spread of the data while SE and CI describe the precision of the summary, and
which you used needs stating.

**Flip axes** swaps the axes. **Exclude cases listwise** drops any case missing
a value on any variable used.

### Group Options

**Dodge width** offsets the groups horizontally so their points and error bars
do not sit on top of one another. A small dodge is usually enough, and is worth
setting whenever error bars overlap.

**Different colors**, **Different line types** and **Different point types**
each distinguish the groups by one visual channel, and can be combined. Using
two channels rather than one is the fix for a figure that must survive being
printed in black and white, or being read by someone who cannot distinguish the
colours.

### Plot & Axis Titles, Axes, Legend

Shared across every plot in this module — see
[titles, axes and legends](topics/plot-appearance.md).

## Results

The plot.

Read the direction and shape of each line first, then whether the lines are
parallel. Non-parallel lines are an interaction — the effect of the x-variable
differs by group — and this plot is the clearest way to see one.

Unlike a bar chart, a line plot's y-axis need not start at zero, because
position rather than length carries the meaning. This makes truncating the axis
legitimate, and makes the axis range worth checking on any line plot you did
not draw yourself, since the same data can be made to look flat or dramatic.

Where error bars are shown, overlapping bars do not straightforwardly mean "no
difference" — that inference depends on which measure you plotted and how the
groups are related.
