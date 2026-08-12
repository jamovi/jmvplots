# Box Plot

## Overview

Summarises a continuous variable's distribution as a box and whiskers: the box
spans the middle half of the data, the line inside it is the median, and the
whiskers reach out towards the extremes.

Its strength is comparison. One box tells you less than a histogram would;
several boxes side by side compare distributions across groups more clearly
than any other plot here.

## When to use it

Use it to compare a continuous variable across the levels of one or two
grouping variables.

- To look at a single distribution in detail — whether it is bimodal, where the
  gaps are — use **Histogram**. A box plot hides both.
- To compare group *means* rather than whole distributions, use **Bar Plot**
  with error bars.
- With two continuous variables, use **Scatter Plot**.

## Options

### Variables

**Variable** takes the continuous variable to summarise. **Grouping Variable
1** draws one box per level along the axis, and **Grouping Variable 2** splits
each of those further, producing side-by-side boxes within each group.

With no grouping variable you get a single box, which is rarely worth the
space.

### General Options

**Width** sets how wide the boxes are drawn — presentation only.

**Show outliers** draws points beyond the whiskers individually. Turn it off
only when there are so many that they obscure the boxes, and say so if you do,
since a box plot without outliers looks tidier than the data is.

**Notch** cuts a waist into each box spanning roughly a 95% interval for the
median. Where two boxes' notches do not overlap, their medians differ at about
that level of confidence, which makes the plot a rough visual test. In a small
group the notch can extend past the box, which looks odd but simply means the
median is poorly determined.

**Flip axes** turns the boxes horizontal, which is the better orientation when
the group names are long.

**Exclude cases listwise** drops any case missing a value on any variable used,
rather than dropping it only from the box it belongs to.

### Plot & Axis Titles, Axes, Legend

Shared across every plot in this module — see
[titles, axes and legends](topics/plot-appearance.md).

## Results

The plot.

The box spans the interquartile range — the middle 50% of cases — and the line
inside it is the median, not the mean. The whiskers extend to the most extreme
case within 1.5 times the interquartile range beyond the box, and anything past
that is drawn as an individual point.

That 1.5 is a convention, not a test. A point beyond the whisker is not
necessarily an error or an outlier in any meaningful sense; in a large sample,
several such points are expected even when nothing is wrong.

Compare medians first, then spread — a group whose box is much taller than the
others differs in a way the medians will not show. Asymmetry between the two
halves of a box, or between the two whiskers, indicates skew.

A box plot cannot show whether a distribution has two peaks. Two distinct
clusters and one broad spread produce the same box, which is the main reason to
look at a **Histogram** as well before drawing conclusions.

## References

- Tukey, J. W. (1977). *Exploratory Data Analysis*. Addison-Wesley.
- McGill, R., Tukey, J. W., & Larsen, W. A. (1978). Variations of box plots.
  *The American Statistician, 32*(1), 12–16.
