# Bar Plot

## Overview

Draws a bar per category. What the bar's height represents depends on which of
three input modes you choose: how many cases fall in the category, a summary of
a continuous variable within it, or a number you supply directly.

Choosing the mode that matches the data you have is the whole of using this
plot; everything else is presentation.

## When to use it

Use it to compare a quantity across categories.

- For a continuous variable's distribution, use **Histogram**. A histogram's
  bars touch because its axis is continuous; a bar plot's do not, because its
  categories are separate.
- To compare whole distributions across groups rather than a single summary per
  group, use **Box Plot** — it shows the spread that a bar hides.
- Where the categories have an order and you want to see a trend across them,
  use **Line Plot**.
- To rank categories by frequency and see cumulative share, use
  **Pareto Plot**.

## Options

### Variable type

The mode, and the first thing to set. It determines which variable boxes appear
above it.

| Mode | Bars show | You supply |
|---|---|---|
| **Categorical** | How many cases per category | One categorical variable |
| **Continuous** | A summary per category | A continuous variable and one or two grouping variables |
| **Counts** | The numbers you give | A counts variable and its labels |

**Categorical** is the common case: hand it a variable and it counts. Use
**Counts** where the counting is already done — a summary table typed into the
spreadsheet, one row per category.

### Continuous Options

Shown in continuous mode only.

**Error bars** add a measure of uncertainty or spread to each bar:

| Option | What the bar spans |
|---|---|
| **None** | No error bars |
| **SD** | One standard deviation — the spread of the data |
| **SE** | One standard error — the precision of the mean |
| **CI** | A confidence interval at the **Width** you set |

These answer different questions and are often confused. SD describes how much
the individual cases vary; SE and CI describe how well the mean is pinned down,
and shrink as the sample grows. Report which you used — a figure showing bars
with error bars and no statement of what they represent cannot be interpreted.

**Error bar width** and **Error bar line size** control their appearance.

### General Options

**Width** sets bar thickness. **Value labels** prints each bar's value on it,
which is worth doing when exact numbers matter and unnecessary when the
comparison is the point.

**Flip axes** turns the bars horizontal — the better choice when category names
are long or numerous.

**Exclude cases listwise** drops any case missing a value on any variable used.

### Group Options

With a grouping variable, **Bar type** chooses between **grouped** bars, drawn
side by side, and **stacked** bars, drawn on top of one another.

Grouped bars compare the groups within each category. Stacked bars show the
total per category and its composition, at the cost of making all but the
bottom segment hard to compare, because those segments do not share a baseline.
Choose grouped when comparison matters, stacked when the total does.

### Plot & Axis Titles, Axes, Legend

Shared across every plot in this module — see
[titles, axes and legends](topics/plot-appearance.md).

## Results

The plot.

Bars encode value as length, which is why **the y-axis must start at zero**.
Truncating it makes a small difference look large, and it is the most common
way a bar chart misleads. This is a real constraint on the axis range options —
see [titles, axes and legends](topics/plot-appearance.md).

A bar shows one number per category and nothing about the spread behind it.
Two groups with identical means and completely different distributions produce
identical bars, which is why error bars are worth adding whenever the bars
represent means, and why a **Box Plot** is often the more honest choice.

Where bars represent counts, check what happened to the empty categories. A
category with no cases has no bar, which reads as absent rather than as zero.
