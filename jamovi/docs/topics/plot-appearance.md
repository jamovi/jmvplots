# Titles, axes and legends

Every plot in this module carries the same three groups of appearance options —
**Plot & Axis Titles**, **Axes** and **Legend**. They work identically
everywhere, so they are described once here rather than in each plot's
document.

None of them change what the plot shows. They change how it reads, which
matters most when the plot is going into a report or a slide rather than being
looked at once and discarded.

## Plot & Axis Titles

Five pieces of text, each with the same four controls:

| Text | Where it appears |
|---|---|
| Title | Above the plot |
| Subtitle | Below the title, in the same block |
| Caption | Beneath the plot, right-aligned by default |
| X-axis title | Below the horizontal axis |
| Y-axis title | Beside the vertical axis |

For each, **Title text** sets the words, and **Font size**, **Font face** and
**Align** set the appearance. Leave the text empty and the axis titles fall
back to the variable names, which is usually what you want while exploring and
almost never what you want in a report — a column called `q3_recoded` means
nothing to a reader.

The caption is the natural home for the things a reader needs but the plot
cannot show: the sample size, the source of the data, what was excluded.

## Axes

**Label font size** and **Label rotation** apply to the tick labels — the
category names or numbers along each axis, not the axis titles above.

Rotation is the fix for the most common layout problem in this module.
Categorical labels that overlap because they are long or numerous become
readable at 45 degrees, and reliably readable at 90. The alternative is
**Flip axes** under General Options, which turns a vertical bar chart into a
horizontal one and gives every label a full line of its own. For more than
about eight categories, flipping usually beats rotating.

**Y-Axis Range** and **X-Axis Range** default to **auto**, which fits the range
to the data. Setting them manually is worth doing in two situations: when you
are putting several plots side by side and need them on the same scale, and
when the automatic range hides something you want visible.

It also carries a trap. A bar chart whose y-axis does not start at zero
**exaggerates the differences between bars**, because the bar's length no
longer represents its value. This is the single most common way a plot
misleads, and it is easy to do by accident when setting a range by hand. Line
plots and scatter plots have no such constraint — their marks encode position,
not length, so a truncated axis is legitimate and often clearer.

## Legend

A legend appears when a grouping variable is set, and not otherwise.

**Title text**, **Title font size**, **Title font face**, **Label font size**
and **Label font face** control the text. **Key width** and **Key height** size
the coloured swatches.

Placement has two modes. **Outside** puts the legend beside the plot, with
**Position** choosing which side and **Justification** where along that side it
sits. **Inside** places it within the plotting area at coordinates you give as
**X-position** and **Y-position**, each running from 0 to 1 across the plot.
Inside placement uses the space better and risks covering data; it works best
where a corner of the plot is empty.

**Direction** sets whether keys stack vertically or run horizontally. A
horizontal legend above or below the plot is often the tidiest arrangement when
there are only two or three groups.

The legend can also be hidden entirely. Consider doing so when the groups are
already labelled in the title or caption, or when there is only one group,
where the legend takes space to tell the reader nothing.

## A note on defaults

The defaults are chosen to produce a reasonable plot with nothing set, and most
of these options can be left alone for exploratory work.

The two worth setting almost every time a plot leaves jamovi are the **axis
titles**, so the reader is not looking at variable names, and the **legend
title**, for the same reason.
