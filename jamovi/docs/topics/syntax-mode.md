# Getting the R code for a plot

Every plot here can show you the R code that produces it. Turn on **Syntax
mode** in jamovi's settings — the ⋮ menu at the top right — and each plot's
results panel gains the code needed to reproduce it outside jamovi.

This is the way past the options panel. These plots deliberately expose a
limited set of controls; the generated code has no such limit, and it gives you
a working ggplot2 script rather than a blank page to start from.

## What the code contains

Three parts, in order:

- **Data preparation** — a dplyr pipeline that reshapes your raw columns into
  the form the plot expects. Counting, aggregating and dropping missing values
  all happen here, which is also the clearest available answer to what the plot
  did with your data before drawing it.
- **The plot** — the ggplot2 call, with the same geoms, scales and coordinate
  settings the rendered plot used.
- **The theme** — the jamovi theme and colour palette you have selected,
  written out explicitly so the plot looks the same outside jamovi as inside.

The code reproduces the rendered plot rather than approximating it; the two are
checked against each other in the module's test suite.

## What to do with it

- **Customise** — add an annotation, a facet, a second geom, a different scale.
  Anything ggplot2 can do is now one edit away.
- **Reproduce** — the script runs on its own, so a figure can be regenerated
  from data without reopening jamovi.
- **Learn ggplot2** — read the code for a plot you already understand. It is a
  worked example whose output you have in front of you.

## If nothing appears

The code is only generated once the plot's required variables are set. A
scatter plot with no Y-axis variable produces nothing, because there is no plot
to describe yet. Fill in the variables and the code appears.
