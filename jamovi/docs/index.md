# Plots bundled with jamovi

Six plots for looking at data, available from the **Plots** menu rather than
the Analyses menu. They produce a figure and nothing else — no tables, no
tests.

They are a convenience layer over the ggplot2 package: a set of controls for
the plots people draw most often, rather than a general drawing tool. Where the
controls run out, [the R code for any plot](topics/syntax-mode.md) is one
setting away, and it will do anything ggplot2 can.

## Which plot?

The choice follows from what your variables are.

| You have | Use |
|---|---|
| One continuous variable | **Histogram** |
| One categorical variable | **Bar Plot**, or **Pareto Plot** to rank the categories |
| Two continuous variables | **Scatter Plot** |
| Continuous by group | **Box Plot** for spread, **Bar Plot** for summaries |
| An ordered x-axis | **Line Plot** |

Two distinctions cause most of the wrong choices:

- **Histogram or bar plot?** A histogram takes a continuous variable and
  divides it into bins you choose; a bar plot takes categories the data already
  has. The touching bars of a histogram mean the axis is continuous.
- **Bar plot or box plot?** A bar shows one number per group, a box shows the
  whole distribution. Two groups with the same mean and very different spreads
  give identical bars and obviously different boxes.

**Scatter Plot** and **Pareto Plot** also appear under Exploration in the
Analyses menu, which is the same plot reached by a different route.

## Ideas that run through the module

- [Titles, axes and legends](topics/plot-appearance.md) — the appearance
  options every plot shares, and the axis choices that can mislead.
- [Getting the R code for a plot](topics/syntax-mode.md) — how to take any plot
  here into ggplot2 and keep going.

## Before you plot

Nothing here reshapes your data. Each plot expects your variables to be the
right *type* — continuous variables measured as continuous, categorical ones as
nominal or ordinal — and jamovi's variable setup is where that is fixed, not
the plot options.

A variable typed as text when it holds numbers is the usual cause of a plot
that comes out looking nothing like it should.

## Reporting a problem

Errors in a plot, and errors in these documents, go to the issue tracker at
<https://github.com/jamovi/jmvplots/issues>.
