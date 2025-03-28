# jmvplot

This is a plotting module for jamovi. It's a convenience wrapper around some of the `ggplot2` 
functionality, and is designed to make it easier to create customized plots in jamovi.

At the moment, this module contains the following plot types:

- Bar plot
- Box plot
- Histogram
- Scatter plot
- Line plot

## Install the module in R

As with all jamovi modules, this can also be installed as an R package (although I recommend just
using `ggplot2` directly if you're working in R).

```R
remotes::install_github('jamovi/jmvplot')
```
