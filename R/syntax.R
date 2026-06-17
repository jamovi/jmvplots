#' Format a variable name as a safe R symbol
#'
#' @param name The column name
#' @return A character string formatted as a symbol
#' @keywords internal
asSymbol <- function(name) {
    if (is.null(name) || name == "") {
        return(NULL)
    }
    # Check if it is a valid syntactic name in R
    if (
        make.names(name) == name &&
            !name %in%
                c(
                    "if",
                    "else",
                    "repeat",
                    "while",
                    "function",
                    "for",
                    "in",
                    "next",
                    "break",
                    "TRUE",
                    "FALSE",
                    "NULL",
                    "Inf",
                    "NaN",
                    "NA"
                )
    ) {
        return(name)
    } else {
        return(paste0("`", name, "`"))
    }
}

#' Quote a string as a single-quoted R literal, escaping as needed
#'
#' @param s The character value
#' @return A character string of the quoted R literal
#' @keywords internal
quoteString <- function(s) {
    encodeString(s, quote = "'")
}

#' Format an R object/value into its code representation
#'
#' @param val The R object
#' @return A character string of the R code
#' @keywords internal
formatValueForCode <- function(val) {
    if (is.null(val)) {
        return("NULL")
    }
    if (is.list(val) && !is.null(val$fun_name) && !is.null(val$args)) {
        fun_name <- val$fun_name

        args_formatted <- c()
        arg_names <- names(val$args)
        for (i in seq_along(val$args)) {
            arg_name <- if (is.null(arg_names)) "" else arg_names[i]
            v <- val$args[[i]]
            v_str <- formatValueForCode(v)
            if (!is.null(v_str)) {
                if (is.na(arg_name) || arg_name == "") {
                    args_formatted <- c(args_formatted, v_str)
                } else {
                    args_formatted <- c(args_formatted, sprintf("%s = %s", arg_name, v_str))
                }
            }
        }
        return(sprintf("%s(%s)", fun_name, paste(args_formatted, collapse = ", ")))
    }
    if (inherits(val, "uneval")) {
        # uneval is a ggplot2 aes mapping list
        mappings <- c()
        for (name in names(val)) {
            expr_val <- val[[name]]
            expr_str <- paste(deparse(expr_val), collapse = "")
            if (startsWith(expr_str, "~")) {
                expr_str <- substring(expr_str, 2)
            }
            mappings <- c(mappings, sprintf("%s = %s", name, expr_str))
        }
        return(sprintf("aes(%s)", paste(mappings, collapse = ", ")))
    }
    if (inherits(val, "unit")) {
        unit_str <- as.character(val)
        pos <- regexpr("[a-zA-Z]", unit_str)
        if (pos > 0) {
            num_part <- substring(unit_str, 1, pos - 1)
            unit_part <- substring(unit_str, pos)
            return(sprintf("unit(%s, '%s')", num_part, unit_part))
        }
        return(sprintf("unit(%s, 'cm')", as.numeric(val)))
    }
    if (inherits(val, "element_text")) {
        args_list <- list()
        if (!is.null(val$size)) args_list$size <- val$size
        if (!is.null(val$hjust)) args_list$hjust <- val$hjust
        if (!is.null(val$face)) args_list$face <- quoteString(val$face)
        if (!is.null(val$angle)) args_list$angle <- val$angle

        args_str <- paste(names(args_list), args_list, sep = " = ", collapse = ", ")
        return(sprintf("element_text(%s)", args_str))
    }
    # PositionDodge2 must be checked before PositionDodge: a position_dodge2()
    # object inherits from both, and the more specific class carries padding.
    if (inherits(val, "PositionDodge2")) {
        preserve_val <- if (is.null(val$preserve)) "single" else val$preserve
        padding_val <- if (is.null(val$padding)) 0.1 else val$padding
        return(sprintf(
            "position_dodge2(width = %s, preserve = '%s', padding = %s)",
            val$width,
            preserve_val,
            padding_val
        ))
    }
    if (inherits(val, "PositionDodge")) {
        return(sprintf("position_dodge(width = %s)", val$width))
    }
    if (inherits(val, "PositionStack")) {
        vjust_val <- if (is.null(val$vjust)) 1 else val$vjust
        return(sprintf("position_stack(vjust = %s)", vjust_val))
    }
    if (is.logical(val)) {
        return(if (val) "TRUE" else "FALSE")
    }
    if (is.numeric(val)) {
        if (length(val) > 1) {
            return(sprintf("c(%s)", paste(val, collapse = ", ")))
        }
        return(as.character(val))
    }
    if (is.character(val)) {
        if (length(val) > 1) {
            return(sprintf("c(%s)", paste(quoteString(val), collapse = ", ")))
        }
        return(quoteString(val))
    }
    if (is.function(val)) {
        if (identical(val, base::rev)) {
            return("rev")
        }
        return(deparse(substitute(val)))
    }

    return(paste(deparse(val), collapse = " "))
}

#' Generate tidyverse data preparation code
#'
#' @param options The analysis options object
#' @param plotType The type of plot ("bar", "box", "hist", "line", "pareto", "scat")
#' @return A character string of R code
#' @keywords internal
generateDataPrepCode <- function(options, plotType = "bar") {
    if (plotType == "bar") {
        mode <- options$mode
        if (mode == "categorical") {
            var <- asSymbol(options$catvar)
            group <- asSymbol(options$catgroup)

            if (is.null(group)) {
                code <- sprintf(
                    "data |>\n  mutate(%s = factor(%s)) |>\n  count(%s, .drop = FALSE) |>\n  rename(y = n, x = %s)",
                    var,
                    var,
                    var,
                    var
                )
            } else {
                code <- sprintf(
                    "data |>\n  mutate(%s = factor(%s), %s = factor(%s)) |>\n  group_by(%s) |>\n  count(%s, .drop = FALSE) |>\n  rename(y = n, x = %s, group = %s)",
                    var,
                    var,
                    group,
                    group,
                    group,
                    var,
                    var,
                    group
                )
            }
            if (options$naOmit) {
                if (is.null(group)) {
                    code <- paste0(code, " |>\n  filter(!is.na(x))")
                } else {
                    code <- paste0(code, " |>\n  filter(!is.na(x) & !is.na(group))")
                }
            }
        } else if (mode == "continuous") {
            var <- asSymbol(options$convar)
            group1 <- asSymbol(options$congroup1)
            group2 <- asSymbol(options$congroup2)

            if (is.null(group1) && is.null(group2)) {
                code <- sprintf(
                    "data |>\n  mutate(%s = jmvcore::toNumeric(%s)) |>\n  rename(y_full = %s) |>\n  summarize(\n    y = mean(y_full, na.rm = TRUE),\n    n = sum(!is.na(y_full)),\n    sd = sd(y_full, na.rm = TRUE)\n  ) |>\n  mutate(\n    se = sd / sqrt(n),\n    ci = se * qt((%s / 100) / 2 + .5, n - 1),\n    x = '1'\n  )",
                    var,
                    var,
                    var,
                    options$ciWidth
                )
            } else if (is.null(group2)) {
                code <- sprintf(
                    "data |>\n  mutate(%s = jmvcore::toNumeric(%s), %s = factor(%s)) |>\n  group_by(%s) |>\n  rename(y_full = %s, x = %s) |>\n  summarize(\n    y = mean(y_full, na.rm = TRUE),\n    n = sum(!is.na(y_full)),\n    sd = sd(y_full, na.rm = TRUE)\n  ) |>\n  mutate(\n    se = sd / sqrt(n),\n    ci = se * qt((%s / 100) / 2 + .5, n - 1)\n  ) |>\n  ungroup()",
                    var,
                    var,
                    group1,
                    group1,
                    group1,
                    var,
                    group1,
                    options$ciWidth
                )
            } else {
                code <- sprintf(
                    "data |>\n  mutate(%s = jmvcore::toNumeric(%s), %s = factor(%s), %s = factor(%s)) |>\n  group_by(%s, %s) |>\n  rename(y_full = %s, x = %s, group = %s) |>\n  summarize(\n    y = mean(y_full, na.rm = TRUE),\n    n = sum(!is.na(y_full)),\n    sd = sd(y_full, na.rm = TRUE)\n  ) |>\n  mutate(\n    se = sd / sqrt(n),\n    ci = se * qt((%s / 100) / 2 + .5, n - 1)\n  ) |>\n  ungroup()",
                    var,
                    var,
                    group1,
                    group1,
                    group2,
                    group2,
                    group1,
                    group2,
                    var,
                    group1,
                    group2,
                    options$ciWidth
                )
            }

            if (options$naOmit) {
                if (is.null(group1) && is.null(group2)) {
                    code <- paste0(code, " |>\n  filter(!is.na(x) & !is.na(y))")
                } else if (is.null(group2)) {
                    code <- paste0(code, " |>\n  filter(!is.na(x) & !is.na(y))")
                } else {
                    code <- paste0(code, " |>\n  filter(!is.na(x) & !is.na(y) & !is.na(group))")
                }
            }
        } else if (mode == "counts") {
            var <- asSymbol(options$counts)
            labels <- asSymbol(options$countsLabels)
            group <- asSymbol(options$countsgroup)

            if (is.null(group)) {
                if (is.null(labels)) {
                    code <- sprintf(
                        "data |>\n  mutate(.row_id = row_number()) |>\n  select(.row_id, %s) |>\n  rename(y = %s, x = .row_id) |>\n  mutate(y = jmvcore::toNumeric(y), x = factor(x))",
                        var,
                        var
                    )
                } else {
                    code <- sprintf(
                        "data |>\n  mutate(.row_id = row_number()) |>\n  select(.row_id, %s, %s) |>\n  rename(y = %s, label_text = %s, x = .row_id) |>\n  mutate(y = jmvcore::toNumeric(y), x = factor(x))",
                        var,
                        labels,
                        var,
                        labels
                    )
                }
            } else {
                if (is.null(labels)) {
                    code <- sprintf(
                        "data |>\n  mutate(.row_id = 1:n()) |>\n  mutate(x_category_num = ceiling(.row_id / nlevels(factor(%s)))) |>\n  select(x_category_num, group_category = %s, y_value = %s) |>\n  mutate(x = factor(x_category_num), group = factor(group_category), y = jmvcore::toNumeric(y_value)) |>\n  select(x, y, group) |>\n  ungroup()",
                        group,
                        group,
                        var
                    )
                } else {
                    code <- sprintf(
                        "data |>\n  select(x_category = %s, group_category = %s, y_value = %s) |>\n  mutate(x = factor(x_category), group = factor(group_category), y = jmvcore::toNumeric(y_value)) |>\n  select(x, y, group) |>\n  ungroup()",
                        labels,
                        group,
                        var
                    )
                }
            }
            if (options$naOmit) {
                if (is.null(group)) {
                    code <- paste0(code, " |>\n  filter(!is.na(x) & !is.na(y))")
                } else {
                    code <- paste0(code, " |>\n  filter(!is.na(x) & !is.na(y) & !is.na(group))")
                }
            }
        }
    } else if (plotType == "box") {
        var <- asSymbol(options$var)
        group1 <- asSymbol(options$group1)
        group2 <- asSymbol(options$group2)

        if (is.null(group1) && is.null(group2)) {
            code <- sprintf(
                "data |>\n  select(y = %s) |>\n  mutate(x = '1', y = jmvcore::toNumeric(y))",
                var
            )
        } else if (is.null(group1) || is.null(group2)) {
            group <- if (is.null(group1)) group2 else group1
            code <- sprintf(
                "data |>\n  select(x = %s, y = %s) |>\n  mutate(y = jmvcore::toNumeric(y), x = factor(x))",
                group,
                var
            )
        } else {
            code <- sprintf(
                "data |>\n  select(x = %s, y = %s, group = %s) |>\n  mutate(y = jmvcore::toNumeric(y), x = factor(x), group = factor(group))",
                group1,
                var,
                group2
            )
        }

        if (options$naOmit) {
            if (is.null(group1) && is.null(group2)) {
                code <- paste0(code, " |>\n  filter(!is.na(x) & !is.na(y))")
            } else if (is.null(group1) || is.null(group2)) {
                code <- paste0(code, " |>\n  filter(!is.na(x) & !is.na(y))")
            } else {
                code <- paste0(code, " |>\n  filter(!is.na(x) & !is.na(y) & !is.na(group))")
            }
        }
    } else if (plotType == "hist") {
        var <- asSymbol(options$var)
        group <- asSymbol(options$group)

        if (is.null(group)) {
            code <- sprintf(
                "data |>\n  select(y = %s) |>\n  mutate(y = jmvcore::toNumeric(y))",
                var
            )
        } else {
            code <- sprintf(
                "data |>\n  select(y = %s, group = %s) |>\n  mutate(y = jmvcore::toNumeric(y), group = factor(group))",
                var,
                group
            )
        }
    } else if (plotType == "line") {
        var_x <- asSymbol(options$x)
        var_y <- asSymbol(options$y)
        group <- asSymbol(options$group)

        mode <- options$mode
        if (mode == "individual") {
            if (is.null(group)) {
                code <- sprintf(
                    "data |>\n  select(x = %s, y = %s) |>\n  mutate(y = jmvcore::toNumeric(y))",
                    var_x,
                    var_y
                )
            } else {
                code <- sprintf(
                    "data |>\n  select(x = %s, y = %s, group = %s) |>\n  mutate(group = factor(group), y = jmvcore::toNumeric(y))",
                    var_x,
                    var_y,
                    group
                )
            }
        } else {
            agg_type <- if (options$aggregateType == "median") "median" else "mean"
            if (is.null(group)) {
                code <- sprintf(
                    "data |>\n  select(x = %s, y_orig = %s) |>\n  mutate(y_orig = jmvcore::toNumeric(y_orig)) |>\n  group_by(x) |>\n  summarize(\n    y = %s(y_orig, na.rm = TRUE),\n    n = sum(!is.na(y_orig)),\n    sd = sd(y_orig, na.rm = TRUE)\n  ) |>\n  mutate(\n    se = sd / sqrt(n),\n    ci = se * qt((%s / 100) / 2 + .5, n - 1)\n  ) |>\n  ungroup()",
                    var_x,
                    var_y,
                    agg_type,
                    options$ciWidth
                )
            } else {
                code <- sprintf(
                    "data |>\n  select(x = %s, y_orig = %s, group = %s) |>\n  mutate(y_orig = jmvcore::toNumeric(y_orig), group = factor(group)) |>\n  group_by(x, group) |>\n  summarize(\n    y = %s(y_orig, na.rm = TRUE),\n    n = sum(!is.na(y_orig)),\n    sd = sd(y_orig, na.rm = TRUE)\n  ) |>\n  mutate(\n    se = sd / sqrt(n),\n    ci = se * qt((%s / 100) / 2 + .5, n - 1)\n  ) |>\n  ungroup() |>\n  select(group, x, y, se, sd, ci)",
                    var_x,
                    var_y,
                    group,
                    agg_type,
                    options$ciWidth
                )
            }
        }

        if (options$naOmit) {
            if (is.null(group)) {
                code <- paste0(code, " |>\n  filter(!is.na(x) & !is.na(y))")
            } else {
                code <- paste0(code, " |>\n  filter(!is.na(x) & !is.na(y) & !is.na(group))")
            }
        }
    } else if (plotType == "pareto") {
        x <- options$x
        counts <- options$counts

        if (!is.null(counts)) {
            code <- sprintf(
                "data.frame(x = factor(data[[ %s ]]), counts = jmvcore::toNumeric(data[[ %s ]])) |>\n  xtabs(counts ~ x, data = _) |>\n  as.data.frame()",
                deparse(x),
                deparse(counts)
            )
        } else {
            code <- sprintf("table(factor(data[[ %s ]])) |>\n  as.data.frame()", deparse(x))
        }

        code <- paste0(
            code,
            " |>\n  `names<-`(c(\"x\", \"counts\")) |>\n  arrange(desc(counts)) |>\n  mutate(\n    x = factor(x, levels = x),\n    cum = cumsum(counts)\n  )"
        )
    } else if (plotType == "scat") {
        var_x <- asSymbol(options$x)
        var_y <- asSymbol(options$y)
        group <- asSymbol(options$group)

        if (is.null(group)) {
            code <- sprintf(
                "data |>\n  select(x = %s, y = %s) |>\n  mutate(x = jmvcore::toNumeric(x), y = jmvcore::toNumeric(y))",
                var_x,
                var_y
            )
        } else {
            code <- sprintf(
                "data |>\n  select(x = %s, y = %s, group = %s) |>\n  mutate(x = jmvcore::toNumeric(x), y = jmvcore::toNumeric(y), group = factor(group))",
                var_x,
                var_y,
                group
            )
        }
    }
    return(code)
}

#' Generate full ggplot2 pipeline syntax code
#'
#' @param call_list The list of ggplot layer call specs
#' @param data_prep_code The data prep pipeline code
#' @param theme_name The theme name
#' @param palette_name The palette name
#' @importFrom R6 R6Class
#' @importFrom dplyr mutate
#' @return A character string of R code
#' @keywords internal
generatePlotCode <- function(
    call_list,
    data_prep_code,
    theme_name = "default",
    palette_name = "jmv"
) {
    if (is.null(theme_name) || theme_name == "") {
        theme_name <- "default"
    }
    if (is.null(palette_name) || palette_name == "") {
        palette_name <- "jmv"
    }

    code_parts <- c()
    for (name in names(call_list)) {
        call_spec <- call_list[[name]]
        args <- call_spec$args
        if (is.null(args) && is.list(call_spec) && length(call_spec) >= 2) {
            args <- call_spec[[2]]
        }

        # Filter NULL and empty arguments
        args <- args[sapply(args, function(x) !is.null(x))]

        if (name == "labs") {
            # filter empty title/labels in labs to keep it clean
            args <- args[sapply(args, function(x) !is.null(x) && x != "")]
        }

        if (startsWith(name, "coord_") && name != "coord_flip") {
            # skip coordinate transformations with no explicit limits
            if (length(args) == 0) {
                next
            }
        }

        # Skip empty layers unless they are ggplot or geom_bar or coord_flip (which could have defaults)
        if (length(args) == 0 && !name %in% c("ggplot", "geom_bar", "coord_flip")) {
            next
        }

        if (name == "ggplot") {
            if (!"data" %in% names(args)) {
                args <- c(list(data = "plot_data"), args)
            }
        }

        formatted_args <- c()
        for (arg_name in names(args)) {
            val <- args[[arg_name]]

            if (arg_name == "data") {
                formatted_args <- c(formatted_args, "data = plot_data")
            } else {
                formatted_val <- formatValueForCode(val)
                if (!is.null(formatted_val)) {
                    if (arg_name == "") {
                        formatted_args <- c(formatted_args, formatted_val)
                    } else {
                        formatted_args <- c(
                            formatted_args,
                            sprintf("%s = %s", arg_name, formatted_val)
                        )
                    }
                }
            }
        }

        indent <- if (length(code_parts) == 0) "" else "    "

        # Determine if we should wrap arguments onto separate lines
        wrap_args <- FALSE
        if (name == "theme") {
            # theme is always wrapped customly
            theme_args_formatted <- c()
            for (arg_name in names(args)) {
                val <- args[[arg_name]]
                formatted_val <- formatValueForCode(val)
                theme_args_formatted <- c(
                    theme_args_formatted,
                    sprintf("        %s = %s", arg_name, formatted_val)
                )
            }
            args_str <- paste0("\n", paste(theme_args_formatted, collapse = ",\n"), "\n    ")
        } else {
            if (length(formatted_args) > 1) {
                total_len <- nchar(name) +
                    sum(nchar(formatted_args)) +
                    2 * (length(formatted_args) - 1) +
                    nchar(indent)
                # Keep ggplot on a single line unless it is extremely long (e.g. > 100 chars)
                limit <- if (name == "ggplot") 100 else 70
                if ((name != "ggplot" && length(formatted_args) > 2) || total_len > limit) {
                    wrap_args <- TRUE
                }
            }

            if (wrap_args) {
                arg_indent <- "        "
                args_str <- paste0(
                    "\n",
                    paste(sprintf("%s%s", arg_indent, formatted_args), collapse = ",\n"),
                    "\n",
                    "    "
                )
            } else {
                args_str <- paste(formatted_args, collapse = ", ")
            }
        }

        code_parts <- c(code_parts, sprintf("%s%s(%s)", indent, name, args_str))
    }

    # Resolve the theme function name
    theme_fun <- "jmvcore::theme_default"
    if (theme_name == "hadley") {
        theme_fun <- "jmvcore::theme_hadley"
    } else if (theme_name == "minimal") {
        theme_fun <- "jmvcore::theme_min"
    } else if (theme_name == "iheartspss") {
        theme_fun <- "jmvcore::theme_spss"
    }

    theme_call <- sprintf("    %s(palette = '%s', scale = 'discrete')", theme_fun, palette_name)
    code_parts <- c(code_parts, theme_call)

    ggplot_pipeline <- paste(code_parts, collapse = " +\n")

    full_code <- paste0(
        "# Load libraries\n",
        "library(dplyr)\n",
        "library(ggplot2)\n\n",
        "# Prepare data\n",
        "plot_data <- ",
        data_prep_code,
        "\n\n",
        "# Generate plot\n",
        "p <- ",
        ggplot_pipeline,
        "\n\n",
        "# Print plot\n",
        "print(p)\n"
    )

    return(full_code)
}

#' Default theme colors used when generating standalone syntax
#'
#' Mirrors `jmvcore::getTheme()` so generated code bakes in the same single
#' colors jamovi injects at render time (geoms use `color[1]` and `fill[2]`).
#'
#' @param themeName The theme name (e.g. "default", "iheartspss")
#' @param palette The palette name passed to the jamovi color palette
#' @return A list with `color` and `fill` color vectors
#' @keywords internal
getSyntaxThemeColors <- function(themeName = "default", palette = "jmv") {
    if (is.null(themeName) || themeName == "") {
        themeName <- "default"
    }
    if (is.null(palette) || palette == "") {
        palette <- "jmv"
    }

    if (themeName == "iheartspss") {
        list(
            color = c("#333333", "#333333"),
            fill = c("#F0F0F0", "#d3ce97")
        )
    } else {
        list(
            color = c("#333333", jmvcore::colorPalette(1, palette, "color")),
            fill = c("#FFFFFF", jmvcore::colorPalette(1, palette, "fill"))
        )
    }
}

#' Attach theme arguments to a call list and generate the full syntax
#'
#' Shared by every analysis' `asSource()`: builds the `theme()` arguments
#' (legend styling is only added when the plot has a legend) and renders the
#' complete ggplot2 pipeline code.
#'
#' @param options The analysis options object
#' @param call_list The plot call list from `.getPlotCallList()`
#' @param data_prep_code The data prep pipeline code
#' @param hasLegend Whether the plot has a legend (adds legend theme args)
#' @param flipAxes Whether the axes are flipped
#' @param legend Whether legend label theme args should be included
#' @param continuousX Whether the x axis is continuous (adds x break scaling)
#' @return A character string of R code
#' @keywords internal
finalizePlotSyntax <- function(
    options,
    call_list,
    data_prep_code,
    hasLegend,
    flipAxes = FALSE,
    legend = TRUE,
    continuousX = FALSE
) {
    theme_call_list_args <- list()
    if (hasLegend) {
        theme_call_list_args <- utils::modifyList(
            theme_call_list_args,
            getLegendThemeCallArgs(options)
        )
    }
    theme_call_list_args <- utils::modifyList(
        theme_call_list_args,
        getLabelsThemeCallArgs(options, flipAxes, legend = legend)
    )

    # Mirror autoscalePlotBreaks(): at the 500px default plot size it targets
    # floor(500 / 80) = 6 breaks on the (always continuous) y axis, and
    # floor(500 / 120) = 4 on a continuous x axis. Merge into an existing scale
    # (e.g. pareto's sec.axis y scale) rather than overwriting it.
    yBreaks <- list(fun_name = "scales::breaks_pretty", args = list(n = 6))
    if (is.null(call_list$scale_y_continuous)) {
        call_list$scale_y_continuous <- list(
            fun = ggplot2::scale_y_continuous,
            args = list(breaks = yBreaks)
        )
    } else {
        call_list$scale_y_continuous$args$breaks <- yBreaks
    }

    if (continuousX) {
        xBreaks <- list(fun_name = "scales::breaks_pretty", args = list(n = 4))
        if (is.null(call_list$scale_x_continuous)) {
            call_list$scale_x_continuous <- list(
                fun = ggplot2::scale_x_continuous,
                args = list(breaks = xBreaks)
            )
        } else {
            call_list$scale_x_continuous$args$breaks <- xBreaks
        }
    }

    if (length(theme_call_list_args) > 0) {
        call_list$theme <- list(
            fun = ggplot2::theme,
            args = theme_call_list_args
        )
    }

    generatePlotCode(call_list, data_prep_code, options$theme, options$palette)
}
