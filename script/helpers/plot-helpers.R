# plotting helper functions ----

gold = (1 + 5**0.5) / 2 # the golden ratio

# custom theme minimal
custom_theme_minimal <- function(base_size = 11, base_family = "", ...) {
  theme_minimal(base_size = base_size, base_family = base_family, ...) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray85", linetype = "dotted"),
      panel.background = element_rect(fill = "white", color = "gray90"),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_text(face = "bold", size = rel(1.7), hjust = 0.5),
      plot.subtitle = element_text(size = rel(1.1)),
      plot.background = element_rect(fill = "white", color = NA),
      strip.background = element_blank(),
      # text = element_text(color = "gray20"),
      # axis.text = element_text(color = "gray30"),
      axis.line = element_line(color = "gray25"),
      strip.text = element_text(face = "bold")
    )
}


# saver with some defaults, aspect ratio = golden ratio
my_ggsave = function(filename, plot = last_plot(), width = 6, height = width / ((1 + 5**0.5) / 2), units = "in", ...) {
  message(sprintf("Saving %g x %g [%s] ...\n%s", width, height, units, filename))
  ggplot2::ggsave(filename, plot, width = width, height = height, units = units, ...)
}


get_label <- function(x, math = FALSE) {
  switch(x,
    lnp = if (math) expression(italic(ln) ~ P) else "ln P",
    lnr = if (math) expression(italic(ln) ~ R) else "ln R",
    lnhpi = if (math) expression(italic(ln) ~ P) else "ln P",
    lnhri = if (math) expression(italic(ln) ~ R) else "ln R",
    x
  )
}


# maintain the same num bins for the log scale of the var as in level with
# a given bin width in the level of the var, 2km bins
binwidth_logscale = function(x, binwidth_level = 2) {
  rng = range(x, na.rm = TRUE)
  binwidth_level * diff(rng) / diff(exp(rng))
}


label_cutoff <- function(x, unit = NULL, scale = 1) {
  if (length(x) < 2L) stop("Expecting len(x) >= 2!", call. = FALSE)
  if (missing(unit) && missing(scale)) {
    unit = units::deparse_unit(x)
  } else if (missing(unit) && !missing(scale)) {
    stop("Can't supply `scale` if `unit` isn't supplied.", call. = FALSE)
  } else if (!missing(unit) && missing(scale)) {
    if (units::deparse_unit(x) != unit) {
      warning("Scale not provided, but a different unit than the object's is provied", call. = FALSE)
    }
  } else if (!missing(unit) && !missing(scale)) {
    if (unit == units::deparse_unit(x)) {
      warning("Scale provided, but the same unit is provided.", call. = FALSE)
    }
  }
  x = unclass(x * scale)
  l = sapply(head(seq_along(x), -1L), \(i) sprintf("%s-%s%s", x[i], x[i + 1], unit))
  l
}


# fitted line for geom_smooth
fitted_text <- function(mod, digits = 2) {
  coefs = coef(mod)
  se = summary(mod)$coefficients[2, 2]
  text = sprintf(
    paste0(
      "italic(y) == %.", digits,
      "g %+.", digits,
      'g * italic(x) * "," ~~ italic(se) == %.', digits,
      "g"
    ),
    coefs[1], coefs[2], se
  )
  text
}


# legend position helper
# rescale a number to be in [0, 1]
trans <- function(x, r) {
  if (r < 0 || r > 1) stop("r should be in [0,1]", call. = FALSE)
  min(x, na.rm = TRUE) + (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * r
}


smooth_vars <- function(mod) {
  vars = all.vars(mod$call$formula)
  df = data.frame(y = predict(mod), x = mod$model[[vars[[2]]]])
  structure(df, varnames = vars)
}


stamp_ref_time = function(v, ref_time) {
  sprintf("%s (%s = 1)", v, format(ref_time, "%b %Y"))
}


# make a ts (line) plot with a ref line on a ref date
# x takes a date variable
make_tplot = function(data, x, y, ..., ref_time) {
  g = ggplot(data, aes(x = {{ x }}, y = {{ y }}))
  y_lab = as.character(substitute(y))

  if (!is.null(ref_time)) {
    g = g + geom_vline(xintercept = ref_time, color = "#c70002", lty = 2L)
    y_lab = stamp_ref_time(y_lab, ref_time)
  }

  g + geom_line(...) +
    scale_x_date(date_labels = "%b-%y", date_breaks = "4 months") +
    labs(
      x = "time",
      y = y_lab
    )
}
