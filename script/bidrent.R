library(data.table)
library(fixest)
library(ggplot2)

source("./script/helpers/helpers.R")
source("./script/helpers/plot-helpers.R")
theme_set(custom_theme_minimal(12)) # default theme

source("./script/constants.R")

# import data ----
hedonic = fread("./data/processed/final_indexes.csv", keepLeadingZeros = TRUE)

# cleaning ----
hedonic = hedonic[year >= DATA_YEAR_START, ]
hedonic[, time := as.Date(time)] # time index

# pick default distance
setnames(hedonic, "dist_inhab", "dist")
hedonic = hedonic[dist <= DIST_MAX_METERS, ]

# qkm, einwoher from https://www.suche-postleitzahl.org/downloads
hedonic[, `:=`(
  lndist = log(1 / 1000 + dist / 1000),
  # zip code level controls
  lnpop_dens = log(inhabitants / area_qkm),
  lnpop = log(1 + inhabitants),
  lnpp = log(1 + tot_purchase_power),
  #lnpp = log(1 + purchase_power),
  #lnpp = log(tot_purchase_power / inhabitants),
  lncases = log(1 + cases),
  # factors
  ftime = factor(time, unique(time), format(unique(time), "%b %Y")),
  amr_name = as.factor(amr_name)
)]


# estimation ----

## helpers ----

# helper to get time:lndist coefficients
# and constructs confidence intervals on them
tidy_coef_table = function(mod) {
  stopifnot(inherits(mod, "fixest"))
  pattern = "(f?time)((?:(?:\\d{1,2})|(?:[A-Z][a-z]{2})[ -])\\d{4})\\:(lndist)"
  coeffs = mod$coeftable[, c("Estimate", "Std. Error", "Pr(>|t|)")]
  coeffs = data.frame(
    time = rownames(coeffs),
    setNames(unclass(coeffs), c("estimate", "std.error", "p.value"))
  )
  # keeps the coefficients for the vars of interest
  keep_important = function(df) {
    matches = grepl(pattern, df$time, perl = TRUE)
    if (!any(matches)) {
      stop("The pattern does not match anything. Check the name of '(f)time' variable and its pattern. Expecting '%b %Y' format.", call. = FALSE)
    }
    df[matches, ]
  }
  coeffs = keep_important(coeffs) |>
    transform(time = sub(pattern, "\\2", time))

  # get the confidence intervals
  cis = confint(mod)
  cis = data.frame(
    time = rownames(cis), ll = cis[["2.5 %"]], ul = cis[["97.5 %"]]
  )
  cis = keep_important(cis) |>
    transform(time = sub(pattern, "\\2", time))

  # adds cis to the coeffs
  coeffs = merge(coeffs, cis, "time")
  coeffs$time = as.Date(paste("01", coeffs$time), "%d %b %Y")
  # adds the dep var name, the char repr of the fml
  coeffs$dep_var = all.vars(formula(mod))[[1]]
  coeffs$fml = deparse1(formula(mod))
  # put the dep var in the first column
  coeffs = coeffs[, c(c("dep_var", "fml"), setdiff(names(coeffs), c("dep_var", "fml")))]
  coeffs
}


# plot the bid rent curve with errorbars
plot_bidrent = function(data, ...) {
  data |>
    transform(time = as.Date(time)) |>
    ggplot(aes(time, estimate)) +
    geom_hline(yintercept = 0, lty = 2, linewidth = 0.1) +
    geom_vline(xintercept = REF_TIME, lty = 2) +
    geom_errorbar(aes(ymin = ll, ymax = ul), color = COLS[[1]]) +
    geom_line(color = COLS[[2]]) +
    geom_point(size = 1.25, color = COLS[[2]], fill = NA, ...) +
    # geom_smooth(method = "lm", se = FALSE, color = COLS[[3]]) +
    scale_x_date(date_breaks = "6 month", date_labels = "%b %Y") +
    labs(y = (Estimate ~ (widehat(delta[t])) ~ and ~ "95%" ~ CI)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))
}


make_fml = function(dep_vars = NULL, x_vars = NULL, controls = NULL, fe_vars = NULL, env = parent.frame()) {
  # `fixest` is capable of handling multiple estimations.
  ## For several dependent variables on the lhs, use the syntax: `c(y1, y2) ~ rhs`.
  ## For multiple regressors on the rhs, you can use any of the stepwise helpers: `sw*`, `csw*`, `mvsw`.

  # defaults
  if (is.null(dep_vars)) {
    dep_vars = c("val") # or HED_VARS
  }
  if (is.null(x_vars)) {
    x_vars = "ftime:lndist"
  }
  if (is.null(controls)) {
    controls = c("lnpp", "share_male_1845", "share_background_german")
  }
  if (is.null(fe_vars)) {
    fe_vars = c("amr_name", "ftime")
  }

  # prepare for multiple estimation on the lhs
  dep_vars = if (length(dep_vars) > 1) {
    sprintf("c(%s)", paste(dep_vars, collapse = ","))
  }

  rhs = paste(c(x_vars, controls), collapse = "+")

  fml = sprintf("%s ~ %s | %s", dep_vars, rhs, paste(fe_vars, collapse = "+"))
  as.formula(fml, env = env)
}


stepwiser = function(swf = NULL, ...) {
  vars = unlist(list(...))
  if (is.null(swf)) {
    return(vars)
  }
  swf = match.arg(swf, c("sw", "sw0", "csw", "csw0", "mvsw"))
  sprintf("%s(%s)", swf, paste0(vars, collapse = ", "))
}


# estimate the slope of the bid rent curve every month
estimate_slope = function(data, fml, combine.quick = F, mem.clean = T, ...) {
  message("Running <", deparse(fml), ">")
  mods = feols(fml, data = data, combine.quick = combine.quick, mem.clean = mem.clean, ...)
  mods
}

model_eval = function(mods) {
  slope_estimates = if (inherits(mods, "fixest_multi")) {
    lapply(mods, tidy_coef_table) |> rbindlist(use.names = TRUE)
  } else if (inherits(mods, "fixest")) {
    tidy_coef_table(mods) |> as.data.table()
  } # pass the output of `tidy_coef_table` directly
  else if (inherits(mods, "data.frame")) {
    mods
  }

  slope_estimates[, .(
    delta = estimate[time == TIMES[[2]]] - estimate[time == TIMES[[1]]],
    nneg = sum(estimate < 0),
    nsignf = sum(p.value <= 0.05)
  ), .(dep_var, fml)]
}


## run estimation ----
# dep vars wide: val* + var
dep_vars_long = c("val", "val_MA", "val_norm", "val_MA_norm")
dep_vars = paste0(
  rep(dep_vars_long, TIMES = length(HED_VARS)), "_",
  rep(HED_VARS, each = length(dep_vars_long))
)
x_vars = "ftime:lndist"
controls = c("lnpp", "share_male_1845", "share_background_german")
# controls = c(stepwiser("sw", "lnpop", "lnpop_dens"), controls)
fe_vars = c("amr_name", "ftime")
fml = make_fml(dep_vars, x_vars, controls, fe_vars)

hedonic |>
  dcast(... ~ var, value.var = dep_vars_long) |>
  estimate_slope(fml) -> mods

# tidy the slope estimates
bidrent = lapply(mods, tidy_coef_table) |> rbindlist(use.names = TRUE)
evals = model_eval(bidrent)

# plot
selected_deps = paste0("val_MA_", HED_VARS)
bidrent[dep_var %in% selected_deps, !"fml"] |>
  transform(dep_var = sub("val_MA_", "", dep_var)) -> bidrent_selected

for (dep.var in HED_VARS) {
  bidrent_selected[dep_var == (dep.var), ] |>
    plot_bidrent() +
    labs(x = NULL) +
    theme(axis.text.y = element_text(angle = 90))

  my_ggsave(
    sprintf("./output/figs/bidrent-curve_slope-estimates_%s.png", dep.var),
    width = 6.5, height = 4
  )
}

# as a facet plot
bidrent_selected |>
  plot_bidrent() +
  facet_wrap(
    ~dep_var,
    scales = "free_y",
    labeller = labeller(dep_var = c(lnhpi = "Prices", lnhri = "Rents"))
  ) +
  labs(x = NULL) +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(angle = 90)
  ) -> errorbar_plot_facet

my_ggsave(
  "./output/figs/bidrent-curve_slope-estimates.png", errorbar_plot_facet,
  width = 13, height = 4
)

# exports
fwrite(bidrent_selected, "./output/tabs/bidrent-curve_slope-estimates.csv")

mods[match(paste0("lhs: ", selected_deps), names(mods))] |>
  etable(
    file = "./output/tabs/bidrent-curve_slope-estimates.tex",
    order = "%dist",
    #group = list("Controls: " = paste0("%", controls)),
    drop = c("201[78].+dist", paste0("%", controls)),
    dict = setNames(getFixest_dict()[HED_VARS], paste0("val_MA_", HED_VARS)),
    style.tex = style.tex("aer"),
    notes = "\\textit{Notes}: We control for the share of males aged 18-45, the share of German background, and the logarithm of total purchasing power at the zip code level. The estimates for 2017 and 2018 are not shown to fit the table. The standard errors are clustered at the level of the LMR.",
    tpt = TRUE,
    float = TRUE,
    placement = "",
    replace = TRUE,
    fontsize = "scriptsize",
    se.below = FALSE,
    postprocess.tex = \(x) {
      x = gsub("ftime", "", x)
      sub(r"(\\caption\{.*\})", "", x)
    }
  )
