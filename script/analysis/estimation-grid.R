
library(data.table)
library(fixest)
library(ggplot2)

hedonic_20 = fread("data/processed/final_indexes_grid.csv")[rank_pop <= 20, ]
# Estimation
## Gupta et.al (2021) ----
hedonic_20[, ave_cases := mean(cases, na.rm = TRUE), .(var, ags)]
setkey(hedonic_20, grid_id, var, time)
hedonic_20[, Dt := factor(format(time, "%m-%Y"))][, lndist := log(1 + dist_grid_cbd)][, lnnads := log(1 + nads)][, lnpop := log(1 + inhabitants)][, lnpp := log(1 + purchase_power)][, lncases := log(1 + cases)][, lnave_cases := log(1 + ave_cases)]

main_vars = c(
  "Dt", "lndist", "lnpop", "lnpp", "lncases", "share_background_german",
  "share_male_1845", "ags"
)
fml = main_vars |>
  paste(collapse = "+") |>
  paste("~ var") |>
  as.formula()

mod = feols(
  c(lnhri, lnhpi) ~ 0 + Dt:lndist + lnpop + lnpp + lncases +
    share_background_german + share_male_1845 | Dt + as.factor(ags),
  dcast(hedonic_20, fml, value.var = "val"),
  cluster = "ags", combine.quick = FALSE
)



# get the bid-rent coefficients
bidrent = lapply(mod, \(m) m$coeftable[, "Estimate", drop = FALSE]) |>
  lapply(\(x) data.frame(
    time = sub("Dt(\\d{2}-\\d{4})\\:.+", "\\1", rownames(x)),
    estimate = x[, 1]
  )) |>
  setNames(names(mod)) |>
  rbindlist(use.names = TRUE, idcol = "var") |>
  subset(time %like% "^\\d\\d-\\d{4}$") |>
  transform(time = as.Date(paste0("01-", time), "%d-%m-%Y"))

cis = lapply(mod, \(m) confint(m)) |>
  lapply(\(.x) {
    data.frame(my = sub("Dt(\\d{2}-\\d{4})\\:lndist", "\\1", rownames(.x)), .x)
  }) |>
  lapply(setNames, nm = c("time", "ll", "ul")) |>
  setNames(names(mod)) |>
  rbindlist(use.names = TRUE, idcol = "var") |>
  subset(time %like% "^\\d\\d-\\d{4}$") |>
  transform(time = as.Date(paste0("01-", time), "%d-%m-%Y"))

bidrent = merge(bidrent, cis, c("time", "var"))

## plot
bidrent[var == "lnhri" & time > "2018-12-01", ] |>
  ggplot(aes(time, estimate)) +
  geom_vline(xintercept = ref_time, lty = 2, color = "darkred") +
  geom_line(color = "darkblue") +
  geom_point() +
  geom_errorbar(aes(ymin = ll, ymax = ul)) +
  scale_x_date(date_breaks = "3 month", date_labels = "%m-%y") +
  labs(
    x = "time",
    y = expression(Estimate ~ (widehat(delta)) ~ and ~ "95%" ~ CI)
  ) +
  cowplot::theme_cowplot(14) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

fwrite(bidrent, "output/bidrent_grid.csv")

## Bloom & Ramani (2020)

df = hedonic_20[time == ref_time, .(grid_id, ags, var, lndist, lnpop, lnpp)]
df = hedonic_20[time %in% times,
                ][order(grid_id, var, time),
                  ][,.(time, val, dx = val-shift(val)),
                    .(grid_id, var)
                  ] |>
  merge(df, c('grid_id', 'var')) |>
  dcast(... ~ var, value.var = "dx")

setnames(df, c('lnhpi', 'lnhri'), c('price_pct_change', 'rent_pct_change'))


m1 <- lm(rent_pct_change ~ lnpop + factor(ags), df)
m2 <- lm(rent_pct_change ~ lndist + factor(ags), df)
m3 <- lm(rent_pct_change ~ lnpp + factor(ags), df)
m4 <- lm(rent_pct_change ~ lnpop + lndist + lnpp + factor(ags), df)


m5 <- lm(price_pct_change ~ lnpop + factor(ags), df)
m6 <- lm(price_pct_change ~ lndist + factor(ags), df)
m7 <- lm(price_pct_change ~ lnpp + factor(ags), df)
m8 <- lm(price_pct_change ~ lnpop + lndist + lnpp + factor(ags), df)


## Create function to get robust standard errors
rse <- function(reg) {
  return(as.vector(summary(reg, robust = TRUE)$coefficients[, "Std. Error"]))
}

mods = list(m1, m2, m3, m4, m5, m6, m7,m8)
names(mods) = rep(paste0(c('rent', 'price'), "_pct_change"), each=4L)

stargazer::stargazer(mods,
  se = lapply(mods, rse),
  dep.var.labels = c("$\\Delta \\ln R$","$\\Delta \\ln P$"),
  dep.var.caption = NULL,
  title = sprintf("Change in index %s to %s", fmy(times[1],"%b %Y"), fmy(times[2],"%b %Y")),
  no.space = TRUE,
  column.sep.width = "2pt",
  omit = "ags",
  omit.stat = c("adj.rsq", "ser", "f"),
  font.size = "tiny",
  header = FALSE
)
