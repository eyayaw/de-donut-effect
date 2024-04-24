library(data.table)
keep_since = 2016

# for making English names
var_dict = read.csv("data/variable-and-value_labels/variables-metadata_manually.csv")
# for removing variables that do not apply to the respective data set
all_nas_patt = c(
  "Variable trifft auf diesen Datensatzyp nicht zu|Variable for other types only",
  "Variable nicht mehr in Einagbemaske vorhanden|Old variable \\(no longer part of the platform\\)"
)
all_nas_num = c(-8, -10) # for ohneText data

# import the joined dataset directly ----

## purchases of homes ----

files = list(
  `home-purchases` = "HK_SUF",
  `home-rents` = "HM_SUF",
  `apartment-purchases` = "WK_SUF",
  `apartment-rents` = "WM_SUF"
)
for (h in seq_along(files)) {
property = haven::read_dta(sprintf("../Common-Data/.RED_v6.1/Stata/%s_ohneText.dta", files[[h]]))
gc()
setDT(property)
property = property[ejahr >= keep_since, ]

# translate names
vars_selected = names(property)
var_names_en = with(var_dict, var_en[match(vars_selected, var_de)])
var_names_en = replace(var_names_en,
is.na(var_names_en), vars_selected[is.na(var_names_en)]
)
setnames(property, vars_selected, var_names_en)

# drop variables that do not apply to purchases of homes
# variables that should be dropped
drops = property |>
  {\(d) names(d)[vapply(
    d[sample.int(nrow(d), 100), ],
    \(x) all(grepl(paste(all_nas_num, collapse = "|"), x)), NA
  )]
}()

property = property[, !(drops), with = FALSE]
gc()

# choose the year and month vars
property[, year := ad_end_year][, mon := ad_end_mon]

### write to disk ----
prefix = names(files)[[h]]
suffix = paste(range(property$year), collapse = "-")
fwrite(property, sprintf("data/%s_%s.csv", prefix, suffix), nThread = getDTthreads() - 1L)
message("`", prefix, "_", suffix, "` has been written.")
rm(property)
}
