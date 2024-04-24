library(data.table)

# translate names
translate_names = function(df) {
  vars_selected = names(df)
  var_names_en = with(var_dict, var_en[match(vars_selected, var_de)])
  var_names_en = replace(
    var_names_en,
    is.na(var_names_en), vars_selected[is.na(var_names_en)]
  )
  setnames(df, vars_selected, var_names_en)
}


tidy_data <- function(file) {
  df = haven::read_stata(file)
  # variables that are all NAs, check in a sample of rows
  drop = names(df)[vapply(
    df[sample.int(nrow(df), 50), ],
    \(x) all(grepl(paste(all_nas_num, collapse = "|"), x)), NA
  )]
  df = df[, (setdiff(names(df), drop))]

  setDT(df)
  # translate vars
  df = translate_names(df)
  # choose the year and month vars
  # df[, `:=`(year = ad_end_year, mon = ad_end_mon)]
  setnames(df, c("ad_end_year", "ad_end_mon"), c("year", "mon"))
  # keep since year
  df = df[df$year >= keep_since, ]
  df
}

# helper to write a list of dfs in parts, each part binded into one big df
write_parts <- function(ls, parse_part = FALSE, ...) {
  # creates a huge df, R may crash on you
  df = rbindlist(ls, use.names = TRUE, fill = TRUE, ...)

  ### write to disk ----
  if (parse_part) {
    part_id = paste0("_part-", sub("(.+_ohneText)(\\d+)$", "\\2", names(ls)) |> paste0(collapse = "-"))
  } else {
    part_id = ""
  }

  prefix = names(dlist)[[h]]
  suffix = paste(range(df$year), collapse = "-")
  file_path = sprintf("data/%s_%s%s.csv", prefix, suffix, part_id)
  fwrite(df, file_path, nThread = getDTthreads() - 1L)
  message(sprintf("`%s` has been saved.", file_path))
  rm(df)
  gc()
}


# params ----
keep_since = 2007
RAW_DATA_DIR = "../Common-Data/.RED_v6.1/Stata/ohneText"
# for making English names
var_dict = read.csv("data/variable-and-value_labels/variables-metadata_manually.csv")
# for removing variables that do not apply to the respective data set
all_nas_patt = c(
  "Variable trifft auf diesen Datensatzyp nicht zu|Variable for other types only",
  "Variable nicht mehr in Einagbemaske vorhanden|Old variable \\(no longer part of the platform\\)"
)
all_nas_num = c(-8, -10) # for ohneText data


# cleaning ----
# import dta files under each directory, remove some vars that are irrelevant, and
# bind data sets across years, translate the vars into English, and write to CSV file

dlist = list(
  `home-purchases` = "HK_SUF",
  `home-rents` = "HM_SUF",
  `apartment-purchases` = "WK_SUF",
  `apartment-rents` = "WM_SUF"
)

for (h in seq_along(dlist)) {
  file_list = dir(
    path = file.path(RAW_DATA_DIR, paste0(dlist[[h]], "_ohneText")),
    pattern = paste0(dlist[[h]], "_ohneText([0-9]+)?[.]dta$"),
    full.names = TRUE
  )
  if (length(file_list) == 0) stop("No list of files found.", call. = FALSE)
  # sorting is necessary because dir() puts HKSUF10 second instead of 10th
  file_list = file_list[order(as.integer(regmatches(
    basename(file_list),
    regexpr("[0-9]+", basename(file_list))
  )))]
  names(file_list) = tools::file_path_sans_ext(basename(file_list))
  list_property_dfs = lapply(file_list, tidy_data)


  # drop vars that are not available across data sets
  common_vars_selected = Reduce(intersect, lapply(list_property_dfs, names))
  all_vars = lapply(list_property_dfs, names) |> Reduce(f = union)
  prob_vars = setdiff(all_vars, common_vars_selected)
  list_property_dfs = lapply(
    list_property_dfs, \(x) x[, (intersect(names(x), common_vars_selected)), with = FALSE]
  )

  # wm_suf has a large number of observations, list_property_dfs[1] has more than 13 million
  if (FALSE) {#dlist[[h]] == "WM_SUF"
    list(1, 2:length(list_property_dfs)) |>
      lapply(\(i) write_parts(list_property_dfs[i], TRUE))
  } else {
    write_parts(list_property_dfs)
  }
}

print("Done.")


# # combining into one for wm_suf parts
# file_list = dir("data/", sprintf("%s_\\d{4}-\\d{4}_part-*", 'apartment-rents'), full.names = T)
# dfs = lapply(file_list, fread)
# dfs = rbindlist(dfs, use.names = T, fill = T)
#
# syear = min(dfs$year)
# eyear = max(dfs$year)
# file_path = paste0(sub("^(.+_)(.*_part.*[.]csv)$", "\\1", file_list[[1]]), syear, "-", eyear, ".csv")
# fwrite(dfs, file_path)
