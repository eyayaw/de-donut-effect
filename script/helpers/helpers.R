# It’s useful as a way of providing a default value in case the output of
# another function is NULL:
# http://adv-r.had.co.nz/Functions.html#special-calls
`%||%` = function(lhs, rhs) if (!is.null(lhs)) lhs else rhs

# safely compare two numeric (floating) vectors, taken from dplyr::near
near = function(x, y, tol = .Machine$double.eps^0.5) {
  abs(x - y) < tol
}

# a practical cut
my_cut = function(x, ..., include.lowest = TRUE, right = FALSE) {
  cut(x, ..., include.lowest = include.lowest, right = right)
}

# taken from adv-r 1st ed http://adv-r.had.co.nz/Functions.html#return-values
in_dir = function(dir, code) {
  old = setwd(dir)
  on.exit(setwd(old))
  force(code)
}


# round function factory, useless?
rnd = function(digits) {
  force(digits)
  function(x) round(x, digits = digits)
}

# always round 0.5 up, taken from the internet
.round = function(x, digits = 0) {
  posneg = sign(x)
  z = abs(x) * 10^digits
  z = z + 0.5
  z = trunc(z)
  z = z / 10^digits
  z * posneg
}


# get the stem of a file (basename of a file without the ext)
# e.g. data/foo.csv -> foo
file.stem = function(path) {
  tools::file_path_sans_ext(basename(path))
}

# Non-overwriting download operation
download_file = function(url, destfile, ...) {
  if (!file.exists(destfile)) {
    download.file(url, destfile, ...)
  } else {
    warning("File already exists. Download operation skipped.", call. = FALSE)
  }
}


# base select, for use in pipeOp
bselect = `[.data.frame`

# makes tidy names
make_names = function(names) {
  names = gsub("[ ]{2,}", " ", names) # rm two or more white spaces anywhere
  patt = "[[:punct:] ]+"
  names = gsub(patt, "_", trimws(tolower(names)))
  names = gsub(patt, "_", names)
  gsub("(^_+)|(_+$)", "", names)
}

# grep returns value
grepv = function(pattern, x, ...) {
  grep(pattern = pattern, x = x, value = TRUE, ...)
}


# miscellaneous helpers

is_outlier = function(x, cutoff = 4, na.rm = TRUE, ...) {
  m = mean(x, na.rm = na.rm, ...)
  s = sd(x, na.rm = na.rm)
  ll = m - cutoff * s
  ul = m + cutoff * s
  message(sprintf("Outliers lie outside [ll, ul]: [%.3f, %.3f]", ll, ul))
  (x <= ll) | (x >= ul)
}


# append 01 to mon-year -> date
mydate = function(mon, year) {
  as.Date(paste0("01-", paste0(mon, "-", year)), "%d-%m-%Y")
}


mdate = function(x) {
  as.Date(paste0("01-", x), "%d-%m-%Y")
}


fmy = function(x, format = "%b-%y") format.Date(x, format = format)


meter = function(x) units::set_units(x, "m")


km = function(x) units::set_units(x, "km")


# tidy fixed effects from fixest::fixef()
tidy_fixeff = function(fe_obj) {
  stopifnot(inherits(fe_obj, "fixest.fixef") && names(fe_obj) != "")
  fe = data.frame(id = names(fe_obj[[1]]), eff = fe_obj[[1]])
  fixeffs = strsplit(names(fe_obj), "^", fixed = TRUE)[[1]]
  .data = do.call("rbind", strsplit(fe$id, "_"))
  if (length(fixeffs) != ncol(.data)) {
    warning("Some of the fixed effects may include a `_`, split has caused more names than expected.\n The returned data may need futher tidying.", call. = FALSE)
    return(setNames(fe, c(paste0(fixeffs, collapse = "^"), "eff")))
  }
  fe[, fixeffs] = .data
  fe = fe[, c(fixeffs, "eff")]
  rownames(fe) = NULL
  fe
}


# STL decomposition
decomp = function(
    x,
    do = c("deseason", "detrend", "remainder", "trend", "seasonal"),
    start, end, freq = 12) {
  # start = c(2018L, 1L), end = c(2021L, 12L)
  if (nrow(x) < 2L) {
    message("Atleast 2 obs needed! No operation")
    return(x)
  }
  stopifnot(all(c("year", "mon", "val") %in% names(x)))
  if (!missing(start) || !missing(end)) {
    stopifnot(length(start) == length(end) && length(start) == 2L)
  }

  if (missing(start) || missing(end)) {
    start = c(min(x$year), min(x$mon))
    end = c(max(x$year), max(x$mon))
    message(sprintf("Auto generated start[y, m]: [%i, %i]", start[1], start[2]))
    message(sprintf("Auto generated end[y, m]: [%i, %i]", end[1], end[2]))
  }
  do = match.arg(do)
  message("STL Decompostion: <", do, "-ing>")
  # x = x[order(x$year, x$mon),] # tidyr::completes does ordering automatically
  xc = tidyr::complete(x, tidyr::expand(x, year = (start[1]:end[1]), mon = (start[2]:end[2])))
  # tidyr::complete may put some year-mons that are not specified at the end
  xc = xc[order(xc$year, xc$mon), ]
  xts = ts(xc$val, start = start, end = end, frequency = freq)
  # deal with NA actual and NA introduced by tidy::complete
  seen = -(setdiff(which(is.na(xc$val)), which(is.na(x$val))))
  if (length(seen) == 0) {
    seen = seq_len(nrow(xc))
  }
  na.act = function(x) {
    zoo::na.approx(x, na.rm = FALSE, rule = 2)
  }
  tryCatch(
    {
      components = stl(xts, s.window = "per", na.action = na.act)$time.series
      xts = rowSums(components) # NA imputed
      my = format(zoo::as.yearmon(time(components)), "%m-%Y")
      stlval = switch(do,
        deseason = xts - components[, "seasonal"],
        detrend = xts - components[, "trend"],
        remainder = components[, "remainder"],
        trend = components[, "trend"],
        seasonal = components[, "reasonal"],
        stop("Did you pass the right val for `return`?", call. = FALSE)
      )
      data.frame(
        year = as.numeric(substr(my, 4, 7)),
        mon = as.integer(substr(my, 1, 2)),
        stlval = as.vector(stlval)
      )
    },
    error = function(e) {
      withCallingHandlers({
        warning(e)
        xc$val[seen]
      })
    }
  )
}


# padding with leading zeros
padd_zero = function(x, n) {
  # Convert to character for padding
  x = as.character(x)

  # Calculate padding length if not provided
  if (missing(n)) {
    n = max(nchar(x))
  }

  # Pad with leading zeros where possible, return original element otherwise
  padded_x = vapply(x, function(y) {
    if (!is.na(as.numeric(y))) {
      return(sprintf(paste0("%0", as.character(n), "d"), as.integer(y)))
    } else {
      return(y)
    }
  }, character(1), USE.NAMES = FALSE)

  return(padded_x)
}


# add notes to a latex table, requires the threeparttable tex package

add_notes = function(text, notes = NULL) {
  if (missing(notes)) {
    return(text)
  }
  text = paste0(text, collapse = "\n") # in case of a vector
  notes = paste0(notes, collapse = "\n")
  # remove Note or Notes at the start,so that we make it italic
  notes = sub("^(Notes?)", r"(\\\\textit{Notes})", trimws(notes))
  notes = sprintf(
    "\\\\begin{tablenotes}\n\\\\item %s\n\\\\end{tablenotes}", notes
  )
  text = sub(
    r"(\\begin\{tabular\}(\{[^}]*\})?)", "\\\\begin{threeparttable}\n\\\\begin{tabular}\\1", text
  )
  text = sub(
    r"(\\end\{tabular\})",
    sprintf("\\\\end{tabular}\n%s\n\\\\end{threeparttable}", notes), text
  )
  text
}


# preview a LaTeX text/table in a standalone document
preview_tex = function(text) {
  if (endsWith(text, ".tex") && length(text) == 1L) {
    text = readLines(text)
  }

  packages = c(
    "booktabs", "dcolumn", "float", "multirow", "amsmath", "amssymb", "rotating", "threeparttable", "adjustbox", "makecell", "xcolor", "hyperref", "graphicx", "caption", "array", "longtable", "multirow", "multicol", "float", "pdflscape", "fancyhdr", "setspace", "geometry"
  )

  template = c(
    "\\documentclass{article}",
    sprintf("\\usepackage{%s}", paste(packages, collapse = ",")),
    "\\begin{document}",
    text,
    "\\end{document}"
  )
  tmp = tempfile(fileext = ".tex")
  write(paste0(template, collapse = "\n"), tmp)
  if (Sys.getenv("RSTUDIO") != "") {
    message("Click on the Compile PDF button in the RStudio IDE to preview the output.")
    invisible(rstudioapi::navigateToFile(tmp))
  } else {
    message("Please open the file ", tinytex::latexmk(tmp, clean = TRUE), " in your pdf viewer to preview the output.")
  }
}


# replace missing value label (text) by the corresponding numerical code
# e.g. "Sonstiges Missing" is encoded as '-9'.
replace_missing_label_by_value = function(x) {
  fcase(
    x %ilike% "Aanonymisiert|Anonymized", "-11",
    x %ilike% "Variable ist erst in zukünftigen Eingabemasken vorhanden|Future variable", "-10",
    x %ilike% "Sonstiges Missing|Other missing", "-9",
    x %ilike% "Variable trifft auf diesen Datensatzyp nicht zu|Variable for other types only", "-8",
    x %ilike% "keine Angabe|Not specified", "-7",
    x %ilike% "Variable nicht mehr in Einagbemaske vorhanden|Old variable \\(no longer part of the platform\\)", "-6",
    x %ilike% "Unplausibler Wert geloescht|Implausible value", "-5",
    rep_len(TRUE, length(x)), x
  )
}
