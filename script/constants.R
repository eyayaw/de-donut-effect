# some constants we use in our scripts and qmd files

HED_VARS = c(Prices = "lnhpi", Rents = "lnhri") # hedonic variables
DIST_VAR = "dist_inhab" # main dist2cbd var
AMENITY_VARS = c(cons = "urbam_index", envt = "environ_area_index")
AMENITY_TYPES = c(cons = "consumption", envt = "environmental")
# the start of the pandemic, and a year after that
TIMES = c("2020-03-01", "2021-03-01") |>
    as.Date() |>
    {
        \(.x) setNames(.x, format(.x, "%b %Y"))
    }()
REF_TIME = TIMES[[1]]
DATA_YEAR_START = 2017 # keep data from this year on
DIST_MAX_METERS = 25000 # upper limit for dist2cbd
DROP_VARS = c("plz", "note", "qkm", "num_towns", "did", "geo_name", "amr_id", "rank_pop") # dropable vars when importing data
COLS = c("#2b8043", "#1746A2", "#c70002") # colors for visualization


## fixest global options
# define a dictionary and set it globally
# define also notes, not just variable names
# The function 'dsb()' is like 'glue()'
FIXEST_DICT = c(
    "(Intercept)" = "Constant",
    lnhpi = "$\\ln P$",
    lnhri = "$\\ln R$",
    lndist = "$\\ln \\text{dist}$",
    amr_name = "LMR",
    mon_year = "Time",
    ftime = "Time",
    lnpp = "$\\ln \\text{Purchasing Power}$",
    share_male_1845 = "Share male aged 18-45",
    share_background_german = "Share background German",
    note1 = fixest::dsb("*Notes*:"),
    source = "*Sources*: Own computations."
)

fixest::setFixest_dict(FIXEST_DICT)

# default style of the table
my_style_tex = fixest::style.tex("aer", tpt = TRUE, notes.tpt.intro = "\\footnotesize")

fixest::setFixest_etable(style.tex = my_style_tex, page.width = "a4")
