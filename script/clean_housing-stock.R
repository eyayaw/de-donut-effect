library(data.table)

skip = 5
na.strings = c("-", ".", "...", "/", "x")
enc = "Latin-1"
keys = c("year", "mid", "name")
fpath = c(
  "data/housing-stock/31231-01-02-5_2008.csv",
  "data/housing-stock/31231-01-02-5_2009.csv",
  "data/housing-stock/31231-01-02-5_2010.csv",
  "data/housing-stock/31231-02-01-5.csv"
)

stock = vector("list", length(fpath))

for (i in seq_along(fpath)) {
  colnum = if (i == 4) 2L else 1L
  metadata = fread(fpath[[i]],
    skip = skip, encoding = enc, header = FALSE, na.strings = na.strings, nrows = 50
  )
  idx = grep("^DG$", metadata[[colnum]])[[1]] - 1
  nms = sapply(metadata[seq_len(idx), ], paste, collapse = "---")
  nms = sub("^-+", "", nms)
  nms = gsub("-{2,}", "--", nms)
  nms = paste0("x", seq_along(nms), nms) # make names unique

  # read the real data
  stock[[i]] = fread(fpath[[i]], skip = skip + idx, dec = ",", header = FALSE, na.strings = na.strings, encoding = enc)
  setnames(stock[[i]], nms)
  if (i != 4) {
    stock[[i]][, year := sub("(x\\d+)(Stichtag: )(\\d\\d[.]\\d\\d[.](\\d{4}))(-+)$", "\\4", nms[[1]])]
    setcolorder(stock[[i]], "year")
  }
  setnames(stock[[i]], names(stock[[i]])[1:3], keys)
  setnames(stock[[i]], gsub("^x[0-9]+", "", names(stock[[i]])))
  rm(metadata, idx, nms)
}


# munging -----
stock_before_2011 = rbindlist(stock[-4], use.names = TRUE)
stock_since_2011 = stock[[4]]

keep_patt = paste(c(keys, "Wohngebäude", "Wohnfläche"), collapse = "|")
stock_before_2011 =
  stock_before_2011[, grep(keep_patt, names(stock_before_2011), value = TRUE),
    with = FALSE
  ]

stock_since_2011 = stock_since_2011[,
  grep(keep_patt, names(stock_since_2011), value = TRUE),
  with = FALSE
]


dict = data.frame(
  de = c(
    "Wohngebäude--Gebäude nach Anzahl der Wohnungen--Insgesamt--Anzahl",
    "Wohngebäude--Gebäude nach Anzahl der Wohnungen--1 Wohnung--Anzahl",
    "Wohngebäude--Gebäude nach Anzahl der Wohnungen--2 Wohnungen--Anzahl",
    "Wohngebäude--Wohngebäude nach Anzahl der Wohnungen--Insgesamt--Anzahl",
    "Wohngebäude--Wohngebäude nach Anzahl der Wohnungen--Wohngebäude mit 1 Wohnung--Anzahl",
    "Wohngebäude--Wohngebäude nach Anzahl der Wohnungen--Wohngebäude mit 2 Wohnungen--Anzahl",
    "Wohngebäude--Wohngebäude nach Anzahl der Wohnungen--Wohngebäude mit 3 und mehr Wohnungen--Anzahl",
    "Wohngebäude--Wohngebäude nach Anzahl der Wohnungen--Wohnheime--Anzahl",
    "Wohnfläche in Wohngebäuden--1000 qm"
  ),
  en = tolower(
    c(
      "Total",
      "Residential buildings with 1 apartment",
      "Residential building with 2 apartments",
      "Total",
      "Residential buildings with 1 apartment",
      "Residential building with 2 apartments",
      "Residential buildings with 3+ apartments",
      "Dormitories",
      "Living space in residential buildings--1000 sqm"
    )
  )
)

# getLabel names
nms = setdiff(names(stock_before_2011), keys)
setnames(stock_before_2011, nms, dict$en[match(nms, dict$de)])
nms = setdiff(names(stock_since_2011), keys)
setnames(stock_since_2011, nms, dict$en[match(nms, dict$de)])

stock_since_2011[, dormitories := NULL]

# buildings with 3+ apartments absent, let's construct it
stock_before_2011[, `residential buildings with 3+ apartments` :=
  total - rowSums(cbind(`residential buildings with 1 apartment`, `residential building with 2 apartments`), na.rm = TRUE)]

setcolorder(stock_before_2011,
  neworder = "residential buildings with 3+ apartments",
  after = "residential building with 2 apartments"
)
stock_since_2011[, year := year(as.Date(year, "%d.%m.%Y"))]

stock = rbind(stock_before_2011, stock_since_2011, use.names = TRUE)
rm(stock_before_2011, stock_since_2011)


fwrite(stock,
sprintf('data/processed/housing-stock_municipality-level_%s.csv', paste(range(stock$year),collapse = "-")))

