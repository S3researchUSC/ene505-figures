# World_Petroleum Imports to US by Country_1973-2014 #

# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

file.loc 	    = '/Users/MEAS/GitHub/ene505-figures' # location of scripts
data.loc      = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts' # location of data file(s)
data.file     = 'World_Petroleum Imports to US by Country_1973_2014.csv' # data file to be used
out.loc       = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts/20170827' # location of where to save figures
country.cols  = c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462",
                  "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F")
ran.cols      = c("#40516b",
                  "#7bb3c9",
                  "#ca949f",
                  "#c3ba8c",
                  "#68392b",
                  "#4d673a",
                  "#72d1b6",
                  "#6e3363",
                  "#9c92d7",
                  "#c6843c",
                  "#c74b4a",
                  "#6ac46c",
                  "#ce58a4",
                  "#b6be49",
                  "#6248ab")

# ---------------------------------------------------------------
# MAIN SCRIPT ---------------------------------------------------
# ---------------------------------------------------------------

# load libraries -------
  library(data.table)
  library(ggplot2)
  library(hrbrthemes)
  library(stringr)
  library(plyr)

# load plot functions -----
  setwd(file.loc)
  source("plotfunctions.R")

# load data ------
  setwd(data.loc)
  dt_raw <- fread(data.file, skip = 2, header = T)

# change column names ------
  colnames(dt_raw) <- c("year",
                        "Total",
                        "Persian Gulf",
                        "OPEC",
                        "Algeria",
                        "Angola",
                        "Ecuador",
                        "Iran",
                        "Iraq",
                        "Kuwait",
                        "Libya",
                        "Nigeria",
                        "Qatar",
                        "Saudi Arabia",
                        "United Arab Emirates",
                        "Venezuela",
                        "Non-OPEC",
                        "Albania",
                        "Argentina",
                        "Aruba",
                        "Australia",
                        "Austria",
                        "Azerbaijan",
                        "Bahama Islands",
                        "Bahrain",
                        "Barbados",
                        "Belarus",
                        "Belgium",
                        "Belize",
                        "Benin",
                        "Bolivia",
                        "Brazil",
                        "Brunei",
                        "Bulgaria",
                        "Burma",
                        "Cameroon",
                        "Canada",
                        "Chad",
                        "Chile",
                        "China",
                        "Colombia",
                        "Congo Brazzaville",
                        "Congo Kinshasa",
                        "Cook Islands",
                        "Costa Rica",
                        "Croatia",
                        "Cyprus",
                        "Czech Republic",
                        "Denmark",
                        "Dominican Republic",
                        "Egypt",
                        "El Salvador",
                        "Equatorial Guinea",
                        "Estonia",
                        "Finland",
                        "France",
                        "Gabon",
                        "Georgia",
                        "Germany",
                        "Ghana",
                        "Gibraltar",
                        "Greece",
                        "Guatemala",
                        "Guinea",
                        "Hong Kong",
                        "Hungary",
                        "India",
                        "Indonesia",
                        "Ireland",
                        "Israel",
                        "Italy",
                        "Ivory Coast",
                        "Jamaica",
                        "Japan",
                        "Kazakhstan",
                        "Korea",
                        "Kyrgyzstan",
                        "Latvia",
                        "Liberia",
                        "Lithuania",
                        "Malaysia",
                        "Malta",
                        "Martinique",
                        "Mauritania",
                        "Mexico",
                        "Midway Islands",
                        "Morocco",
                        "Namibia",
                        "Netherlands",
                        "Netherlands Antilles",
                        "New Zealand",
                        "Nicaragua",
                        "Niue",
                        "Norway",
                        "Oman",
                        "Pakistan",
                        "Panama",
                        "Papua New Guinea",
                        "Peru",
                        "Philippines",
                        "Poland",
                        "Portugal",
                        "Puerto Rico",
                        "Romania",
                        "Russia",
                        "Senegal",
                        "Singapore",
                        "Slovakia",
                        "South Africa",
                        "Spain",
                        "Spratly Islands",
                        "Suriname",
                        "Swaziland",
                        "Sweden",
                        "Switzerland",
                        "Syria",
                        "Taiwan",
                        "Thailand",
                        "Togo",
                        "Tonga",
                        "Trinidad and Tobago",
                        "Tunisia",
                        "Turkey",
                        "Turkmenistan",
                        "Ukraine",
                        "United Kingdom",
                        "Uruguay",
                        "Uzbekistan",
                        "Vietnam",
                        "Virgin Islands",
                        "Yemen")
  
# melt data table from wide to long format -----
  dt_long <- melt(dt_raw, measure.vars = colnames(dt_raw)[2:131],
                 variable.name = "country", value.name = "value")
  
# convert value and year columns to numeric -----
  dt_long[, value := as.numeric(value)]
  dt_long[, year := as.numeric(as.character(year))]
  
# order country by ------
  
  country.levs <- levels(with(dt_long[ year == "2014" & ! country %in% c("Total", "Persian Gulf", "OPEC", "Non-OPEC") ], reorder(country, -value)))
  country.select <- country.levs[1:14]
  
# only keep 2014 data -----
  dt_2014 <- na.omit(dt_long[ year == "2014" ])

# assign category ------
  dt_2014[ country %in% c(country.select, "Total", "Persian Gulf", "OPEC", "Non-OPEC"), category := country ]
  dt_2014[ ! country %in% c(country.select, "Total", "Persian Gulf", "OPEC", "Non-OPEC"), category := "Other" ]
  
# aggregate by category -----
  dt_2014 <- dt_2014[, .(value = sum(value)), by = c("category")] 
  
  
# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------
  
  setwd(out.loc) 
  
  # BAR PLOT -------------
  
  dt = dt_2014[, category := factor(category, levels(with(dt_2014, reorder(category, value))))][! category %in% c("Total", "Persian Gulf", "OPEC", "Non-OPEC") ]
  xval = dt[, category]
  yval = dt[, value]
  fillval = dt[, category]
  tlab = "2014 Petroleum Imports to U.S. by Country"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "X"
  xlab = NULL
  ylab = "Thousand Barrels per Day"
  leglab = ""
  leg.ord = levels(factor(fillval))
  plot.cols = rev(ran.cols)
  
  bar_world_petro_imports_2012 = f.barplot(dt, xval, yval, fillval, 
                                       tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
    coord_flip() + 
    scale_y_comma(breaks = seq(0,3500,500), expand = c(0,0), limits = c(0, 3500)) +
    guides(fill=FALSE)
  
  ggsave(bar_world_petro_imports_2012, 
         filename = "World_Petroleum Imports to U.S. by Country_2014_BP.png", 
         width = 11.1, 
         height = 6.25, 
         dpi = 400)
  