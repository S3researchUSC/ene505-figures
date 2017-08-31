# World_Petroleum Imports to US by Country_OPEC vs NonOPEC_1994-2014 #

# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

file.loc 	    = '/Users/MEAS/GitHub/ene505-figures' # location of scripts
data.loc      = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts' # location of data file(s)
data.file     = 'World_Petroleum Imports to US by Country_1973_2014.csv' # data file to be used
out.loc       = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts/20170827' # location of where to save figures

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
  
# only keep OPEC and Non-OPEC values ------
  dt_long <- dt_long[ country %in% c("OPEC", "Non-OPEC")]

# convert value and year columns to numeric -----
  dt_long[, value := as.numeric(value)]
  dt_long[, year := as.numeric(as.character(year))]

# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------

  setwd(out.loc) 

  dt <- dt_long
  
  line_world_petro_imports_opec = ggplot(dt, aes(x = year, y = value, linetype = country)) + 
    geom_line(stat = "identity", color = "gray10", size = 1) +
    labs(title = "1994 - 2014 Petroleum Imports to U.S. by OPEC and Non-OPEC Countries",
         subtitle = "Data: EIA Annual Energy Review", 
         x = NULL,
         y = "Thousand Barrels per Day",
         linetype = NULL) +
    theme_ipsum_rc(grid = "Y") +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.text = element_text(size = 13, face = "bold"),
          legend.position = "bottom") + 
    scale_x_continuous(breaks = seq(1993,2015,2), expand = c(0,0), limits = c(1994, 2014)) +
    scale_y_comma(breaks = seq(1500,8500,1000), expand = c(0,0), limits = c(1500, 8500))
  
  ggsave(line_world_petro_imports_opec, 
         filename = "World_Petroleum Imports to US_OPEC_1994-2014_LTS.png", 
         width = 11.1, 
         height = 6.25, 
         dpi = 400)
