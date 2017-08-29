# Energy_1.2_Primary Energy Production by Source 1949-2014 #

# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

file.loc 	  = '/Users/MEAS/GitHub/ene505-figures' # location of scripts
data.loc      = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts' # location of data file(s)
data.file     = 'Energy_1.2_Primary Energy Production by Source_1949_2012.csv' # data file to be used
out.loc       = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts/20170827' # location of where to save figures
source.cols   = c("Geothermal" = "#880015",
                "Solar PV" = "#ff7f27",
                "Wind" = "#3f48cc",
                "Hydroelectric" = "#00baff",
                "Biomass" = "#b5734e",
                "Nuclear" = "#22b14c",
                "Crude Oil" = "#fff200",
                "Coal" = "#000000",
                "Natural Gas (Dry)" = "#ed1c24",
                "Natural Gas (Liquid)" = "#ffa0a4")

# ---------------------------------------------------------------
# MAIN SCRIPT ---------------------------------------------------
# ---------------------------------------------------------------

# load libraries -------
  library(data.table)
  library(ggplot2)
  library(hrbrthemes)

# load plot functions -----
  setwd(file.loc)
  source("plotfunctions.R")

# import data ------
  setwd(data.loc)
  dt_raw = fread(data.file, header = T)

# get annual values only (not monthly) ----
  dt_annual = dt_raw[ str_sub(YYYYMM, start = -2) == "13" ] # last two characters of YYYYMM should be "13"

# rename MSN factor levels -------
  dt_annual[, MSN := revalue(MSN, c(BMPRBUS = "Biomass",
                                    CLPRBUS = "Coal",
                                    FFPRBUS = "Total Fossil Fuels",
                                    GETCBUS = "Geothermal",
                                    HVTCBUS = "Hydroelectric",
                                    NGPRBUS = "Natural Gas (Dry)",
                                    NLPRBUS = "Natural Gas (Liquid)",
                                    NUETBUS = "Nuclear",
                                    PAPRBUS = "Crude Oil",
                                    REPRBUS = "Total Renewable Energy",
                                    SOTCBUS = "Solar PV",
                                    TEPRBUS = "Total Primary Energy",
                                    WYTCBUS = "Wind"))]

# remove rows with data "Not Available" ------
  dt_annual = dt_annual[ ! Value == "Not Available" ]
  
# create column of years -----
  dt_annual[, YYYY := as.numeric(str_sub(YYYYMM, 1, 4)) ] # only keep first four characters
  
# make Value column numeric -----
  dt_annual[, Value := as.numeric(Value) ]
  
# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------
  
  setwd(out.loc) 

# AREA PLOT -------------
  
    dt = dt_annual[, MSN := factor(MSN, levels = c("Total Primary Energy", 
                                                   "Total Fossil Fuels",
                                                   "Total Renewable Energy",
                                                   "Biomass",
                                                   "Wind",
                                                   "Solar/PV",
                                                   "Geothermal",
                                                   "Hydroelectric",
                                                   "Nuclear",
                                                   "Crude Oil",
                                                   "Natural Gas (Liquid)",
                                                   "Natural Gas (Dry)",
                                                   "Coal"))][ ! MSN %in% c("Total Fossil Fuels",
                                                                           "Total Renewable Energy",
                                                                           "Total Primary Energy")] # customize reorder MSN levels for       stacked area chart
    xval = dt[, YYYY]
    yval = dt[, Value]
    fillval = dt[, MSN]
    tlab = "1949 - 2014 Annual U.S. Primary Energy Production by Source"
    sublab = "Data: EIA Annual Energy Review"
    gval = "Y"
    xlab = NULL
    ylab = "Quadrillion BTU"
    leglab = ""
    leg.ord = levels(fillval)
    plot.cols = source.cols
    
    area_pe_prod_1949_2014 = f.areaplot(dt, xval, yval, fillval, tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
      scale_x_continuous(breaks = seq(1945,2015,10), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,80,10), expand = c(0,0))
    
    ggsave(area_pe_prod_1949_2014, 
           filename = "Energy_1.2_Primary Energy Production by Source_1949-2014_ATS.png", 
           width = 11.1, 
           height = 6.25, 
           dpi = 400)
  
  # LINE PLOT -------------

    dt = dt_annual[ ! MSN %in% c("Total Fossil Fuels",
                                 "Total Renewable Energy",
                                 "Total Primary Energy")] 
    xval = dt[, YYYY]
    yval = dt[, Value]
    fillval = dt[, MSN]
    tlab = "1949 - 2014 Annual U.S. Primary Energy Production by Source"
    sublab = "Data: EIA Annual Energy Review"
    gval = "Y"
    xlab = NULL
    ylab = "Quadrillion BTU"
    leglab = ""
    leg.ord = levels(with(dt[YYYY == "2014"], reorder(MSN, -Value))) # reorder legend of line plot to order 2014 values from least to             greatest
    plot.cols = source.cols
    
    line_pe_prod_1949_2014 = f.lineplot(dt, xval, yval, fillval, tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
      scale_x_continuous(breaks = seq(1945,2015,10), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,30,5), expand = c(0,0), limits = c(0, 30))
    
    ggsave(line_pe_prod_1949_2014, 
           filename = "Energy_1.2_Primary Energy Production by Source_1949-2014_LTS.png", 
           width = 11.1, 
           height = 6.25, 
           dpi = 400)
  
  # SEGMENT PLOT -------------
    
    dt = dt_annual[ ! MSN %in% c("Total Fossil Fuels",
                                 "Total Renewable Energy",
                                 "Total Primary Energy")] 
    xval = dt[, YYYY]
    yval = dt[, Value]
    fillval = dt[, MSN]
    wsize = 3
    csize = 2
    tlab = "1949 - 2014 Annual U.S. Primary Energy Production by Source"
    sublab = "Data: EIA Annual Energy Review"
    gval = "Y"
    xlab = NULL
    ylab = "Quadrillion BTU"
    leglab = ""
    leg.ord = levels(with(dt[YYYY == "2014"], reorder(MSN, -Value))) # reorder legend of line plot to order 2014 values from least to             greatest
    plot.cols = source.cols
    
    seg_pe_prod_1949_2014 = f.segplot(dt, xval, yval, fillval, wsize, csize, tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) +
      scale_x_continuous(breaks = seq(1945,2015,10), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,30,5), expand = c(0,0), limits = c(0, 30))
    
    ggsave(seg_pe_prod_1949_2014, 
           filename = "Energy_1.2_Primary Energy Production by Source_1949-2014_STS.png", 
           width = 11.1, 
           height = 6.25, 
           dpi = 400)
    
        