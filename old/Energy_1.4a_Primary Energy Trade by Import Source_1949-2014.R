# Energy_1.4a_Primary Energy Trade by Import Source #

# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

file.loc 	  = '/Users/MEAS/GitHub/ene505-figures' # location of scripts
data.loc      = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts' # location of data file(s)
data.file     = 'Energy_1.4a_Primary Energy Trade by Source_Imports_1949_2012.csv' # data file to be used
out.loc       = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts/20170827' # location of where to save figures
source.cols   = c("Biofuels" = "#b5734e",
                  "Electricity" = "#00b6eb",
                  "Petroleum Products" = "#fff200",
                  "Crude Oil" = "#001eff",
                  "Natural Gas" = "#ed1c24",
                  "Coal Coke" = "#c3c3c3",
                  "Coal" = "#000000")

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
  dt_annual[, MSN := revalue(MSN, c(BFIMBUS = "Biofuels",
                                    CCIMBUS = "Coal Coke",
                                    CLIMBUS = "Coal",
                                    COIMBUS = "Crude Oil",
                                    ELIMBUS = "Electricity",
                                    NGIMBUS = "Natural Gas",
                                    PMIMBUS = "Total Petroleum",
                                    RQIMBUS = "Petroleum Products",
                                    TEIMBUS = "Total Primary Energy"))]
  
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
                                                   "Total Petroleum",
                                                   "Biofuels", 
                                                   "Electricity",
                                                   "Petroleum Products",
                                                   "Crude Oil",
                                                   "Natural Gas",
                                                   "Coal Coke",
                                                   "Coal"))][ ! MSN %in% c("Total Petroleum",
                                                                           "Total Primary Energy")] # customize reorder MSN levels for            stacked area chart
    xval = dt[, YYYY]
    yval = dt[, Value]
    fillval = dt[, MSN]
    tlab = "1949 - 2015 Annual U.S. Primary Energy Trade by Import Source"
    sublab = "Data: EIA Annual Energy Review"
    gval = "Y"
    xlab = NULL
    ylab = "Quadrillion BTU"
    leglab = ""
    leg.ord = levels(fillval)
    plot.cols = source.cols
    
    area_energy_import_1949_2014 = f.areaplot(dt, xval, yval, fillval, 
                                              tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
      scale_x_continuous(breaks = seq(1945,2015,10), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,35,5), expand = c(0,0))
    
    ggsave(area_energy_import_1949_2014, 
           filename = "Energy_1.4a_Primary Energy Trade by Import Source_1949-2014_ATS.png", 
           width = 11.1, 
           height = 6.25, 
           dpi = 400)
  
  # LINE PLOT -------------
  
    dt = dt_annual[ ! MSN %in% c("Total Petroleum",
                                 "Total Primary Energy")] 
    xval = dt[, YYYY]
    yval = dt[, Value]
    fillval = dt[, MSN]
    tlab = "1949 - 2015 Annual U.S. Primary Energy Trade by Import Source"
    sublab = "Data: EIA Annual Energy Review"
    gval = "Y"
    xlab = NULL
    ylab = "Quadrillion BTU"
    leglab = ""
    leg.ord = levels(with(dt[YYYY == "2014"], reorder(MSN, -Value))) # reorder legend of line plot to order 2014 values from least to             greatest
    plot.cols = source.cols
    
    line_energy_import_1949_2014 = f.lineplot(dt, xval, yval, fillval, 
                                              tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
      scale_x_continuous(breaks = seq(1945,2015,10), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,25,5), expand = c(0,0), limits = c(0, 25))
    
    ggsave(line_energy_import_1949_2014, 
           filename = "Energy_1.4a_Primary Energy Trade by Import Source_1949-2014_LTS.png", 
           width = 11.1, 
           height = 6.25, 
           dpi = 400)
  
  # SEGMENT PLOT -------------
  
    dt = dt_annual[ ! MSN %in% c("Total Petroleum",
                                   "Total Primary Energy")] 
    xval = dt[, YYYY]
    yval = dt[, Value]
    fillval = dt[, MSN]
    wsize = 3
    csize = 2
    tlab = "1949 - 2015 Annual U.S. Primary Energy Trade by Import Source"
    sublab = "Data: EIA Annual Energy Review"
    gval = "Y"
    xlab = NULL
    ylab = "Quadrillion BTU"
    leglab = ""
    leg.ord = levels(with(dt[YYYY == "2014"], reorder(MSN, -Value))) # reorder legend of line plot to order 2014 values from least to             greatest
    plot.cols = source.cols
    
    seg_energy_import_1949_2014 = f.segplot(dt, xval, yval, fillval, wsize, csize, 
                                            tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) +
      scale_x_continuous(breaks = seq(1945,2015,10), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,25,5), expand = c(0,0), limits = c(0, 25))
    
    ggsave(seg_energy_import_1949_2014, 
           filename = "Energy_1.4a_Primary Energy Trade by Import Source_1949-2014_STS.png", 
           width = 11.1, 
           height = 6.25, 
           dpi = 400)
    