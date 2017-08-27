# Electricity_8.2a_Electricity Net Gen Total All Sectors_Script #

# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

file.loc 	= '/Users/MEAS/GitHub/ene505-figures' # location of scripts
data.loc    = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts' # location of data file(s)
data.file   = 'Electricity_8.2a_Electricity Net Gen Total All Sectors.csv' # data file to be used
source.cols   = c("Geothermal" = "#880015",
                  "Solar PV" = "#ff7f27",
                  "Waste" = "#c3c3c3",
                  "Wood" = "#b5734e",
                  "Wind" = "#3f48cc",
                  "Conventional Hydroelectric" = "#00baff",
                  "Biomass" = "#b5734e",
                  "Nuclear" = "#22b14c",
                  "Petroleum" = "#fff200",
                  "Coal" = "#000000",
                  "Natural Gas" = "#ed1c24",
                  "Other Gases" = "#e9d8ff")
out.loc   = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts/20170827' # location of where to save figures

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
  dt_raw = fread(data.file, header = T)

# get annual values only (not monthly) ----
  dt_annual = dt_raw[ str_sub(YYYYMM, start = -2) == "13" ] # last two characters of YYYYMM should be "13"

# rename MSN factor levels -------
  dt_annual[, MSN := revalue(MSN, c(CLETPUS = "Coal",
                                    GEETPUS = "Geothermal",
                                    HPETPUS = "Hydroelectric Pumped Storage",
                                    HVETPUS = "Conventional Hydroelectric",
                                    NGETPUS = "Natural Gas",
                                    NUETPUS = "Nuclear",
                                    OJETPUS = "Other Gases",
                                    PAETPUS = "Petroleum",
                                    SOETPUS = "Solar PV",
                                    WDETPUS = "Wood",
                                    WSETPUS = "Waste",
                                    WYETPUS = "Wind",
                                    ELETPUS = "Total"))]

# remove pumped storage ------
  dt_annual = dt_annual[ ! MSN == "Hydroelectric Pumped Storage" ]

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

  dt = dt_annual[, MSN := factor(MSN, levels = c("Total",
                                                 "Wind",
                                                 "Solar PV",
                                                 "Geothermal",
                                                 "Waste",
                                                 "Wood",
                                                 "Conventional Hydroelectric",
                                                 "Nuclear",
                                                 "Other Gases",
                                                 "Natural Gas",
                                                 "Petroleum",
                                                 "Coal"))][ ! MSN %in% c("Total")] # customize reorder MSN levels for stacked area chart
  xval = dt[, YYYY]
  yval = dt[, Value]/1000000
  fillval = dt[, MSN]
  tlab = "1949 - 2014 Electricity Generation By Source"
  sublab = "Data: EIA Annual Energy Review"
  gval = "Y"
  xlab = ""
  ylab = "Kilowatt-hour (Trillions)"
  leglab = ""
  leg.ord = levels(fillval)
  plot.cols = source.cols
  
  area_elec_netgen_1949_2014 = f.areaplot(dt, xval, yval, fillval, tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
    scale_x_continuous(breaks = seq(1948,2014,10), expand = c(0,0)) +
    scale_y_continuous(breaks = seq(0,4.5,0.5), expand = c(0,0))
  
  ggsave(area_elec_netgen_1949_2014, 
         filename = "Electricity_8.2a_Electricity Net Gen Total All Sectors_ATS.png", 
         width = 14.5, 
         height = 8.16, 
         dpi = 400)

# LINE PLOT -------------
  
  dt = dt_annual[ ! MSN %in% c("Total")]
  xval = dt[, YYYY]
  yval = dt[, Value]/1000000
  fillval = dt[, MSN]
  tlab = "1949 - 2014 Electricity Generation By Source"
  sublab = "Data: EIA Annual Energy Review"
  gval = "Y"
  xlab = ""
  ylab = "Kilowatt-hour (Trillions)"
  leglab = ""
  leg.ord = levels(with(dt[YYYY == "2014"], reorder(MSN, -Value))) # reorder legend of line plot to order 2014 values from least to greatest
  plot.cols = source.cols
  
  line_elec_netgen_1949_2014 = f.lineplot(dt, xval, yval, fillval, tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
    scale_x_continuous(breaks = seq(1948,2014,10), expand = c(0,0)) +
    scale_y_continuous(breaks = seq(0,2.5,0.5), expand = c(0,0), limits = c(0, 2.5))
  
  ggsave(line_elec_netgen_1949_2014, 
         filename = "Electricity_8.2a_Electricity Net Gen Total All Sectors_LTS.png", 
         width = 14.5, 
         height = 8.16, 
         dpi = 400)
  
# SEGMENT PLOT -------------
  
  dt = dt_annual[ ! MSN %in% c("Total")]
  xval = dt[, YYYY]
  yval = dt[, Value]/1000000
  fillval = dt[, MSN]
  tlab = "1949 - 2014 Electricity Generation By Source"
  sublab = "Data: EIA Annual Energy Review"
  gval = "Y"
  xlab = ""
  ylab = "Kilowatt-hour (Trillions)"
  leglab = ""
  leg.ord = levels(with(dt[YYYY == "2014"], reorder(MSN, -Value)))
  plot.cols = source.cols
  
  seg_elec_netgen_1949_2014 = f.segplot(dt, xval, yval, fillval, tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
    scale_x_continuous(breaks = seq(1948,2014,10), expand = c(0,0)) +
    scale_y_continuous(breaks = seq(0,2.5,0.5), expand = c(0,0), limits = c(0, 2.5))
  
  ggsave(seg_elec_netgen_1949_2014, 
         filename = "Electricity_8.2a_Electricity Net Gen Total All Sectors_STS.png", 
         width = 14.5, 
         height = 8.16, 
         dpi = 400)
  
  
  