# Electricity_CA_Electricity Net Gen Total RE Only #

# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

file.loc 	= '/Users/MEAS/GitHub/ene505-figures' # location of scripts
data.loc    = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts' # location of data file(s)
data.file   = 'Electricity_CA Electricity Net Gen Total All Sectors_1983_2014.csv' # data file to be used
out.loc   = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts/20170827' # location of where to save figures
source.cols   = c("Other Imports" = "#e9d8ff",
                  "Wind" = "#3f48cc", 
                  "Solar" = "#ff7f27", 
                  "Geothermal" = "#880015", 
                  "Biomass" = "#b5734e", 
                  "Hydroelectric" = "#00baff", 
                  "Nuclear" = "#22b14c", 
                  "Natural Gas" = "#ed1c24", 
                  "Oil" = "#fff200", 
                  "Coal Imports" = "#c3c3c3",
                  "Coal" = "#000000",
                  "Other" = "#1effd2"
)

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
  dt_raw = fread(data.file, header = T)[2:16]   # skip first row because it's just the units; remove any row after the 16th row since it's data we don't need
  colnames(dt_raw)[1] = "MSN"

# rename MSN factor levels -------
  dt_raw[, MSN := revalue(MSN, c("Biomass" = "Biomass",
                                 "California Generation plus Net Imports *" = "Total",
                                 "Direct Coal Imports***" = "Coal Imports",
                                 "Geothermal" = "Geothermal",
                                 "In-State Coal" = "Coal",
                                 "Large Hydroelectric" = "Large Hydroelectric",
                                 "Natural Gas **" = "Natural Gas", 
                                 "Nuclear" = "Nuclear",
                                 "Oil"  = "Oil",
                                 "Other" = "Other",
                                 "Other Imports****"  = "Other Imports",
                                 "Small Hydroeletric" = "Small Hydroelectric",
                                 "Solar"  = "Solar",
                                 "Total Hydroelectric"  = "Hydroelectric",
                                 "Wind"  = "Wind"
  ))]

# remove small and large hydroelectic and non-RE -----
  dt_new = dt_raw[ MSN %in% c("Biomass", "Geothermal", "Solar", "Hydroelectric", "Wind", "Total")]

# transpose data -----
  dt_wide = transpose(dt_new)[2:33]
  colnames(dt_wide) = dt_new[[1]]

# add header of original data table as column of years in transposed data table -----
  dt_wide[, Year := as.numeric(colnames(dt_new)[2:33])]

# melt data table from wide to long format -----
  dt_long = melt(dt_wide, measure.vars = colnames(dt_wide)[1:6],
                 variable.name = "MSN", value.name = "Value")

# convert value to numeric ------
  dt_long[, Value := as.numeric(gsub(",", "", Value))]

# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------

setwd(out.loc) 

  # AREA PLOT -------------
  
  dt = dt_long[, MSN := factor(MSN, levels = c("Total",
                                               "Other Imports",
                                               "Wind",
                                               "Solar",
                                               "Geothermal",
                                               "Biomass",
                                               "Hydroelectric",
                                               "Nuclear",
                                               "Natural Gas",
                                               "Oil",
                                               "Coal Imports",
                                               "Coal",
                                               "Other"))][ ! MSN %in% c("Total")] # customize reorder MSN levels for stacked area chart
  xval = dt[, Year]
  yval = dt[, Value]/1000
  fillval = dt[, MSN]
  tlab = "1983-2014 California Electricity Generation by Source"
  sublab = "Data: California Energy Commission"
  gval = "Y"
  xlab = ""
  ylab = "Terawatt-hour"
  leglab = ""
  leg.ord = levels(fillval)
  plot.cols = source.cols
  
  area_elec_CA_netgen_RE_1983_2014 = f.areaplot(dt, xval, yval, fillval, tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
    scale_x_continuous(breaks = seq(1983,2014,10), expand = c(0,0)) +
    scale_y_continuous(breaks = seq(0,80,10), expand = c(0,0))
  
  ggsave(area_elec_CA_netgen_RE_1983_2014, 
         filename = "Electricity_CA_Electricity Net Gen Total RE Only_ATS.png", 
         width = 14.5, 
         height = 8.16, 
         dpi = 400)
  
  # LINE PLOT -------------
  
  dt = dt_long[ ! MSN %in% c("Total")]
  xval = dt[, Year]
  yval = dt[, Value]/1000
  fillval = dt[, MSN]
  tlab = "1983-2014 California Electricity Generation by Source"
  sublab = "Data: California Energy Commission"
  gval = "Y"
  xlab = ""
  ylab = "Terawatt-hour"
  leglab = ""
  leg.ord = levels(with(dt[Year == "2014"], reorder(MSN, -Value))) # reorder legend of line plot to order 2014 values from least to greatest
  plot.cols = source.cols
  
  line_elec_CA_netgen_RE_1949_2014 = f.lineplot(dt, xval, yval, fillval, tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
    scale_x_continuous(breaks = seq(1948,2014,10), expand = c(0,0)) +
    scale_y_continuous(breaks = seq(0,60,10), expand = c(0,0), limits = c(0, 60))
  
  ggsave(line_elec_CA_netgen_RE_1949_2014, 
         filename = "Electricity_CA_Electricity Net Gen Total RE Only_LTS.png", 
         width = 14.5, 
         height = 8.16, 
         dpi = 400)
  
# SEGMENT PLOT -------------
  
  dt = dt_long[ ! MSN %in% c("Total")]
  xval = dt[, Year]
  yval = dt[, Value]/1000
  fillval = dt[, MSN]
  wsize = 3
  csize = 2
  tlab = "1983-2014 California Electricity Generation by Source"
  sublab = "Data: California Energy Commission"
  gval = "Y"
  xlab = ""
  ylab = "Terawatt-hour"
  leglab = ""
  leg.ord = levels(with(dt[Year == "2014"], reorder(MSN, -Value))) # reorder legend of line plot to order 2014 values from least to greatest
  plot.cols = source.cols
  
  seg_elec_CA_netgen_RE_1949_2014 = f.segplot(dt, xval, yval, fillval, wsize, csize, tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
    scale_x_continuous(breaks = seq(1983,2014,10), expand = c(0,0)) +
    scale_y_continuous(breaks = seq(0,60,10), expand = c(0,0), limits = c(0, 60))
  
  ggsave(seg_elec_CA_netgen_RE_1949_2014, 
         filename = "Electricity_CA_Electricity Net Gen Total RE Only_STS.png", 
         width = 14.5, 
         height = 8.16, 
         dpi = 400)
