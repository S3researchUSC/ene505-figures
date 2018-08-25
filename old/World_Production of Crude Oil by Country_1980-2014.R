# World_Production of Crude Oil by Country_1980-2014 #

# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

file.loc 	    = '/Users/MEAS/GitHub/ene505-figures' # location of scripts
data.loc      = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts' # location of data file(s)
data.file     = 'World_Production of Crude Oil by Country_1980_2014.csv' # data file to be used
out.loc       = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts/20170827' # location of where to save figures
ran.cols        = c("#42344c",
                    "#70d54f",
                    "#6c47cc",
                    "#cbd64a",
                    "#cc52ca",
                    "#75d597",
                    "#512d76",
                    "#cf9c3d",
                    "#7486c9",
                    "#d2522f",
                    "#82ced7",
                    "#db4d6b",
                    "#5e8439",
                    "#b54d8a",
                    "#cbcb9c",
                    "#772f2f",
                    "#55837e",
                    "#af7a56",
                    "#ce9ebc",
                    "#414328")

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
  dt_raw <- fread(data.file, skip = 2, header = T)[2:21, c(1, 3:37), with = FALSE]

# melt data table from wide to long format -----
  dt_long <- melt(dt_raw, measure.vars = colnames(dt_raw)[2:36],
                  variable.name = "year", value.name = "value")
  colnames(dt_long)[1] <- "country"
  
# convert value into numeric -----
  dt_long[, year := as.numeric(as.character(year))]
  dt_long[, value := as.numeric(value)]
  
# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------
  
  setwd(out.loc) 
  
# AREA PLOT -------------
  
  dt <- dt_long[, country := factor(country, levels(with(dt_long[year == "2014"],
                                                         reorder(country, -value))))]
  xval = dt[, year]
  yval = dt[, value]
  fillval = dt[, country]
  tlab = "1980 - 2014 Production of Crude Oil by Country"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Thousand Barrels per Day"
  leglab = ""
  leg.ord = levels(factor(fillval))
  plot.cols = ran.cols
  
  area_world_crude_production = f.areaplot(dt, xval, yval, fillval, tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
    scale_x_continuous(breaks = seq(1978,2015,3), expand = c(0,0)) +
    scale_y_comma(breaks = seq(0,60000,10000), expand = c(0,0))
  
  ggsave(area_world_crude_production, 
         filename = "World_Production of Crude Oil by Country_1980-2014_ATS.png", 
         width = 11.1, 
         height = 6.25, 
         dpi = 400)
  
  # LINE PLOT -------------
  
  dt <- dt_long[, country := factor(country, levels(with(dt_long[year == "2014"],
                                                         reorder(country, -value))))]
  xval = dt[, year]
  yval = dt[, value]
  fillval = dt[, country]
  tlab = "1980 - 2014 Production of Crude Oil by Country"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Thousand Barrels per Day"
  leglab = ""
  leg.ord = levels(with(dt[year == "2014"], reorder(country, -value)))
  plot.cols = ran.cols
  
  line_world_crude_production = f.lineplot(dt, xval, yval, fillval, tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
    scale_x_continuous(breaks = seq(1978,2015,3), expand = c(0,0)) +
    scale_y_comma(breaks = seq(0,10000,2000), expand = c(0,0), limits = c(0, 10000))
  
  ggsave(line_world_crude_production, 
         filename = "World_Production of Crude Oil by Country_1980-2014_LTS.png", 
         width = 11.1, 
         height = 6.25, 
         dpi = 400)
  
  # SEGMENT PLOT -------------
  
  dt <- dt_long[, country := factor(country, levels(with(dt_long[year == "2014"],
                                                         reorder(country, -value))))]
  xval = dt[, year]
  yval = dt[, value]
  fillval = dt[, country]
  wsize = 2
  csize = 1
  tlab = "1980 - 2014 Production of Crude Oil by Country"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Thousand Barrels per Day"
  leglab = ""
  leg.ord = levels(with(dt[year == "2014"], reorder(country, -value)))
  plot.cols = ran.cols
  
  seg_world_crude_production = f.segplot(dt, xval, yval, fillval, wsize, csize, 
                                      tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
    scale_x_continuous(breaks = seq(1978,2015,3), expand = c(0,0)) +
    scale_y_comma(breaks = seq(0,10000,2000), expand = c(0,0), limits = c(0, 10000))
  
  ggsave(seg_world_crude_production, 
         filename = "World_Production of Crude Oil by Country_1980-2014_STS.png", 
         width = 11.1, 
         height = 6.25, 
         dpi = 400)
  