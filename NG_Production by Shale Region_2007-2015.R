# NG_Production by Shale Region_2007-2015 #

# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

file.loc 	    = '/Users/MEAS/GitHub/ene505-figures' # location of scripts
data.loc      = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts' # location of data file(s)
data.file     = c('NG_Production by Region_2007_2015.csv') # data file to be used
out.loc       = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts/20170827' # location of where to save figures
reg.cols      = c("Bakken" = "snow3",
                  "Eagle Ford" = "mediumpurple3",
                  "Haynesville" = "darkgoldenrod2", 
                  "Marcellus" = "brown3",
                  "Niobrara" = "springgreen4",
                  "Permian" = "steelblue3",
                  "Utica" = "hotpink4"
                  )

# ---------------------------------------------------------------
# MAIN SCRIPT ---------------------------------------------------
# ---------------------------------------------------------------

# load libraries -------
  library(data.table)
  library(ggplot2)
  library(hrbrthemes)
  library(lubridate)

# load plot functions -----
  setwd(file.loc)
  source("plotfunctions.R")

# import data ------
  setwd(data.loc)
  
  dt_raw = fread(data.file, skip = 1, header = T)
  
# melt data from wide format to long -----
  dt_long = melt(dt_raw, measure.vars = list(2:8), variable.name = "region", value.name = "value")

# convert month column into date format -----
  dt_long[, day := mdy(Month)]
  
# convert values to numeric -----
  dt_long[, value := as.numeric(gsub(",", "", value))]
  
# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------
  
  setwd(out.loc) 
  
  # AREA PLOT -------------
  
  dt = dt_long
  xval = dt[, day]
  yval = dt[, value]
  fillval = dt[, region]
  tlab = "2007 - 2015 Daily Natural Gas Production by Shale Region"
  sublab = "Data: U.S Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "MCF/Day"
  leglab = NULL
  leg.ord = levels(fillval)
  plot.cols = reg.cols
  
  area_ng_prod_shale_region = f.areaplot(dt, xval, yval, fillval, 
                                      tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
    scale_x_date(breaks = seq(as.Date("2006-01-01"), as.Date("2016-01-01"), by = "2 years"), date_labels = "%b %Y", expand = c(0,0)) +
    scale_y_comma(breaks = seq(0,50000000,5000000), expand = c(0,0))
  
  ggsave(area_ng_prod_shale_region, 
         filename = "NG_Natural Gas Production by Shale Region_2007-2015_ATS.png", 
         width = 11.1, 
         height = 6.25, 
         dpi = 400)
  
  # LINE PLOT -------------
  
  dt = dt_long
  xval = dt[, day]
  yval = dt[, value]
  fillval = dt[, region]
  tlab = "2007 - 2015 Daily Natural Gas Production by Shale Region"
  sublab = "Data: U.S Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "MCF/Day"
  leglab = NULL
  leg.ord = levels(with(dt[year(day) == "2015"], reorder(region, -value)))
  plot.cols = reg.cols
  
  line_ng_prod_shale_region = f.lineplot(dt, xval, yval, fillval, 
                                          tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
    scale_x_date(breaks = seq(as.Date("2006-01-01"), as.Date("2016-01-01"), by = "2 years"), date_labels = "%b %Y", expand = c(0,0)) +
    scale_y_comma(breaks = seq(0,18000000,3000000), limits = c(0,18000000), expand = c(0,0))
  
  ggsave(line_ng_prod_shale_region, 
         filename = "NG_Natural Gas Production by Shale Region_2007-2015_LTS.png", 
         width = 11.1, 
         height = 6.25, 
         dpi = 400)
  
  # SEGMENT PLOT -------------
  
  dt = dt_long
  xval = dt[, day]
  yval = dt[, value]
  fillval = dt[, region]
  wsize = 2
  csize = 1
  tlab = "2007 - 2015 Daily Natural Gas Production by Shale Region"
  sublab = "Data: U.S Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "MCF/Day"
  leglab = NULL
  leg.ord = levels(with(dt[year(day) == "2015"], reorder(region, -value)))
  plot.cols = reg.cols
  
  seg_ng_prod_shale_region = f.segplot(dt, xval, yval, fillval, wsize, csize, 
                                        tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
    scale_x_date(breaks = seq(as.Date("2006-01-01"), as.Date("2016-01-01"), by = "2 years"), date_labels = "%b %Y", expand = c(0,0)) +
    scale_y_comma(breaks = seq(0,18000000,3000000), limits = c(0,18000000), expand = c(0,0))
  
  ggsave(seg_ng_prod_shale_region, 
         filename = "NG_Natural Gas Production by Shale Region_2007-2015_STS.png", 
         width = 11.1, 
         height = 6.25, 
         dpi = 400)
  