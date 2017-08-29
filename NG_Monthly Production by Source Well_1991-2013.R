# NG_Monthly Production by Source Well_1991-2013 #

# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

file.loc 	  = '/Users/MEAS/GitHub/ene505-figures' # location of scripts
data.loc      = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts' # location of data file(s)
data.file     = c('NG_PROD_SUM_DCU_NUS_M.csv') # data file to be used
out.loc       = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts/20170827' # location of where to save figures
source.cols   = c("Gas Wells" = "lightcoral",
                  "Oil Wells" = "sienna4",
                  "Shale Gas" = "seagreen3",
                  "Coalbed" = "gray15")

# ---------------------------------------------------------------
# MAIN SCRIPT ---------------------------------------------------
# ---------------------------------------------------------------

# ---------------------------------------------------------------
# MAIN SCRIPT ---------------------------------------------------
# ---------------------------------------------------------------

# load libraries -------
  library(data.table)
  library(ggplot2)
  library(hrbrthemes)
  library(stringr)
  library(plyr)
  library(lubridate)

# load plot functions -----
  setwd(file.loc)
  source("plotfunctions.R")

# load data ------
  setwd(data.loc)
  dt_raw = fread(data.file, skip = 2, header = T)

# remove unused columns -----
  dt_raw = dt_raw[, c(1,3:6), with = FALSE]
  
# rename columns -----
  colnames(dt_raw) = c("Date", "Gas Wells", "Oil Wells", "Shale Gas", "Coalbed")

# change date column type ------
  dt_raw[, Date := as.Date(paste("01", Date, sep="-"), "%d-%b-%Y")]
  dt_raw[, Date := as.Date(Date, "%m/%d/%Y")]

# convert data table from wide to long ------
  dt_long = melt(dt_raw, measure.vars = list(2:5), variable.name = "source", value.name = "value")
  
# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------
  
  setwd(out.loc) 
  
  # AREA PLOT -------------
  
  dt = dt_long
  xval = dt[, Date]
  yval = dt[, value]/1000
  fillval = dt[, source]
  tlab = "1991 - 2013 Monthly Natural Gas Production by Source Well"
  sublab = "Data: EIA Annual Energy Review"
  gval = "Y"
  xlab = ""
  ylab = "Billion Cubic Feet (BCF)"
  leglab = ""
  leg.ord = levels(fillval)
  plot.cols = source.cols
  
  area_ng_prod_well_1991_2013 = f.areaplot(dt, xval, yval, fillval, tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
    scale_x_date(breaks = seq(as.Date("1990-01-01"), as.Date("2015-01-01"), by = "5 years"), 
                 limits = c(as.Date("1990-12-01"), as.Date("2014-01-01")), 
                 date_labels = "%Y", expand = c(0,0)) +
    scale_y_comma(breaks = seq(0,3000,500), expand = c(0,0))
  
  ggsave(area_ng_prod_well_1991_2013, 
         filename = "NG_Monthly Production by Source Well_1991-2013_ATS.png", 
         width = 11.1, 
         height = 6.25, 
         dpi = 400)
  
  # LINE PLOT -------------
  
  dt = dt_long 
  xval = dt[, Date]
  yval = dt[, value]/1000
  fillval = dt[, source]
  tlab = "1991 - 2013 Monthly Natural Gas Production by Source Well"
  sublab = "Data: EIA Annual Energy Review"
  gval = "Y"
  xlab = ""
  ylab = "Billion Cubic Feet (BCF)"
  leglab = ""
  leg.ord = levels(with(dt[year(Date) == "2013"], reorder(source, -value))) # reorder legend of line plot to order 2014 values from least to greatest
  plot.cols = source.cols
  
  line_ng_prod_well_1991_2013 = f.lineplot(dt, xval, yval, fillval, tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
    scale_x_date(breaks = seq(as.Date("1990-01-01"), as.Date("2015-01-01"), by = "5 years"), limits = c(as.Date("1990-12-01"), as.Date("2014-01-01")), date_labels = "%Y", expand = c(0,0)) +
    scale_y_comma(breaks = seq(0,1600,400), expand = c(0,0), limits = c(0, 1600))
  
  ggsave(line_ng_prod_well_1991_2013, 
         filename = "NG_Monthly Production by Source Well_1991-2013_LTS.png", 
         width = 11.1, 
         height = 6.25, 
         dpi = 400)
  
  # SEGMENT PLOT -------------
  
  dt = dt_long 
  xval = dt[, Date]
  yval = dt[, value]/1000
  fillval = dt[, source]
  wsize = 2
  csize = 1
  tlab = "1991 - 2013 Monthly Natural Gas Production by Source Well"
  sublab = "Data: EIA Annual Energy Review"
  gval = "Y"
  xlab = ""
  ylab = "Billion Cubic Feet (BCF)"
  leglab = ""
  leg.ord = levels(with(dt[year(Date) == "2013"], reorder(source, -value))) # reorder legend of line plot to order 2014 values from least to greatest
  plot.cols = source.cols
  
  seg_ng_prod_well_1991_2013 = f.segplot(dt, xval, yval, fillval, wsize, csize, tlab, 
                                        sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols)  + 
    scale_x_date(breaks = seq(as.Date("1990-01-01"), as.Date("2015-01-01"), by = "5 years"), 
                 limits = c(as.Date("1990-12-01"), as.Date("2014-01-01")), 
                 date_labels = "%Y", expand = c(0,0)) +
    scale_y_comma(breaks = seq(0,1600,400), expand = c(0,0), limits = c(0, 1600))
  
  
  ggsave(seg_ng_prod_well_1991_2013, 
         filename = "NG_Monthly Production by Source Well_1991-2013_STS.png", 
         width = 11.1, 
         height = 6.25, 
         dpi = 400)
  