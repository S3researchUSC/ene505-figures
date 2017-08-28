# Energy_Primary Energy Consumption by Sector_1949-2014 #

# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

file.loc 	  = '/Users/MEAS/GitHub/ene505-figures' # location of scripts
data.loc      = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts' # location of data file(s)
data.file     = 'Energy_Primary Energy Consumption by Sector_1949_2014.csv' # data file to be used
out.loc       = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts/20170827' # location of where to save figures
sector.cols   = c("Residential" = "#1b9e77", 
                  "Commerical" = "#d95f02",
                  "Industrial" = "#7570b3",
                  "Transportation" = "#e7298a")

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
  dt_raw = fread(data.file, skip = 10, header = T)[2:67]
  
# rename columns ------
  colnames(dt_raw) <- c("Year",
                        "Residential",
                        "Commerical",
                        "Industrial",
                        "Transportation")
  
# melt data table from wide to long format -----
  dt_long = melt(dt_raw, measure.vars = colnames(dt_raw)[2:5],
                 variable.name = "Sector", value.name = "Value")
  
# convert value to numeric ------
  dt_long[, Value := as.numeric(Value)]
  
# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------
  
  setwd(out.loc) 
  
  # AREA PLOT -------------
  
    dt = dt_long # customize reorder MSN levels for stacked area chart
    xval = dt[, Year]
    yval = dt[, Value]
    fillval = dt[, Sector]
    tlab = "1949 - 2014 U.S. Primary Energy Consumption by Sector"
    sublab = "Data: U.S. Energy Information Administration"
    gval = "Y"
    xlab = ""
    ylab = "BTU (Trillions)"
    leglab = ""
    leg.ord = levels(fillval)
    plot.cols = sector.cols
    
    area_energy_cons_sector_1945_2014 = f.areaplot(dt, xval, yval, fillval, tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
      scale_x_continuous(breaks = seq(1945,2014,10), expand = c(0,0)) +
      scale_y_comma(breaks = seq(0,100000,25000), expand = c(0,0))
    
    ggsave(area_energy_cons_sector_1945_2014, 
           filename = "Energy_Primary Energy Consumption by Sector_ATS.png", 
           width = 14.5, 
           height = 8.16, 
           dpi = 400)
  
  # LINE PLOT -------------
    
    dt = dt_long # customize reorder MSN levels for stacked area chart
    xval = dt[, Year]
    yval = dt[, Value]
    fillval = dt[, Sector]
    tlab = "1949 - 2014 U.S. Primary Energy Consumption by Sector"
    sublab = "Data: U.S. Energy Information Administration"
    gval = "Y"
    xlab = ""
    ylab = "BTU (Trillions)"
    leglab = ""
    leg.ord = levels(with(dt[Year == "2014"], reorder(Sector, -Value))) # reorder legend of line plot to order 2014 values from least to greatest
    plot.cols = sector.cols
    
    line_energy_cons_sector_1945_2014 = f.lineplot(dt, xval, yval, fillval, tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
      scale_x_continuous(breaks = seq(1945,2014,10), expand = c(0,0)) +
      scale_y_comma(breaks = seq(0,40000,5000), expand = c(0,0), limits = c(0, 40000))
    
    ggsave(line_energy_cons_sector_1945_2014, 
           filename = "Energy_Primary Energy Consumption by Sector_LTS.png", 
           width = 14.5, 
           height = 8.16, 
           dpi = 400)
    
  # SEGMENT PLOT -------------
    
    dt = dt_long # customize reorder MSN levels for stacked area chart
    xval = dt[, Year]
    yval = dt[, Value]
    fillval = dt[, Sector]
    wsize = 3
    csize = 2
    tlab = "1949 - 2014 U.S. Primary Energy Consumption by Sector"
    sublab = "Data: U.S. Energy Information Administration"
    gval = "Y"
    xlab = ""
    ylab = "BTU (Trillions)"
    leglab = ""
    leg.ord = levels(with(dt[Year == "2014"], reorder(Sector, -Value))) # reorder legend of line plot to order 2014 values from least to greatest
    plot.cols = sector.cols
    
    seg_energy_cons_sector_1945_2014 = f.segplot(dt, xval, yval, fillval, wsize, csize, tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
      scale_x_continuous(breaks = seq(1945,2014,10), expand = c(0,0)) +
      scale_y_comma(breaks = seq(0,40000,5000), expand = c(0,0), limits = c(0, 40000))
    
    ggsave(seg_energy_cons_sector_1945_2014, 
           filename = "Energy_Primary Energy Consumption by Sector_STS.png", 
           width = 14.5, 
           height = 8.16, 
           dpi = 400)
    
  