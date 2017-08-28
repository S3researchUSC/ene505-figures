# Energy_Primary Energy Consumption per Capita_1980-2011 #

# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

file.loc 	    = '/Users/MEAS/GitHub/ene505-figures' # location of scripts
data.loc      = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts' # location of data file(s)
data.file     = 'Energy_Primary Energy Consumption per Capita_1980_2011.csv' # data file to be used
out.loc       = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts/20170827' # location of where to save figures
reg.cols      = c("#e41a1c",
                  "#377eb8",
                  "#4daf4a",
                  "#984ea3",
                  "#ff7f00",
                  "#ffff33",
                  "#a65628",
                  "#f781bf",
                  "#999999")

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
  dt_raw = fread(data.file, skip = 2, header = T)[2:10]
  dt_raw[, V2 := NULL]

# rename columns ------
  colnames(dt_raw)[1] <- c("Region")

# melt data table from wide to long format -----
  dt_long = melt(dt_raw, measure.vars = colnames(dt_raw)[2:33],
                 variable.name = "Year", value.name = "Value")

# convert value to numeric ------
  dt_long[, Year := as.numeric(as.character(Year))]
  dt_long[, Value := as.numeric(Value)]
  
# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------
  
  setwd(out.loc) 
  names(reg.cols) = levels(factor(dt_long[, Region]))
  
  # AREA PLOT -------------
  
    dt = dt_long
    xval = dt[, Year]
    yval = dt[, Value]
    fillval = dt[, Region]
    tlab = "1980 - 2011 Primary Energy Consumption per Capita by Region"
    sublab = "Data: U.S. Energy Information Administration"
    gval = "Y"
    xlab = ""
    ylab = "MMBTU per Person"
    leglab = ""
    leg.ord = levels(factor(fillval))
    plot.cols = reg.cols
    
    area_energy_cons_percapita_region_1980_2011 = f.areaplot(dt, xval, yval, fillval, tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
      scale_x_continuous(breaks = seq(1980,2011,10), expand = c(0,0)) +
      scale_y_comma(breaks = seq(0,1250,250), expand = c(0,0))
    
    ggsave(area_energy_cons_percapita_region_1980_2011, 
           filename = "Energy_Primary Energy Consumption per Capita by Region_ATS.png", 
           width = 14.5, 
           height = 8.16, 
           dpi = 400)
  
  # LINE PLOT -------------
  
    dt = dt_long
    xval = dt[, Year]
    yval = dt[, Value]
    fillval = dt[, Region]
    tlab = "1980 - 2011 Primary Energy Consumption per Capita by Region"
    sublab = "Data: U.S. Energy Information Administration"
    gval = "Y"
    xlab = ""
    ylab = "MMBTU per Person"
    leglab = ""
    leg.ord = levels(with(dt[Year == "2011"], reorder(Region, -Value)))
    plot.cols = reg.cols

    line_energy_cons_percapita_region_1980_2011 = f.lineplot(dt, xval, yval, fillval, tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
      scale_x_continuous(breaks = seq(1980,2011,10), expand = c(0,0)) +
      scale_y_comma(breaks = seq(0,400,50), expand = c(0,0), limits = c(0, 400))
    
    ggsave(line_energy_cons_percapita_region_1980_2011, 
           filename = "Energy_Primary Energy Consumption per Capita by Region_LTS.png", 
           width = 14.5, 
           height = 8.16, 
           dpi = 400)
  
  # SEGMENT PLOT -------------
  
    dt = dt_long
    xval = dt[, Year]
    yval = dt[, Value]
    fillval = dt[, Region]
    tlab = "1980 - 2011 Primary Energy Consumption per Capita by Region"
    sublab = "Data: U.S. Energy Information Administration"
    gval = "Y"
    wsize = 4
    csize = 2
    xlab = ""
    ylab = "MMBTU per Person"
    leglab = ""
    leg.ord = levels(with(dt[Year == "2011"], reorder(Region, -Value)))
    plot.cols = reg.cols
    
    seg_energy_cons_percapita_region_1980_2011 = f.segplot(dt, xval, yval, fillval, wsize, csize, tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
      scale_x_continuous(breaks = seq(1980,2011,10), expand = c(0,0)) +
      scale_y_comma(breaks = seq(0,400,50), expand = c(0,0), limits = c(0, 400))
  
  ggsave(seg_energy_cons_percapita_region_1980_2011, 
         filename = "Energy_Primary Energy Consumption per Capita by Region_STS.png", 
         width = 14.5, 
         height = 8.16, 
         dpi = 400)
