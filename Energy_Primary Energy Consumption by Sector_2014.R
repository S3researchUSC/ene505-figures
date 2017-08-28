# Energy_Primary Energy Consumption by Sector_2014 #

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
  
# only keep 2014 data ------
  dt_2014 = dt_raw[ Year == "2014" ]
  
# melt data table from wide to long format -----
  dt_long = melt(dt_2014, measure.vars = colnames(dt_2014)[2:5],
                 variable.name = "Sector", value.name = "Value")
  
# convert value to numeric ------
  dt_long[, Value := as.numeric(Value)]

# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------
  
  setwd(out.loc) 
  
  # BAR PLOT -------------
  
    dt = dt_long # customize reorder MSN levels for stacked area chart
    xval = dt[, reorder(Sector, Value)]
    yval = dt[, Value]
    fillval = dt[, Sector]
    tlab = "2014 U.S. Primary Energy Consumption by Sector"
    sublab = "Data: U.S. Energy Information Administration"
    gval = "X"
    xlab = ""
    ylab = "BTU (Trillions)"
    leglab = ""
    leg.ord = levels(with(dt, reorder(Sector, -Value)))
    plot.cols = sector.cols
  
    bar_energy_cons_sector_2014 = f.barplot(dt, xval, yval, fillval, tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
      coord_flip() + 
      scale_y_comma(breaks = seq(0,35000,5000), expand = c(0,0)) +
      guides(fill=FALSE)
    
    ggsave(bar_energy_cons_sector_2014, 
           filename = "Energy_US_Primary Energy Consumption by Sector_2014_BP.png", 
           width = 14.5, 
           height = 8.16, 
           dpi = 400)
  
  