# Energy_Transportation_Transporation Sector Consumption by Source_1949-2014 #

# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

file.loc 	  = '/Users/MEAS/GitHub/ene505-figures' # location of scripts
data.loc      = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts' # location of data file(s)
data.file     = 'Energy_Transporation Sector Consumption by Source_1949_2014.csv' # data file to be used
out.loc       = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts/20170827' # location of where to save figures
source.cols   = c("Electrical System Losses" = "#1effd2",
                  "Electricity Retail Sales" = "#ff00de",
                  "Biomass" = "#b5734e",
                  "Petroleum" = "#fff200",
                  "Coal" = "#000000",
                  "Natural Gas" = "#ed1c24")

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
                        "Coal",
                        "Natural Gas",
                        "Petroleum",
                        "Total Fossil Fuels",
                        "Biomass",
                        "Total Primary Energy",
                        "Electricity Retail Sales",
                        "Electrical System Losses",
                        "Total Energy")
  
# melt data table from wide to long format -----
  dt_long = melt(dt_raw, measure.vars = colnames(dt_raw)[2:10],
                 variable.name = "MSN", value.name = "Value")
  
# convert value to numeric ------
  dt_long[, Year := as.numeric(Year)]
  dt_long[, Value := as.numeric(Value)]
  
# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------
  
  setwd(out.loc) 
  
  # AREA PLOT -------------
  
  dt = dt_long[, MSN := factor(MSN, levels = rev(levels(factor(dt_long[, MSN]))))][! MSN %in% c("Total Energy", "Total Primary Energy", "Total Fossil Fuels")]
  xval = dt[, Year]
  yval = dt[, Value]
  fillval = dt[, MSN]
  tlab = "1949 - 2014 Annual Primary Energy Consumption for Transportation by Source"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "BTU (Trillions)"
  leglab = ""
  leg.ord = levels(factor(fillval))
  plot.cols = source.cols
  
  area_transport_cons_source_1949_2014 = f.areaplot(dt, xval, yval, fillval, 
                                                    tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
    scale_x_continuous(breaks = seq(1945,2014,10), expand = c(0,0)) +
    scale_y_comma(breaks = seq(0,30000,5000), expand = c(0,0))
  
  ggsave(area_transport_cons_source_1949_2014, 
         filename = "Energy_Transporation Sector Consumption by Source_1949-2014_ATS.png", 
         width = 11.1, 
         height = 6.25, 
         dpi = 400)
  
  # LINE PLOT -------------
  
    dt = dt_long[! MSN %in% c("Total Energy", "Total Primary Energy", "Total Fossil Fuels")]
    xval = dt[, Year]
    yval = dt[, Value]
    fillval = dt[, MSN]
    tlab = "1949 - 2014 Annual Primary Energy Consumption for Transportation by Source"
    sublab = "Data: U.S. Energy Information Administration"
    gval = "Y"
    xlab = NULL
    ylab = "BTU (Trillions)"
    leglab = ""
    leg.ord = levels(with(dt[Year == "2014"], reorder(MSN, -Value))) # reorder legend of line plot to order 2014 values from least to greatest
    plot.cols = source.cols
    
    line_transport_cons_source_1949_2014 = f.lineplot(dt, xval, yval, fillval, 
                                                      tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
      scale_x_continuous(breaks = seq(1945,2014,10), expand = c(0,0)) +
      scale_y_comma(breaks = seq(0,30000,5000), expand = c(0,0), limits = c(0, 30000))
    
    ggsave(line_transport_cons_source_1949_2014, 
           filename = "Energy_Transporation Sector Consumption by Source_1949-2014_LTS.png", 
           width = 11.1, 
           height = 6.25, 
           dpi = 400)
  
  # SEGMENT PLOT -------------
  
    dt = dt_long[! MSN %in% c("Total Energy", "Total Primary Energy", "Total Fossil Fuels")]
    xval = dt[, Year]
    yval = dt[, Value]
    fillval = dt[, MSN]
    wsize = 3
    csize = 2
    tlab = "1949 - 2014 Annual Primary Energy Consumption for Transportation by Source"
    sublab = "Data: U.S. Energy Information Administration"
    gval = "Y"
    xlab = NULL
    ylab = "BTU (Trillions)"
    leglab = ""
    leg.ord = levels(with(dt[Year == "2014"], reorder(MSN, -Value))) # reorder legend of line plot to order 2014 values from least to greatest
    plot.cols = source.cols
    
    seg_transport_cons_source_1949_2014 = f.segplot(dt, xval, yval, fillval, wsize, csize, 
                                                    tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
      scale_x_continuous(breaks = seq(1945,2014,10), expand = c(0,0)) +
      scale_y_comma(breaks = seq(0,40000,5000), expand = c(0,0), limits = c(0, 40000))
    
    ggsave(seg_transport_cons_source_1949_2014, 
           filename = "Energy_Transporation Sector Consumption by Source_1949-2014_STS.png", 
           width = 11.1, 
           height = 6.25, 
           dpi = 400)
    