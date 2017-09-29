# Electricity_US_Net Generation by Energy Source in 2016 #

# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

file.loc 	    = '/Users/MEAS/GitHub/ene505-figures' # location of scripts
data.loc      = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts/data_2017' # location of data file(s)
data.file     = 'chart123.csv' # data file to be used
out.loc       = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts/20170929' # location of where to save figures
source.cols   = c("Other Gases" = "#e9d8ff",
                  "Other" = "#1effd2",
                  "Petroleum" = "#fff200",
                  "Hydroelectric" = "#00baff",
                  "Solar" = "#ff7f27",
                  "Wind" = "#3f48cc",
                  "Wood" = "#b5734e",
                  "Waste" = "#c3c3c3",
                  "Geothermal" = "#880015",
                  "Nuclear" = "#22b14c",
                  "Natural Gas" = "#ed1c24",
                  "Coal" = "#000000"
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
  dt_raw = fread(data.file, header = T, skip = 23)[2]

# # only keep 2014 data, and remove the 2nd and 3rd ------
#   dt_2014 = dt_raw[ Period == "2014" ][1]

# rename columns ------
  colnames(dt_raw) <- c("Year",
                      "Coal",
                      "Petroleum",
                      "Natural Gas",
                      "Other Gases",
                      "Nuclear",
                      "Hydroelectric Pumped Storage",
                      "Hydroelectric",
                      "Wood",
                      "Waste",
                      "Geothermal",
                      "Solar",
                      "Wind")

# melt from wide to long -----
  dt_long = melt(dt_raw, measure.vars = c(2:13),
                 variable.name = "MSN", value.name = "value")
  
# convert value to numeric ------
  dt_long[, value := as.numeric(value)]
  
# remove pumped storage -------
  dt_long <- dt_long[ ! MSN == "Hydroelectric Pumped Storage"]
# # create table for source ------
#   dt_long[, source := MSN]
#   dt_long[ MSN %like% "Petroleum", source := "Petroleum" ]
#   dt_long[ MSN %like% "Hydroelectric", source := "Hydroelectric" ]
#   dt_long = dt_long[, .(value = sum(value)), by = c("source")] # aggregate (sum) data by "source

# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------

setwd(out.loc) 

# BAR PLOT -------------
  
  dt = copy(dt_long)
  xval = dt[, reorder(MSN, value)]
  yval = dt[, value]/1000
  fillval = dt[, MSN]
  tlab = "2016 U.S. Annual Electricity Net Generation by Source"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "X"
  xlab = NULL
  ylab = "kWh (Billions)"
  leglab = ""
  plot.cols = source.cols
  
  bar_elec_netgen_2016 = f.barplot(dt, xval, yval, fillval, 
                                   tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
    coord_flip() + 
    scale_y_comma(breaks = seq(0,1500,200), expand = c(0,0)) +
    guides(fill=FALSE)
  
  ggsave(bar_elec_netgen_2016, 
         filename = "Electricity_US_Net Generation by Energy Source in 2016_BP.png", 
         width = 11.1, 
         height = 6.25, 
         dpi = 400)
  