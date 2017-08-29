# Electricity_US_Net Generation by Energy Source in 2014 #

# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

file.loc 	    = '/Users/MEAS/GitHub/ene505-figures' # location of scripts
data.loc      = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts' # location of data file(s)
data.file     = 'Electricity_Net Generation by Energy Source_2014.csv' # data file to be used
out.loc       = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts/20170827' # location of where to save figures
source.cols   = c("Other Gas" = "#c3c3c3",
                  "Other" = "#1effd2",
                  "Petroleum" = "#fff200",
                  "Hydroelectric" = "#00baff",
                  "Non-Hydro Renewables" = "#ff00de",
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
  dt_raw = fread(data.file, header = T, skip = 2)

# only keep 2014 data, and remove the 2nd and 3rd ------
  dt_2014 = dt_raw[ Period == "2014" ][1]

# rename columns ------
  colnames(dt_2014) <- c("Year",
                      "Coal",
                      "Petroleum Liquids",
                      "Petroleum Coke",
                      "Natural Gas",
                      "Other Gas",
                      "Nuclear",
                      "Hydroelectric",
                      "Non-Hydro Renewables",
                      "Hydroelectric Pumped Storage",
                      "Other",
                      "Total")

# melt from wide to long -----
  dt_long = melt(dt_2014, measure.vars = colnames(dt_2014)[2:12],
                 variable.name = "MSN", value.name = "value")
  
# convert value to numeric ------
  dt_long[, value := as.numeric(gsub(",", "", value))]
  
# create table for source ------
  dt_long[, source := MSN]
  dt_long[ MSN %like% "Petroleum", source := "Petroleum" ]
  dt_long[ MSN %like% "Hydroelectric", source := "Hydroelectric" ]
  dt_long = dt_long[, .(value = sum(value)), by = c("source")] # aggregate (sum) data by "source

# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------

setwd(out.loc) 

# BAR PLOT -------------
  
  dt = dt_long[ ! source %in% c("Total")]
  xval = dt[, reorder(source, value)]
  yval = dt[, value]/1000
  fillval = dt[, source]
  tlab = "2014 U.S. Annual Electricity Net Generation by Source"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "X"
  xlab = NULL
  ylab = "MWh (Millions)"
  leglab = ""
  leg.ord = c("Coal", 
              "Natural Gas",
              "Nuclear",
              "Non-Hydro Renewables",
              "Hydroelectric",
              "Petroleum",
              "Other",
              "Other Gas")
  plot.cols = source.cols
  
  bar_elec_netgen_2014 = f.barplot(dt, xval, yval, fillval, 
                                   tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
    coord_flip() + 
    scale_y_comma(breaks = seq(0,1500,250), expand = c(0,0)) +
    guides(fill=FALSE)
  
  ggsave(bar_elec_netgen_2014, 
         filename = "Electricity_US_Net Generation by Energy Source in 2014_BP.png", 
         width = 11.1, 
         height = 6.25, 
         dpi = 400)
  