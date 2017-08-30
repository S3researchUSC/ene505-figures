# World_Hydroelectricity Net Generation by Country_1980-2012 #

# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

file.loc 	    = '/Users/MEAS/GitHub/ene505-figures' # location of scripts
data.loc      = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts' # location of data file(s)
data.file     = 'World_Hydroelectric Net Generation by Country_1980_2012.csv' # data file to be used
out.loc       = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts/20170827' # location of where to save figures
# country.cols  = c("#ced049",
#                   "#6a41c9",
#                   "#79d645",
#                   "#c74dca",
#                   "#73d78b",
#                   "#5b2d76",
#                   "#5d8639",
#                   "#c54587",
#                   "#75cabd",
#                   "#d24e29",
#                   "#6f78ce",
#                   "#c38a39",
#                   "#83a7c7",
#                   "#ce4958",
#                   "#c9c994",
#                   "#494560",
#                   "#c9948a",
#                   "#46573b",
#                   "#cb8dc5",
#                   "#733930")
country.cols    = c("aquamarine3",
                    "azure4",
                    "brown3",
                    "burlywood2",
                    "cadetblue3",
                    "darkgoldenrod1",
                    "chocolate3",
                    "chartreuse3",
                    "coral2",
                    "darkgreen",
                    "cornflowerblue",
                    "pink1",
                    "darkmagenta",
                    "darkcyan",
                    "darkorange1",
                    "yellow1",
                    "slategray4",
                    "darkred",
                    "darksalmon",
                    "darkseagreen3"
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
  dt_raw <- fread(data.file, skip = 2, header = T)[2:21]
  dt_raw <- dt_raw[, c(1,3:35), with = FALSE] # remove second column (empty)

# rename first column -----
  colnames(dt_raw)[1] <- "country"
  
# melt data table from wide to long format -----
  dt_long = melt(dt_raw, measure.vars = colnames(dt_raw)[2:34],
                 variable.name = "year", value.name = "value")
  
# convert value and year columns to numeric -----
  dt_long[, value := as.numeric(value)]
  dt_long[, year := as.numeric(as.character(year))]
  
# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------
  
  setwd(out.loc) 
  
  # AREA PLOT -------------
  
  dt = dt_long[, country := factor(country, levels(with(dt_long[year == "2012"], reorder(country, -value))))]
  xval = dt[, year]
  yval = dt[, value]
  fillval = dt[, country]
  tlab = "1980 - 2012 Annual Hydroelectricity Net Generation by Country"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Billion KWh"
  leglab = ""
  leg.ord = levels(factor(fillval))
  plot.cols = country.cols
  
  area_world_hydro_netgen = f.areaplot(dt, xval, yval, fillval, tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
    scale_x_continuous(breaks = seq(1978,2012,4), expand = c(0,0)) +
    scale_y_comma(breaks = seq(0,3000,500), expand = c(0,0))
  
  ggsave(area_world_hydro_netgen, 
         filename = "World_Hydroelectricity Net Generation by Country_1980-2012_ATS.png", 
         width = 11.1, 
         height = 6.25, 
         dpi = 400)
  
  # LINE PLOT -------------
  
  dt = dt_long
  xval = dt[, year]
  yval = dt[, value]
  fillval = dt[, country]
  tlab = "1980 - 2012 Annual Hydroelectricity Net Generation by Country"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Billion KWh"
  leglab = ""
  leg.ord = levels(with(dt[year == "2012"], reorder(country, -value)))
  plot.cols = country.cols
  
  line_world_hydro_netgen = f.lineplot(dt, xval, yval, fillval, tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
    scale_x_continuous(breaks = seq(1978,2012,4), expand = c(0,0)) +
    scale_y_continuous(breaks = seq(0,900,100), expand = c(0,0), limits = c(0, 900))
  
  ggsave(line_world_hydro_netgen, 
         filename = "World_Hydroelectricity Net Generation by Country_1980-2012_LTS.png", 
         width = 11.1, 
         height = 6.25, 
         dpi = 400)
  
  # SEGMENT PLOT -------------
  
  dt = dt_long
  xval = dt[, year]
  yval = dt[, value]
  fillval = dt[, country]
  wsize = 3
  csize = 2
  tlab = "1980 - 2012 Annual Hydroelectricity Net Generation by Country"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Billion KWh"
  leglab = ""
  leg.ord = levels(with(dt[year == "2012"], reorder(country, -value)))
  plot.cols = country.cols
  
  seg_world_hydro_netgen = f.segplot(dt, xval, yval, fillval, wsize, csize, 
                                              tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
    scale_x_continuous(breaks = seq(1978,2012,4), expand = c(0,0)) +
    scale_y_continuous(breaks = seq(0,900,100), expand = c(0,0), limits = c(0, 900))
  
  ggsave(seg_world_hydro_netgen, 
         filename = "World_Hydroelectricity Net Generation by Country_1980-2012_STS.png", 
         width = 11.1, 
         height = 6.25, 
         dpi = 400)
  