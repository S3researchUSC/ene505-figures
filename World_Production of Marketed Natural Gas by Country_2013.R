# World_Production of Marketed Natural Gas by Country_2013 #

# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

file.loc 	    = '/Users/MEAS/GitHub/ene505-figures' # location of scripts
data.loc      = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts' # location of data file(s)
data.file     = 'World_Production of Marketed Natural Gas by Country_1990_2013.csv' # data file to be used
out.loc       = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts/20170827' # location of where to save figures
ran.cols        = c("#c88ed1",
                    "#78d851",
                    "#b948d3",
                    "#cfce48",
                    "#604ccc",
                    "#578b3f",
                    "#d1419d",
                    "#65d4a0",
                    "#59317e",
                    "#c6d19b",
                    "#d0403b",
                    "#7dbdc6",
                    "#d97b3a",
                    "#6783bc",
                    "#a38541",
                    "#422e47",
                    "#c99a9b",
                    "#3f533b",
                    "#ba4e71",
                    "#72382b")

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
  dt_raw <- fread(data.file, skip = 2, header = T)[2:21, c(1, 3:26), with = FALSE]

# melt data table from wide to long format -----
  dt_long <- melt(dt_raw, measure.vars = colnames(dt_raw)[2:25],
                  variable.name = "year", value.name = "value")
  colnames(dt_long)[1] <- "country"

# convert value into numeric -----
  dt_long[, year := as.numeric(as.character(year))]
  dt_long[, value := as.numeric(value)]
  
# only keep 2014 data ------
  dt_2013 <- dt_long[year == "2013"]
  
# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------
  
  setwd(out.loc) 
  
  # BAR PLOT -------------
  
  dt = copy(dt_2013)[, country := factor(country, levels(with(dt_2013, reorder(country, value))))]
  xval = dt[, country]
  yval = dt[, value]
  fillval = dt[, country]
  tlab = "2013 Production of Marketed Natural Gas by Country"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "X"
  xlab = NULL
  ylab = "Cubic Feet (Billions)"
  leglab = ""
  leg.ord = levels(with(dt, reorder(country, -value)))
  plot.cols = rev(ran.cols)
  
  bar_world_marketedng_prod = f.barplot(dt, xval, yval, fillval, 
                                         tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
    coord_flip() + 
    scale_y_comma(breaks = seq(0,30000,5000), expand = c(0,0), limits = c(0, 30000)) +
    guides(fill=FALSE)
  
  ggsave(bar_world_marketedng_prod, 
         filename = "World_Production of Marketed Natural Gas by Country_2013_BP.png", 
         width = 11.1, 
         height = 6.25, 
         dpi = 400)
