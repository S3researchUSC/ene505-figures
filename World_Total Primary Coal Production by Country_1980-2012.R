# World_Total Primary Coal Production by Country_1980-2012 #

# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

file.loc 	    = '/Users/MEAS/GitHub/ene505-figures' # location of scripts
data.loc      = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts' # location of data file(s)
data.file     = 'World_Total Primary Coal Production by Country_1980_2012.csv' # data file to be used
out.loc       = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts/20170827' # location of where to save figures
ran.cols        = c("#392a4b",
                    "#6ed24b",
                    "#b84cd8",
                    "#cbd349",
                    "#5434a9",
                    "#73d492",
                    "#d24ba6",
                    "#5d7c38",
                    "#7373ce",
                    "#c9923b",
                    "#80386b",
                    "#c9c995",
                    "#d0496a",
                    "#7ec9c3",
                    "#d15031",
                    "#6f93b9",
                    "#733a2b",
                    "#cf99c9",
                    "#40513e",
                    "#c79487")

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
  dt_raw <- fread(data.file, skip = 2, header = T)[1:20, c(1, 3:35), with = FALSE]

# melt data table from wide to long format -----
  dt_long <- melt(dt_raw, measure.vars = colnames(dt_raw)[2:34],
                  variable.name = "year", value.name = "value")
  colnames(dt_long)[1] <- "country"

# convert value into numeric -----
  dt_long[, year := as.numeric(as.character(year))]
  dt_long[, value := as.numeric(value)]

# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------

  setwd(out.loc) 
  
  # AREA PLOT -------------
  
  dt <- copy(dt_long)[, country := factor(country, levels(with(dt_long[year == "2012"],
                                                         reorder(country, -value))))]
  xval = dt[, year]
  yval = dt[, value]/1000
  fillval = dt[, country]
  tlab = "1980 - 2012 Total Primary Coal Production by Country"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Short Ton (Millions)"
  leglab = ""
  leg.ord = levels(factor(fillval))
  plot.cols = ran.cols
  
  area_world_coal_prod = f.areaplot(dt, xval, yval, fillval, tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
    scale_x_continuous(breaks = seq(1979,2012,3), expand = c(0,0)) +
    scale_y_comma(breaks = seq(0,9000,1000), expand = c(0,0))
  
  ggsave(area_world_coal_prod, 
         filename = "World_Total Primary Coal Production by Country_1980-2012_ATS.png", 
         width = 11.1, 
         height = 6.25, 
         dpi = 400)
  
  # LINE PLOT -------------
  
  dt <- copy(dt_long)[, country := factor(country, levels(with(dt_long[year == "2012"],
                                                         reorder(country, -value))))]
  xval = dt[, year]
  yval = dt[, value]/1000
  fillval = dt[, country]
  tlab = "1980 - 2012 Total Primary Coal Production by Country"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Short Ton (Millions)"
  leglab = ""
  leg.ord = levels(with(dt[year == "2014"], reorder(country, -value)))
  plot.cols = ran.cols
  
  line_world_coal_prod = f.lineplot(dt, xval, yval, fillval, tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
    scale_x_continuous(breaks = seq(1979,2012,3), expand = c(0,0)) +
    scale_y_comma(breaks = seq(0,4500,500), expand = c(0,0), limits = c(0, 4500))
  
  ggsave(line_world_coal_prod, 
         filename = "World_Total Primary Coal Production by Country_1980-2012_LTS.png", 
         width = 11.1, 
         height = 6.25, 
         dpi = 400)
  