# Primary Energy Consumption by Source (2014) #

# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

file.loc 	= '/Users/MEAS/GitHub/ene505-figures' # location of scripts
data.loc    = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts' # location of data file(s)
data.file   = 'Energy_1.3_Primary Energy Consumption by Source_2014.csv' # data file to be used
out.loc   = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts/20170827' # location of where to save figures
fuel.cols   = c("Geothermal" = "#880015",
                "Solar PV" = "#ff7f27",
                "Wind" = "#3f48cc",
                "Hydroelectric" = "#00baff",
                "Biomass" = "#b5734e",
                "Nuclear" = "#22b14c",
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

# import data (skipping the first 10 rows) ------
setwd(data.loc)
dt_raw = fread(data.file, skip = 10, header = T)[2]

# rename columns ------
colnames(dt_raw) <- c("Year",
                      "Coal",
                      "Natural Gas",
                      "Petroleum",
                      "Total Fossil Fuels",
                      "Nuclear",
                      "Hydroelectric",
                      "Geothermal",
                      "Solar PV",
                      "Wind",
                      "Biomass",
                      "Total Renewable",
                      "Total Primary")

# convert data table from wide to long ------
  dt_long = melt(dt_raw, measure.vars = list(2:13), variable.name = "source", value.name = "value")
  dt_long[, value := as.numeric(value)]
  dt_long[, units := "Quadrillion Btu"] # create column for units


# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------

setwd(out.loc) 

  # BAR PLOT ---------------------------------------------------
  
    dt = dt_long[ ! source %in% c("Total Renewable", "Total Primary", "Total Fossil Fuels")]
    xval = dt[, reorder(source, value)]
    yval = dt[, value]
    fillval = dt[,source]
    tlab = "2014 U.S. Annual Primary Energy Consumption by Source"
    sublab = "Data: U.S. Energy Information Administration"
    gval = "X"
    xlab = ""
    ylab = "Btu (Quads)"
    leglab = ""
    leg.ord = levels(with(dt, reorder(source, -value)))
    plot.cols = fuel.cols
    
    bar_energy_cons_2014 = f.barplot(dt, xval, yval, fillval, tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
      coord_flip() + 
      scale_y_continuous(breaks = seq(0,35,5), expand = c(0,0)) +
      guides(fill=FALSE)
    
    
    ggsave(bar_energy_cons_2014, 
           filename = "Energy_1.3_Primary Energy Consumption by Source in 2014_BP.png", 
           width = 14.5, 
           height = 8.16, 
           dpi = 400)
    
