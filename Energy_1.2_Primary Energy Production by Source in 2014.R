# Energy_1.2_Primary Energy Production by Source_2014 #

# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

file.loc 	= '/Users/MEAS/GitHub/ene505-figures' # location of scripts
data.loc    = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts' # location of data file(s)
data.file   = 'Energy_1.2_Primary Energy Production by Source_2014.csv' # data file to be used
fuel.cols   = c("Geothermal" = "#880015",
               "Solar PV" = "#ff7f27",
               "Wind" = "#3f48cc",
               "Hydroelectric" = "#00baff",
               "Biomass" = "#b5734e",
               "Nuclear" = "#22b14c",
               "Crude Oil" = "#fff200",
               "Coal" = "#000000",
               "Natural Gas" = "#ed1c24")
out.loc   = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts/20170827' # location of where to save figures

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
  dt_raw = fread(data.file, skip = 10, header = T)

# rename columns ------
  colnames(dt_raw) <- c("Year",
                       "Coal",
                       "Natural Gas (Dry)",
                       "Crude Oil",
                       "Natural Gas (Liquid)",
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
  dt_long = melt(dt_raw, measure.vars = list(2:14), variable.name = "fuel", value.name = "value")
  dt_long[, value := as.numeric(value)]
  dt_long[, units := "Quadrillion Btu"] # create column for units
  dt_long = dt_long[! value == "(Quadrillion Btu)"] # remove rows with "(Quadrillion Btu)" as values

# sum natural gas values ------
  dt_long[, source := fuel] # create new column named "source"
  dt_long[ fuel %like% "Natural Gas", source := "Natural Gas"] # for natural gas fuels, make them have the same "source"
  dt_long = dt_long[, .(value = sum(value)), by = c("source")] # aggregate (sum) data by "source


  
  
# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------

setwd(out.loc) 

  dt = dt_long[ ! source %in% c("Total Renewable", "Total Primary", "Total Fossil Fuels")]
  xval = dt[, reorder(source, value)]
  yval = dt[, value]
  fillval = dt[,source]
  tlab = "2014 U.S. Primary Energy Production by Source"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "X"
  xlab = NULL
  ylab = "Btu (Quads)"
  leglab = ""
  leg.ord = levels(with(dt, reorder(source, -value)))
  plot.cols = fuel.cols

  bar_PE_prod_2014 = f.barplot(dt, xval, yval, fillval, tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
    coord_flip() + 
    scale_y_continuous(breaks = seq(0,30,5), expand = c(0,0)) +
    guides(fill=FALSE)
  
  
  ggsave(bar_PE_prod_2014, 
         filename = "Energy_1.2_Primary Energy Production by Source in 2014_BP.png", 
         width = 11.1, 
         height = 6.25, 
         dpi = 400)
