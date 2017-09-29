# Energy_1.2_Primary Energy Production by Source_2016 #

# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

file.loc 	    = '/Users/MEAS/GitHub/ene505-figures' # location of scripts
data.loc      = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts/data_2017' # location of data file(s)
data.file     = 'chart123.csv' # data file to be used
out.loc       = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts/20170929' # location of where to save figures
fuel.cols   = c("Geothermal" = "#880015",
                "Solar" = "#ff7f27",
                "Wind" = "#3f48cc",
                "Hydroelectric" = "#00baff",
                "Biomass" = "#b5734e",
                "Nuclear" = "#22b14c",
                "Crude Oil" = "#fff200",
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
  dt_raw = fread(data.file, header = T, skip = 7)[2, c(1:11), with = FALSE]

# rename columns ------
  colnames(dt_raw) <- c("Year",
                       "Coal",
                       "Natural Gas (Dry)",
                       "Crude Oil",
                       "Natural Gas (Liquid)",
                       "Nuclear",
                       "Hydroelectric",
                       "Geothermal",
                       "Solar",
                       "Wind",
                       "Biomass")

# convert data table from wide to long ------
  dt_long = melt(dt_raw, measure.vars = list(2:11), variable.name = "fuel", value.name = "value")
  dt_long[, value := as.numeric(value)]
  dt_long[, units := "Quadrillion Btu"] # create column for units

# sum natural gas values ------
  dt_long[, source := fuel] # create new column named "source"
  dt_long[ fuel %like% "Natural Gas", source := "Natural Gas"] # for natural gas fuels, make them have the same "source"
  dt_long = dt_long[, .(value = sum(value)), by = c("source")] # aggregate (sum) data by "source


  
  
# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------

setwd(out.loc) 

  dt = dt_long
  xval = dt[, reorder(source, value)]
  yval = dt[, value]
  fillval = dt[,source]
  tlab = "2016 U.S. Primary Energy Production by Source"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "X"
  xlab = NULL
  ylab = "Btu (Quads)"
  leglab = ""
  leg.ord = levels(with(dt, reorder(source, -value)))
  plot.cols = fuel.cols

  bar_PE_prod_2016 = f.barplot(dt, xval, yval, fillval, tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
    coord_flip() + 
    scale_y_continuous(breaks = seq(0,30,5), expand = c(0,0)) +
    guides(fill=FALSE)
  
  
  ggsave(bar_PE_prod_2016, 
         filename = "Energy_1.2_Primary Energy Production by Source in 2016_BP.png", 
         width = 11.1, 
         height = 6.25, 
         dpi = 400)
