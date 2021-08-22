# Energy_1.3_Primary Energy Consumption Estimates by Source 1973-2016 #

# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

file.loc 	  = '/Users/MEAS/GitHub/ene505-figures' # location of scripts
data.loc      = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts/data_2017' # location of data file(s)
data.file     = 'chart789_data.csv' # data file to be used
out.loc       = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts/20170929' # location of where to save figures
source.cols   = c("Geothermal" = "#880015",
                  "Solar" = "#ff7f27",
                  "Wind" = "#3f48cc",
                  "Hydroelectric" = "#00baff",
                  "Biomass" = "#b5734e",
                  "Nuclear" = "#22b14c",
                  "Petroleum" = "#fff200",
                  "Coal" = "#000000",
                  "Natural Gas" = "#ed1c24",
                  "Total Primary Energy" = "#1b9e77",
                  "Total Fossil Fuels" = "#d95f02",
                  "Total Renewable Energy" = "#7570b3")

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
  dt_raw = fread(data.file, header = T)
  
# melt data table from wide to long format -----
  dt_long = melt(dt_raw, measure.vars = colnames(dt_raw)[2:13],
                 variable.name = "MSN", value.name = "Value")

# change date format ----
  dt_long[, YYYYMM := paste0("01 ", Month)]
  dt_long[, YYYYMM := as.Date(YYYYMM, format = "%d %Y %B")]
  
# rename MSN factor levels -------
  dt_long[, MSN := revalue(MSN, c('Biomass Energy Consumption' = "Biomass",
                                  'Coal Consumption' = "Coal",
                                  'Total Fossil Fuels Consumption' = "Total Fossil Fuels",
                                  'Geothermal Energy Consumption' = "Geothermal",
                                  'Hydroelectric Power Consumption' = "Hydroelectric",
                                  'Natural Gas Consumption (Excluding Supplemental Gaseous Fuels)' = "Natural Gas",
                                  'Nuclear Electric Power Consumption' = "Nuclear",
                                  'Petroleum Consumption (Excluding Biofuels)' = "Petroleum",
                                  'Total Renewable Energy Consumption' = "Total Renewable Energy",
                                  'Solar Energy Consumption' = "Solar",
                                  'Total Primary Energy Consumption' = "Total Primary Energy",
                                  'Wind Energy Consumption' = "Wind"))]

# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------

setwd(out.loc) 
  
  # AREA PLOT (NO TOTALS) -------------
  
    dt = copy(dt_long)[, MSN := factor(MSN, levels = c("Total Primary Energy", 
                                                   "Total Fossil Fuels",
                                                   "Total Renewable Energy",
                                                   "Biomass",
                                                   "Wind",
                                                   "Solar",
                                                   "Geothermal",
                                                   "Hydroelectric",
                                                   "Nuclear",
                                                   "Petroleum",
                                                   "Natural Gas",
                                                   "Coal"))][ ! MSN %in% c("Total Fossil Fuels",
                                                                           "Total Renewable Energy",
                                                                           "Total Primary Energy")] # customize reorder MSN levels for            stacked area chart
    xval = dt[, YYYYMM]
    yval = dt[, Value]
    fillval = dt[, MSN]
    tlab = "1973 - 2016 U.S. Monthly Primary Energy Consumption Estimates by Source"
    sublab = "Data: EIA Annual Energy Review"
    gval = "Y"
    xlab = NULL
    ylab = "Quadrillion BTU"
    leglab = ""
    leg.ord = levels(fillval)
    plot.cols = source.cols
    
    area_pe_cons_1973_2016 = f.areaplot(dt, xval, yval, fillval, 
                                        tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
      scale_x_date(date_breaks = "5 years", date_labels = "%Y", expand = c(0,1)) +
      scale_y_continuous(breaks = seq(0,10,2), expand = c(0,0))
    
    ggsave(area_pe_cons_1973_2016, 
           filename = "Energy_1.3_Primary Energy Consumption Estimates by Source_1973-2016_ATS.png", 
           width = 11.1, 
           height = 6.25, 
           dpi = 400)
  
    # LINE PLOT  (NO TOTALS) -------------
    
    dt = copy(dt_long)[ ! MSN %in% c("Total Fossil Fuels",
                                 "Total Renewable Energy",
                                 "Total Primary Energy")] 
    xval = dt[, YYYYMM]
    yval = dt[, Value]
    fillval = dt[, MSN]
    tlab = "1973 - 2016 U.S. Monthly Primary Energy Consumption Estimates by Source"
    sublab = "Data: EIA Annual Energy Review"
    gval = "Y"
    xlab = NULL
    ylab = "Quadrillion BTU"
    leglab = ""
    leg.ord = levels(with(dt[YYYYMM == "2017-04-01"], reorder(MSN, -Value))) # reorder legend of line plot to order 2014 values from least to             greatest
    plot.cols = source.cols
    
    line_energy_cons_1973_2016 = f.lineplot(dt, xval, yval, fillval, 
                                            tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
      scale_x_date(date_breaks = "5 years", date_labels = "%Y", expand = c(0,1)) +
      scale_y_continuous(breaks = seq(0,4,0.5), expand = c(0,0), limits = c(0, 4))
    
    ggsave(line_energy_cons_1973_2016, 
           filename = "Energy_1.3_Primary Energy Consumption Estimates by Source_1973-2016_LTS.png", 
           width = 11.1, 
           height = 6.25, 
           dpi = 400)

    # AREA PLOT (TOTALS) -------------
    
    dt = copy(dt_long)[, MSN := factor(MSN, levels = c("Total Primary Energy", 
                                                       "Total Fossil Fuels",
                                                       "Total Renewable Energy",
                                                       "Biomass",
                                                       "Wind",
                                                       "Solar",
                                                       "Geothermal",
                                                       "Hydroelectric",
                                                       "Nuclear",
                                                       "Petroleum",
                                                       "Natural Gas",
                                                       "Coal"))][ MSN %in% c("Total Renewable Energy",
                                                                             "Total Fossil Fuels")] # customize reorder MSN levels for       stacked area chart
    xval = dt[, YYYYMM]
    yval = dt[, Value]
    fillval = dt[, MSN]
    tlab = "1973 - 2016 U.S. Monthly Primary Energy Consumption by Source"
    sublab = "Data: EIA Annual Energy Review"
    gval = "Y"
    xlab = NULL
    ylab = "Quadrillion BTU"
    leglab = ""
    leg.ord = levels(fillval)
    plot.cols = source.cols
    
    area_pe_cons_1973_2016_totals = f.areaplot(dt, xval, yval, fillval, tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
      scale_x_date(date_breaks = "5 years", date_labels = "%Y", expand = c(0,1)) +
      scale_y_continuous(limit = c(0,10), breaks = seq(0,10,2), expand = c(0,0))
    
    ggsave(area_pe_cons_1973_2016_totals, 
           filename = "Energy_1.3_Primary Energy Consumption Estimates by Source_Total_1973-2016_ATS.png", 
           width = 11.1, 
           height = 6.25, 
           dpi = 400)
    
    # LINE PLOT  (TOTALS) -------------
    
    dt = copy(dt_long)[ MSN %in% c("Total Fossil Fuels",
                                   "Total Renewable Energy",
                                   "Total Primary Energy")] 
    xval = dt[, YYYYMM]
    yval = dt[, Value]
    fillval = dt[, MSN]
    tlab = "1973 - 2016 U.S. Monthly Primary Energy Consumption by Source"
    sublab = "Data: EIA Annual Energy Review"
    gval = "Y"
    xlab = NULL
    ylab = "Quadrillion BTU"
    leglab = ""
    leg.ord = levels(with(dt[YYYYMM == "2017-04-01"], reorder(MSN, -Value))) # reorder legend of line plot to order 2014 values from least to             greatest
    plot.cols = source.cols
    
    line_pe_cons_1973_2016_totals = f.lineplot(dt, xval, yval, fillval, tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
      scale_x_date(date_breaks = "5 years", date_labels = "%Y", expand = c(0,1)) +
      scale_y_continuous(breaks = seq(0,10,2), expand = c(0,0), limits = c(0, 10))
    
    ggsave(line_pe_cons_1973_2016_totals, 
           filename = "Energy_1.3_Primary Energy Consumption Estimates by Source_Totals_1973-2016_LTS.png", 
           width = 11.1, 
           height = 6.25, 
           dpi = 400)
    