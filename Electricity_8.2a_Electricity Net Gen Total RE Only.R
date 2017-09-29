# Electricity_8.2a_Electricity Net Gen Total All Sectors_Script #

# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

file.loc 	= '/Users/MEAS/GitHub/ene505-figures' # location of scripts
data.loc    = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts/data_2017' # location of data file(s)
data.file   = 'chart10-13_data.csv' # data file to be used
out.loc   = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts/20170929' # location of where to save figures
source.cols   = c("Geothermal" = "#880015",
                  "Solar" = "#ff7f27",
                  "Waste" = "#c3c3c3",
                  "Wood" = "#b5734e",
                  "Wind" = "#3f48cc",
                  "Hydroelectric" = "#00baff",
                  "Biomass" = "#b5734e",
                  "Nuclear" = "#22b14c",
                  "Petroleum" = "#fff200",
                  "Coal" = "#000000",
                  "Natural Gas" = "#ed1c24",
                  "Other Gases" = "#e9d8ff")

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
dt_raw = fread(data.file, header = T)

# rename columns -----
colnames(dt_raw) <- c("Month",
                      "Coal",
                      "Petroleum",
                      "Natural Gas",
                      "Other Gases",
                      "Nuclear",
                      "Hydroelectric",
                      "Wood",
                      "Waste",
                      "Geothermal",
                      "Solar",
                      "Wind",
                      "Total")

# change date format ----
dt_raw[, YYYYMM := paste0("01 ", Month)]
dt_raw[, YYYYMM := as.Date(YYYYMM, format = "%d %Y %B")]

# melt data table from wide to long format -----
dt_long = melt(dt_raw, measure.vars = colnames(dt_raw)[2:13],
               variable.name = "MSN", value.name = "Value")

# remove non-renewables ------
dt_long = dt_long[ ! MSN %in% c("Coal", "Petroleum", "Natural Gas", "Nuclear", "Other Gases") ]


# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------

setwd(out.loc) 

  # AREA PLOT -------------
  
    dt = copy(dt_long)[, MSN := factor(MSN, levels = c("Total",
                                                       "Hydroelectric",
                                                       "Wood",
                                                       "Waste",
                                                       "Geothermal",
                                                       "Solar",
                                                       "Wind"
                                                   ))][ ! MSN %in% c("Total")] # customize reorder MSN levels for stacked area chart
  xval = dt[, YYYYMM]
  yval = dt[, Value]/1000
  fillval = dt[, MSN]
  tlab = "1973 - 2016 U.S. Monthly Electricity Generation By Source"
  sublab = "Data: EIA Annual Energy Review"
  gval = "Y"
  xlab = NULL
  ylab = "Kilowatt-hour (Billions)"
  leglab = ""
  leg.ord = levels(fillval)
  plot.cols = source.cols
  
    area_elec_netgen_RE_1973_2017 = f.areaplot(dt, xval, yval, fillval, 
                                               tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
      scale_x_date(date_breaks = "5 years", date_labels = "%Y", expand = c(0,1)) +
      scale_y_continuous(breaks = seq(0,70,10), expand = c(0,0), limits = c(0, 70))
    
    ggsave(area_elec_netgen_RE_1973_2017, 
           filename = "Electricity_8.2a_Electricity Net Gen Total RE Only_ATS.png", 
           width = 11.1, 
           height = 6.25, 
           dpi = 400)
  
  # LINE PLOT -------------
  
    dt = dt_long[ ! MSN %in% c("Total")]
    xval = dt[, YYYYMM]
    yval = dt[, Value]/1000
    fillval = dt[, MSN]
    tlab = "1973 - 2016 U.S. Monthly Electricity Generation By Source"
    sublab = "Data: EIA Annual Energy Review"
    gval = "Y"
    xlab = NULL
    ylab = "Kilowatt-hour (Billions)"
    leglab = ""
    leg.ord = levels(with(dt[YYYYMM == "2017-04-01"], reorder(MSN, -Value))) # reorder legend of line plot to order 2014 values from least to greatest
    plot.cols = source.cols
    
    line_elec_netgen_RE_1973_2017 = f.lineplot(dt, xval, yval, fillval, 
                                               tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
      scale_x_date(date_breaks = "5 years", date_labels = "%Y", expand = c(0,1)) +
      scale_y_continuous(breaks = seq(0,40,10), expand = c(0,0), limits = c(0, 40))
    
    ggsave(line_elec_netgen_RE_1973_2017, 
           filename = "Electricity_8.2a_Electricity Net Gen Total RE Only_LTS.png", 
           width = 11.1, 
           height = 6.25, 
           dpi = 400)
  