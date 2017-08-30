# Petroleum_Monthly Primary Prod vs Price_1986-2015 #

# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

file.loc 	  = '/Users/MEAS/GitHub/ene505-figures' # location of scripts
data.loc      = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts' # location of data file(s)
data.file     = c('Petroleum_U.S._Field_Production_of_Crude_Oil.csv', 'Petroleum_Cushing_OK_WTI_Spot_Price_FOB.csv') # data file to be used
out.loc       = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts/20170827' # location of where to save figures

# ---------------------------------------------------------------
# MAIN SCRIPT ---------------------------------------------------
# ---------------------------------------------------------------

# load libraries -------
  library(data.table)
  library(ggplot2)
  library(hrbrthemes)
  library(reshape2)
  library(gtable)
  library(lubridate)

# load plot functions -----
  setwd(file.loc)
  source("dualplotfunc.R")

# import data ------
  setwd(data.loc)
  dt_prod = fread(data.file[1], skip = 4, header = T)
  dt_price = fread(data.file[2], skip = 4, header = T)
  
# change column names ------
  colnames(dt_prod) = c("Date", "Production")
  colnames(dt_price) = c("Date", "Price")
  
# format date columns -----  
  dt_prod[, Date := mdy(Date)]
  dt_price[, Date := mdy(Date)]
  
# take monthly average of prices -----
  dt_price[, month_year := paste0(month(Date), "-", year(Date))]
  dt_price <- dt_price[, .(Price = mean(Price)), by = c("month_year")]
  dt_price[, Date := dmy(paste("01", month_year, sep="-"))]

# merge data -----
  dt_all = dt_prod[dt_price, on = "Date", nomatch = 0]

# make production column numeric -----
  dt_all[, Production := as.numeric(Production)]

# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------
  
  setwd(out.loc) 
  
  # COMBINED PLOTS -------  
  
  png(file="Petroleum_Monthly Primary Production vs Price_1985-2015.png", width = 4400, height = 2500, res = 400)
  
  with(dt_all, dualplot(x1 = Date, y1 = Production, y2 = Price, 
                        xbreaks = c(seq(as.Date("1984-01-01"), as.Date("2016-01-01"), by = "4 years")),
                        y1breaks = seq(0, 320000, 80000),
                        y2breaks = seq(0, 160, 40),
                        col = c("firebrick", "dodgerblue3"),
                        lwd = 1.2, colgrid = NULL, 
                        main = "1984 - 2015 U.S. Monthly Production of Crude and WTI Crude Oil Spot Price",
                        sub = "Source: EIA Annual Energy Review",
                        ylim.ref = NULL, 
                        ylab1 = "Thousand Barrels",
                        ylab2 = "USD per Barrel",
                        yleg1 = "Monthly Crude Oil Production (left axis)",
                        yleg2 = "Monthly Average WTI Spot Price (right axis)",
                        mar = c(6,6,3,6)))
  
  dev.off()
  