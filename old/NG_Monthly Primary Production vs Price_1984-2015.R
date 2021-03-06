# NG_Monthly Primary Prod vs Price_1984-2014 #

# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

file.loc 	  = '/Users/MEAS/GitHub/ene505-figures' # location of scripts
data.loc      = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts' # location of data file(s)
data.file     = c('U.S._Dry_Natural_Gas_Production.csv', 'U.S._Natural_Gas_Citygate_Price.csv') # data file to be used
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

# load plot functions -----
  setwd(file.loc)
  source("dualplotfunc.R")

# import data ------
  setwd(data.loc)
  dt_prod = fread(data.file[1], skip = 4, header = T)
    colnames(dt_prod) = c("Date", "Production")
    dt_prod[, Date := as.Date(paste("01", Date, sep="-"), "%d-%b %Y")]
    dt_prod[, Date := as.Date(Date, "%m/%d/%Y")]

  dt_price = fread(data.file[2], skip = 4, header = T)
    colnames(dt_price) = c("Date", "Price")
    dt_price[, Date := as.Date(paste("01", Date, sep="-"), "%d-%b %Y")]
    dt_price[, Date := as.Date(Date, "%m/%d/%Y")]

  dt_all = dt_prod[dt_price, on = "Date", nomatch = 0]
  
# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------
  
  setwd(out.loc) 
  
  # COMBINED PLOTS -------  
  
  png(file="NG_Monthly Primary Production vs Price_1984-2015.png", width = 4400, height = 2500, res = 400)
  
  with(dt_all, dualplot(x1 = Date, y1 = Production, y2 = Price, 
                        xbreaks = c(seq(as.Date("1984-01-01"), as.Date("2015-01-01"), by = "2 years"), as.Date("2015-01-01")),
                        y1breaks = seq(0, 2500, 500),
                        y2breaks = seq(0, 15, 3),
                        col = c("firebrick", "dodgerblue3"),
                        lwd = 1.2, colgrid = NULL, 
                        main = "1984 - 2014 U.S. Monthly Natural Gas Production and Prices",
                        sub = "Source: EIA Annual Energy Review",
                        ylim.ref = NULL, 
                        ylab1 = "Billion Cubic Feet",
                        ylab2 = "USD per Thousand Cubic Feet",
                        yleg1 = "Natural Gas Production (left axis)",
                        yleg2 = "Natural Gas Prices (right axis)",
                        mar = c(6,6,3,6)))
  
  dev.off()
  