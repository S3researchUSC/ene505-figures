# NG_Monthly Gross Withdrawals vs Wellhead Price_1980-2015 #

# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

file.loc 	  = '/Users/MEAS/GitHub/ene505-figures' # location of scripts
data.loc      = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts' # location of data file(s)
data.file     = c('NG_PROD_SUM_DCU_NUS_M.csv', 'N9190US3m.csv') # data file to be used
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

  dt_withdrawal = fread(data.file[1], skip = 2, header = T)[, c(1,2), with = FALSE]
    colnames(dt_withdrawal) = c("Date", "Withdrawals")
    dt_withdrawal[, Date := as.Date(paste("01", Date, sep="-"), "%d-%b-%Y")]
    dt_withdrawal[, Date := as.Date(Date, "%m/%d/%Y")]
    dt_withdrawal[, Withdrawals := as.numeric(Withdrawals)]
  
  dt_price = fread(data.file[2], skip = 2, header = T)
    colnames(dt_price) = c("Date", "Price")
    dt_price[, Date := as.Date(paste("01", Date, sep="-"), "%d-%b-%Y")]
    dt_price[, Date := as.Date(Date, "%m/%d/%Y")]

# merge data ------  
  dt_all = dt_withdrawal[dt_price, on = "Date", nomatch = 0]

# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------

  setwd(out.loc) 

  # COMBINED PLOTS -------  

  png(file="NG_Monthly Gross Withdrawals vs Wellhead Price_1980-2015.png", width = 4400, height = 2500, res = 400)
  
  with(dt_all, dualplot(x1 = Date, y1 = Withdrawals/1000, y2 = Price, 
                        xbreaks = c(seq(as.Date("1980-01-01"), as.Date("2016-01-01"), by = "4 years")),
                        y1breaks = seq(0, 3000, 600),
                        y2breaks = seq(0, 15, 3),
                        col = c("firebrick", "dodgerblue3"),
                        lwd = 1.2, colgrid = NULL, 
                        main = "1980 - 2015 U.S. Monthly Natural Gas Gross Withdrawals and Wellhead Prices",
                        ylim.ref = NULL, 
                        ylab1 = "Billion Cubic Feet",
                        ylab2 = "USD per Thousand Cubic Feet",
                        yleg1 = "Natural Gas Gross Withdrawals (left axis)",
                        yleg2 = "Natural Gas Wellhead Prices (right axis)",
                        mar = c(5,6,3,6)))
  
  dev.off()
  