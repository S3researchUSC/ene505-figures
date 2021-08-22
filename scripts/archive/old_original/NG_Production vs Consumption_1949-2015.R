# NG_Production vs Consumption_1949-2015 #

# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

file.loc 	  = '/Users/MEAS/GitHub/ene505-figures' # location of scripts
data.loc      = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts' # location of data file(s)
data.file     = c('Energy_Production_1949-2015.csv', 'Energy_Consumption_1949-2015.csv') # data file to be used
out.loc       = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts/20170827' # location of where to save figures
type.cols     = c("Production" = "coral3",
                  "Consumption" = "darkcyan")

# ---------------------------------------------------------------
# MAIN SCRIPT ---------------------------------------------------
# ---------------------------------------------------------------

# load libraries -------
  library(data.table)
  library(ggplot2)
  library(hrbrthemes)
  library(lubridate)
  library(stringr)

# load plot functions -----
  setwd(file.loc)
  source("plotfunctions.R")

# import data ------
  setwd(data.loc)

  dt_prod = fread(data.file[1], header = T)
  dt_cons = fread(data.file[2], header = T)

# only keep natural gas production (dry and liquid) ------
  dt_prod = dt_prod[ Description %like% "Natural Gas", c(1:3), with = FALSE ]
  
# only keep natural gas consumption ------
  dt_cons = dt_cons[ Description %like% "Natural Gas", c(2:3), with = FALSE ]

# convert values to numeric ------
  dt_prod[, Value := as.numeric(Value)]
  dt_cons[, Value := as.numeric(Value)]
  
# sum natural gas values (sum all values for each year) for production -----
  dt_prod = dt_prod[, .(Value = sum(Value)), by = c("YYYYMM")]
  
# rename column ------
  colnames(dt_prod)[2] = "Production"
  colnames(dt_cons)[2] = "Consumption"
  
# merge production and consumption datatables  ------    
  dt_all = dt_prod[dt_cons, on = "YYYYMM", nomatch = 0]
  
# keep annual values only ------
  dt_annual = dt_all[ str_sub(YYYYMM, start = -2) == "13" ]
  
# keep monthly values only ------
  dt_monthly = dt_all[ ! str_sub(YYYYMM, start = -2) == "13" ]
  
# create year/date columns ------
  dt_annual[, year := as.numeric(str_sub(YYYYMM, 1, 4)) ]
  dt_monthly[, date := ymd(paste0(YYYYMM, "01"))]
  
# melt data tables from wide to long formats ------
  dt_annual = melt(dt_annual, measure.vars = list(2:3), variable.name = "type", value.name = "value")
  dt_monthly = melt(dt_monthly, measure.vars = list(2:3), variable.name = "type", value.name = "value")

# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------
  
  setwd(out.loc) 
  
  # LINE PLOT -------------
  
  line_ng_annual_prod_vs_cons = ggplot(dt_annual, aes(x = year, y = value, linetype = type)) + 
    geom_line(stat = "identity", color = "gray10", size = 1) +
    labs(title = "1949 - 2014 Annual U.S. Natural Gas Production and Consumption",
         subtitle = "Data: EIA Annual Energy Review", 
         x = NULL,
         y = "Quadrillion BTU",
         linetype = NULL) +
    theme_ipsum_rc(grid = "Y") +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.text = element_text(size = 13, face = "bold"),
          legend.position = "bottom") + 
    scale_x_continuous(breaks = seq(1945,2015,5), expand = c(0,0)) +
    scale_y_continuous(breaks = seq(0,35,5), expand = c(0,0), limits = c(0, 35))
  
  ggsave(line_ng_annual_prod_vs_cons, 
         filename = "NG_Production vs Consumption_1949-2014_Annual_LTS.png", 
         width = 11.1, 
         height = 6.25, 
         dpi = 400)
  
  line_ng_monthly_prod_vs_cons = ggplot(dt_monthly, aes(x = date, y = value, linetype = type)) + 
    geom_line(stat = "identity", color = "gray10") +
    labs(title = "1973 - 2014 Monthly U.S. Natural Gas Production and Consumption",
         subtitle = "Data: EIA Annual Energy Review", 
         x = NULL,
         y = "Quadrillion BTU",
         linetype = NULL) +
    theme_ipsum_rc(grid = "Y") +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.text = element_text(size = 13, face = "bold"),
          legend.position = "bottom") + 
    scale_x_date(breaks = seq(as.Date("1970-01-01"), as.Date("2015-01-01"), by = "5 years"), 
                 limits = c(as.Date("1973-01-01"), as.Date("2015-01-01")),
                 date_labels = "%Y", expand = c(0,0)) +
    scale_y_continuous(breaks = seq(0,3.5,0.5), expand = c(0,0), limits = c(0, 3.5))
  
  ggsave(line_ng_monthly_prod_vs_cons, 
         filename = "NG_Production vs Consumption_1973-2014_Monthly_LTS.png", 
         width = 11.1, 
         height = 6.25, 
         dpi = 400)
  
# SEGMENT PLOT -------------
  
  dt = dt_annual
  xval = dt[, year]
  yval = dt[, value]
  fillval = dt[, type]
  wsize = 3
  csize = 2
  tlab = "1949 - 2014 Annual U.S. Natural Gas Production and Consumption"
  sublab = "Data: EIA Annual Energy Review"
  gval = "Y"
  xlab = NULL
  ylab = "Quadrillion BTU"
  leglab = ""
  leg.ord = levels(with(dt[year == "2014"], reorder(type, -value)))
  plot.cols = type.cols
  
  seg_ng_prod_vs_cons_1949_2015 = f.segplot(dt, xval, yval, fillval, wsize, csize, 
                                        tlab, sublab, xlab, ylab, leglab, gval, leg.ord, plot.cols) + 
    scale_x_continuous(breaks = seq(1945,2015,5), expand = c(0,0)) +
    scale_y_continuous(breaks = seq(0,35,5), expand = c(0,0), limits = c(0, 35)) +
    theme(legend.position = "bottom")
  
  ggsave(seg_ng_prod_vs_cons_1949_2015, 
         filename = "NG_Production vs Consumption_1949-2014_Annual_STS.png", 
         width = 11.1, 
         height = 6.25, 
         dpi = 400)
  