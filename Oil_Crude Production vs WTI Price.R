# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

data.loc      = '/Users/MEAS/Google Drive/ta-materials/ENE505 - Fall 2015/ENE 505 Charts/data_2017-2018' # location of data file(s)
prod.mo       = 'PET_CRD_CRPDN_ADC_MBBL_M.xlsx'
prod.an       = 'PET_CRD_CRPDN_ADC_MBBL_A.xlsx'
price.mon     = 'PET_PRI_SPT_S1_M.xlsx'
price.an      = 'PET_PRI_SPT_S1_A.xlsx'
out.loc       = '/Users/MEAS/Google Drive/ta-materials/ENE505 - Fall 2015/ENE 505 Charts/20180823' # location of where to save figures
pal.col       = c('Crude Production' = '#c44e52',
                  'WTI Spot Price' = '#4c72b0')
pal.lin       = c('Crude Production' = 1,
                  'WTI Spot Price' = 2)

# ---------------------------------------------------------------
# MAIN SCRIPT ---------------------------------------------------
# ---------------------------------------------------------------

# load libraries -------

  library(data.table)
  library(openxlsx)
  library(ggplot2)
  library(hrbrthemes)
  library(stringr)
  library(plyr)
  library(directlabels)
  library(grid)

# load data ------
  
  setwd(data.loc)
  
  prod_month = as.data.table(read.xlsx(prod.mo, sheet = "Data 1", startRow = 3, cols = c(1,2), detectDates = T))
  prod_annual = as.data.table(read.xlsx(prod.an, sheet = "Data 1", startRow = 3, cols = c(1,2), detectDates = T))
  
  price_month = as.data.table(read.xlsx(price.mon, sheet = "Data 1", startRow = 3, cols = c(1,2), detectDates = T))
  price_annual = as.data.table(read.xlsx(price.an, sheet = "Data 1", startRow = 3, cols = c(1,2), detectDates = T))
  
# rename columns -----
  
  colnames(prod_month) = c("Month", "Crude Production")
  colnames(prod_annual) = c("Year", "Crude Production")
  
  colnames(price_month) = c("Month", "WTI Spot Price")
  colnames(price_annual) = c("Year", "WTI Spot Price")
  
# merge data tables ------
  
  dt_month = prod_month[price_month, on = "Month"]
  dt_annual = prod_annual[price_annual, on = "Year"]
  dt_annual[, Year := year(Year)]
  
# melt data table from wide to long format -----
  
  dt_month = melt(dt_month, measure.vars = colnames(dt_month)[2:3], variable.name = "type", value.name = "value")
  dt_annual = melt(dt_annual, measure.vars = colnames(dt_annual)[2:3], variable.name = "type", value.name = "value")
  

# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------

  setwd(out.loc) 
  
  # MONTHLY WITHDRAWALS VS PRICE -------
  
  tlab = "Monthly U.S. Crude Oil Production and WTI Spot Price (January 1986 - August 2018)"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Crude Oil Production (Million Barrels)"
  ylab2 = "WTI Spot Price (Dollars per Barrel)"
  leglab = ""
  plot.cols = pal.col
  
  line_monthly = ggplot() + 
    geom_line(data = dt_month[type == "Crude Production"], size = 0.8,
              aes(x = Month, y = value/1000, color = type, linetype = type)) +
    geom_line(data = dt_month[type == "WTI Spot Price"], size = 0.8, 
              aes(x = Month, y = value*4, color = type, linetype = type)) +
    scale_color_manual(values = plot.cols) +
    scale_linetype_manual(values = pal.lin) +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         color = leglab,
         linetype = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_x_date(limits = c(as.Date("1986-01-15"), as.Date("2018-08-15")), 
                 date_breaks = "5 years", date_labels = "%Y", expand = c(0,0)) +
    scale_y_comma(expand = c(0.02,0), sec.axis = sec_axis(~./4, name = ylab2, breaks = seq(0, 150, 25))) +
    theme(plot.title = element_text(size = 19, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          legend.text = element_text(size = 15),
          strip.text.x = element_text(size = 14, face = "bold"),
          strip.text.y = element_text(size = 14, face = "bold")) +
    theme(plot.margin = unit(c(1,1,1,1), "lines"))
  
  ggsave(line_monthly, 
         filename = "Oil_Crude Production vs WTI Spot Price_Monthly_Jan1986-Aug2018_LTS.png", 
         width = 12.75, 
         height = 7, 
         dpi = 400)
  
  
  # ANNUAL WITHDRAWALS VS PRICE -------
  
  tlab = "Annual U.S. Crude Oil Production and WTI Spot Price (1986 - 2017)"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Crude Oil Production (Million Barrels)"
  ylab2 = "WTI Spot Price (Dollars per Barrel)"
  leglab = ""
  plot.cols = pal.col
  
  line_annual = ggplot() + 
    geom_line(data = dt_annual[type == "Crude Production"], size = 1, 
              aes(x = Year, y = value/1000, color = type, linetype = type)) +
    geom_line(data = dt_annual[type == "WTI Spot Price"], size = 1, 
              aes(x = Year, y = value*40, color = type, linetype = type)) +
    scale_color_manual(values = plot.cols) +
    scale_linetype_manual(values = pal.lin) +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         color = leglab,
         linetype = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_x_continuous(breaks = seq(1986,2017,5), limits = c(1986, 2017), expand = c(0,0)) +
    scale_y_comma(expand = c(0.02,0), sec.axis = sec_axis(~./40, name = ylab2)) +
    theme(plot.title = element_text(size = 19, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          legend.text = element_text(size = 15),
          strip.text.x = element_text(size = 14, face = "bold"),
          strip.text.y = element_text(size = 14, face = "bold")) +
    theme(plot.margin = unit(c(1,1,1,1), "lines"))
  
  ggsave(line_annual, 
         filename = "Oil_Crude Production vs WTI Spot Price_Annual_1986-2017_LTS.png", 
         width = 12.75, 
         height = 7, 
         dpi = 400)
