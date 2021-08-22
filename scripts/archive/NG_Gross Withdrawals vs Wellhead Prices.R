# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

data.loc      = '/Users/MEAS/Google Drive/ta-materials/ENE505 - Fall 2015/ENE 505 Charts/data_2017-2018' # location of data file(s)
with.mo       = 'NG_PROD_SUM_DC_NUS_MMCF_M.xlsx'
with.an       = 'NG_PROD_SUM_DC_NUS_MMCF_A.xlsx'
price.mon     = 'NG_PRI_SUM_DCU_NUS_M.xlsx'
price.an      = 'NG_PRI_SUM_DCU_NUS_A.xlsx'
out.loc       = '/Users/MEAS/Google Drive/ta-materials/ENE505 - Fall 2015/ENE 505 Charts/20180823' # location of where to save figures
pal.col       = c('Gross Withdrawals' = '#c44e52',
                  'Wellhead Price' = '#4c72b0')

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

  with_month = as.data.table(read.xlsx(with.mo, sheet = "Data 1", startRow = 3, cols = c(1:2), detectDates = T))
  with_annual = as.data.table(read.xlsx(with.an, sheet = "Data 1", startRow = 3, cols = c(1:2), detectDates = T))
  
  price_month = as.data.table(read.xlsx(price.mon, sheet = "Data 1", startRow = 3, cols = c(1:2), detectDates = T))
  price_annual = as.data.table(read.xlsx(price.an, sheet = "Data 1", startRow = 3, cols = c(1:2), detectDates = T))
  
# rename columns -----
  
  colnames(with_month) = c("Month", "Gross Withdrawals")
  colnames(with_annual) = c("Year", "Gross Withdrawals")
  
  colnames(price_month) = c("Month", "Wellhead Price")
  colnames(price_annual) = c("Year", "Wellhead Price")
  
# merge data tables ------
  
  dt_month = with_month[price_month, on = "Month"]
  dt_annual = with_annual[price_annual, on = "Year"]
  dt_annual[, Year := year(Year)]

  
# melt data table from wide to long format -----
  
  dt_month = melt(dt_month, measure.vars = colnames(dt_month)[2:3], variable.name = "type", value.name = "value")
  dt_annual = melt(dt_annual, measure.vars = colnames(dt_annual)[2:3], variable.name = "type", value.name = "value")
  
# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------

  setwd(out.loc) 
  
  # MONTHLY WITHDRAWALS VS PRICE -------
  
  tlab = "Monthly U.S. Natural Gas Gross Withdrawals and Wellhead Price (January 1980 - December 2012)"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Gross Withdrawals (Billion Cubic Feet)"
  ylab2 = "Wellhead Price (Dollars per Thousand Cubic Feet)"
  leglab = ""
  plot.cols = pal.col
  
  
  line_monthly = ggplot() + 
    geom_line(data = dt_month[type == "Gross Withdrawals"], size = 0.7, 
              aes(x = Month, y = value/1000, color = type, linetype = type)) +
    geom_line(data = dt_month[type == "Wellhead Price"], size = 0.7, 
              aes(x = Month, y = value*400, color = type, linetype = type)) +
    scale_color_manual(values = plot.cols) +
    # scale_linetype_manual(values = rig.lines) +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         color = leglab,
         linetype = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_x_date(limits = c(as.Date("1980-01-15"), as.Date("2012-12-15")), 
                 date_breaks = "5 years", date_labels = "%Y", expand = c(0,0)) +
    scale_y_comma(expand = c(0.02,0), sec.axis = sec_axis(~./400, name = ylab2)) +
    # guides(color = FALSE) +
    theme(plot.title = element_text(size = 19, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          legend.text = element_text(size = 13),
          strip.text.x = element_text(size = 14, face = "bold"),
          strip.text.y = element_text(size = 14, face = "bold")) +
    theme(plot.margin = unit(c(1,1,1,1), "lines"))
  
  ggsave(line_monthly, 
         filename = "NG_Gross Withdrawals and Wellhead Price_Monthly_Jan1980-Dec2012_LTS.png", 
         width = 12.75, 
         height = 7, 
         dpi = 400)
  

  # MONTHLY WITHDRAWALS VS PRICE -------
  
  tlab = "Annual U.S. Natural Gas Gross Withdrawals and Wellhead Price (1939 - 2012)"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Gross Withdrawals (Billion Cubic Feet)"
  ylab2 = "Wellhead Price (Dollars per Thousand Cubic Feet)"
  leglab = ""
  plot.cols = pal.col
  
  
  line_annual = ggplot() + 
    geom_line(data = dt_annual[type == "Gross Withdrawals"], size = 0.7, 
              aes(x = Year, y = value/1000, color = type, linetype = type)) +
    geom_line(data = dt_annual[type == "Wellhead Price"], size = 0.7, 
              aes(x = Year, y = value*5000, color = type, linetype = type)) +
    scale_color_manual(values = plot.cols) +
    # scale_linetype_manual(values = rig.lines) +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         color = leglab,
         linetype = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_x_continuous(breaks = seq(1939,2014,5), limits = c(1936, 2012), expand = c(0,0)) +
    scale_y_comma(expand = c(0.02,0), sec.axis = sec_axis(~./5000, name = ylab2)) +
    # guides(color = FALSE) +
    theme(plot.title = element_text(size = 19, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          legend.text = element_text(size = 13),
          strip.text.x = element_text(size = 14, face = "bold"),
          strip.text.y = element_text(size = 14, face = "bold")) +
    theme(plot.margin = unit(c(1,1,1,1), "lines"))
  
  ggsave(line_annual, 
         filename = "NG_Gross Withdrawals and Wellhead Price_Annual_1936-2012_LTS.png", 
         width = 12.75, 
         height = 7, 
         dpi = 400)
  
  
  