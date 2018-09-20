# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

data.loc      = '/Users/MEAS/Google Drive/ta-materials/ENE505 - Fall 2015/ENE 505 Charts/data_2017-2018' # location of data file(s)
prod.mo       = 'NG_PROD_SUM_DC_NUS_MMCF_M.xlsx'
prod.an       = 'NG_PROD_SUM_DC_NUS_MMCF_A.xlsx'
price.mon     = 'NG_PRI_SUM_DCU_NUS_M.xlsx'
price.an      = 'NG_PRI_SUM_DCU_NUS_A.xlsx'
out.loc       = '/Users/MEAS/Google Drive/ta-materials/ENE505 - Fall 2015/ENE 505 Charts/20180823' # location of where to save figures
pal.col       = c('Dry Production' = '#c44e52',
                  'Citygate Price' = '#4c72b0')
pal.lin       = c('Dry Production' = 1,
                  'Citygate Price' = 2)

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
  
  prod_month = as.data.table(read.xlsx(prod.mo, sheet = "Data 1", startRow = 3, cols = c(1,12), detectDates = T))
  prod_annual = as.data.table(read.xlsx(prod.an, sheet = "Data 1", startRow = 3, cols = c(1,12), detectDates = T))
  
  price_month = as.data.table(read.xlsx(price.mon, sheet = "Data 1", startRow = 3, cols = c(1,9), detectDates = T))
  price_annual = as.data.table(read.xlsx(price.an, sheet = "Data 1", startRow = 3, cols = c(1,10), detectDates = T))

# rename columns -----

  colnames(prod_month) = c("Month", "Dry Production")
  colnames(prod_annual) = c("Year", "Dry Production")
  
  colnames(price_month) = c("Month", "Citygate Price")
  colnames(price_annual) = c("Year", "Citygate Price")

# merge data tables ------

  dt_month = prod_month[price_month, on = "Month"]
  dt_annual = prod_annual[price_annual, on = "Year"]
  dt_annual[, Year := year(Year)]

# melt data table from wide to long format -----

  dt_month = melt(dt_month, measure.vars = colnames(dt_month)[2:3], variable.name = "type", value.name = "value")
  dt_annual = melt(dt_annual, measure.vars = colnames(dt_annual)[2:3], variable.name = "type", value.name = "value")
  
# reoder levels ------
  
  # dt_month[, ("type") := factor(get("type"), levels = c("Dry Production", "Citygate Price"))]
  # dt_annual[, ("type") := factor(get("type"), levels = c("Dry Production", "Citygate Price"))]
  

# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------

  setwd(out.loc) 

# MONTHLY WITHDRAWALS VS PRICE -------
  
  tlab = "Monthly U.S. Natural Gas Dry Production and Citygate Price (January 1997 - May 2018)"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Dry Production (Billion Cubic Feet)"
  ylab2 = "Citygate Price (Dollars per Thousand Cubic Feet)"
  leglab = ""
  plot.cols = pal.col
  
  
  line_monthly = ggplot() + 
    geom_line(data = dt_month[type == "Dry Production"], size = 0.8,
              aes(x = Month, y = value/1000, color = type, linetype = type)) +
    geom_line(data = dt_month[type == "Citygate Price"], size = 0.8, 
              aes(x = Month, y = value*200, color = type, linetype = type)) +
    scale_color_manual(values = plot.cols) +
    scale_linetype_manual(values = pal.lin) +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         color = leglab,
         linetype = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_x_date(limits = c(as.Date("1997-01-15"), as.Date("2018-05-15")), 
                 date_breaks = "5 years", date_labels = "%Y", expand = c(0,0)) +
    scale_y_comma(expand = c(0.02,0), sec.axis = sec_axis(~./200, name = ylab2)) +
    guides(color = guide_legend(reverse=TRUE),
           linetype = guide_legend(reverse=TRUE)) +
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
         filename = "NG_Dry Production vs Citygate Price_Monthly_Jan1997-May2018_LTS.png", 
         width = 12.75, 
         height = 7, 
         dpi = 400)


# ANNUAL WITHDRAWALS VS PRICE -------

  tlab = "Annual U.S. Natural Gas Dry Production and Citygate Price (1984 - 2017)"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Dry Production (Billion Cubic Feet)"
  ylab2 = "Citygate Price (Dollars per Thousand Cubic Feet)"
  leglab = ""
  plot.cols = pal.col
  
  
  line_annual = ggplot() + 
    geom_line(data = dt_annual[type == "Dry Production"], size = 1, 
              aes(x = Year, y = value/1000, color = type, linetype = type)) +
    geom_line(data = dt_annual[type == "Citygate Price"], size = 1, 
              aes(x = Year, y = value*4000, color = type, linetype = type)) +
    scale_color_manual(values = plot.cols) +
    scale_linetype_manual(values = pal.lin) +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         color = leglab,
         linetype = leglab) +
    theme_ipsum_rc(grid = gval) +
    guides(color = guide_legend(reverse=TRUE),
           linetype = guide_legend(reverse=TRUE)) +
    scale_x_continuous(breaks = seq(1984,2017,5), limits = c(1984, 2017), expand = c(0,0)) +
    scale_y_comma(expand = c(0.02,0), sec.axis = sec_axis(~./4000, name = ylab2)) +
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
         filename = "NG_Dry Production vs Citygate Price_Annual_1984-2017_LTS.png", 
         width = 12.75, 
         height = 7, 
         dpi = 400)