# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

data.loc      = '/Users/MEAS/Google Drive/ta-materials/ENE505 - Fall 2015/ENE 505 Charts/data_2017-2018' # location of data file(s)
prod.fil      = 'Table_1.2_Primary_Energy_Production_by_Source.xlsx'
cons.fil      = 'Table_1.3_Primary_Energy_Consumption_by_Source.xlsx'
out.loc       = '/Users/MEAS/Google Drive/ta-materials/ENE505 - Fall 2015/ENE 505 Charts/20180823' # location of where to save figures

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

  prod_month = as.data.table(read.xlsx(prod.fil, sheet = "Monthly Data", startRow = 11, cols = c(1,4), detectDates = T))
  prod_month = prod_month[2:nrow(prod_month)]
  
  prod_annual = as.data.table(read.xlsx(prod.fil, sheet = "Annual Data", startRow = 11, cols = c(1,4), detectDates = T))
  prod_annual = prod_annual[2:nrow(prod_annual)]
  
  cons_month = as.data.table(read.xlsx(cons.fil, sheet = "Monthly Data", startRow = 11, cols = c(1,4), detectDates = T))
  cons_month = cons_month[2:nrow(cons_month)]
  
  cons_annual = as.data.table(read.xlsx(cons.fil, sheet = "Annual Data", startRow = 11, cols = c(1,4), detectDates = T))
  cons_annual = cons_annual[2:nrow(cons_annual)]

# rename columns ------
  
  colnames(prod_month) = c("Month", "Production")
  colnames(prod_annual) = c("Year", "Production")
  
  colnames(cons_month) = c("Month", "Consumption")
  colnames(cons_annual) = c("Year", "Consumption")
  
# convert columns to numeric and add production columns ------
  
  prod_month[, Production := as.numeric(Production)]
  prod_annual[, Production := as.numeric(Production)]
  
  cons_month[, Consumption := as.numeric(Consumption)]
  cons_annual[, Consumption := as.numeric(Consumption)]

# melt data table from wide to long format -----
  
  all_month = prod_month[cons_month, on = "Month", nomatch = 0]
  all_annual = prod_annual[cons_annual, on = "Year", nomatch = 0]
  
  all_month = melt(all_month, measure.vars = colnames(all_month)[2:3],
                   variable.name = "Type", value.name = "Value")
  
  all_annual = melt(all_annual, measure.vars = colnames(all_annual)[2:3],
                    variable.name = "Type", value.name = "Value")

# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------
  
  setwd(out.loc) 
  
  # ANNUAL LINE PLOT (V2) -------------
  
  dt = all_annual
  xval = dt[, Year]
  yval = dt[, Value]
  fillval = dt[, Type]
  tlab = "Annual U.S. Petroleum Production and Consumption (1949 - 2017)"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Quadrillion BTU"
  leglab = ""
  leg.ord = levels(fillval)
  
  line_annual = ggplot(dt, aes(x = xval, y = yval, linetype = fillval)) + 
    geom_line(stat = "identity", size = 0.9) +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         linetype = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_x_continuous(breaks = seq(1950,2017,5), expand = c(0,0)) +
    scale_y_comma(expand = c(0.01,0)) +
    geom_dl(aes(label = Type), method = list(dl.trans(x = x + .3), "last.bumpup", cex = 1.3, fontfamily = "Roboto Condensed")) +
    guides(linetype = FALSE) +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.text = element_text(size = 13, face = "bold")) +
    theme(plot.margin = unit(c(1,8,1,1), "lines"))
  
  line_annual_2 <- ggplotGrob(line_annual)
  line_annual_2$layout$clip[line_annual_2$layout$name == "panel"] <- "off"
  
  ggsave(line_annual_2, 
         filename = "Oil_Production and Consumption_Annual_1949-2017_LTS_v2.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  # MONTHLY LINE PLOT (V2) -------------
  
  dt = all_month
  xval = dt[, Month]
  yval = dt[, Value]
  fillval = dt[, Type]
  tlab = "Monthly U.S. Petroleum Production and Consumption (January 1973 - April 2018)"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Quadrillion BTU"
  leglab = ""
  leg.ord = levels(fillval)
  
  line_annual = ggplot(dt, aes(x = xval, y = yval, linetype = fillval)) + 
    geom_line(stat = "identity", size = 0.7) +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         linetype = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y", expand = c(0,0)) +
    scale_y_comma(expand = c(0.01,0)) +
    geom_dl(aes(label = Type), method = list(dl.trans(x = x + .3), "last.bumpup", cex = 1.3, fontfamily = "Roboto Condensed")) +
    guides(linetype = FALSE) +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.text = element_text(size = 13, face = "bold")) +
    theme(plot.margin = unit(c(1,8,1,1), "lines"))
  
  line_annual_2 <- ggplotGrob(line_annual)
  line_annual_2$layout$clip[line_annual_2$layout$name == "panel"] <- "off"
  
  ggsave(line_annual_2, 
         filename = "Oil_Production and Consumption_Month_Jan1973-Apr2018_LTS_v2.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  