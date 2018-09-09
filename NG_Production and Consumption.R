# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

data.loc      = '/Users/MEAS/Google Drive/ta-materials/ENE505 - Fall 2015/ENE 505 Charts/data_2017-2018' # location of data file(s)
data.file     = 'Table_4.1_Natural_Gas_Overview.xlsx'
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
  
  dt_month = as.data.table(read.xlsx(data.file, sheet = "Monthly Data", startRow = 11, cols = c(1,3:5,12), detectDates = T))
  dt_month = dt_month[2:nrow(dt_month)]
  
  dt_annual = as.data.table(read.xlsx(data.file, sheet = "Annual Data", startRow = 11, cols = c(1,3:5,12), detectDates = T))
  dt_annual = dt_annual[2:nrow(dt_annual)]
  
  prod_month = as.data.table(read.xlsx(prod.fil, sheet = "Monthly Data", startRow = 11, cols = c(1,3,5), detectDates = T))
  prod_month = prod_month[2:nrow(prod_month)]
  
  prod_annual = as.data.table(read.xlsx(prod.fil, sheet = "Annual Data", startRow = 11, cols = c(1,3,5), detectDates = T))
  prod_annual = prod_annual[2:nrow(prod_annual)]
  
  cons_month = as.data.table(read.xlsx(cons.fil, sheet = "Monthly Data", startRow = 11, cols = c(1,3), detectDates = T))
  cons_month = cons_month[2:nrow(cons_month)]
  
  cons_annual = as.data.table(read.xlsx(cons.fil, sheet = "Annual Data", startRow = 11, cols = c(1,3), detectDates = T))
  cons_annual = cons_annual[2:nrow(cons_annual)]

# rename columns ------
  
  colnames(dt_month) = c("Month", "Wet_Production", "NGPL_Production", "Dry_Production", "Consumption")
  colnames(dt_annual) = c("Year", "Wet_Production", "NGPL_Production", "Dry_Production", "Consumption")
  
  colnames(prod_month) = c("Month", "Dry_Production", "Liquid_Production")
  colnames(prod_annual) = c("Year", "Dry_Production", "Liquid_Production")
  
  colnames(cons_month) = c("Month", "Consumption")
  colnames(cons_annual) = c("Year", "Consumption")
  
# convert columns to numeric and add production columns ------
  
  numcols = c("Wet_Production", "NGPL_Production", "Dry_Production", "Consumption")
  
  dt_month[,(numcols):= lapply(.SD, as.numeric), .SDcols = numcols]
  dt_annual[,(numcols):= lapply(.SD, as.numeric), .SDcols = numcols]
  
  dt_month[, Production := Wet_Production + NGPL_Production + Dry_Production]
  dt_annual[, Production := Wet_Production + NGPL_Production + Dry_Production]
  
  prod_month[,(c("Dry_Production", "Liquid_Production")):= lapply(.SD, as.numeric), .SDcols = c("Dry_Production", "Liquid_Production")]
  prod_annual[,(c("Dry_Production", "Liquid_Production")):= lapply(.SD, as.numeric), .SDcols = c("Dry_Production", "Liquid_Production")]
  
  cons_month[,("Consumption"):= lapply(.SD, as.numeric), .SDcols = "Consumption"]
  cons_annual[,("Consumption"):= lapply(.SD, as.numeric), .SDcols = "Consumption"]
  
  prod_month[, Production := Dry_Production + Liquid_Production]
  prod_annual[, Production := Dry_Production + Liquid_Production]
  
# remove other production columns -------
  
  dt_month = dt_month[, c("Month", "Production", "Consumption")]
  dt_annual = dt_annual[, c("Year", "Production", "Consumption")]
  
  prod_month = prod_month[, c("Month", "Production")]
  prod_annual = prod_annual[, c("Year", "Production")]

# melt data table from wide to long format -----
  
  dt_month = melt(dt_month, measure.vars = colnames(dt_month)[2:3],
                  variable.name = "Type", value.name = "Value")
  
  dt_annual = melt(dt_annual, measure.vars = colnames(dt_annual)[2:3],
                   variable.name = "Type", value.name = "Value")
  
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
  
  # ANNUAL LINE PLOT (V1) -------------
  
  dt = dt_annual
  xval = dt[, Year]
  yval = dt[, Value]
  fillval = dt[, Type]
  tlab = "Annual U.S. Natural Gas Production and Consumption (1949 - 2017)"
  sublab = "Based on sum of dry, liquid, and NGPL production \n Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Billion Cubic Feet"
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
         filename = "NG_Production and Consumption_Annual_1949-2017_LTS_v1.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  

  
  # MONTHLY LINE PLOT (V1) -------------
  
  dt = dt_month
  xval = dt[, Month]
  yval = dt[, Value]
  fillval = dt[, Type]
  tlab = "Based on sum of dry, liquid, and NGPL production \n Monthly U.S. Natural Gas Production and Consumption (January 1973 - April 2018)"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Billion Cubic Feet"
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
         filename = "NG_Production and Consumption_Month_Jan1973-Apr2018_LTS_v1.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  
  
  
  # ANNUAL LINE PLOT (V2) -------------
  
  dt = all_annual
  xval = dt[, Year]
  yval = dt[, Value]
  fillval = dt[, Type]
  tlab = "Annual U.S. Natural Gas Production and Consumption (1949 - 2017)"
  sublab = "Based on primary energy production and consumption reports \n Data: U.S. Energy Information Administration"
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
         filename = "NG_Production and Consumption_Annual_1949-2017_LTS_v1.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  # MONTHLY LINE PLOT (V2) -------------
  
  dt = all_month
  xval = dt[, Month]
  yval = dt[, Value]
  fillval = dt[, Type]
  tlab = "Monthly U.S. Natural Gas Production and Consumption (January 1973 - April 2018)"
  sublab = "Based on primary energy production and consumption reports \n Data: U.S. Energy Information Administration"
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
         filename = "NG_Production and Consumption_Month_Jan1973-Apr2018_LTS_v2.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  