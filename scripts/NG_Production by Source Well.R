# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

data.loc      = '/Users/MEAS/Google Drive/ta-materials/ENE505 - Fall 2015/ENE 505 Charts/data_2017-2018' # location of data file(s)
ann.fil       = 'NG_PROD_SUM_DC_NUS_MMCF_A.xlsx'
mon.fil       = 'NG_PROD_SUM_DC_NUS_MMCF_M.xlsx'
out.loc       = '/Users/MEAS/Google Drive/ta-materials/ENE505 - Fall 2015/ENE 505 Charts/20180823' # location of where to save figures
source.cols   = c('#9b59b6', "#3498db", "#95a5a6", "#e74c3c")

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
  
  dt_annual = as.data.table(read.xlsx(ann.fil, sheet = "Data 1", startRow = 3, cols = c(1,3:6), detectDates = T))
  dt_month = as.data.table(read.xlsx(mon.fil, sheet = "Data 1", startRow = 3, cols = c(1,3:6), detectDates = T))

# rename columns ------
  
  colnames(dt_annual) = c("Date", "Gas Wells", "Oil Wells", "Shale Gas", "Coalbed Wells")
  colnames(dt_month) = c("Date", "Gas Wells", "Oil Wells", "Shale Gas", "Coalbed Wells")
  
# melt data table from wide to long format -----
  
  dt_annual = melt(dt_annual, measure.vars = colnames(dt_annual)[2:5],
                   variable.name = "Source", value.name = "Value")
  
  dt_month = melt(dt_month, measure.vars = colnames(dt_month)[2:5],
                  variable.name = "Source", value.name = "Value")
  
  dt_annual = dt_annual[!is.na(Value)]
  dt_month = dt_month[!is.na(Value)]

# assign colors -----
  
  names(source.cols) = c("Gas Wells", "Oil Wells", "Shale Gas", "Coalbed Wells")

# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------
  
  setwd(out.loc) 
  
  
# ANNUAL AREA PLOT AND LINE PLOT -------------
  
  dt = copy(dt_annual)[, Source := factor(Source, levels = c("Gas Wells", "Oil Wells", "Shale Gas", "Coalbed Wells"))]
  xval = year(dt[, Date])
  yval = dt[, Value]/1000
  fillval = dt[, Source]
  tlab = "Annual U.S. Natural Gas Production by Source Well (1967 - 2016)"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Billion Cubic Feet"
  leglab = ""
  leg.ord = levels(fillval)
  plot.cols = source.cols
  
  area_annual = ggplot(dt, aes(x = xval, y = yval, fill = fillval)) + 
    geom_area(stat = "identity") +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         fill = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_fill_manual(breaks = leg.ord, values = plot.cols) +
    scale_x_continuous(breaks = seq(1967,2017,5), expand = c(0,0)) +
    scale_y_comma(expand = c(0.01,0)) +    
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.text = element_text(size = 13)) +
    theme(plot.margin = unit(c(1,1,1,1), "lines"))
  
  area_annual_2 <- ggplotGrob(area_annual)
  area_annual_2$layout$clip[area_annual_2$layout$name == "panel"] <- "off"
  grid.draw(area_annual_2)
  
  ggsave(area_annual_2, 
         filename = "NG_Natural Gas Production by Source Well_Annual_1967-2016_ATS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  line_annual = ggplot(dt, aes(x = xval, y = yval, color = fillval)) + 
    geom_line(stat = "identity", size = 0.7) +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         color = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_color_manual(breaks = leg.ord, values = plot.cols) + 
    scale_x_continuous(breaks = seq(1967,2017,5), expand = c(0,0)) +
    scale_y_comma(expand = c(0.01,0)) +
    geom_dl(aes(label = Source), method = list(dl.trans(x = x + .3), "last.bumpup", cex = 1.1, fontfamily = "Roboto Condensed")) +
    guides(color = FALSE) +
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
  # grid.draw(gt1)
  
  
  ggsave(line_annual_2, 
         filename = "NG_Natural Gas Production by Source Well_Annual_1967-2016_LTS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  
# MONTHLY AREA PLOT AND LINE PLOT -------------
  
  dt = copy(dt_month)[, Source := factor(Source, levels = c("Gas Wells", "Oil Wells", "Shale Gas", "Coalbed Wells"))]
  xval = (dt[, Date])
  yval = dt[, Value]/1000
  fillval = dt[, Source]
  tlab = "Monthly U.S. Natural Gas Production by Source Well (January 1991 - December 2016)"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Billion Cubic Feet"
  leglab = ""
  leg.ord = levels(fillval)
  plot.cols = source.cols
  
  area_month = ggplot(dt, aes(x = xval, y = yval, fill = fillval)) + 
    geom_area(stat = "identity") +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         fill = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_fill_manual(breaks = leg.ord, values = plot.cols) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y", expand = c(0,0)) +
    scale_y_comma(expand = c(0.01,0)) +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.text = element_text(size = 13, face = "bold")) +
    theme(plot.margin = unit(c(1,1,1,1), "lines"))
  
  ggsave(area_month, 
         filename = "NG_Natural Gas Production by Source Well_Monthly_Jan1991-Dec2016_ATS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  line_month = ggplot(dt, aes(x = xval, y = yval, color = fillval)) + 
    geom_line(stat = "identity", size = 0.7) +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         color = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_color_manual(breaks = leg.ord, values = plot.cols) + 
    scale_x_date(date_breaks = "5 years", date_labels = "%Y", expand = c(0,0)) +
    scale_y_continuous(expand = c(0.01,0)) +
    geom_dl(aes(label = Source), method = list(dl.trans(x = x + .3), "last.bumpup", cex = 1.1, fontfamily = "Roboto Condensed")) +
    guides(color = FALSE) +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.text = element_text(size = 13, face = "bold")) +
    theme(plot.margin = unit(c(1,8,1,1), "lines"))
  
  
  line_month_2 <- ggplotGrob(line_month)
  line_month_2$layout$clip[line_month_2$layout$name == "panel"] <- "off"
  # grid.draw(gt1)
  
  
  ggsave(line_month_2, 
         filename = "NG_Natural Gas Production by Source Well_Monthly_Jan1991-Dec2016_LTS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  
# LATEST YEAR BAR PLOT -------------
  
  dt = copy(dt_annual)[ Date == max(Date) ]
  xval = dt[, reorder(Source, Value)]
  yval = dt[, Value]/1000
  fillval = dt[, Source]
  tlab = "2016 U.S. Natural Gas Production by Source Well"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "X"
  xlab = NULL
  ylab = "Billion Cubic Feet"
  leglab = ""
  leg.ord = levels(fillval)
  plot.cols = source.cols
  
  bar_annual = ggplot(dt, aes(x = xval, y = yval, fill = fillval)) + 
    geom_bar(stat = "identity") +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         fill = leglab) +
    theme_ipsum_rc(grid = gval) + 
    coord_flip() + 
    scale_y_comma(expand = c(0,0)) +
    guides(fill = FALSE) +
    scale_fill_manual(breaks = leg.ord, values = plot.cols) +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.text = element_text(size = 13)) +
    theme(plot.margin = unit(c(1,1,1,1), "lines"))
  
  ggsave(bar_annual, 
         filename = "NG_Natural Gas Production by Source Well_Annual_2016_BP.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  