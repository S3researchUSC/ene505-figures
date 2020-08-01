# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

data.loc      = '/Users/MEAS/Google Drive/ta-materials/ENE505 - Fall 2015/ENE 505 Charts/data_2017-2018' # location of data file(s)
data.file     = 'Table_2.1_Energy_Consumption_by_Sector.xlsx' # data file to be used
out.loc       = '/Users/MEAS/Google Drive/ta-materials/ENE505 - Fall 2015/ENE 505 Charts/20180823' # location of where to save figures
sector.cols   = c("#fd6968", "#383e56", "#9ecffd", "#fbd55b", "#48be9d")

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

  dt_month = as.data.table(read.xlsx(data.file, sheet = "Monthly Data", startRow = 11, cols = c(1,3,5,7,9), detectDates = T))
  dt_month = dt_month[2:nrow(dt_month)]
  
  dt_annual = as.data.table(read.xlsx(data.file, sheet = "Annual Data", startRow = 11, cols = c(1,3,5,7,9), detectDates = T))
  dt_annual = dt_annual[2:nrow(dt_annual)]
  
# rename columns ----
  
  colnames(dt_month) = c("Month", "Residential", "Commercial", "Industrial", "Transportation")
  colnames(dt_annual) = c("Year", "Residential", "Commercial", "Industrial", "Transportation")
  
# melt data table from wide to long format -----
  
  dt_month = melt(dt_month, measure.vars = colnames(dt_month)[2:5],
                  variable.name = "Sector", value.name = "Value")
  
  dt_annual = melt(dt_annual, measure.vars = colnames(dt_annual)[2:5],
                   variable.name = "Sector", value.name = "Value")
  

# convert value column to numeric ----
  
  dt_month[, Value := as.numeric(Value)]
  dt_annual[, Value := as.numeric(Value)]
  
# remove NA month or year entries ----
  
  dt_month = dt_month[!is.na(Month)]
  dt_annual = dt_annual[!is.na(Year)]
  
# assign colors ------
  
  names(sector.cols) = c("Residential", "Commercial", "Industrial", "Transportation")
  

# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------

setwd(out.loc) 
  
# ANNUAL AREA PLOT AND LINE PLOT -------------
  
  dt = dt_annual[! Sector %in% c("Total")]
  xval = dt[, Year]
  yval = dt[, Value]
  fillval = dt[, Sector]
  tlab = "Annual U.S. Total Energy Consumption by Sector (1949 - 2017)"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Trillion BTU"
  leglab = ""
  leg.ord = levels(fillval)
  plot.cols = sector.cols
  
  area_annual = ggplot(dt, aes(x = xval, y = yval, fill = fillval)) + 
    geom_area(stat = "identity") +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         fill = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_fill_manual(breaks = leg.ord, values = plot.cols) +
    scale_x_continuous(breaks = seq(1950,2017,5), expand = c(0,0)) +
    scale_y_comma(expand = c(0.01,0)) +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.text = element_text(size = 13, face = "bold")) +
    theme(plot.margin = unit(c(1,1,1,1), "lines"))
  
  ggsave(area_annual, 
         filename = "Energy_Total Energy Consumption by Sector_Annual_1949-2017_ATS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  # leg.ord = levels(with(dt[Month == max(dt[,Month])], reorder(Sector, -Value)))
  
  line_annual = ggplot(dt, aes(x = xval, y = yval, color = fillval)) + 
    geom_line(stat = "identity", size = 0.7) +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         color = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_color_manual(breaks = leg.ord, values = plot.cols) + 
    scale_x_continuous(breaks = seq(1950,2017,5), expand = c(0,0)) +
    scale_y_comma(expand = c(0.01,0)) +
    geom_dl(aes(label = Sector), method = list(dl.trans(x = x + .3), "last.bumpup", cex = 1.1, fontfamily = "Roboto Condensed")) +
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
         filename = "Energy_Total Energy Consumption by Sector_Annual_1949-2017_LTS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  

# MONTHLY AREA PLOT AND LINE PLOT -------------

  dt = dt_month[! Sector %in% c("Total")]
  xval = dt[, Month]
  yval = dt[, Value]
  fillval = dt[, Sector]
  tlab = "Monthly U.S. Total Energy Consumption by Sector (January 1973 - April 2018)"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Trillion BTU"
  leglab = ""
  leg.ord = levels(fillval)
  plot.cols = sector.cols
  
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
         filename = "Energy_Total Energy Consumption by Sector_Monthly_Jan1973-Apr2018_ATS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  # leg.ord = levels(with(dt[Month == max(dt[,Month])], reorder(Sector, -Value)))

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
    scale_y_comma(expand = c(0.01,0)) +
    geom_dl(aes(label = Sector), method = list(dl.trans(x = x + .3), "last.bumpup", cex = 1.1, fontfamily = "Roboto Condensed")) +
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
         filename = "Energy_Total Energy Consumption by Sector_Monthly_Jan1973-Apr2018_LTS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  
# LATEST YEAR BAR PLOT -------------
  
  dt = dt_annual[ Year == max(Year) & !  Sector %in% c("Total")]
  xval = dt[, reorder(Sector, Value)]
  yval = dt[, Value]
  fillval = dt[, Sector]
  tlab = "2017 U.S. Total Energy Consumption by Sector"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "X"
  xlab = NULL
  ylab = "Trillion BTU"
  leglab = ""
  leg.ord = levels(fillval)
  plot.cols = sector.cols
  
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
            legend.text = element_text(size = 13, face = "bold")) +
    theme(plot.margin = unit(c(1,1,1,1), "lines"))

  ggsave(bar_annual, 
         filename = "Energy_Total Energy Consumption by Sector_Annual_2017_BP.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
