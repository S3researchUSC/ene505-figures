# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

file.loc 	    = '/Users/MEAS/GitHub/ene505-figures' # location of scripts
data.loc      = '/Users/MEAS/Google Drive/ta-materials/ENE505 - Fall 2015/ENE 505 Charts/data_2017-2018' # location of data file(s)
data.file     = 'Table_7.2b_Electricity_Net_Generation__Electric_Power_Sector.xlsx' # data file to be used
out.loc       = '/Users/MEAS/Google Drive/ta-materials/ENE505 - Fall 2015/ENE 505 Charts/20180823' # location of where to save figures
source.cols   = c("Geothermal" = "#6a3d9a",
                  "Solar" = "#ff7f00",
                  "Waste" = "#858585",
                  "Wood" = "#8c613c",
                  "Wind" = "#1f78b4",
                  "Hydroelectric" = "#a6cee3",
                  "Nuclear" = "#55a868",
                  "Petroleum" = "#fdbf6f",
                  "Coal" = "#12253d",
                  "Natural Gas" = "#c44e52",
                  "Other Gases" = "#fb9a99")

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

  dt_month = as.data.table(read.xlsx(data.file, sheet = "Monthly Data", startRow = 11, detectDates = T))
  dt_month = dt_month[2:nrow(dt_month)]
  
  dt_annual = as.data.table(read.xlsx(data.file, sheet = "Annual Data", startRow = 11, detectDates = T))
  dt_annual = dt_annual[2:nrow(dt_annual)]

# rename columns -----
  
  colnames(dt_month) = c("Month",
                        "Coal",
                        "Petroleum",
                        "Natural Gas",
                        "Other Gases",
                        "Nuclear",
                        "Pumped Storage",
                        "Hydroelectric",
                        "Wood",
                        "Waste",
                        "Geothermal",
                        "Solar",
                        "Wind",
                        "Total")
  
  colnames(dt_annual) = c("Year",
                          "Coal",
                          "Petroleum",
                          "Natural Gas",
                          "Other Gases",
                          "Nuclear",
                          "Pumped Storage",
                          "Hydroelectric",
                          "Wood",
                          "Waste",
                          "Geothermal",
                          "Solar",
                          "Wind",
                          "Total")
  

# remove pumped storage -----
  
  dt_month[, ("Pumped Storage") := NULL]
  dt_annual[, ("Pumped Storage") := NULL]

# melt data table from wide to long format -----
  
  dt_month = melt(dt_month, measure.vars = colnames(dt_month)[2:13],
                 variable.name = "MSN", value.name = "Value")
  
  dt_annual = melt(dt_annual, measure.vars = colnames(dt_annual)[2:13],
                  variable.name = "MSN", value.name = "Value")
  
# convert value column to numeric ----
  
  dt_month[, Value := as.numeric(Value)]
  dt_annual[, Value := as.numeric(Value)]
  
# remove NA month or year entries ----
  
  dt_month = dt_month[!is.na(Month)]
  dt_annual = dt_annual[!is.na(Year)]
  
# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------

setwd(out.loc) 
  
# ANNUAL AREA PLOT AND LINE PLOT -------------
  
  dt = dt_annual[ ! MSN %in% c("Total")][, MSN := factor(MSN, levels = c("Wind",
                                                                         "Solar",
                                                                         "Geothermal",
                                                                         "Waste",
                                                                         "Wood",
                                                                         "Hydroelectric",
                                                                         "Nuclear",
                                                                         "Other Gases",
                                                                         "Natural Gas",
                                                                         "Petroleum",
                                                                         "Coal"))]
  xval = dt[, Year]
  yval = dt[, Value]/1000
  fillval = dt[, MSN]
  tlab = "Annual U.S. Electricity Generation By Source (1949 - 2017)"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Kilowatt-hour (Billions)"
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
    scale_x_continuous(breaks = seq(1950,2017,5), limits = c(1949, 2017), expand = c(0,0)) +
    scale_y_continuous(expand = c(0.01,0)) +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.text = element_text(size = 13, face = "bold")) +
    theme(plot.margin = unit(c(1,1,1,1), "lines"))
  
  ggsave(area_annual, 
         filename = "Electricity_Net Electricity Gen by Source_Annual_1949-2017_ATS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  # leg.ord = levels(with(dt[Month == max(dt[,Month])], reorder(MSN, -Value)))
  
  line_annual = ggplot(dt, aes(x = xval, y = yval, color = fillval)) + 
    geom_line(stat = "identity", size = 0.7) +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         color = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_color_manual(breaks = leg.ord, values = plot.cols) + 
    scale_x_continuous(breaks = seq(1950,2017,5), limits = c(1949, 2017), expand = c(0,0)) +
    scale_y_continuous(expand = c(0.01,0)) +
    geom_dl(aes(label = MSN), method = list(dl.trans(x = x + .3), "last.bumpup", cex = 1.1, fontfamily = "Roboto Condensed")) +
    guides(color = FALSE) +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.text = element_text(size = 13, face = "bold")) +
    theme(plot.margin = unit(c(1,7,1,1), "lines"))
  
  
  line_annual_2 <- ggplotGrob(line_annual)
  line_annual_2$layout$clip[line_annual_2$layout$name == "panel"] <- "off"
  # grid.draw(gt1)
  
  
  ggsave(line_annual_2, 
         filename = "Electricity_Net Electricity Gen by Source_Annual_1949-2017_LTS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  

# MONTHLY AREA PLOT AND LINE PLOT -------------

  dt = dt_month[ ! MSN %in% c("Total")][, MSN := factor(MSN, levels = c("Wind",
                                                 "Solar",
                                                 "Geothermal",
                                                 "Waste",
                                                 "Wood",
                                                 "Hydroelectric",
                                                 "Nuclear",
                                                 "Other Gases",
                                                 "Natural Gas",
                                                 "Petroleum",
                                                 "Coal"))] 
  xval = dt[, Month]
  yval = dt[, Value]/1000
  fillval = dt[, MSN]
  tlab = "Monthly U.S. Electricity Generation By Source (January 1973 - April 2018)"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Kilowatt-hour (Billions)"
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
    scale_y_continuous(expand = c(0.01,0)) +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.text = element_text(size = 13, face = "bold")) +
    theme(plot.margin = unit(c(1,1,1,1), "lines"))
  
  ggsave(area_month, 
         filename = "Electricity_Net Electricity Gen By Source_Monthly_Jan1973-Apr2018_ATS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  # leg.ord = levels(with(dt[Month == max(dt[,Month])], reorder(MSN, -Value)))

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
    geom_dl(aes(label = MSN), method = list(dl.trans(x = x + .3), "last.bumpup", cex = 1.1, fontfamily = "Roboto Condensed")) +
    guides(color = FALSE) +
    theme(plot.margin = unit(c(1,7,1,1), "lines"))  +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.text = element_text(size = 13, face = "bold"))
  
  line_month_2 <- ggplotGrob(line_month)
  line_month_2$layout$clip[line_month_2$layout$name == "panel"] <- "off"
  # grid.draw(gt1)
  
  
  ggsave(line_month_2, 
         filename = "Electricity_Net Electricity Gen By Source_Monthly_Jan1973-Apr2018_LTS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  
# LATEST YEAR BAR PLOT -------------
  
  dt = dt_annual[ Year == max(Year) & ! MSN == "Total"]
  xval = dt[, reorder(MSN, Value)]
  yval = dt[, Value]/1000
  fillval = dt[, MSN]
  tlab = "2017 U.S. Electricity Generation By Source"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "X"
  xlab = NULL
  ylab = "Kilowatt-hour (Billions)"
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
            legend.text = element_text(size = 13, face = "bold")) +
    theme(plot.margin = unit(c(1,1,1,1), "lines"))

  ggsave(bar_annual, 
         filename = "Electricity_Net Electricity Gen by Source_Annual_2017_BP.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
# (RE ONLY) ANNUAL AREA PLOT AND LINE PLOT -------------
  
  dt = dt_annual[ ! MSN %in% c("Total", "Coal", "Petroleum", "Natural Gas", 
                               "Nuclear", "Other Gases")][, MSN := factor(MSN, levels = c("Wind",
                                                                                          "Solar",
                                                                                          "Geothermal",
                                                                                          "Waste",
                                                                                          "Wood",
                                                                                          "Hydroelectric",
                                                                                          "Nuclear",
                                                                                          "Other Gases",
                                                                                          "Natural Gas",
                                                                                          "Petroleum",
                                                                                          "Coal"))]
  xval = dt[, Year]
  yval = dt[, Value]/1000
  fillval = dt[, MSN]
  tlab = "Annual U.S. Renewable Electricity Generation By Source (1949 - 2017)"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Kilowatt-hour (Billions)"
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
    scale_x_continuous(breaks = seq(1950,2017,5), limits = c(1949, 2017), expand = c(0,0)) +
    scale_y_continuous(expand = c(0.01,0)) +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.text = element_text(size = 13, face = "bold")) +
    theme(plot.margin = unit(c(1,1,1,1), "lines"))
  
  ggsave(area_annual, 
         filename = "Electricity_Net Electricity Gen by Source_Annual_1949-2017_RE_ATS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  # leg.ord = levels(with(dt[Month == max(dt[,Month])], reorder(MSN, -Value)))
  
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
    scale_y_continuous(expand = c(0.01,0)) +
    geom_dl(aes(label = MSN), method = list(dl.trans(x = x + .3), "last.bumpup", cex = 1.1, fontfamily = "Roboto Condensed")) +
    guides(color = FALSE) +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.text = element_text(size = 13, face = "bold")) +
    theme(plot.margin = unit(c(1,7,1,1), "lines"))
  
  
  line_annual_2 <- ggplotGrob(line_annual)
  line_annual_2$layout$clip[line_annual_2$layout$name == "panel"] <- "off"
  # grid.draw(gt1)
  
  
  ggsave(line_annual_2, 
         filename = "Electricity_Net Electricity Gen by Source_Annual_1949-2017_RE_LTS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  
# (RE ONLY) MONTHLY AREA PLOT AND LINE PLOT -------------
  
  dt = dt_month[ ! MSN %in% c("Total", "Coal", "Petroleum", "Natural Gas", 
                              "Nuclear", "Other Gases")][, MSN := factor(MSN, levels = c("Wind",
                                                                                         "Solar",
                                                                                         "Geothermal",
                                                                                         "Waste",
                                                                                         "Wood",
                                                                                         "Hydroelectric",
                                                                                         "Nuclear",
                                                                                         "Other Gases",
                                                                                         "Natural Gas",
                                                                                         "Petroleum",
                                                                                         "Coal"))] 
  xval = dt[, Month]
  yval = dt[, Value]/1000
  fillval = dt[, MSN]
  tlab = "Monthly U.S. Renewable Electricity Generation By Source (January 1973 - April 2018)"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Kilowatt-hour (Billions)"
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
    scale_y_continuous(expand = c(0.01,0)) +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.text = element_text(size = 13, face = "bold")) +
    theme(plot.margin = unit(c(1,1,1,1), "lines"))
  
  ggsave(area_month, 
         filename = "Electricity_Net Electricity Gen By Source_Monthly_Jan1973-Apr2018_RE_ATS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  # leg.ord = levels(with(dt[Month == max(dt[,Month])], reorder(MSN, -Value)))
  
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
    geom_dl(aes(label = MSN), method = list(dl.trans(x = x + .3), "last.bumpup", cex = 1.1, fontfamily = "Roboto Condensed")) +
    guides(color = FALSE) +
    theme(plot.margin = unit(c(1,7,1,1), "lines"))  +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.text = element_text(size = 13, face = "bold"))
  
  line_month_2 <- ggplotGrob(line_month)
  line_month_2$layout$clip[line_month_2$layout$name == "panel"] <- "off"
  # grid.draw(gt1)
  
  
  ggsave(line_month_2, 
         filename = "Electricity_Net Electricity Gen By Source_Monthly_Jan1973-Apr2018_RE_LTS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  
# (RE ONLY) LATEST YEAR BAR PLOT -------------
  
  dt = dt_annual[ Year == max(Year) & !  MSN %in% c("Total", "Coal", "Petroleum", "Natural Gas", "Nuclear", "Other Gases")]
  xval = dt[, reorder(MSN, Value)]
  yval = dt[, Value]/1000
  fillval = dt[, MSN]
  tlab = "2017 U.S. Renewable Electricity Generation By Source"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "X"
  xlab = NULL
  ylab = "Kilowatt-hour (Billions)"
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
          legend.text = element_text(size = 13, face = "bold")) +
    theme(plot.margin = unit(c(1,1,1,1), "lines"))
  
  ggsave(bar_annual, 
         filename = "Electricity_Net Electricity Gen by Source_Annual_2017_RE_BP.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  
  
  