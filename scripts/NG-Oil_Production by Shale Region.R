# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

data.loc      = '/Users/MEAS/Google Drive/ta-materials/ENE505 - Fall 2015/ENE 505 Charts/data_2017-2018' # location of data file(s)
data.fil      = 'Drilling Productivity Report_August2018.xlsx'
out.loc       = '/Users/MEAS/Google Drive/ta-materials/ENE505 - Fall 2015/ENE 505 Charts/20180823'
reg.cols      = c("#b14c4d", "#577a97", "#649563", "#88638d", "#bf7f3f", "#cccc65", "#865e47" )
source.lines  = c("Oil" = "solid", "Natural Gas" = "dashed")
rig.lines     = c("Rig Count" = "dashed", "Production per Rig" = "solid")

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
  library(scales)

# load data ------

  setwd(data.loc)

  regions = getSheetNames(data.fil)
  regions = gsub(" Region", "", regions)
  regions = regions[1:7]
  
  list_oil = list()
    for (i in 1:length(regions)) {
      
      dt = as.data.table(read.xlsx(data.fil, sheet = i, startRow = 2, cols = c(1:5), detectDates = T))
      colnames(dt) = c("date", "rig_count", "production_per_rig", "legacy_production_change", "total_production" )
      
      dt[, region := regions[i]]
      
      list_oil[[i]] = dt
      
      rm(dt)
      
    }
  dt_oil = rbindlist(list_oil)
  
  
  list_ng = list()
    for (i in 1:length(regions)) {
      
      dt = as.data.table(read.xlsx(data.fil, sheet = i, startRow = 2, cols = c(1:2,6:8), detectDates = T))
      colnames(dt) = c("date", "rig_count", "production_per_rig", "legacy_production_change", "total_production" )
      
      dt[, region := regions[i]]
      
      list_ng[[i]] = dt
      
      rm(dt)
    }
  dt_ng = rbindlist(list_ng)
  
  
# melt data table from wide to long format -----
  
  # dt_ng = melt(dt_ng, measure.vars = colnames(dt_ng)[2:5], variable.name = "type", value.name = "value")
  
# aggregate data for yearly values ------
  
  # dt_oil[, year := year(date)]  
  # dt_ng[, year := year(date)]
  # 
  # oil_year = dt_oil[, lapply(.SD, mean, na.rm = TRUE), by = c("year", "region"), 
  #                      .SDcols = c("rig_count", "production_per_rig", "legacy_production_change", "total_production") ] 
  # 
  # 
  # ng_year = dt_ng[, lapply(.SD, mean, na.rm = TRUE), by = c("year", "region"), 
  #                      .SDcols = c("rig_count", "production_per_rig", "legacy_production_change", "total_production") ] 
  
# join data sets -----
  
  dt_oil = dt_oil[, c("date", "region", "rig_count", "production_per_rig", "total_production")]
  dt_ng = dt_ng[, c("date", "region", "production_per_rig", "total_production")]
  
  dt_all = dt_oil[dt_ng, on = c("date", "region"), nomatch = 0]
  colnames(dt_all)[3:7] = c("rig_count", "bbl_per_rig", "total_bbl", "mcf_per_rig", "total_mcf")
  
# convert units to mmbtu -----
  
  dt_all[, total_oil_mmbtu := total_bbl * 5.8]
  dt_all[, total_ng_mmbtu := total_mcf * 1.028]
  dt_all[, total_mmbtu := total_oil_mmbtu + total_ng_mmbtu]
  
  dt_all[, oil_mmbtu_per_rig := bbl_per_rig * 5.8]
  dt_all[, ng_mmbtu_per_rig := mcf_per_rig * 1.028]
  dt_all[, mmbtu_per_rig := oil_mmbtu_per_rig + ng_mmbtu_per_rig]
  
  # dt_all[, mmbtu_per_rig := total_mmbtu/rig_count]

# production only -----
  
  dt_prod = dt_all[, c("date", "region", "total_oil_mmbtu", "total_ng_mmbtu")]
  colnames(dt_prod)[3:4] = c("Oil", "Natural Gas")
  dt_prod = melt(dt_prod, measure.vars = colnames(dt_prod)[3:4], variable.name = "source", value.name = "value")
  
# rigs only ------
  
  dt_rig = dt_all[, c("date", "region", "rig_count", "mmbtu_per_rig")]
  colnames(dt_rig)[3:4] = c("Rig Count", "MMBTU per Rig")
  dt_rig = melt(dt_rig, measure.vars = colnames(dt_rig)[3:4], variable.name = "source", value.name = "value")
  
  dt_oil_rig = dt_all[, c("date", "region", "rig_count", "bbl_per_rig")]
  colnames(dt_oil_rig)[3:4] = c("Rig Count", "Production per Rig")
  dt_oil_rig = melt(dt_oil_rig, measure.vars = colnames(dt_oil_rig)[3:4], variable.name = "source", value.name = "value")
  
  dt_ng_rig = dt_all[, c("date", "region", "rig_count", "mcf_per_rig")]
  colnames(dt_ng_rig)[3:4] = c("Rig Count", "Production per Rig")
  dt_ng_rig = melt(dt_ng_rig, measure.vars = colnames(dt_ng_rig)[3:4], variable.name = "source", value.name = "value")
  
  
# set colors ------
  
  names(reg.cols) = regions
  
# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------
  
  setwd(out.loc) 
  
# (NG) DAILY AREA PLOT AND LINE PLOT -------------
  
  dt = copy(dt_ng)
  xval = (dt[, date])
  yval = dt[, total_production]/1000000
  fillval = dt[, region]
  tlab = "Daily U.S. Natural Gas Production by Shale Region (January 2007 - September 2018)"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Billion Cubic Feet per Day"
  leglab = ""
  leg.ord = levels(factor(fillval))
  plot.cols = reg.cols
  
  area_daily = ggplot(dt, aes(x = xval, y = yval, fill = fillval)) + 
    geom_area(stat = "identity") +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         fill = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_fill_manual(breaks = leg.ord, values = plot.cols) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = c(0,0)) +
    scale_y_comma(expand = c(0.01,0)) +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face = "bold"),
          axis.text.y = element_text(size = 15, face = "bold"),
          legend.text = element_text(size = 13)) +
    theme(plot.margin = unit(c(1,1,1,1), "lines"))
  
  ggsave(area_daily, 
         filename = "NG_Production by Shale Region_Daily_Jan2007-Sep2018_ATS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  line_daily = ggplot(dt, aes(x = xval, y = yval, color = fillval)) + 
    geom_line(stat = "identity", size = 0.7) +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         color = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_color_manual(breaks = leg.ord, values = plot.cols) + 
    scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = c(0,0)) +
    scale_y_comma(expand = c(0.02,0)) +
    geom_dl(aes(label = region), method = list(dl.trans(x = x + .3), "last.bumpup", cex = 1.1, fontfamily = "Roboto Condensed")) +
    guides(color = FALSE) +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.text = element_text(size = 13, face = "bold")) +
    theme(plot.margin = unit(c(1,8,1,1), "lines"))
  
  
  line_daily_2 <- ggplotGrob(line_daily)
  line_daily_2$layout$clip[line_daily_2$layout$name == "panel"] <- "off"
  # grid.draw(gt1)
  
  
  ggsave(line_daily_2, 
         filename = "NG_Production by Shale Region_Daily_Jan2007-Sep2018_LTS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  

# (OIL) DAILY AREA PLOT AND LINE PLOT -------------
  
  dt = copy(dt_oil)
  xval = (dt[, date])
  yval = dt[, total_production]/1000000
  fillval = dt[, region]
  tlab = "Daily U.S. Oil Production by Shale Region (January 2007 - September 2018)"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Million Barrels per Day"
  leglab = ""
  leg.ord = levels(factor(fillval))
  plot.cols = reg.cols
  
  area_daily = ggplot(dt, aes(x = xval, y = yval, fill = fillval)) + 
    geom_area(stat = "identity") +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         fill = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_fill_manual(breaks = leg.ord, values = plot.cols) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = c(0,0)) +
    scale_y_comma(expand = c(0.01,0)) +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face = "bold"),
          axis.text.y = element_text(size = 15, face = "bold"),
          legend.text = element_text(size = 13)) +
    theme(plot.margin = unit(c(1,1,1,1), "lines"))
  
  ggsave(area_daily, 
         filename = "Oil_Production by Shale Region_Daily_Jan2007-Sep2018_ATS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  line_daily = ggplot(dt, aes(x = xval, y = yval, color = fillval)) + 
    geom_line(stat = "identity", size = 0.7) +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         color = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_color_manual(breaks = leg.ord, values = plot.cols) + 
    scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = c(0,0)) +
    scale_y_comma(expand = c(0.02,0)) +
    geom_dl(aes(label = region), method = list(dl.trans(x = x + .3), "last.bumpup", cex = 1.1, fontfamily = "Roboto Condensed")) +
    guides(color = FALSE) +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.text = element_text(size = 13, face = "bold")) +
    theme(plot.margin = unit(c(1,8,1,1), "lines")) 
  
  
  line_daily_2 <- ggplotGrob(line_daily)
  line_daily_2$layout$clip[line_daily_2$layout$name == "panel"] <- "off"

  ggsave(line_daily_2, 
         filename = "Oil_Production by Shale Region_Daily_Jan2007-Sep2018_LTS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  
  
# (NG) DAILY SINGLE LINE PLOT OF RIG COUNT AND PRODUCTIVITY PER RIG -------
  
  tlab = "Daily U.S. Rig Count and Natural Gas Productivity (January 2007 - September 2018)"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Production per Rig (Thousand Barrels per Day)"
  ylab2 = "Rig Count (Number of Rigs)"
  leglab = ""
  plot.cols = reg.cols
  
  
  line_daily = ggplot() + 
    geom_line(data = dt_ng_rig[source == "Production per Rig"], size = 0.7, 
              aes(x = date, y = value/1000, color = region, linetype = source)) +
    geom_line(data = dt_ng_rig[source == "Rig Count"], size = 0.7, 
              aes(x = date, y = value/40, color = region, linetype = source)) +
    scale_color_manual(values = plot.cols) +
    scale_linetype_manual(values = rig.lines) +
    # facet_wrap( ~ region, ncol = 3, scales = "free") +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         color = leglab,
         linetype = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y", expand = c(0,0)) +
    scale_y_comma(expand = c(0.02,0), sec.axis = sec_axis(~.*40, name = ylab2, label = comma)) +
    # guides(color = FALSE) +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          legend.text = element_text(size = 13),
          strip.text.x = element_text(size = 14, face = "bold"),
          strip.text.y = element_text(size = 14, face = "bold")) +
    theme(plot.margin = unit(c(1,1,1,1), "lines"))
  
  ggsave(line_daily, 
         filename = "NG_Rig Count and Productivity_Daily_Jan2007-Sep2018_Single_LTS.png", 
         width = 12.75, 
         height = 7, 
         dpi = 400)
  
  
# (NG) DAILY FACET LINE PLOT OF RIG COUNT AND PRODUCTIVITY PER RIG -------
  
  tlab = "Daily U.S. Rig Count and Natural Gas Productivity (January 2007 - September 2018)"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Production per Rig (Thousand Barrels per Day)"
  ylab2 = "Rig Count (Number of Rigs)"
  leglab = ""
  plot.cols = reg.cols
  
  
  line_daily = ggplot() + 
    geom_line(data = dt_ng_rig[source == "Production per Rig"], size = 0.7, 
              aes(x = date, y = value/1000, color = region, linetype = source)) +
    geom_line(data = dt_ng_rig[source == "Rig Count"], size = 0.7, 
              aes(x = date, y = value/40, color = region, linetype = source)) +
    scale_color_manual(values = plot.cols) +
    scale_linetype_manual(values = rig.lines) +
    facet_wrap( ~ region, ncol = 3, scales = "free") +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         color = leglab,
         linetype = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y", expand = c(0,0)) +
    scale_y_comma(expand = c(0.02,0), sec.axis = sec_axis(~.*40, name = ylab2, label = comma)) +
    guides(color = FALSE) +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          legend.text = element_text(size = 13),
          strip.text.x = element_text(size = 14, face = "bold"),
          strip.text.y = element_text(size = 14, face = "bold")) +
    theme(plot.margin = unit(c(1,1,1,1), "lines"))
  
  ggsave(line_daily, 
         filename = "NG_Rig Count and Productivity_Daily_Jan2007-Sep2018_Facet_LTS.png", 
         width = 12.75, 
         height = 7, 
         dpi = 400)
  
  
# (OIL) DAILY SINGLE LINE PLOT OF RIG COUNT AND PRODUCTIVITY PER RIG -------
  
  tlab = "Daily U.S. Rig Count and Oil Productivity (January 2007 - September 2018)"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Production per Rig (Thousand Barrels per Day)"
  ylab2 = "Rig Count (Number of Rigs)"
  leglab = ""
  plot.cols = reg.cols
  
  
  line_daily = ggplot() + 
    geom_line(data = dt_oil_rig[source == "Production per Rig"], size = 0.7, 
              aes(x = date, y = value/1000, color = region, linetype = source)) +
    geom_line(data = dt_oil_rig[source == "Rig Count"], size = 0.7, 
              aes(x = date, y = value/400, color = region, linetype = source)) +
    scale_color_manual(values = plot.cols) +
    scale_linetype_manual(values = rig.lines) +
    # facet_wrap( ~ region, ncol = 3, scales = "free") +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         color = leglab,
         linetype = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y", expand = c(0,0)) +
    scale_y_comma(expand = c(0.02,0), sec.axis = sec_axis(~.*400, name = ylab2)) +
    # guides(color = FALSE) +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          legend.text = element_text(size = 13),
          strip.text.x = element_text(size = 14, face = "bold"),
          strip.text.y = element_text(size = 14, face = "bold")) +
    theme(plot.margin = unit(c(1,1,1,1), "lines"))
  
  ggsave(line_daily, 
         filename = "Oil_Rig Count and Productivity_Daily_Jan2007-Sep2018_Single_LTS.png", 
         width = 12.75, 
         height = 7, 
         dpi = 400)
  
  
# (OIL) DAILY FACET LINE PLOT OF RIG COUNT AND PRODUCTIVITY PER RIG -------
  
  tlab = "Daily U.S. Rig Count and Oil Productivity (January 2007 - September 2018)"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Production per Rig (Barrels per Day)"
  ylab2 = "Rig Count (Number of Rigs)"
  leglab = ""
  plot.cols = reg.cols
  
  
  line_daily = ggplot() + 
    geom_line(data = dt_oil_rig[source == "Production per Rig"], size = 0.7, 
              aes(x = date, y = value, color = region, linetype = source)) +
    geom_line(data = dt_oil_rig[source == "Rig Count"], size = 0.7, 
              aes(x = date, y = value*2, color = region, linetype = source)) +
    scale_color_manual(values = plot.cols) +
    scale_linetype_manual(values = rig.lines) +
    facet_wrap( ~ region, ncol = 3, scales = "free") +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         color = leglab,
         linetype = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y", expand = c(0,0)) +
    scale_y_comma(expand = c(0.02,0), sec.axis = sec_axis(~./2, name = ylab2)) +
    guides(color = FALSE) +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          legend.text = element_text(size = 13),
          strip.text.x = element_text(size = 14, face = "bold"),
          strip.text.y = element_text(size = 14, face = "bold")) +
    theme(plot.margin = unit(c(1,1,1,1), "lines"))
  
  ggsave(line_daily, 
         filename = "Oil_Rig Count and Productivity_Daily_Jan2007-Sep2018_Facet_LTS.png", 
         width = 12.75, 
         height = 7, 
         dpi = 400)
  

# (OIL + NG) DAILY SINGLE LINE PLOT OF TOTAL PRODUCTION -------
  
  tlab = "Daily U.S. Oil and Natural Gas Production by Shale Region (January 2007 - September 2018)"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "MMBTU per Day (Millions)"
  leglab = ""
  leg.ord = levels(factor(fillval))
  plot.cols = reg.cols
  
  line_daily = ggplot(data = dt_prod, aes(x = date, y = value/1000000, color = region, linetype = source)) + 
    geom_line(size = 0.7) +
    scale_color_manual(values = plot.cols) +
    scale_linetype_manual(values = source.lines) +
    # facet_wrap( ~ region, ncol = 3, scales = "free") +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         color = leglab,
         linetype = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y", expand = c(0,0)) +
    scale_y_comma(expand = c(0.02,0)) +
    # guides(color = FALSE) +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          legend.text = element_text(size = 13),
          strip.text.x = element_text(size = 14, face = "bold"),
          strip.text.y = element_text(size = 14, face = "bold")) +
    theme(plot.margin = unit(c(1,1,1,1), "lines"))
  
  ggsave(line_daily, 
         filename = "NG-Oil_Production by Shale Region_Daily_Jan2007-Sep2018_Single_LTS.png", 
         width = 12.75, 
         height = 7, 
         dpi = 400)
  
  
# (OIL + NG) DAILY FACET LINE PLOT OF TOTAL PRODUCTION -------
  
  tlab = "Daily U.S. Oil and Natural Gas Production by Shale Region (January 2007 - September 2018)"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "MMBTU per Day (Millions)"
  leglab = ""
  leg.ord = levels(factor(fillval))
  plot.cols = reg.cols
  
  line_daily = ggplot(data = dt_prod, aes(x = date, y = value/1000000, color = region, linetype = source)) + 
    geom_line(size = 0.7) +
    scale_color_manual(values = plot.cols) +
    scale_linetype_manual(values = source.lines) +
    facet_wrap( ~ region, ncol = 3, scales = "free") +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         color = leglab,
         linetype = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y", expand = c(0,0)) +
    scale_y_comma(expand = c(0.02,0)) +
    guides(color = FALSE) +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          legend.text = element_text(size = 13),
          strip.text.x = element_text(size = 14, face = "bold"),
          strip.text.y = element_text(size = 14, face = "bold")) +
    theme(plot.margin = unit(c(1,1,1,1), "lines"))
  
  ggsave(line_daily, 
         filename = "NG-Oil_Production by Shale Region_Daily_Jan2007-Sep2018_Facet_LTS.png", 
         width = 12.75, 
         height = 7, 
         dpi = 400)
  
