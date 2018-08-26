# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

data.loc      = '/Users/MEAS/Google Drive/ta-materials/ENE505 - Fall 2015/ENE 505 Charts/data_2017-2018' # location of data file(s)
reg.fil       = 'International_Primary Energy by Region_1980-2015.csv' 
cntry.fil     = 'International_Primary Energy by Country_1980-2015.csv' 
gdp.fil       = 'International_Gross Domestic Product_1980-2015.csv'
out.loc       = '/Users/MEAS/Google Drive/ta-materials/ENE505 - Fall 2015/ENE 505 Charts/20180823' # location of where to save figures
reg.cols      = c("#4C72B0", "#DD8452", "#55A868", "#C44E52", "#8172B3", "#937860", "#DA8BC3", "#8C8C8C", "#CCB974", "#64B5CD")
cntry.cols    = c("#0173B2", "#DE8F05", "#029E73", "#D55E00", "#CC78BC", "#CA9161", "#FBAFE4", "#949494", "#ECE133", "#56B4E9")

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

# read in data -------

  setwd(data.loc)

  dt_reg = fread(reg.fil, header = FALSE, drop = c(1,3), col.names = c("Region",  c(1980:2015)))
  dt_country = fread(cntry.fil, header = FALSE, drop = c(1,3), col.names = c("Region",  c(1980:2015)))
  
  dt_gdp = fread(gdp.fil, header = FALSE, drop = c(1,3), col.names = c("Region",  c(1980:2017)))

# melt data table from wide to long format -----
  
  dt_reg = melt(dt_reg, measure.vars = colnames(dt_reg)[2:37],
                variable.name = "Year", value.name = "Value")
  
  dt_country = melt(dt_country, measure.vars = colnames(dt_country)[2:37],
                    variable.name = "Year", value.name = "Value")
  
  dt_gdp = melt(dt_gdp, measure.vars = colnames(dt_gdp)[2:39],
                variable.name = "Year", value.name = "Value")
  
  
# convert years and values to numeric ----
  
  dt_reg[, Year := as.numeric(as.character(Year))]
  dt_reg[, Value := as.numeric(as.character(Value))]
  
  dt_country[, Year := as.numeric(as.character(Year))]
  dt_country[, Value := as.numeric(as.character(Value))]
  
  dt_gdp[, Year := as.numeric(as.character(Year))]
  dt_gdp[, Value := as.numeric(as.character(Value))]
  
# determine which countries to plot based on gdp -----
  
  dt_gdp = dt_gdp[ Year == max(Year) & !is.na(Value)]
  setorder(dt_gdp, -Value)
  
# combine datasets ------
  
  pltcountries = c("United States", "China")
  dt_all = rbindlist(list(dt_reg, dt_country[ Region %in% pltcountries ]))
  
# assign colors ------
  
  names(reg.cols) = levels(factor(dt_reg[, Region]))
  names(reg.cols)[9:10] = pltcountries
  
  # names(cntry.cols) = dt_gdp[, Region][1:10]
  
# convert years and values to numeric ----
  
  dt_all[, Year := as.numeric(as.character(Year))]
  dt_all[, Value := as.numeric(as.character(Value))]
  
  
# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------
  
  setwd(out.loc) 
  
  # (REGIONS ONLY) ANNUAL AREA PLOT AND LINE PLOT -------------
  
  dt = dt_reg[ ! Region %in% c("World")]
  xval = dt[, Year]
  yval = dt[, Value]
  fillval = dt[, Region]
  tlab = "Annual Primary Energy Consumption by Region (1980 - 2015)"
  sublab = "Data: EIA International Energy Statistics"
  gval = "Y"
  xlab = NULL
  ylab = "Quad BTU"
  leglab = ""
  leg.ord = levels(fillval)
  plot.cols = reg.cols
  
  area_annual = ggplot(dt, aes(x = xval, y = yval, fill = fillval)) + 
    geom_area(stat = "identity") +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         fill = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_fill_manual(values = plot.cols) +
    scale_x_continuous(breaks = seq(1980,2015,5), expand = c(0,0)) +
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
         filename = "World_Primary Energy Consumption by Region_Annual_1980-2015_ATS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  dt = dt_reg
  xval = dt[, Year]
  yval = dt[, Value]
  fillval = dt[, Region]
  tlab = "Annual Primary Energy Consumption by Region (1980 - 2015)"
  sublab = "Data: EIA International Energy Statistics"
  gval = "Y"
  xlab = NULL
  ylab = "Quad BTU"
  leglab = ""
  leg.ord = levels(fillval)
  plot.cols = reg.cols
  

  line_annual = ggplot(dt, aes(x = xval, y = yval, color = fillval)) + 
    geom_line(stat = "identity", size = 0.7) +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         color = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_color_manual(breaks = leg.ord, values = plot.cols) + 
    scale_x_continuous(breaks = seq(1980,2015,5), expand = c(0,0)) +
    scale_y_comma(expand = c(0.01,0)) +
    geom_dl(aes(label = Region), method = list(dl.trans(x = x + .3), "last.bumpup", cex = 1.1, fontfamily = "Roboto Condensed")) +
    guides(color = FALSE) +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.text = element_text(size = 13, face = "bold")) +
    theme(plot.margin = unit(c(1,10,1,1), "lines"))
  
  line_annual_2 <- ggplotGrob(line_annual)
  line_annual_2$layout$clip[line_annual_2$layout$name == "panel"] <- "off"
  # grid.draw(gt1)
  
  
  ggsave(line_annual_2, 
         filename = "World_Primary Energy Consumption by Region_Annual_1980-2015_LTS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  # (COUNTRIES WITH TOP 10 GDP ONLY) ANNUAL AREA PLOT AND LINE PLOT -------------
  
  dt = dt_country[ Region %in% dt_gdp[, Region][1:10]]
  xval = dt[, Year]
  yval = dt[, Value]
  fillval = dt[, Region]
  tlab = "Annual Primary Energy Consumption by Country (1980 - 2015)"
  sublab = "Countries with 10 Highest GDP \n Data: EIA International Energy Statistics"
  gval = "Y"
  xlab = NULL
  ylab = "Quad BTU"
  leglab = ""
  leg.ord = levels(factor(fillval))
  plot.cols = cntry.cols
  
  area_annual = ggplot(dt, aes(x = xval, y = yval, fill = fillval)) + 
    geom_area(stat = "identity") +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         fill = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_fill_manual(breaks = leg.ord, values = plot.cols) +
    scale_x_continuous(breaks = seq(1980,2015,5), expand = c(0,0)) +
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
         filename = "World_Primary Energy Consumption by Country_Annual_1980-2015_ATS.png", 
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
    scale_x_continuous(breaks = seq(1980,2015,5), expand = c(0,0)) +
    scale_y_comma(expand = c(0.02,0)) +
    geom_dl(aes(label = Region), method = list(dl.trans(x = x + .3), "last.bumpup", cex = 1.1, fontfamily = "Roboto Condensed")) +
    guides(color = FALSE) +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.text = element_text(size = 13, face = "bold")) +
    theme(plot.margin = unit(c(1,10,1,1), "lines"))
  
  line_annual_2 <- ggplotGrob(line_annual)
  line_annual_2$layout$clip[line_annual_2$layout$name == "panel"] <- "off"
  # grid.draw(gt1)
  
  
  ggsave(line_annual_2, 
         filename = "World_Primary Energy Consumption by Country_Annual_1980-2015_LTS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  
  
  
  
  
  # (BOTH) ANNUAL AREA PLOT AND LINE PLOT -------------
  
  rm(dt)
  dt = dt_all
  xval = dt[, Year]
  yval = dt[, Value]
  fillval = dt[, Region]
  tlab = "Annual Primary Energy Consumption by Region (1980 - 2015)"
  sublab = "Data: EIA International Energy Statistics"
  gval = "Y"
  xlab = NULL
  ylab = "Quad BTU"
  leglab = ""
  leg.ord = levels(fillval)
  plot.cols = reg.cols
  
  
  line_annual = ggplot(dt, aes(x = xval, y = yval, color = fillval)) + 
    geom_line(stat = "identity", size = 0.7) +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         color = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_color_manual(breaks = leg.ord, values = plot.cols) + 
    scale_x_continuous(breaks = seq(1980,2015,5), expand = c(0,0)) +
    scale_y_comma(expand = c(0.01,0)) +
    geom_dl(aes(label = Region), method = list(dl.trans(x = x + .3), "last.bumpup", cex = 1.1, fontfamily = "Roboto Condensed")) +
    guides(color = FALSE) +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.text = element_text(size = 13, face = "bold")) +
    theme(plot.margin = unit(c(1,10,1,1), "lines"))
  
  line_annual_2 <- ggplotGrob(line_annual)
  line_annual_2$layout$clip[line_annual_2$layout$name == "panel"] <- "off"
  # grid.draw(gt1)
  
  
  ggsave(line_annual_2, 
         filename = "World_Primary Energy Consumption by Region and Country_Annual_1980-2015_LTS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  