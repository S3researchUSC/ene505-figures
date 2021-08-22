# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

data.loc      = '/Users/MEAS/Google Drive/ta-materials/ENE505 - Fall 2015/ENE 505 Charts/data_2017-2018' # location of data file(s)
data.fil      = 'PET_MOVE_IMPCUS_A2_NUS_EP00_IM0_MBBLPD_A.xlsx'
# cntry.fil     = 'International_Petroleum Imports by Country_1980-2016.csv' 
# reg.fil       = 'International_Petroleum Imports by Country Group_1980-2016.csv'
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
  library(lubridate)

# read in data -------

  setwd(data.loc)
  
  alldata = as.data.table(read.xlsx(data.fil, sheet = "Data 1", startRow = 3, detectDates = TRUE))
  alldata[, Date := year(ymd(Date))]
  colnames(alldata)[1] = "Year"
  
  regdat = alldata[, c(1,4,20)]
  cntrydat = alldata[, c(1,5:19,21:137)]
  gdpdat = fread(gdp.fil, skip = 3, header = FALSE, drop = c(1,3), col.names = c("Country",  c(1980:2017)))

# melt data table from wide to long format -----
  
  regdat = melt(regdat, measure.vars = colnames(regdat)[2:3],
                 variable.name = "Region", value.name = "Value")
  regdat[, Region := gsub("U.S..Imports.from.", "", Region)]
  regdat[, Region := gsub(".Countries.of.Crude.Oil.and.Petroleum.Products.\\(Thousand.Barrels.per.Day\\)", "", Region)]
  
  cntrydat = melt(cntrydat, measure.vars = colnames(cntrydat)[2:133],
                   variable.name = "Country", value.name = "Value")
  cntrydat[, Country := gsub("U.S..Imports.from.", "", Country)]
  cntrydat[, Country := gsub(".of.Crude.Oil.and.Petroleum.Products.\\(Thousand.Barrels.per.Day\\)", "", Country)]
  cntrydat[, Country := gsub("\\.", " ", Country)]
  
  gdpdat = melt(gdpdat, measure.vars = colnames(gdpdat)[2:39],
                variable.name = "Year", value.name = "Value")
  
# convert years and values to numeric ----
  
  cntrydat[, Year := as.numeric(as.character(Year))]
  cntrydat[, Value := as.numeric(as.character(Value))]
  
  regdat[, Year := as.numeric(as.character(Year))]
  regdat[, Value := as.numeric(as.character(Value))]
  
  gdpdat[, Year := as.numeric(as.character(Year))]
  gdpdat[, Value := as.numeric(as.character(Value))]
  
# determine which countries to plot based on highest latest hydro, gen, and gdp -----
  
  cntrydat_rec = cntrydat[ Year == 2015 & !is.na(Value)]
  setorder(cntrydat_rec, -Value)
  
  gdpdat_rec = gdpdat[ Year == 2015 & !is.na(Value) & ! Country == "United States"]
  setorder(gdpdat_rec, -Value)
  
# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------
  
  setwd(out.loc) 
  
  # (REGIONS) ANNUAL AREA PLOT AND LINE PLOT OF PETROLEUM IMPORTS -------------
  
  dt = regdat
  xval = dt[, Year]
  yval = dt[, Value]
  fillval = dt[, Region]
  tlab = "Annual Petroleum Imports to U.S. by Region (1973 - 2017)"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Thousand Barrels per Day"
  leglab = ""
  leg.ord = levels(factor(fillval))
  plot.cols = reg.cols
  
  area_imports = ggplot(dt, aes(x = xval, y = yval, fill = fillval)) + 
    geom_area(stat = "identity") +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         fill = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_fill_manual(breaks = leg.ord, values = plot.cols) +
    scale_x_continuous(breaks = seq(1973,2017,5), expand = c(0,0), limits = c(1973,2017)) +
    scale_y_comma(expand = c(0.01,0)) +
    theme(plot.title = element_text(size = 21, face = "bold"),
          plot.subtitle = element_text(size = 15),
          axis.title.x = element_text(size = 17, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.text = element_text(size = 13, face = "bold")) +
    theme(plot.margin = unit(c(1,1,1,1), "lines"))
  
  ggsave(area_imports, 
         filename = "World_Petroleum Imports by Region_Annual_1973-2017_ATS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  line_imports = ggplot(dt, aes(x = xval, y = yval, color = fillval)) + 
    geom_line(stat = "identity", size = 0.7) +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         color = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_color_manual(breaks = leg.ord, values = plot.cols) + 
    scale_x_continuous(breaks = seq(1973,2017,5), expand = c(0,0), limits = c(1973,2017)) +
    scale_y_comma(expand = c(0.02,0)) +
    geom_dl(aes(label = Region), method = list(dl.trans(x = x + .3), "last.bumpup", cex = 1.1, fontfamily = "Roboto Condensed")) +
    guides(color = FALSE) +
    theme(plot.title = element_text(size = 21, face = "bold"),
          plot.subtitle = element_text(size = 15),
          axis.title.x = element_text(size = 17, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.text = element_text(size = 13, face = "bold")) +
    theme(plot.margin = unit(c(1,10,1,1), "lines"))
  
  line_imports_2 <- ggplotGrob(line_imports)
  line_imports_2$layout$clip[line_imports_2$layout$name == "panel"] <- "off"
  # grid.draw(gt1)
  
  
  ggsave(line_imports_2, 
         filename = "World_Petroleum Imports by Region_Annual_1973-2017_LTS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  
  # (COUNTRIES WITH TOP 10 IMPORTS) ANNUAL AREA PLOT AND LINE PLOT OF PETROLEUM IMPORTS -------------
  
  dt = cntrydat[ Country %in% cntrydat_rec[, Country][1:10]]
  xval = dt[, Year]
  yval = dt[, Value]
  fillval = dt[, Country]
  tlab = "Annual Petroleum Imports to U.S. by Country (1973 - 2017)"
  sublab = "Countries with 10 Highest Petroleum Imports in 2015 \nData: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Thousand Barrels per Day"
  leglab = ""
  leg.ord = levels(factor(fillval))
  plot.cols = cntry.cols
  
  area_imports = ggplot(dt, aes(x = xval, y = yval, fill = fillval)) + 
    geom_area(stat = "identity") +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         fill = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_fill_manual(breaks = leg.ord, values = plot.cols) +
    scale_x_continuous(breaks = seq(1973,2017,5), expand = c(0,0), limits = c(1973,2017)) +
    scale_y_comma(expand = c(0.01,0)) +
    theme(plot.title = element_text(size = 21, face = "bold"),
          plot.subtitle = element_text(size = 15),
          axis.title.x = element_text(size = 17, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.text = element_text(size = 13, face = "bold")) +
    theme(plot.margin = unit(c(1,1,1,1), "lines"))
  
  ggsave(area_imports, 
         filename = "World_Petroleum Imports by Country_Annual_1973-2017_ATS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  line_imports = ggplot(dt, aes(x = xval, y = yval, color = fillval)) + 
    geom_line(stat = "identity", size = 0.7) +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         color = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_color_manual(breaks = leg.ord, values = plot.cols) + 
    scale_x_continuous(breaks = seq(1973,2017,5), expand = c(0,0), limits = c(1973,2017)) +
    scale_y_comma(expand = c(0.02,0)) +
    geom_dl(aes(label = Country), method = list(dl.trans(x = x + .3), "last.bumpup", cex = 1.1, fontfamily = "Roboto Condensed")) +
    guides(color = FALSE) +
    theme(plot.title = element_text(size = 21, face = "bold"),
          plot.subtitle = element_text(size = 15),
          axis.title.x = element_text(size = 17, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.text = element_text(size = 13, face = "bold")) +
    theme(plot.margin = unit(c(1,10,1,1), "lines"))
  
  line_imports_2 <- ggplotGrob(line_imports)
  line_imports_2$layout$clip[line_imports_2$layout$name == "panel"] <- "off"
  # grid.draw(gt1)
  
  
  ggsave(line_imports_2, 
         filename = "World_Petroleum Imports by Country_Annual_1973-2017_LTS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  # (COUNTRIES WITH TOP 10 GDP) ANNUAL AREA PLOT AND LINE PLOT OF PETROLEUM IMPORTS -------------
  # 
  # dt = cntrydat[ Country %in% gdpdat_rec[, Country][1:10]]
  # xval = dt[, Year]
  # yval = dt[, Value]
  # fillval = dt[, Country]
  # tlab = "Annual Petroleum Imports to U.S. by Country (1973 - 2017)"
  # sublab = "Countries with 10 Highest GDP in 2015 \nData: U.S. Energy Information Administration"
  # gval = "Y"
  # xlab = NULL
  # ylab = "Thousand Barrels per Day"
  # leglab = ""
  # leg.ord = levels(factor(fillval))
  # plot.cols = cntry.cols
  # 
  # area_imports = ggplot(dt, aes(x = xval, y = yval, fill = fillval)) + 
  #   geom_area(stat = "identity") +
  #   labs(title = tlab,
  #        subtitle = sublab, 
  #        x = xlab,
  #        y = ylab,
  #        fill = leglab) +
  #   theme_ipsum_rc(grid = gval) +
  #   scale_fill_manual(breaks = leg.ord, values = plot.cols) +
  #   scale_x_continuous(breaks = seq(1973,2017,5), expand = c(0,0), limits = c(1973,2017)) +
  #   scale_y_comma(expand = c(0.01,0)) +
  #   theme(plot.title = element_text(size = 21, face = "bold"),
  #         plot.subtitle = element_text(size = 15),
  #         axis.title.x = element_text(size = 17, face = "bold"),
  #         axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
  #         axis.text.x = element_text(size = 15, face="bold"),
  #         axis.text.y = element_text(size = 15, face="bold"),
  #         legend.text = element_text(size = 13, face = "bold")) +
  #   theme(plot.margin = unit(c(1,1,1,1), "lines"))
  # 
  # ggsave(area_imports, 
  #        filename = "World_Petroleum Imports by Country_GDP_Annual_1973-2017_ATS.png", 
  #        width = 11.75, 
  #        height = 6.25, 
  #        dpi = 400)
  # 
  # line_imports = ggplot(dt, aes(x = xval, y = yval, color = fillval)) + 
  #   geom_line(stat = "identity", size = 0.7) +
  #   labs(title = tlab,
  #        subtitle = sublab, 
  #        x = xlab,
  #        y = ylab,
  #        color = leglab) +
  #   theme_ipsum_rc(grid = gval) +
  #   scale_color_manual(breaks = leg.ord, values = plot.cols) + 
  #   scale_x_continuous(breaks = seq(1973,2017,5), expand = c(0,0), limits = c(1973,2017)) +
  #   scale_y_comma(expand = c(0.02,0)) +
  #   geom_dl(aes(label = Country), method = list(dl.trans(x = x + .3), "last.bumpup", cex = 1.1, fontfamily = "Roboto Condensed")) +
  #   guides(color = FALSE) +
  #   theme(plot.title = element_text(size = 21, face = "bold"),
  #         plot.subtitle = element_text(size = 15),
  #         axis.title.x = element_text(size = 17, face = "bold"),
  #         axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
  #         axis.text.x = element_text(size = 15, face="bold"),
  #         axis.text.y = element_text(size = 15, face="bold"),
  #         legend.text = element_text(size = 13, face = "bold")) +
  #   theme(plot.margin = unit(c(1,10,1,1), "lines"))
  # 
  # line_imports_2 <- ggplotGrob(line_imports)
  # line_imports_2$layout$clip[line_imports_2$layout$name == "panel"] <- "off"
  # # grid.draw(gt1)
  # 
  # 
  # ggsave(line_imports_2, 
  #        filename = "World_Petroleum Imports by Country_GDP_Annual_1973-2017_LTS.png", 
  #        width = 11.75, 
  #        height = 6.25, 
  #        dpi = 400)
  # 
  # 
