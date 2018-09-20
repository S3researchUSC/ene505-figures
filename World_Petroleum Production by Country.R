# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

data.loc      = '/Users/MEAS/Google Drive/ta-materials/ENE505 - Fall 2015/ENE 505 Charts/data_2017-2018' # location of data file(s)
data.fil      = 'International_Petroleum Production by Country Group_1980-2017.csv' 
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
  
  proddat = fread(data.fil, skip = 4, nrows = 233, header = FALSE, drop = c(1,3), col.names = c("Country",  c(1980:2017)))
  gdpdat = fread(gdp.fil, skip = 3, header = FALSE, drop = c(1,3), col.names = c("Country",  c(1980:2017)))

# melt data table from wide to long format -----

  proddat = melt(proddat, measure.vars = colnames(proddat)[2:39],
                 variable.name = "Year", value.name = "Value")
  
  gdpdat = melt(gdpdat, measure.vars = colnames(gdpdat)[2:39],
                variable.name = "Year", value.name = "Value")

# convert years and values to numeric ----

  proddat[, Year := as.numeric(as.character(Year))]
  proddat[, Value := as.numeric(as.character(Value))]
  
  gdpdat[, Year := as.numeric(as.character(Year))]
  gdpdat[, Value := as.numeric(as.character(Value))]

# determine which countries to plot based on highest latest hydro, gen, and gdp -----
  
  proddat_rec = proddat[ Year == 2015 & !is.na(Value)]
  setorder(proddat_rec, -Value)
  
  gdpdat_rec = gdpdat[ Year == 2015 & !is.na(Value)]
  setorder(gdpdat_rec, -Value)

# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------

  setwd(out.loc) 

# (COUNTRIES WITH TOP 10 PROD) ANNUAL AREA PLOT AND LINE PLOT OF PETRO PROD -------------

  dt = proddat[ Country %in% proddat_rec[, Country][1:10]]
  xval = dt[, Year]
  yval = dt[, Value]/1000
  fillval = dt[, Country]
  tlab = "Annual Petroleum Production by Country (1980 - 2017)"
  sublab = "Countries with 10 Highest Petroleum Production in 2015 \nData: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Million Barrels per Day"
  leglab = ""
  leg.ord = levels(factor(fillval))
  plot.cols = cntry.cols

  area_cons = ggplot(dt, aes(x = xval, y = yval, fill = fillval)) + 
    geom_area(stat = "identity") +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         fill = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_fill_manual(breaks = leg.ord, values = plot.cols) +
    scale_x_continuous(breaks = seq(1980,2017,5), expand = c(0,0), limits = c(1980,2017)) +
    scale_y_comma(expand = c(0.01,0)) +
    theme(plot.title = element_text(size = 21, face = "bold"),
          plot.subtitle = element_text(size = 15),
          axis.title.x = element_text(size = 17, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.text = element_text(size = 13, face = "bold")) +
    theme(plot.margin = unit(c(1,1,1,1), "lines"))

ggsave(area_cons, 
       filename = "World_Petroleum Production by Country_Most_Annual_1980-2017_ATS.png", 
       width = 11.75, 
       height = 6.25, 
       dpi = 400)

line_cons = ggplot(dt, aes(x = xval, y = yval, color = fillval)) + 
  geom_line(stat = "identity", size = 0.7) +
  labs(title = tlab,
       subtitle = sublab, 
       x = xlab,
       y = ylab,
       color = leglab) +
  theme_ipsum_rc(grid = gval) +
  scale_color_manual(breaks = leg.ord, values = plot.cols) + 
  scale_x_continuous(breaks = seq(1980,2017,5), expand = c(0,0), limits = c(1980,2017)) +
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

line_cons_2 <- ggplotGrob(line_cons)
line_cons_2$layout$clip[line_cons_2$layout$name == "panel"] <- "off"
# grid.draw(gt1)


ggsave(line_cons_2, 
       filename = "World_Petroleum Production by Country_Most_Annual_1980-2017_LTS.png", 
       width = 11.75, 
       height = 6.25, 
       dpi = 400)

# (COUNTRIES WITH TOP 10 GDP) ANNUAL AREA PLOT AND LINE PLOT OF HYDRO GEN -------------

  dt = proddat[ Country %in% gdpdat_rec[, Country][1:10]]
  xval = dt[, Year]
  yval = dt[, Value]/1000
  fillval = dt[, Country]
  tlab = "Annual Petroleum Production by Country (1980 - 2017)"
  sublab = "Countries with 10 Highest GDP in 2015 \nData: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Million Barrels per Day"
  leglab = ""
  leg.ord = levels(factor(fillval))
  plot.cols = cntry.cols
  
  area_cons = ggplot(dt, aes(x = xval, y = yval, fill = fillval)) + 
    geom_area(stat = "identity") +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         fill = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_fill_manual(breaks = leg.ord, values = plot.cols) +
    scale_x_continuous(breaks = seq(1980,2017,5), expand = c(0,0), limits = c(1980,2017)) +
    scale_y_comma(expand = c(0.01,0)) +
    theme(plot.title = element_text(size = 21, face = "bold"),
          plot.subtitle = element_text(size = 15),
          axis.title.x = element_text(size = 17, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.text = element_text(size = 13, face = "bold")) +
    theme(plot.margin = unit(c(1,1,1,1), "lines"))
  
  ggsave(area_cons, 
         filename = "World_Petroleum Production by Country_GDP_Annual_1980-2017_ATS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  line_cons = ggplot(dt, aes(x = xval, y = yval, color = fillval)) + 
    geom_line(stat = "identity", size = 0.7) +
    labs(title = tlab,
         subtitle = sublab, 
         x = xlab,
         y = ylab,
         color = leglab) +
    theme_ipsum_rc(grid = gval) +
    scale_color_manual(breaks = leg.ord, values = plot.cols) + 
    scale_x_continuous(breaks = seq(1980,2017,5), expand = c(0,0), limits = c(1980,2017)) +
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
  
  line_cons_2 <- ggplotGrob(line_cons)
  line_cons_2$layout$clip[line_cons_2$layout$name == "panel"] <- "off"
  # grid.draw(gt1)
  
  
  ggsave(line_cons_2, 
         filename = "World_Petroleum Production by Country_GDP_Annual_1980-2017_LTS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  
