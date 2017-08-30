# Petroleum and NG_Production by Shale Region_2007-2015 #

# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

file.loc 	    = '/Users/MEAS/GitHub/ene505-figures' # location of scripts
data.loc      = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts' # location of data file(s)
data.file     = c('Petroleum and NG_Combined Production by Region_2007_2015.csv') # data file to be used
out.loc       = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts/20170827' # location of where to save figures
reg.cols      = c("Bakken" = "hotpink1",
                  "Eagle Ford" = "mediumpurple3",
                  "Haynesville" = "darkgoldenrod2", 
                  "Marcellus" = "brown3",
                  "Niobrara" = "springgreen4",
                  "Permian" = "steelblue3",
                  "Utica" = "hotpink4")

# ---------------------------------------------------------------
# MAIN SCRIPT ---------------------------------------------------
# ---------------------------------------------------------------

# load libraries -------
  library(data.table)
  library(ggplot2)
  library(hrbrthemes)
  library(lubridate)
  library(tidyr)

# load plot functions -----
  setwd(file.loc)
  source("plotfunctions.R")

# import data ------
  setwd(data.loc)
  dt_raw = fread(data.file, header = T)[2:106]

# rename columns -----
  colnames(dt_raw) = c("Date",
                       "Bakken-Oil",
                       "Eagle Ford-Oil",
                       "Haynesville-Oil",
                       "Marcellus-Oil",
                       "Niobrara-Oil",
                       "Permian-Oil",
                       "Utica-Oil",
                       "Bakken-NG",
                       "Eagle Ford-NG",
                       "Haynesville-NG",
                       "Marcellus-NG",
                       "Niobrara-NG",
                       "Permian-NG",
                       "Utica-NG")
  
# melt data from wide format to long -----
  dt_long = melt(dt_raw, measure.vars = list(2:15), variable.name = "region_source", value.name = "value")
  
# split shale region and source into separate columns -----
  dt_long[, c("region", "source") := tstrsplit(region_source, "-", type.convert = TRUE, fixed = TRUE)]
  dt_long[ source == "NG", source := "Natural Gas"]
  
# convert month column into date format -----
  dt_long[, day := mdy(Date)]

# convert values to numeric -----
  dt_long[, value := as.numeric(gsub(",", "", value))]

# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------
  
  setwd(out.loc) 
  
  # LINE PLOT -------------
  
  dt = copy(dt_long)
  dt = dt[, source := factor(source, levels = rev(levels(factor(source))))]
  
  area_petro_ng_production = ggplot(dt, aes(x = day, y = value, color = region, linetype = source)) + 
    geom_line(stat = "identity", size = 0.7) +
    labs(title = "2007 - 2014 Daily U.S. Oil and Natural Gas Production by Shale Region",
         subtitle = "Data: U.S. Energy Information Administration", 
         x = NULL,
         y = "Trillion BTU/Day",
         color = "Shale Region",
         linetype = "Source") +
    theme_ipsum_rc(grid = "Y") +
    scale_color_manual(values = reg.cols) +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.title = element_text(size = 13, face = "bold"),
          legend.text = element_text(size = 12)) + 
    scale_x_date(breaks = seq(as.Date("2007-01-01"), as.Date("2015-12-01"), by = "2 years"), date_labels = "%b %Y", expand = c(0,0)) +
    scale_y_comma(breaks = seq(0,18,3), limits = c(0,18), expand = c(0,0))
  
  ggsave(area_petro_ng_production, 
         filename = "Petroleum and NG_Production by Shale Region_2007-2014_LTS.png", 
         width = 11.1, 
         height = 6.25, 
         dpi = 400)
  