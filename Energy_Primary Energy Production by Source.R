# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

data.loc      = '/data_2017-2018' # location of data file(s)
data.file     = 'Table_1.2_Primary_Energy_Production_by_Source.xlsx' # data file to be used
out.loc       = '/figures' # location of where to save figures
source.cols   = c("Geothermal" = "#6a3d9a",
                  "Solar" = "#ff7f00",
                  "Waste" = "#858585",
                  "Biomass" = "#8c613c",
                  "Wind" = "#1f78b4",
                  "Hydroelectric" = "#a6cee3",
                  "Nuclear" = "#55a868",
                  "Crude Oil" = "#fdbf6f",
                  "Coal" = "#12253d",
                  "Natural Gas (Dry)" = "#c44e52",
                  "Natural Gas (Liquid)" = "#fb9a99",
                  "Total Primary Energy" = "#fc8d62",
                  "Total Fossil Fuels" = "#383e56",
                  "Total Renewable Energy" = "#66c2a5")

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
  library(rstudioapi) 

# get file location as working directory -----

  current.fil = getActiveDocumentContext()$path 
  current.loc = dirname(current.fil)
  setwd(dirname(current.fil))

# set working directory as data file location ------

  setwd(paste0(current.loc, data.loc))

# load data ------

  dt_month = as.data.table(read.xlsx(data.file, sheet = "Monthly Data", startRow = 11, detectDates = T))
  dt_month = dt_month[2:nrow(dt_month)]
  
  dt_annual = as.data.table(read.xlsx(data.file, sheet = "Annual Data", startRow = 11, detectDates = T))
  dt_annual = dt_annual[2:nrow(dt_annual)]
  
# rename column ----
  
  colnames(dt_annual)[1] = "Year"
  
# melt data table from wide to long format -----
  
  dt_month = melt(dt_month, measure.vars = colnames(dt_month)[2:14],
                  variable.name = "MSN", value.name = "Value")
  
  dt_annual = melt(dt_annual, measure.vars = colnames(dt_annual)[2:14],
                   variable.name = "MSN", value.name = "Value")
  
# rename MSN factor levels -------
  
  dt_month[, MSN := revalue(MSN, c('Biomass.Energy.Production' = "Biomass",
                                   'Coal.Production' = "Coal",
                                   'Total.Fossil.Fuels.Production' = "Total Fossil Fuels",
                                   'Geothermal.Energy.Production' = "Geothermal",
                                   'Hydroelectric.Power.Production' = "Hydroelectric",
                                   'Natural.Gas.(Dry).Production' = "Natural Gas (Dry)",
                                   'Natural.Gas.Plant.Liquids.Production' = "Natural Gas (Liquid)",
                                   'Nuclear.Electric.Power.Production' = "Nuclear",
                                   'Crude.Oil.Production' = "Crude Oil",
                                   'Total.Renewable.Energy.Production' = "Total Renewable Energy",
                                   'Solar.Energy.Production' = "Solar",
                                   'Total.Primary.Energy.Production' = "Total Primary Energy",
                                   'Wind.Energy.Production' = "Wind"))]
 
  dt_annual[, MSN := revalue(MSN, c('Biomass.Energy.Production' = "Biomass",
                                    'Coal.Production' = "Coal",
                                    'Total.Fossil.Fuels.Production' = "Total Fossil Fuels",
                                    'Geothermal.Energy.Production' = "Geothermal",
                                    'Hydroelectric.Power.Production' = "Hydroelectric",
                                    'Natural.Gas.(Dry).Production' = "Natural Gas (Dry)",
                                    'Natural.Gas.Plant.Liquids.Production' = "Natural Gas (Liquid)",
                                    'Nuclear.Electric.Power.Production' = "Nuclear",
                                    'Crude.Oil.Production' = "Crude Oil",
                                    'Total.Renewable.Energy.Production' = "Total Renewable Energy",
                                    'Solar.Energy.Production' = "Solar",
                                    'Total.Primary.Energy.Production' = "Total Primary Energy",
                                    'Wind.Energy.Production' = "Wind"))]
  

# convert value column to numeric ----
  
  dt_month[, Value := as.numeric(Value)]
  dt_annual[, Value := as.numeric(Value)]
  
# remove NA month or year entries ----
  
  dt_month = dt_month[!is.na(Month)]
  dt_annual = dt_annual[!is.na(Year)]
  

# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------

  setwd(paste0(current.loc, out.loc))
  
# ANNUAL AREA PLOT AND LINE PLOT -------------
  
  dt = dt_annual[! MSN %in% c("Total Fossil Fuels",
                              "Total Renewable Energy",
                              "Total Primary Energy")][, MSN := factor(MSN, levels = c("Wind",
                                                       "Solar",
                                                       "Geothermal",
                                                       "Biomass",
                                                       "Hydroelectric",
                                                       "Nuclear",
                                                       "Natural Gas (Liquid)",
                                                       "Natural Gas (Dry)",
                                                       "Crude Oil",
                                                       "Coal"))]
  xval = dt[, Year]
  yval = dt[, Value]
  fillval = dt[, MSN]
  tlab = "Annual U.S. Primary Energy Production by Source (1949 - 2017)"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Quadrillion BTU"
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
    # scale_color_manual(breaks = leg.ord, values = plot.cols) +
    scale_x_continuous(breaks = seq(1950,2017,5), expand = c(0,0)) +
    scale_y_continuous(expand = c(0.01,0)) +    
    # geom_dl(aes(label = MSN), position = "stack", method = list(dl.trans(x = x + .3), "last.bumpup", cex = 1.1, fontfamily = "Roboto Condensed")) +
    # guides(fill = FALSE, color = FALSE) +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.text = element_text(size = 13, face = "bold")) +
    theme(plot.margin = unit(c(1,1,1,1), "lines"))
  
  area_annual_2 <- ggplotGrob(area_annual)
  area_annual_2$layout$clip[area_annual_2$layout$name == "panel"] <- "off"
  grid.draw(area_annual_2)
  
  ggsave(area_annual_2, 
         filename = "Energy_Primary Energy Production by Source_Annual_1949-2017_ATS.png", 
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
    theme(plot.margin = unit(c(1,8,1,1), "lines"))
  
  
  line_annual_2 <- ggplotGrob(line_annual)
  line_annual_2$layout$clip[line_annual_2$layout$name == "panel"] <- "off"

  ggsave(line_annual_2, 
         filename = "Energy_Primary Energy Production by Source_Annual_1949-2017_LTS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  

# MONTHLY AREA PLOT AND LINE PLOT -------------

  dt = dt_month[! MSN %in% c("Total Fossil Fuels",
                              "Total Renewable Energy",
                              "Total Primary Energy")][, MSN := factor(MSN, levels = c("Wind",
                                                                                       "Solar",
                                                                                       "Geothermal",
                                                                                       "Biomass",
                                                                                       "Hydroelectric",
                                                                                       "Nuclear",
                                                                                       "Natural Gas (Liquid)",
                                                                                       "Natural Gas (Dry)",
                                                                                       "Crude Oil",
                                                                                       "Coal"))]
  xval = dt[, Month]
  yval = dt[, Value]
  fillval = dt[, MSN]
  tlab = "Monthly U.S. Primary Energy Production By Source (January 1973 - April 2018)"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Quadrillion BTU"
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
         filename = "Energy_Primary Energy Production by Source_Monthly_Jan1973-Apr2018_ATS.png", 
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
    geom_dl(aes(label = MSN), method = list(dl.trans(x = x + .3), "last.bumpup", cex = 1.1, fontfamily = "Roboto Condensed")) +
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

  ggsave(line_month_2, 
         filename = "Energy_Primary Energy Production by Source_Monthly_Jan1973-Apr2018_LTS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  
# LATEST YEAR BAR PLOT -------------
  
  dt = dt_annual[ Year == max(Year) & ! MSN %in% c("Total Fossil Fuels",
                                                         "Total Renewable Energy",
                                                         "Total Primary Energy")]
  xval = dt[, reorder(MSN, Value)]
  yval = dt[, Value]
  fillval = dt[, MSN]
  tlab = "2017 U.S. Primary Energy Production By Source"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "X"
  xlab = NULL
  ylab = "Quadrillion BTU"
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
         filename = "Energy_Primary Energy Production by Source_Annual_2017_BP.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
# (TOTALS ONLY) ANNUAL AREA PLOT AND LINE PLOT -------------
  
  dt = dt_annual[ MSN %in% c("Total Fossil Fuels",
                              "Total Renewable Energy")][, MSN := factor(MSN, levels = c("Total Renewable Energy", 
                                                                                         "Total Fossil Fuels"))]
  xval = dt[, Year]
  yval = dt[, Value]
  fillval = dt[, MSN]
  tlab = "Annual U.S. Primary Energy Production by Source (1949 - 2017)"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Quadrillion BTU"
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
    scale_x_continuous(breaks = seq(1950,2017,5), expand = c(0,0)) +
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
         filename = "Energy_Primary Energy Production by Source_Annual_1949-2017_Totals_ATS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  dt = dt_annual[ MSN %in% c("Total Fossil Fuels",
                             "Total Renewable Energy",
                             "Total Primary Energy")][, MSN := factor(MSN, levels = c("Total Renewable Energy", 
                                                                                      "Total Fossil Fuels",
                                                                                      "Total Primary Energy"))]
  xval = dt[, Year]
  yval = dt[, Value]
  fillval = dt[, MSN]
  tlab = "1949 - 2017 Annual U.S. Primary Energy Production by Source"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Quadrillion BTU"
  leglab = ""
  leg.ord = levels(fillval)
  plot.cols = source.cols
  
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
    theme(plot.margin = unit(c(1,10,1,1), "lines"))
  
  
  line_annual_2 <- ggplotGrob(line_annual)
  line_annual_2$layout$clip[line_annual_2$layout$name == "panel"] <- "off"

  ggsave(line_annual_2, 
         filename = "Energy_Primary Energy Production by Source_Annual_1949-2017_Totals_LTS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  
  
# (TOTALS ONLY) MONTHLY AREA PLOT AND LINE PLOT -------------
  
  dt = dt_month[ MSN %in% c("Total Fossil Fuels",
                             "Total Renewable Energy")][, MSN := factor(MSN, levels = c("Total Renewable Energy", 
                                                                                        "Total Fossil Fuels"))]
  xval = dt[, Month]
  yval = dt[, Value]
  fillval = dt[, MSN]
  tlab = "Monthly U.S. Primary Energy Production By Source (January 1973 - April 2018)"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Quadrillion BTU"
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
         filename = "Energy_Primary Energy Production by Source_Monthly_Jan1973-Apr2018_Totals_ATS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  
  dt = dt_month[ MSN %in% c("Total Fossil Fuels",
                             "Total Renewable Energy",
                             "Total Primary Energy")][, MSN := factor(MSN, levels = c("Total Renewable Energy", 
                                                                                      "Total Fossil Fuels",
                                                                                      "Total Primary Energy"))]
  xval = dt[, Month]
  yval = dt[, Value]
  fillval = dt[, MSN]
  tlab = "Monthly U.S. Primary Energy Production By Source (January 1973 - April 2018)"
  sublab = "Data: U.S. Energy Information Administration"
  gval = "Y"
  xlab = NULL
  ylab = "Quadrillion BTU"
  leglab = ""
  leg.ord = levels(fillval)
  plot.cols = source.cols
  
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
    theme(plot.margin = unit(c(1,10,1,1), "lines"))  +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 17, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 15, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          legend.text = element_text(size = 13, face = "bold"))
  
  line_month_2 <- ggplotGrob(line_month)
  line_month_2$layout$clip[line_month_2$layout$name == "panel"] <- "off"

  ggsave(line_month_2, 
         filename = "Energy_Primary Energy Production by Source_Monthly_Jan1973-Apr2018_Totals_LTS.png", 
         width = 11.75, 
         height = 6.25, 
         dpi = 400)
  