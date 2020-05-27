# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------
data.loc      = '/data_2017-2018' # location of data file(s)
exp.file      = 'Table_1.4b_Primary_Energy_Exports_by_Source_and_Total_Net_Imports.xlsx' # energy export data
imp.file      = 'Table_1.4a_Primary_Energy_Imports_by_Source.xlsx' # energy mport data
out.loc       = '/figures' # location of where to save figures
source.cols   = c("Biomass" = "#8c613c",
                  "Hydroelectric" = "#a6cee3",
                  "Nuclear" = "#55a868",
                  "Petroleum Products" = "#e59024",
                  "Crude Oil" = "#fdbf6f",
                  "Coal" = "#12253d",
                  "Coal Coke" = "#858585",
                  "Natural Gas" = "#c44e52",
                  "Total Primary Energy" = "#fc8d62",
                  "Total Petroleum" = "#383e56",
                  "Electricity" = "#ff9f9b",
                  "Total Energy Net Imports" = "#8da0cb")

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
  library(extrafont)
  # font_import() # run these next two lines if you want to save images as pdf (since i use roboto condensed)
  # loadfonts()

# get file location as working directory -----

  current.fil = getActiveDocumentContext()$path 
  current.loc = dirname(current.fil)
  setwd(dirname(current.fil))

# set working directory as data file location ------

  setwd(paste0(current.loc, data.loc))

# import data ------

  dt_exp = as.data.table(read.xlsx(exp.file, sheet = "Annual Data", startRow = 11, detectDates = T))
  dt_exp = dt_exp[2:nrow(dt_exp)]
  
  dt_imp = as.data.table(read.xlsx(imp.file, sheet = "Annual Data", startRow = 11, detectDates = T))
  dt_imp = dt_imp[2:nrow(dt_imp)]

# rename columns ----

  colnames(dt_exp)[1] = c("Year")
  colnames(dt_imp)[1] = c("Year")

# melt data table from wide to long format -----

  dt_exp = melt(dt_exp, measure.vars = colnames(dt_exp)[2:11],
                variable.name = "MSN", value.name = "Value")
  
  dt_imp = melt(dt_imp, measure.vars = colnames(dt_imp)[2:10],
                   variable.name = "MSN", value.name = "Value")

# rename MSN factor levels -------

  dt_exp[, MSN := revalue(MSN, c('Biomass.Exports' = "Biomass Exports",
                                 'Coal.Exports' = "Coal Exports",
                                 'Coal.Coke.Exports' = "Coal Coke Exports",
                                 'Natural.Gas.Exports' = "Natural Gas Exports",
                                 'Crude.Oil.Exports' = "Crude Oil Exports",
                                 "Petroleum.Products,.Excluding.Biofuels,.Exports" = "Petroleum Products Exports",
                                 "Total.Petroleum,.Excluding.Biofuels,.Exports" = "Total Petroleum Exports", 
                                 'Electricity.Exports' = "Electricity Exports",
                                 'Total.Energy.Exports' = "Total Primary Energy Exports",
                                 'Total.Energy.Net.Imports' = 'Total Energy Net Imports Exports'))]
  
  dt_imp[, MSN := revalue(MSN, c('Biomass.Imports' = "Biomass Imports",
                                 'Coal.Imports' = "Coal Imports",
                                 'Coal.Coke.Imports' = "Coal Coke Imports",
                                 'Natural.Gas.Imports' = "Natural Gas Imports",
                                 'Crude.Oil.Imports' = "Crude Oil Imports",
                                 "Petroleum.Products,.Excluding.Biofuels,.Imports" = "Petroleum Products Imports",
                                 "Total.Petroleum,.Excluding.Biofuels,.Imports" = "Total Petroleum Imports", 
                                 'Electricity.Imports' = "Electricity Imports",
                                 'Total.Primary.Energy.Imports' = "Total Primary Energy Imports"))]

  
# create new column for fuel/source only ------
  
  dt_exp[, source := gsub(" Exports", "", MSN)] # replace the " Exports" phrase with blank
  dt_imp[, source := gsub(" Imports", "", MSN)] # replace the " Imports" phrase with blank
  
# convert value column to numeric ----
  
  dt_exp[, Value := as.numeric(Value)]
  dt_imp[, Value := as.numeric(Value)]
  
# remove NA month or year entries ----
  
  dt_exp = dt_exp[!is.na(Year)]
  dt_imp = dt_imp[!is.na(Year)]
  
# change imports data to negative ------
  
  dt_imp[, Value := -Value]
  
# combine exports and imports data into one data table --------
  
  dt_all = rbindlist(list(dt_exp, dt_imp))

# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------
  
  setwd(paste0(current.loc, out.loc))
  
  # this is to create another data table that is just a subset of the complete data table (removing 'Total Petroleum', 'Total Primary Energy', and 'Total Energy Net Imports') and then reorganizing the source/fuels
  dt = dt_all[! source %in% c("Total Petroleum",
                              "Total Primary Energy",
                              "Total Energy Net Imports")][, source := factor(source, 
                                                                              levels = c("Biomass",
                                                                                         "Electricity",
                                                                                         "Natural Gas",
                                                                                         "Petroleum Products",
                                                                                         "Crude Oil",
                                                                                         "Coal Coke",
                                                                                         "Coal"))]
  
  area_annual = ggplot(dt, aes(x = as.integer(Year), y = Value, group = MSN, fill = source)) + 
    geom_area() +
    theme_ipsum_rc(grid = "Y")+
    labs(title = "Annual U.S. Primary Energy Exports and Imports by Source (1949 - 2017)",
         subtitle = "Data: U.S. Energy Information Administration", 
         x = NULL,
         y = "Quadrillion BTU",
         fill = "") +
    theme_ipsum_rc(grid = "Y") +
    annotate("text", x = 1965, y = 8, label = "Energy exports on positive axis", 
             size = 7, family = "Roboto Condensed") + 
    annotate("text", x = 1965, y = -25, label = "Energy imports on negative axis",
             size = 7, family = "Roboto Condensed") + 
    scale_fill_manual(values = source.cols) +
    scale_x_continuous(breaks = seq(1950,2017,5), expand = c(0,0)) +
    scale_y_continuous(expand = c(0.01,0)) +
    theme(plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.title.x = element_text(size = 24, hjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 24, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 18, face="bold"),
          axis.text.y = element_text(size = 18, face="bold"),
          legend.text = element_text(size = 14, face = "bold")) +
    theme(plot.margin = unit(c(1,1,1,1), "lines")) 
  area_annual  
  
  # save as png
  ggsave(filename = paste0("Energy_Primary Energy Exports and Imports by Source_Annual_1949-2017_ATS.png"),
         plot = area_annual,
         width = 11.75, 
         height = 6.25,
         dpi = 800)
  