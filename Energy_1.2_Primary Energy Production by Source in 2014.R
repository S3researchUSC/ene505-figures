########## Primary Energy Production by Source (2014) ###########

# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

file.loc    = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts' # location of data file(s)
data.file   = 'Energy_1.2_Primary Energy Production by Source_2014.csv' # data file to be used
fuel.cols   = c("Geothermal" = "#880015",
               "Solar PV" = "#ff7f27",
               "Wind" = "#3f48cc",
               "Hydroelectric" = "#00baff",
               "Biomass" = "#b5734e",
               "Nuclear" = "#22b14c",
               "Crude Oil" = "#fff200",
               "Coal" = "#000000",
               "Natural Gas" = "#ed1c24")
out.loc   = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts/20170826' # location of where to save figures

# ---------------------------------------------------------------
# MAIN SCRIPT ---------------------------------------------------
# ---------------------------------------------------------------


# load libraries -------
library(data.table)
library(ggplot2)
library(hrbrthemes)

# import data (skipping the first 10 rows) ------
dt_raw = fread(data.file, skip = 10, header = T)

# rename columns ------
colnames(dt_raw) <- c("Year",
                     "Coal",
                     "Natural Gas (Dry)",
                     "Crude Oil",
                     "Natural Gas (Liquid)",
                     "Total Fossil Fuels",
                     "Nuclear",
                     "Hydroelectric",
                     "Geothermal",
                     "Solar PV",
                     "Wind",
                     "Biomass",
                     "Total Renewable",
                     "Total Primary")

# convert data table from wide to long ------
dt_long = melt(dt_raw, measure.vars = list(2:14), variable.name = "fuel", value.name = "value")
dt_long[, value := as.numeric(value)]
dt_long[, units := "Quadrillion Btu"] # create column for units
dt_long = dt_long[! value == "(Quadrillion Btu)"] # remove rows with "(Quadrillion Btu)" as values

# sum natural gas values ------
dt_long[, source := fuel] # create new column named "source"
dt_long[ fuel %like% "Natural Gas", source := "Natural Gas"] # for natural gas fuels, make them have the same "source"
dt_long = dt_long[, .(value = sum(value)), by = c("source")] # aggregate (sum) data by "source


# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------

setwd(out.loc) 


bar_PE_prod_2014 = ggplot(dt_long[ ! source %in% c("Total Renewable", "Total Primary", "Total Fossil Fuels")], 
                          aes(x = reorder(source, value), y = value, fill = source)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "2014 U.S. Annual Primary Energy Production by Source",
       subtitle = "Data: U.S. Energy Information Administration", 
       x = "",
       y = "Btu (Quads)",
       fill = "") +
  theme_ipsum_rc(grid = "X") +
  scale_fill_manual(values = fuel.cols) +
  scale_y_continuous(breaks = seq(0,30,5), expand = c(0,0)) +
  theme(plot.title = element_text(size = 25, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.text.x = element_text(size = 18, face="bold"),
        axis.text.y = element_text(size = 18, face="bold"),
        legend.text = element_text(size = 14, face = "bold"))

ggsave(bar_PE_prod_2014, filename = "Energy_1.2_Primary Energy Production by Source in 2014_BP.png", width = 14.5, height = 8.16, dpi = 400)
