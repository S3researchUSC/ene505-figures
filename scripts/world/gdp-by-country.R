# plots generated:
  # 
  #
  #
  #
  #

# ------------------------------------ inputs ------------------------------------

  gdp_file      = "international-gdp-by-country-1980-2019.csv"

# ------------------------------------ outputs ------------------------------------
  
  out_gdp_2019  = "gdp-by-country-ranked-2019.csv"
  
# ------------------------------------ main script ------------------------------------
  
# load libraries -------

  library(data.table)
  library(lubridate)
  library(ggplot2)
  library(hrbrthemes)
  library(directlabels)
  library(grid)
  library(extrafont)
  library(cowplot)

# source color palettes and plot themes ------

  items = list.files(here::here('src'))
  sapply(here::here('src', items), source)
  
# load data ------
  
  # read in csv (without API column)
    gdp_data = fread(here::here('src', gdp_file), skip = 1, select = 2:42, header = T)
  
  # rename first column name
    setnames(gdp_data, 'V2', 'country')

  # extract first row as unit
    unit = gdp_data[1, country]
  
  # remove first row
    gdp_data = gdp_data[ ! country == unit ]
    
  # remove whitespace from country names
    gdp_data[, country := trimws(country, which = 'both')]
    
# melt data from wide to long format -----
    
  gdp_long = melt(gdp_data, measure.vars = as.character(1980:2019),
                  variable.name = 'year', value.name = 'gdp_billion_usd')
  
# convert year and gdp values to numeric type -----
    
  gdp_long[, year := as.numeric(as.character(year))]
  gdp_long[, gdp_billion_usd := as.numeric(gdp_billion_usd)]
  
# get top ten GDP in latest year -----
  
  gdp_most_recent = gdp_long[year == max(year) & ! country == 'World']
  setorder(gdp_most_recent, -'gdp_billion_usd', na.last = T)
  gdp_most_recent[, rank := 1:nrow(gdp_most_recent)]
  
  top_10_gdp = gdp_most_recent[rank %in% 1:10]
  
# export ranked gdp in 2019 to csv file ----
  
  fwrite(gdp_most_recent, here::here('data', out_gdp_2019), row.names = F)
