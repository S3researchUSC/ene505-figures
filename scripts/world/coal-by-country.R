# plots generated:
# 

# ------------------------------------ inputs ------------------------------------

  coal_file       = "international-coal-by-country-1980-2018.csv"
  gdp_file        = "gdp-by-country-ranked-2019.csv"

# ------------------------------------ main script ------------------------------------

# load libraries -------

  library(data.table)
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

  # read in csv of coal data
    coal_data = fread(here::here('data', coal_file), skip = 1, header = T)
  
  # rename V2 column
    setnames(coal_data, 'V2', 'country')
  
  # remove whitespace from country names
    coal_data[, country := trimws(country, which = 'both')]
    
  # read in csv of 2019 gdp by country
    gdp_2019 = fread(here::here('data', gdp_file), header = T)
  
# melt data from wide to long format -----
    
  coal_long = melt(coal_data, measure.vars = as.character(1980:2018),
                  variable.name = 'year', value.name = 'value')
    
# convert year and values to numeric type -----
    
  coal_long[, year := as.numeric(as.character(year))]
  coal_long[, value := as.numeric(value)]
  
# get countries with top 10 gdp -----
  
  top10_gdp = gdp_2019[rank %in% 1:10]
  
# label all non-top 10 countries as "Other" -----
  
  coal_long[, label := ifelse(country %in% c('World', top10_gdp[, country]),
                              country,
                              'Other')]
  
# categorize values by type using API -----
  
  coal_long[API %like% 'INTL.7-1', type := 'production']
  coal_long[API %like% 'INTL.7-2', type := 'consumption']
  coal_long[API %like% 'INTL.7-3', type := 'imports']
  coal_long[API %like% 'INTL.7-4', type := 'exports']
  coal_long[API %like% 'INTL.7-6', type := 'reserves']
  
# add units -----
  
  coal_long[, unit := ifelse(type == 'reserves',
                             'million short tons',
                             'thousand short tons')]

# remove rows that are just labels from the raw data ----
  
  coal_long = coal_long[!is.na(type)]
  
# aggregate by new country grouping -----
  
  coal_agg = coal_long[, .(value = sum(value, na.rm = T)), by = .(year, label, type, unit)]
  setcolorder(coal_agg, c('year', 'type', 'label', 'value', 'unit'))
