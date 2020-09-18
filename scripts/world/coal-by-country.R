# plots generated:
# 

# ------------------------------------ inputs ------------------------------------

  coal_file       = "international-coal-by-country-1980-2018.csv"

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
    
# melt data from wide to long format -----
    
  coal_long = melt(coal_data, measure.vars = as.character(1980:2018),
                  variable.name = 'year', value.name = 'value')
    
# convert year and values to numeric type -----
    
  coal_long[, year := as.numeric(as.character(year))]
  coal_long[, value := as.numeric(value)]
  
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
  
# rank values in each category (production, consumption, imports, exports, and reserves) in 2018 ------
  
  coal_max_year = coal_long[!is.na(value) & ! country == 'World']
  coal_max_year[, max_year := max(year), by = 'type']
  coal_max_year = coal_max_year[year == max_year]
  coal_max_year[, rank := frank(-value), by = 'type']
  rank_country = unique(coal_max_year[, c('country', 'type', 'rank')])
  
# merge with main coal data ----
  
  coal_ranked = merge(coal_long, rank_country, by = c('country', 'type'))

# label all non-top 5 countries as "Other" -----
  
  coal_ranked[, label := ifelse(rank %in% 1:5,
                                country,
                                'All Other Countries')]
  
# aggregate by new country grouping -----
  
  coal_agg = coal_ranked[, .(value = sum(value, na.rm = T)), by = .(year, label, type, unit)]
  setcolorder(coal_agg, c('year', 'type', 'label', 'value', 'unit'))
  
# get top 5 for each type -----
  
  top5_prod = rank_country[type == 'production' & rank %in% 1:5][order(rank), country]
  top5_cons = rank_country[type == 'consumption' & rank %in% 1:5][order(rank), country]
  top5_imp = rank_country[type == 'imports' & rank %in% 1:5][order(rank), country]
  top5_exp = rank_country[type == 'exports' & rank %in% 1:5][order(rank), country]
  top5_res = rank_country[type == 'reserves' & rank %in% 1:5][order(rank), country]
  